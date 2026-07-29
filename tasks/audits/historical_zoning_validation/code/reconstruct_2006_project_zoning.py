# setwd("tasks/audits/historical_zoning_validation/code")

import json
import re

import pandas as pd


def normalize_application(value) -> str | None:
    if pd.isna(value):
        return None
    normalized = re.sub(r"[^A-Z0-9]", "", str(value).upper())
    return normalized or None


def split_groups(value) -> list[str]:
    if pd.isna(value) or not str(value).strip():
        return []
    return sorted({part.strip() for part in str(value).split(";") if part.strip()})


def map_keys(value) -> list[str]:
    if pd.isna(value):
        return []
    text = str(value).upper().replace("NUMBER", " ").replace("NO.", " ")
    matches = re.findall(r"\b([0-9IL]{1,2})\s*-?\s*([A-Z0-9])\b", text)
    return sorted(
        {
            f"{number.replace('I', '1').replace('L', '1')}-{suffix.replace('1', 'I')}"
            for number, suffix in matches
        }
    )


def historical_origins(clause_json: str, final_group: str) -> list[str]:
    try:
        transitions = json.loads(clause_json)
    except (TypeError, json.JSONDecodeError):
        return []

    reverse_edges = {}
    for transition in transitions:
        destination = transition.get("destination_group")
        for source in transition.get("source_groups", []):
            reverse_edges.setdefault(destination, set()).add(source)

    ancestors = set()
    frontier = [final_group]
    while frontier:
        current = frontier.pop()
        for parent in reverse_edges.get(current, set()):
            if parent not in ancestors:
                ancestors.add(parent)
                frontier.append(parent)

    roots = {
        ancestor
        for ancestor in ancestors
        if not reverse_edges.get(ancestor, set()) - {ancestor}
    }
    if roots:
        return sorted(roots)
    return sorted(reverse_edges.get(final_group, set()))


projects = pd.read_csv(
    "../output/historical_zoning_project_comparison.csv",
    dtype={"pin": "string"},
    low_memory=False,
)
historical = pd.read_csv(
    "../output/historical_zoning_ordinances_2006_2012.csv", low_memory=False
)
modern = pd.read_csv(
    "../output/historical_zoning_matter_parsing.csv", low_memory=False
)
project_maps = pd.read_csv(
    "../output/historical_zoning_project_map_grids.csv", dtype={"pin": "string"}
)
projects = projects.merge(
    project_maps[["pin", "zoning_map_grid"]],
    on="pin",
    how="left",
    validate="one_to_one",
)

historical["event_date"] = pd.to_datetime(
    historical["journal_meeting_date"], errors="coerce"
)
historical["application_key"] = historical["application_key"].map(normalize_application)
historical["event_map_keys"] = historical["map_numbers_raw"].map(map_keys)
historical_events = historical.assign(event_source="clerk_journal_full_ordinance")[
    [
        "event_date",
        "application_key",
        "event_source",
        "journal_filename",
        "journal_pdf_url",
        "pdf_page_number",
        "map_numbers_raw",
        "event_map_keys",
        "source_groups",
        "destination_groups",
        "clause_count",
        "clause_transitions_json",
        "common_addresses",
        "bounded_areas",
        "ordinance_excerpt",
    ]
].copy()

modern["event_date"] = pd.to_datetime(modern["matter_passed_date"], errors="coerce")
modern["application_key"] = modern["application_key"].map(normalize_application)
modern_events = modern.loc[modern["event_date"] >= pd.Timestamp("2010-11-01")].copy()
modern_events["event_source"] = "elms_ordinance"
modern_events["journal_filename"] = pd.NA
modern_events["journal_pdf_url"] = pd.NA
modern_events["pdf_page_number"] = pd.NA
modern_events["map_numbers_raw"] = pd.NA
modern_events["event_map_keys"] = [[] for _ in range(len(modern_events))]
modern_events["source_groups"] = modern_events["from_groups"]
modern_events["destination_groups"] = modern_events["to_groups"]
modern_events["clause_count"] = modern_events["amendment_clause_count"]
modern_events["clause_transitions_json"] = pd.NA
modern_events["common_addresses"] = modern_events["address_raw"]
modern_events["bounded_areas"] = pd.NA
modern_events["ordinance_excerpt"] = modern_events["matter_title"]
modern_events = modern_events[
    [
        "event_date",
        "application_key",
        "event_source",
        "journal_filename",
        "journal_pdf_url",
        "pdf_page_number",
        "map_numbers_raw",
        "event_map_keys",
        "source_groups",
        "destination_groups",
        "clause_count",
        "clause_transitions_json",
        "common_addresses",
        "bounded_areas",
        "ordinance_excerpt",
    ]
]

events = pd.concat([historical_events, modern_events], ignore_index=True)
events = events.loc[
    events["event_date"].between("2006-01-01", "2012-10-31")
    & events["application_key"].notna()
].copy()
events["source_priority"] = events["event_source"].map(
    {"clerk_journal_full_ordinance": 1, "elms_ordinance": 2}
)
events = events.sort_values(
    ["event_date", "application_key", "source_priority"], kind="stable"
)

anchor_date_text = projects["ordinance_date_2012"].fillna("").astype(str).str.strip()
anchor_date_text = anchor_date_text.mask(anchor_date_text.str.startswith("-"), "")
projects["anchor_ordinance_date"] = pd.to_datetime(anchor_date_text, errors="coerce")
projects["application_key_2012"] = projects["ordinance_number_2012"].map(
    normalize_application
)
projects["application_key"] = projects["application_key_2012"]
projects["application_key_source"] = "2012_zoning_anchor"

later_date = pd.to_datetime(
    projects["ordinance_date_2016"], format="mixed", errors="coerce"
)
later_application = projects["ordinance_number_2016"].map(normalize_application)
known_event_keys = set(zip(events["event_date"], events["application_key"]))
missing_anchor_event = [
    (date, application) not in known_event_keys
    for date, application in zip(
        projects["anchor_ordinance_date"], projects["application_key_2012"]
    )
]
use_later_application = (
    pd.Series(missing_anchor_event, index=projects.index)
    & projects["anchor_ordinance_date"].eq(later_date)
    & projects["zone_group_2012"].eq(projects["zone_group_2016"])
    & later_application.notna()
    & pd.Series(
        [
            (date, application) in known_event_keys
            for date, application in zip(later_date, later_application)
        ],
        index=projects.index,
    )
)
projects.loc[use_later_application, "application_key"] = later_application.loc[
    use_later_application
]
projects.loc[use_later_application, "application_key_source"] = (
    "2016_same_date_same_group_fallback"
)
event_columns = [column for column in events.columns if column not in {"event_date", "application_key"}]
selected_events = []
event_selection_status = []
for row in projects.itertuples(index=False):
    candidates = events.loc[
        events["event_date"].eq(row.anchor_ordinance_date)
        & events["application_key"].eq(row.application_key)
    ]
    if len(candidates) > 1:
        map_candidates = candidates.loc[
            candidates["event_map_keys"].map(lambda keys: row.zoning_map_grid in keys)
        ]
        if not map_candidates.empty:
            candidates = map_candidates
    if len(candidates) > 1:
        destination_candidates = candidates.loc[
            candidates["destination_groups"].map(
                lambda value: row.zone_group_2012 in split_groups(value)
            )
        ]
        if not destination_candidates.empty:
            candidates = destination_candidates

    if len(candidates) == 1:
        selected_events.append(candidates.iloc[0][event_columns].to_dict())
        event_selection_status.append("unique_event")
    elif len(candidates) == 0:
        selected_events.append({column: pd.NA for column in event_columns})
        event_selection_status.append("event_not_found")
    else:
        selected_events.append({column: pd.NA for column in event_columns})
        event_selection_status.append("event_key_ambiguous")

projects = pd.concat(
    [projects.reset_index(drop=True), pd.DataFrame(selected_events)], axis=1
)
projects["event_selection_status"] = event_selection_status

reconstructed_groups = []
statuses = []
origin_group_values = []
for row in projects.itertuples(index=False):
    anchor_group = row.zone_group_2012
    ordinance_date = row.anchor_ordinance_date

    if pd.isna(ordinance_date) or ordinance_date < pd.Timestamp("2006-01-01"):
        origins = [anchor_group]
        reconstructed = anchor_group
        status = "anchor_predates_2006"
    elif ordinance_date > pd.Timestamp("2012-10-31"):
        origins = []
        reconstructed = None
        status = "anchor_date_after_snapshot_cutoff"
    elif row.event_selection_status == "event_key_ambiguous":
        origins = []
        reconstructed = None
        status = "ordinance_key_ambiguous"
    elif pd.isna(row.event_source):
        origins = []
        reconstructed = None
        status = "ordinance_not_extracted"
    elif row.event_source == "clerk_journal_full_ordinance":
        origins = historical_origins(row.clause_transitions_json, anchor_group)
        if len(origins) == 1:
            reconstructed = origins[0]
            status = "unique_historical_origin"
        elif len(origins) > 1:
            reconstructed = None
            status = "multiple_historical_origins"
        else:
            reconstructed = None
            status = "historical_transition_not_parsed"
    else:
        destination_groups = split_groups(row.destination_groups)
        origins = split_groups(row.source_groups)
        if anchor_group not in destination_groups:
            reconstructed = None
            status = "modern_destination_disagrees_with_anchor"
        elif len(origins) == 1:
            reconstructed = origins[0]
            status = "unique_modern_origin"
        elif len(origins) > 1:
            reconstructed = None
            status = "multiple_modern_origins"
        else:
            reconstructed = None
            status = "modern_transition_not_parsed"

    reconstructed_groups.append(reconstructed)
    statuses.append(status)
    origin_group_values.append(";".join(origins))

projects["candidate_zone_group_2006"] = reconstructed_groups
projects["reconstruction_status"] = statuses
projects["candidate_origin_groups"] = origin_group_values
projects["within_500ft"] = projects["within_500ft"].astype(str).str.lower().eq("true")
projects["multifamily"] = projects["multifamily"].astype(str).str.lower().eq("true")
projects["reconstruction_resolved"] = projects["candidate_zone_group_2006"].notna()
projects["broad_group_changed_since_2006"] = (
    projects["reconstruction_resolved"]
    & projects["candidate_zone_group_2006"].ne(projects["zone_group_2012"])
)

output_columns = [
    "pin",
    "construction_year",
    "unitscount",
    "dist_to_boundary",
    "ward_pair",
    "zoning_map_grid",
    "within_500ft",
    "multifamily",
    "longitude",
    "latitude",
    "zone_code_2012",
    "zone_group_2012",
    "ordinance_number_2012",
    "ordinance_date_2012",
    "application_key_2012",
    "application_key",
    "application_key_source",
    "event_selection_status",
    "candidate_zone_group_2006",
    "candidate_origin_groups",
    "reconstruction_status",
    "reconstruction_resolved",
    "broad_group_changed_since_2006",
    "event_source",
    "journal_filename",
    "journal_pdf_url",
    "pdf_page_number",
    "map_numbers_raw",
    "source_groups",
    "destination_groups",
    "clause_count",
    "common_addresses",
    "bounded_areas",
    "ordinance_excerpt",
]
projects[output_columns].to_csv(
    "../output/historical_zoning_2006_project_map.csv", index=False
)

summary_rows = []
samples = {
    "all_density_points": pd.Series(True, index=projects.index),
    "within_500ft": projects["within_500ft"],
    "multifamily_within_500ft": projects["within_500ft"] & projects["multifamily"],
}
for sample_name, sample_mask in samples.items():
    sample = projects.loc[sample_mask]
    for status, group in sample.groupby("reconstruction_status", dropna=False):
        summary_rows.append(
            {
                "sample": sample_name,
                "reconstruction_status": status,
                "projects": len(group),
                "unique_applications": group["application_key"].nunique(),
                "resolved_projects": int(group["reconstruction_resolved"].sum()),
                "changed_projects": int(group["broad_group_changed_since_2006"].sum()),
            }
        )
pd.DataFrame(summary_rows).to_csv(
    "../output/historical_zoning_2006_reconstruction_summary.csv", index=False
)
