# setwd("tasks/new_construction_cleaning/code")

import re
from pathlib import Path

import pandas as pd


def normalize_application(value):
    if pd.isna(value):
        return None
    normalized = re.sub(r"[^A-Z0-9]", "", str(value).upper())
    normalized = re.sub(r"T[0-9]+$", "", normalized)
    return normalized or None


def split_groups(value):
    if pd.isna(value):
        return set()
    return {group for group in str(value).split(";") if group}


seeds = pd.read_csv(
    "../output/historical_zoning_paper_sample_history_seeds.csv",
    dtype={"pin": str, "application_key": str},
    low_memory=False,
)
snapshots = pd.read_csv(
    "../output/historical_zoning_project_comparison.csv",
    dtype={"pin": str},
    low_memory=False,
)
event_links = pd.read_csv(
    "../output/historical_zoning_event_links.csv",
    dtype={"pin": str, "matter_id": str},
    low_memory=False,
)
repeat_reviews = pd.read_csv(
    "../output/historical_zoning_2006_repeat_event_review_paper_sample.csv",
    dtype={"application_key": str},
    low_memory=False,
)
matters = pd.read_csv(
    "../output/historical_zoning_matter_parsing.csv",
    dtype=str,
    low_memory=False,
)
historical_ordinances = pd.read_csv(
    "../output/historical_zoning_ordinances_2006_2012.csv",
    dtype=str,
    low_memory=False,
)
all_matters = pd.read_csv(
    "../input/matters_20101101_20260212.csv",
    usecols=[
        "matter_id",
        "matter_title",
        "matter_passed_date",
    ],
    dtype=str,
    low_memory=False,
)

if seeds["pin"].duplicated().any():
    raise RuntimeError("History seeds are not unique by PIN.")
if event_links[["pin", "matter_id"]].duplicated().any():
    raise RuntimeError("Modern event links are not unique by PIN and matter.")

snapshots = snapshots.loc[
    pd.to_numeric(snapshots["dist_to_boundary"], errors="coerce") <= 1500,
    [
        "pin",
        "zone_group_2012",
        "zone_group_2014",
        "zone_group_2016",
        "zone_code_2025",
        "zone_group_2025",
        "ordinance_number_2025",
        "ordinance_date_2025",
        "clerk_document_2025",
    ],
].copy()
if snapshots["pin"].duplicated().any():
    raise RuntimeError("Snapshot comparison is not unique by PIN.")
if set(seeds["pin"]) != set(snapshots["pin"]):
    raise RuntimeError("History seeds and the 1,500-foot snapshot sample differ.")

projects = seeds.merge(
    snapshots,
    on="pin",
    how="left",
    validate="one_to_one",
    suffixes=("", "_official"),
)
projects.loc[
    projects["zone_code_2025"]
    .str.upper()
    .str.match(r"^(RT|RM)-?[0-9]", na=False),
    "zone_group_2025",
] = "Multi-Family Residential"
for column in [
    "construction_proxy_date",
    "history_seed_date",
    "ordinance_date_2012",
    "ordinance_date_2025",
]:
    projects[column] = pd.to_datetime(
        projects[column], errors="coerce", utc=True
    ).dt.tz_localize(None).dt.normalize()

snapshot_2012_date = pd.Timestamp("2012-10-31")
snapshot_2014_date = pd.Timestamp("2014-07-30")
snapshot_2016_date = pd.Timestamp("2015-11-18")

projects["anchor_date"] = projects["history_seed_date"]
projects["anchor_zone_group"] = projects["history_seed_zone_group"]
projects["anchor_source"] = "reviewed_history_seed"

use_2012 = (
    (projects["construction_proxy_date"] > snapshot_2012_date)
    & (projects["construction_proxy_date"] <= snapshot_2014_date)
)
projects.loc[use_2012, "anchor_date"] = snapshot_2012_date
projects.loc[use_2012, "anchor_zone_group"] = projects.loc[
    use_2012, "zone_group_2012_official"
]
projects.loc[use_2012, "anchor_source"] = "official_october_2012_snapshot"

use_2014 = (
    (projects["construction_proxy_date"] > snapshot_2014_date)
    & (projects["construction_proxy_date"] <= snapshot_2016_date)
)
projects.loc[use_2014, "anchor_date"] = snapshot_2014_date
projects.loc[use_2014, "anchor_zone_group"] = projects.loc[
    use_2014, "zone_group_2014"
]
projects.loc[use_2014, "anchor_source"] = "official_july_2014_snapshot"

use_2016 = projects["construction_proxy_date"] > snapshot_2016_date
projects.loc[use_2016, "anchor_date"] = snapshot_2016_date
projects.loc[use_2016, "anchor_zone_group"] = projects.loc[
    use_2016, "zone_group_2016"
]
projects.loc[use_2016, "anchor_source"] = "official_november_2015_snapshot"

if projects["anchor_zone_group"].isna().any():
    raise RuntimeError("At least one project lacks a zoning group at its anchor.")
if (projects["anchor_date"] > projects["construction_proxy_date"]).any():
    raise RuntimeError("At least one zoning anchor postdates construction.")

event_links["matter_passed_date"] = pd.to_datetime(
    event_links["matter_passed_date"], errors="coerce"
)
modern_events = event_links[
    [
        "pin",
        "matter_id",
        "matter_passed_date",
        "from_groups",
        "to_group",
        "project_match_method",
        "parcel_match_method",
    ]
].copy()
modern_events["event_source"] = "modern_ordinance_link"

# These linked matters require the full ordinance rather than the parsed title.
reviewed_event_keys = pd.read_csv(
    "../adjudication/historical_zoning_reviewed_events.csv", dtype=str
)
if reviewed_event_keys[["pin", "matter_id"]].duplicated().any():
    raise RuntimeError("Reviewed zoning events are not unique.")
if not reviewed_event_keys["replace_modern_link"].isin(["true", "false"]).all():
    raise RuntimeError("Invalid reviewed-event replacement flag.")
reviewed_event_keys = reviewed_event_keys.loc[
    reviewed_event_keys["replace_modern_link"].eq("true"), ["pin", "matter_id"]
]
modern_events = modern_events.merge(
    reviewed_event_keys.assign(reviewed_event=True),
    on=["pin", "matter_id"],
    how="left",
    validate="one_to_one",
)
if modern_events["reviewed_event"].notna().sum() != len(reviewed_event_keys):
    raise RuntimeError("A reviewed event is missing from the modern event links.")
modern_events = modern_events.loc[modern_events["reviewed_event"].isna()].drop(
    columns="reviewed_event"
)

reviewed_corrections = seeds.loc[
    seeds["repeat_event_review_status"].eq("reviewed_candidate_corrected")
].merge(
    repeat_reviews[["application_key", "reviewed_prior_event_date"]],
    on="application_key",
    how="left",
    validate="many_to_one",
)
reviewed_events = pd.DataFrame(
    {
        "pin": reviewed_corrections["pin"],
        "matter_id": "reviewed_prior_" + reviewed_corrections["application_key"],
        "matter_passed_date": pd.to_datetime(
            reviewed_corrections["reviewed_prior_event_date"], errors="coerce"
        ),
        "from_groups": reviewed_corrections["reviewed_zone_group_2006"],
        "to_group": reviewed_corrections["candidate_zone_group_2006"],
        "project_match_method": "reviewed_same_site_repeat",
        "parcel_match_method": "manual_location_review",
        "event_source": "reviewed_pre_2012_repeat",
    }
)
if reviewed_events["matter_passed_date"].isna().any():
    raise RuntimeError("A reviewed pre-2012 correction lacks an event date.")

anchor_events = seeds.loc[
    seeds["ordinance_date_2012"].notna()
    & seeds["application_key_2012"].notna()
].copy()
anchor_events = pd.DataFrame(
    {
        "pin": anchor_events["pin"],
        "matter_id": "journal_" + anchor_events["application_key_2012"],
        "matter_passed_date": pd.to_datetime(
            anchor_events["ordinance_date_2012"], errors="coerce"
        ),
        "from_groups": anchor_events["source_groups"],
        "to_group": anchor_events["zone_group_2012"],
        "project_match_method": "reviewed_2012_anchor_event",
        "parcel_match_method": "official_2012_polygon",
        "event_source": "reviewed_pre_2012_anchor",
    }
)

reviewed_project_events = pd.read_csv(
    "../adjudication/historical_zoning_reviewed_events.csv", dtype=str
).drop(columns="replace_modern_link")
reviewed_project_events["matter_passed_date"] = pd.to_datetime(
    reviewed_project_events["matter_passed_date"]
)
missing_review_pdfs = [
    source_pdf
    for source_pdf in reviewed_project_events["source_pdf"].unique()
    if not Path(source_pdf).is_file()
]
if missing_review_pdfs:
    raise RuntimeError(f"Reviewed ordinance PDFs are missing: {missing_review_pdfs}")
reviewed_matter_check = reviewed_project_events.merge(
    all_matters,
    on="matter_id",
    how="left",
    validate="many_to_one",
    suffixes=("", "_raw"),
)
reviewed_matter_check["matter_passed_date_raw"] = pd.to_datetime(
    reviewed_matter_check["matter_passed_date_raw"], errors="coerce"
)
if reviewed_matter_check["matter_title"].isna().any():
    raise RuntimeError("A reviewed project event is absent from the ELMS matter file.")
if not reviewed_matter_check["matter_passed_date"].eq(
    reviewed_matter_check["matter_passed_date_raw"]
).all():
    raise RuntimeError("A reviewed project event date disagrees with ELMS.")

events = pd.concat(
    [modern_events, reviewed_events, anchor_events, reviewed_project_events],
    ignore_index=True,
).merge(
    projects[["pin", "anchor_date", "anchor_source"]],
    on="pin",
    how="inner",
    validate="many_to_one",
)
events = events.loc[
    (
        events["anchor_source"].eq("reviewed_history_seed")
        & (
            ~events["event_source"].eq("modern_ordinance_link")
            | (events["matter_passed_date"] > snapshot_2012_date)
        )
    )
    | (
        ~events["anchor_source"].eq("reviewed_history_seed")
    )
].copy()
events = events.loc[events["matter_passed_date"] > events["anchor_date"]]
events = (
    events.sort_values(["matter_passed_date", "matter_id", "pin"])
    .drop_duplicates(["pin", "matter_id"])
    .reset_index(drop=True)
)

project_dates = projects.set_index("pin")


def replay_to(end_dates, replay_label):
    state = dict(zip(projects["pin"], projects["anchor_zone_group"]))
    linked_counts = {pin: 0 for pin in projects["pin"]}
    applied_counts = {pin: 0 for pin in projects["pin"]}
    applied_ids = {pin: [] for pin in projects["pin"]}
    log_rows = []

    for event in events.itertuples(index=False):
        if event.matter_passed_date > end_dates[event.pin]:
            continue
        linked_counts[event.pin] += 1
        group_before = state[event.pin]
        origin_matches = group_before in split_groups(event.from_groups)
        destination_available = pd.notna(event.to_group)
        group_changes = (
            origin_matches
            and destination_available
            and group_before != event.to_group
        )
        if group_changes:
            state[event.pin] = event.to_group
            applied_counts[event.pin] += 1
            applied_ids[event.pin].append(event.matter_id)
        log_rows.append(
            {
                "replay": replay_label,
                "pin": event.pin,
                "matter_id": event.matter_id,
                "matter_passed_date": event.matter_passed_date,
                "event_source": event.event_source,
                "project_match_method": event.project_match_method,
                "parcel_match_method": event.parcel_match_method,
                "group_before": group_before,
                "from_groups": event.from_groups,
                "to_group": event.to_group,
                "origin_matches": origin_matches,
                "group_changed": group_changes,
                "group_after": state[event.pin],
            }
        )

    replay = pd.DataFrame(
        {
            "pin": projects["pin"],
            f"{replay_label}_replayed_zone_group": projects["pin"].map(state),
            f"{replay_label}_linked_event_count": projects["pin"].map(
                linked_counts
            ),
            f"{replay_label}_applied_event_count": projects["pin"].map(
                applied_counts
            ),
            f"{replay_label}_applied_matter_ids": projects["pin"].map(
                lambda pin: ";".join(applied_ids[pin]) or None
            ),
        }
    )
    return replay, pd.DataFrame(log_rows)


construction_dates = project_dates["construction_proxy_date"].to_dict()
construction_replay, construction_log = replay_to(
    construction_dates, "construction"
)
all_linked_dates = {
    pin: pd.Timestamp("2025-09-10") for pin in projects["pin"]
}
current_replay, current_log = replay_to(all_linked_dates, "current_check")

projects = projects.merge(
    construction_replay, on="pin", how="left", validate="one_to_one"
).merge(current_replay, on="pin", how="left", validate="one_to_one")
projects["current_last_event_preconstruction"] = (
    projects["ordinance_date_2025"].notna()
    & (
        projects["ordinance_date_2025"]
        <= projects["construction_proxy_date"]
    )
)
projects["current_ordinance_date_recovered"] = pd.NaT
recovered_dates = pd.read_csv(
    "../adjudication/historical_zoning_recovered_ordinance_dates.csv", dtype=str
)
if recovered_dates["application_key"].duplicated().any():
    raise RuntimeError("Recovered ordinance dates are not unique by application.")
for recovery in recovered_dates.itertuples(index=False):
    if not Path(recovery.source_pdf).is_file():
        raise RuntimeError("Recovered ordinance-date source PDF is missing.")
    selected = projects["ordinance_number_2025"].map(normalize_application).eq(recovery.application_key)
    if not selected.any():
        raise RuntimeError("Recovered ordinance-date application is absent from the project input.")
    projects.loc[selected, "current_ordinance_date_recovered"] = pd.Timestamp(recovery.event_date)
projects["current_event_date_for_validation"] = projects[
    "ordinance_date_2025"
].fillna(projects["current_ordinance_date_recovered"])
projects["current_last_event_preconstruction"] = (
    projects["current_event_date_for_validation"].notna()
    & (
        projects["current_event_date_for_validation"]
        <= projects["construction_proxy_date"]
    )
)
projects["construction_zone_group"] = projects[
    "construction_replayed_zone_group"
]
projects.loc[
    projects["current_last_event_preconstruction"], "construction_zone_group"
] = projects.loc[
    projects["current_last_event_preconstruction"], "zone_group_2025"
]
projects["current_replay_matches_2025"] = projects[
    "current_check_replayed_zone_group"
].eq(projects["zone_group_2025"])
projects["construction_group_matches_2025"] = projects[
    "construction_zone_group"
].eq(projects["zone_group_2025"])

matters["normalized_application"] = matters["application_key"].map(
    normalize_application
)
matters["matter_passed_date"] = pd.to_datetime(
    matters["matter_passed_date"], errors="coerce"
)
matters["matter_document_key_normalized"] = matters[
    "matter_document_key"
].str.replace(r"^S(?=O)", "", regex=True)

current_transitions = projects[
    [
        "pin",
        "ordinance_number_2025",
        "ordinance_date_2025",
        "clerk_document_2025",
        "zone_group_2025",
    ]
].copy()
current_transitions["normalized_application"] = current_transitions[
    "ordinance_number_2025"
].map(normalize_application)
current_transitions["matter_document_key_normalized"] = current_transitions[
    "clerk_document_2025"
].str.replace(r"^S(?=O)", "", regex=True)

matter_transition_rows = []
for project in current_transitions.itertuples(index=False):
    candidates = matters.loc[
        matters["matter_passed_date"].eq(project.ordinance_date_2025)
        & (
            matters["matter_document_key_normalized"].eq(
                project.matter_document_key_normalized
            )
            | matters["normalized_application"].eq(
                project.normalized_application
            )
        )
    ].copy()
    if len(candidates) == 1:
        candidate = candidates.iloc[0]
        matter_transition_rows.append(
            {
                "pin": project.pin,
                "current_event_source_groups": candidate["from_groups"],
                "current_event_destination_groups": candidate["to_groups"],
                "current_event_transition_source": "modern_matter_parser",
            }
        )

historical_ordinances["journal_meeting_date"] = pd.to_datetime(
    historical_ordinances["journal_meeting_date"], errors="coerce"
)
historical_ordinances["normalized_application"] = historical_ordinances[
    "application_key"
].map(normalize_application)
for project in current_transitions.itertuples(index=False):
    if any(row["pin"] == project.pin for row in matter_transition_rows):
        continue
    candidates = historical_ordinances.loc[
        historical_ordinances["journal_meeting_date"].eq(
            project.ordinance_date_2025
        )
        & historical_ordinances["normalized_application"].eq(
            project.normalized_application
        )
    ].copy()
    if len(candidates) == 1:
        candidate = candidates.iloc[0]
        matter_transition_rows.append(
            {
                "pin": project.pin,
                "current_event_source_groups": candidate["source_groups"],
                "current_event_destination_groups": candidate[
                    "destination_groups"
                ],
                "current_event_transition_source": "council_journal_parser",
            }
        )

current_transition_evidence = pd.DataFrame(matter_transition_rows)
if len(current_transition_evidence) == 0:
    current_transition_evidence = pd.DataFrame(
        columns=[
            "pin",
            "current_event_source_groups",
            "current_event_destination_groups",
            "current_event_transition_source",
        ]
    )
if current_transition_evidence["pin"].duplicated().any():
    raise RuntimeError("Current-event transition evidence is not unique by PIN.")
projects = projects.merge(
    current_transition_evidence,
    on="pin",
    how="left",
    validate="one_to_one",
)
projects["current_event_origin_supports_construction_group"] = projects.apply(
    lambda row: row["construction_zone_group"]
    in split_groups(row["current_event_source_groups"]),
    axis=1,
)

projects["construction_zoning_status"] = (
    "provisional_anchor_replay_unresolved_current_history"
)
early_history = projects["construction_proxy_date"] <= snapshot_2012_date
validated_forward = (
    (projects["construction_proxy_date"] > snapshot_2012_date)
    & (projects["construction_proxy_date"] <= pd.Timestamp("2016-12-31"))
)
projects.loc[
    early_history, "construction_zoning_status"
] = "supported_reviewed_2006_history"
projects.loc[
    validated_forward, "construction_zoning_status"
] = "supported_validated_snapshot_replay"
projects.loc[
    (projects["construction_proxy_date"] > pd.Timestamp("2016-12-31"))
    & projects["construction_group_matches_2025"],
    "construction_zoning_status",
] = "supported_anchor_replay_matches_current_group"
projects.loc[
    (projects["construction_proxy_date"] > pd.Timestamp("2016-12-31"))
    & ~projects["construction_group_matches_2025"]
    & projects["current_replay_matches_2025"],
    "construction_zoning_status",
] = "supported_event_replay_reproduces_current_group"
projects.loc[
    (projects["construction_proxy_date"] > pd.Timestamp("2016-12-31"))
    & ~projects["construction_group_matches_2025"]
    & ~projects["current_replay_matches_2025"]
    & (projects["ordinance_date_2025"] > projects["construction_proxy_date"])
    & projects["current_event_origin_supports_construction_group"],
    "construction_zoning_status",
] = "supported_latest_current_event_origin"
exact_support = pd.read_csv(
    "../adjudication/historical_zoning_exact_preconstruction_support.csv", dtype=str
)
if exact_support["pin"].duplicated().any() or not set(exact_support["pin"]).issubset(projects["pin"]):
    raise RuntimeError("Exact preconstruction support IDs must be unique and present.")
exact_preconstruction_pins = set(exact_support["pin"])

exact_preconstruction_event = (
    (projects["construction_proxy_date"] > pd.Timestamp("2016-12-31"))
    & (projects["construction_applied_event_count"] > 0)
    & projects["pin"].isin(exact_preconstruction_pins)
)
projects.loc[
    exact_preconstruction_event, "construction_zoning_status"
] = "supported_exact_preconstruction_ordinance"
projects.loc[
    projects["current_last_event_preconstruction"],
    "construction_zoning_status",
] = "supported_current_polygon_last_event_preconstruction"
projects["construction_zone_group_supported"] = ~projects[
    "construction_zoning_status"
].eq("provisional_anchor_replay_unresolved_current_history")

if projects["construction_zone_group"].isna().any():
    raise RuntimeError("At least one project lacks a construction-year group.")

output_columns = [
    "pin",
    "construction_year",
    "construction_proxy_date",
    "multifamily",
    "dist_to_boundary",
    "ward_pair",
    "longitude",
    "latitude",
    "anchor_date",
    "anchor_zone_group",
    "anchor_source",
    "construction_replayed_zone_group",
    "construction_linked_event_count",
    "construction_applied_event_count",
    "construction_applied_matter_ids",
    "zone_code_2025",
    "zone_group_2025",
    "ordinance_number_2025",
    "ordinance_date_2025",
    "current_ordinance_date_recovered",
    "current_event_date_for_validation",
    "current_check_replayed_zone_group",
    "current_check_linked_event_count",
    "current_check_applied_event_count",
    "current_check_applied_matter_ids",
    "current_last_event_preconstruction",
    "current_replay_matches_2025",
    "current_event_source_groups",
    "current_event_destination_groups",
    "current_event_transition_source",
    "current_event_origin_supports_construction_group",
    "construction_zone_group",
    "construction_zoning_status",
    "construction_zone_group_supported",
]
projects[output_columns].to_csv(
    "../output/historical_zoning_project_construction_year.csv",
    index=False,
    date_format="%Y-%m-%d",
)
