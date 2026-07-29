# setwd("tasks/audits/historical_zoning_validation/code")

import re

import pandas as pd


def normalize_map_keys(value) -> set[str]:
    matches = re.findall(
        r"\b([0-9IL]{1,2})\s*-?\s*([A-Z0-9])\b", str(value).upper()
    )
    return {
        f"{number.replace('I', '1').replace('L', '1')}-{suffix.replace('1', 'I')}"
        for number, suffix in matches
    }


street_pattern = re.compile(
    r"\b(?:NORTH|SOUTH|EAST|WEST)?\s*([A-Z][A-Z -]{1,30}?)\s+"
    r"(STREET|AVENUE|ROAD|BOULEVARD|PLACE|COURT|DRIVE|PARKWAY|TERRACE)\b"
)


def street_names(value) -> set[str]:
    return {
        f"{name.strip()} {suffix}"
        for name, suffix in street_pattern.findall(str(value).upper())
    }


def normalize_text(value) -> str:
    value = str(value).upper()
    value = re.sub(
        r"\d{1,2}\s*/\s*\d{1,2}\s*/\s*\d{4}\s+"
        r"REPORTS OF COMMITTEES\s+\d+",
        " ",
        value,
    )
    value = re.sub(r"[^A-Z0-9]+", " ", value)
    return re.sub(r"\s+", " ", value).strip()


projects = pd.read_csv(
    "../output/historical_zoning_2006_project_map.csv", low_memory=False
)
targets = projects.loc[
    pd.to_numeric(projects["dist_to_boundary"], errors="coerce").le(1500)
    & projects["reconstruction_status"].isin(
        ["unique_historical_origin", "unique_modern_origin"]
    )
].copy()
targets["event_date"] = pd.to_datetime(
    targets["ordinance_date_2012"], errors="coerce"
)
targets = targets.drop_duplicates(
    ["event_date", "application_key", "zone_group_2012"]
)

ordinances = pd.read_csv(
    "../output/historical_zoning_ordinances_2006_2012.csv", low_memory=False
)
ordinances["event_date"] = pd.to_datetime(
    ordinances["journal_meeting_date"], errors="coerce"
)
ordinances["event_map_keys"] = ordinances["map_numbers_raw"].map(
    normalize_map_keys
)
ordinances["event_street_names"] = (
    ordinances["common_addresses"].fillna("")
    + " "
    + ordinances["bounded_areas"].fillna("")
    + " "
    + ordinances["ordinance_excerpt"].fillna("")
).map(street_names)
ordinances["bounded_area_normalized"] = ordinances["bounded_areas"].fillna("").map(
    normalize_text
)
ordinances["common_address_normalized"] = ordinances["common_addresses"].fillna(
    ""
).map(normalize_text)

candidate_pairs = []
for target in targets.itertuples(index=False):
    target_rows = ordinances.loc[
        ordinances["application_key"].astype(str).eq(str(target.application_key))
        & ordinances["event_date"].eq(target.event_date)
    ].copy()
    if len(target_rows) > 1:
        map_rows = target_rows.loc[
            target_rows["event_map_keys"].map(
                lambda keys: target.zoning_map_grid in keys
            )
        ]
        if not map_rows.empty:
            target_rows = map_rows
        parsed_rows = target_rows.loc[target_rows["clause_count"].fillna(0).gt(0)]
        if not parsed_rows.empty:
            target_rows = parsed_rows

    if len(target_rows) == 1:
        target_event = target_rows.iloc[0]
        target_maps = target_event["event_map_keys"]
        target_streets = target_event["event_street_names"]
        target_bounds = target_event["bounded_area_normalized"]
        target_address = target_event["common_address_normalized"]
        target_bounded_area = target_event["bounded_areas"]
        target_common_address = target_event["common_addresses"]
    elif target.event_source == "elms_ordinance":
        target_maps = {target.zoning_map_grid}
        target_streets = street_names(
            f"{target.common_addresses} {target.bounded_areas} {target.ordinance_excerpt}"
        )
        target_bounds = normalize_text(target.bounded_areas)
        target_address = normalize_text(target.common_addresses)
        target_bounded_area = target.bounded_areas
        target_common_address = target.common_addresses
    else:
        raise RuntimeError(
            f"Could not select the target ordinance for {target.application_key}."
        )

    prior_rows = ordinances.loc[
        ordinances["event_date"].between(
            "2006-01-01", target.event_date, inclusive="left"
        )
        & ordinances["event_map_keys"].map(lambda keys: bool(keys & target_maps))
    ].copy()
    prior_rows["exact_bounded_area"] = prior_rows[
        "bounded_area_normalized"
    ].eq(target_bounds) & (len(target_bounds) > 30)
    prior_rows["exact_common_address"] = prior_rows[
        "common_address_normalized"
    ].eq(target_address) & (len(target_address) > 5)
    prior_rows["shared_street_count"] = prior_rows["event_street_names"].map(
        lambda names: len(names & target_streets)
    )
    prior_rows = prior_rows.loc[
        prior_rows["exact_bounded_area"]
        | prior_rows["exact_common_address"]
        | prior_rows["shared_street_count"].ge(2)
    ].copy()
    prior_rows = prior_rows.drop_duplicates(
        ["event_date", "application_key", "bounded_area_normalized"]
    )

    for prior in prior_rows.itertuples(index=False):
        reasons = []
        if prior.exact_bounded_area:
            reasons.append("exact_bounded_area")
        if prior.exact_common_address:
            reasons.append("exact_common_address")
        if prior.shared_street_count >= 2:
            reasons.append("shared_boundary_streets")
        candidate_pairs.append(
            {
                "application_key": str(target.application_key),
                "prior_application_key": str(prior.application_key),
                "target_event_date": target.event_date.date(),
                "prior_event_date": prior.event_date.date(),
                "target_changes_broad_group": str(
                    target.broad_group_changed_since_2006
                ).lower()
                == "true",
                "target_map_numbers": ";".join(sorted(target_maps)),
                "prior_map_numbers": prior.map_numbers_raw,
                "screen_reason": ";".join(reasons),
                "shared_street_count": prior.shared_street_count,
                "shared_street_names": ";".join(
                    sorted(target_streets & prior.event_street_names)
                ),
                "target_source_groups": target.source_groups,
                "target_destination_groups": target.destination_groups,
                "prior_source_groups": prior.source_groups,
                "prior_destination_groups": prior.destination_groups,
                "target_common_address": target_common_address,
                "prior_common_address": prior.common_addresses,
                "target_bounded_area": target_bounded_area,
                "prior_bounded_area": prior.bounded_areas,
                "prior_journal_filename": prior.journal_filename,
                "prior_pdf_page_number": prior.pdf_page_number,
                "prior_journal_pdf_url": prior.journal_pdf_url,
            }
        )

candidate_pairs = pd.DataFrame(candidate_pairs).sort_values(
    ["target_event_date", "application_key", "prior_event_date", "prior_application_key"],
    kind="stable",
)
candidate_pairs.to_csv(
    "../output/historical_zoning_2006_repeat_event_screen_paper_sample.csv",
    index=False,
)

pd.DataFrame(
    [
        {
            "post_2005_applications_in_paper_sample": len(targets),
            "broad_group_change_applications_in_paper_sample": int(
                targets["broad_group_changed_since_2006"]
                .astype(str)
                .str.lower()
                .eq("true")
                .sum()
            ),
            "applications_flagged_for_review": candidate_pairs[
                "application_key"
            ].nunique(),
            "candidate_prior_ordinances": len(candidate_pairs),
            "exact_bounded_area_pairs": candidate_pairs["screen_reason"]
            .str.contains("exact_bounded_area")
            .sum(),
            "exact_common_address_pairs": candidate_pairs["screen_reason"]
            .str.contains("exact_common_address")
            .sum(),
        }
    ]
).to_csv(
    "../output/historical_zoning_2006_repeat_event_screen_paper_sample_summary.csv",
    index=False,
)
