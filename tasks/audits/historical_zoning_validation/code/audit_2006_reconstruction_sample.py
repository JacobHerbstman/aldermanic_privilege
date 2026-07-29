# setwd("tasks/audits/historical_zoning_validation/code")

import hashlib
import re

import pandas as pd


manual_review = pd.DataFrame(
    [
        ("15608", "Neighborhood Mixed-Use", "Neighborhood Mixed-Use"),
        ("15678", "Industrial", "Commercial"),
        ("15868", "Multi-Family Residential", "Multi-Family Residential"),
        ("15503", "Multi-Family Residential", "Multi-Family Residential"),
        ("15875", "Single-Family Residential", "Multi-Family Residential"),
        ("A7194", "Single-Family Residential", "Multi-Family Residential"),
        ("15591", "Single-Family Residential", "Multi-Family Residential"),
        ("15814", "Single-Family Residential", "Multi-Family Residential"),
        ("16606", "Single-Family Residential", "Multi-Family Residential"),
        ("A7433", "Commercial", "Neighborhood Mixed-Use"),
        ("A7380", "Planned Development", "Downtown"),
        ("16603", "Single-Family Residential", "Multi-Family Residential"),
        ("16718", "Single-Family Residential", "Multi-Family Residential"),
        ("16866", "Commercial", "Planned Development"),
        ("16731", "Single-Family Residential", "Single-Family Residential"),
        ("16768", "Single-Family Residential", "Multi-Family Residential"),
        ("16952", "Single-Family Residential", "Multi-Family Residential"),
        ("17078", "Neighborhood Mixed-Use", "Neighborhood Mixed-Use"),
        ("A7591", "Single-Family Residential", "Single-Family Residential"),
        ("17136", "Planned Development", "Planned Development"),
        ("17101", "Neighborhood Mixed-Use", "Commercial"),
        ("17205", "Downtown", "Planned Development"),
        ("17094", "Single-Family Residential", "Multi-Family Residential"),
        ("17370", "Single-Family Residential", "Multi-Family Residential"),
        ("17351", "Neighborhood Mixed-Use", "Multi-Family Residential"),
        ("17382", "Neighborhood Mixed-Use", "Planned Development"),
        ("17533", "Single-Family Residential", "Multi-Family Residential"),
        ("17542", "Neighborhood Mixed-Use", "Single-Family Residential"),
    ],
    columns=[
        "application_key",
        "manual_origin_group",
        "manual_destination_group",
    ],
)

projects = pd.read_csv(
    "../output/historical_zoning_2006_project_map.csv",
    dtype={"pin": "string"},
    low_memory=False,
)
projects["ordinance_year"] = pd.to_datetime(
    projects["ordinance_date_2012"], format="mixed"
).dt.year

candidates = projects.loc[
    projects["within_500ft"]
    & projects["reconstruction_status"].eq("unique_historical_origin")
].copy()
candidates = candidates.sort_values(
    [
        "ordinance_year",
        "multifamily",
        "broad_group_changed_since_2006",
        "application_key",
        "pin",
    ],
    ascending=[True, False, False, True, True],
    kind="stable",
).drop_duplicates("application_key")
candidates["sample_order"] = candidates["application_key"].map(
    lambda value: int(hashlib.sha256(value.encode()).hexdigest()[:8], 16)
)
sample = (
    candidates.sort_values(
        ["ordinance_year", "multifamily", "sample_order"],
        ascending=[True, False, True],
        kind="stable",
    )
    .groupby(["ordinance_year", "multifamily"], group_keys=False)
    .head(2)
)

if set(sample["application_key"]) != set(manual_review["application_key"]):
    raise RuntimeError("The deterministic audit sample changed; repeat the journal review.")

sample = sample.merge(
    manual_review,
    on="application_key",
    how="left",
    validate="one_to_one",
)


def normalize_map(value: str) -> set[str]:
    matches = re.findall(
        r"\b([0-9IL]{1,2})\s*-\s*([A-Z0-9])\b", str(value).upper()
    )
    return {
        f"{number.replace('I', '1').replace('L', '1')}-{suffix.replace('1', 'I')}"
        for number, suffix in matches
    }


sample["header_map_matches_project"] = sample.apply(
    lambda row: row["zoning_map_grid"] in normalize_map(row["map_numbers_raw"]),
    axis=1,
)
sample["origin_matches_manual_review"] = sample["candidate_zone_group_2006"].eq(
    sample["manual_origin_group"]
)
sample["destination_matches_manual_review"] = sample["zone_group_2012"].eq(
    sample["manual_destination_group"]
)
sample["manual_review_result"] = "pass"
sample.loc[
    ~(
        sample["origin_matches_manual_review"]
        & sample["destination_matches_manual_review"]
    ),
    "manual_review_result",
] = "fail"
sample["location_review_note"] = "Ordinance map sheet matches the project map sheet."
sample.loc[sample["application_key"].eq("15868"), "location_review_note"] = (
    "The project at 1332 W Melrose is inside the written ordinance bounds at "
    "Melrose and Southport; the journal labels the sheet 9-H while the current "
    "City index assigns 9-G."
)
sample.loc[sample["application_key"].eq("15591"), "location_review_note"] = (
    "The project at 1521 W Huron is inside the written ordinance bounds at Huron "
    "and Armour; the journal labels the sheet 5-J while the current City index "
    "assigns 1-G."
)

sample = sample.sort_values(
    ["ordinance_year", "multifamily", "application_key"],
    ascending=[True, False, True],
    kind="stable",
)
sample[
    [
        "ordinance_year",
        "application_key",
        "pin",
        "multifamily",
        "zoning_map_grid",
        "map_numbers_raw",
        "manual_origin_group",
        "candidate_zone_group_2006",
        "manual_destination_group",
        "zone_group_2012",
        "header_map_matches_project",
        "origin_matches_manual_review",
        "destination_matches_manual_review",
        "manual_review_result",
        "location_review_note",
        "journal_filename",
        "journal_pdf_url",
        "pdf_page_number",
        "ordinance_excerpt",
    ]
].to_csv("../output/historical_zoning_2006_manual_audit.csv", index=False)

pd.DataFrame(
    [
        {
            "reviewed_applications": len(sample),
            "reviewed_years": sample["ordinance_year"].nunique(),
            "map_header_matches": int(sample["header_map_matches_project"].sum()),
            "origin_matches": int(sample["origin_matches_manual_review"].sum()),
            "destination_matches": int(
                sample["destination_matches_manual_review"].sum()
            ),
            "fully_passing_applications": int(
                sample["manual_review_result"].eq("pass").sum()
            ),
        }
    ]
).to_csv("../output/historical_zoning_2006_manual_audit_summary.csv", index=False)
