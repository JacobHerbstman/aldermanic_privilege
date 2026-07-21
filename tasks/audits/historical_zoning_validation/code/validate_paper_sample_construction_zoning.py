# setwd("tasks/audits/historical_zoning_validation/code")

import pandas as pd


construction = pd.read_csv(
    "../output/historical_zoning_project_construction_year.csv",
    dtype={"pin": str},
    low_memory=False,
)
history_reviews = pd.read_csv(
    "../output/historical_zoning_paper_sample_seed_reviews.csv",
    dtype={"pin": str},
    low_memory=False,
)
ordinance_reviews = pd.read_csv(
    "../output/historical_zoning_project_construction_reviewed_events.csv",
    dtype={"pin": str},
    low_memory=False,
)
production_projects = pd.read_csv(
    "../input/parcels_with_ward_distances.csv",
    dtype={"pin": str},
    low_memory=False,
)
backcast_2012 = pd.read_csv(
    "../output/historical_zoning_2016_to_2012_backcast_project_check.csv",
    dtype={"pin": str},
    low_memory=False,
)
backcast_2014 = pd.read_csv(
    "../output/historical_zoning_2016_to_2014_backcast_project_check.csv",
    dtype={"pin": str},
    low_memory=False,
)

if construction["pin"].duplicated().any():
    raise RuntimeError("Construction-year zoning is not unique by PIN.")
if history_reviews["pin"].duplicated().any():
    raise RuntimeError("History-seed reviews are not unique by PIN.")
if production_projects["pin"].duplicated().any():
    raise RuntimeError("Production density projects are not unique by PIN.")
if len(construction) != 9609:
    raise RuntimeError("The 1,500-foot reconstruction does not contain 9,609 PINs.")
if construction["dist_to_boundary"].gt(1500).any():
    raise RuntimeError("The reconstruction contains a project beyond 1,500 feet.")
if not construction["construction_zone_group_supported"].all():
    raise RuntimeError("At least one construction-year zoning group is unsupported.")
if construction["construction_zone_group"].isna().any():
    raise RuntimeError("At least one construction-year zoning group is missing.")

production_1500 = production_projects.loc[
    production_projects["construction_year"].between(2006, 2022)
    & production_projects["signed_distance_m"].abs().le(457.2)
].copy()
missing_production_pins = set(production_1500["pin"]) - set(construction["pin"])
if missing_production_pins:
    raise RuntimeError(
        "The lookup misses production projects within 1,500 feet: "
        f"{sorted(missing_production_pins)[:10]}"
    )
construction_year_check = production_1500[["pin", "construction_year"]].merge(
    construction[["pin", "construction_year"]],
    on="pin",
    how="left",
    validate="one_to_one",
    suffixes=("_production", "_reconstruction"),
)
if not construction_year_check["construction_year_production"].eq(
    construction_year_check["construction_year_reconstruction"]
).all():
    raise RuntimeError("Construction years differ from the production density file.")

lookup = construction.loc[
    construction["pin"].isin(production_1500["pin"]),
    ["pin", "construction_year", "construction_zone_group"]
].sort_values("pin")
if len(lookup) != len(production_1500):
    raise RuntimeError("The lookup and production 1,500-foot universe differ.")
lookup.to_csv(
    "../output/historical_zoning_project_construction_year_lookup.csv",
    index=False,
)

ordinance_evidence = (
    ordinance_reviews.sort_values(["pin", "matter_passed_date", "matter_id"])
    .groupby("pin", as_index=False)
    .agg(
        reviewed_matter_ids=(
            "matter_id",
            lambda values: ";".join(dict.fromkeys(values.dropna())),
        ),
        reviewed_source_pdfs=(
            "source_pdf",
            lambda values: ";".join(dict.fromkeys(values.dropna())),
        ),
        reviewed_event_notes=(
            "review_note",
            lambda values: " | ".join(dict.fromkeys(values.dropna())),
        ),
    )
)

validation = construction.merge(
    history_reviews[
        [
            "pin",
            "history_seed_status",
            "history_seed_evidence",
            "source_url",
        ]
    ].assign(manual_history_seed_review=True),
    on="pin",
    how="left",
    validate="one_to_one",
).merge(
    ordinance_evidence.assign(reviewed_ordinance_event=True),
    on="pin",
    how="left",
    validate="one_to_one",
)
validation["manual_history_seed_review"] = validation[
    "manual_history_seed_review"
].fillna(False)
validation["reviewed_ordinance_event"] = validation[
    "reviewed_ordinance_event"
].fillna(False)
validation["changed_from_2025"] = validation["construction_zone_group"].ne(
    validation["zone_group_2025"]
)
validation["added_beyond_500ft"] = validation["dist_to_boundary"].gt(500)
validation["included_in_validation_file"] = (
    validation["added_beyond_500ft"]
    & (
        validation["changed_from_2025"]
        | validation["manual_history_seed_review"]
        | validation["reviewed_ordinance_event"]
    )
)
validation = validation.loc[
    validation["pin"].isin(production_1500["pin"])
].copy()

validation_columns = [
    "pin",
    "construction_year",
    "construction_proxy_date",
    "multifamily",
    "dist_to_boundary",
    "ward_pair",
    "construction_zone_group",
    "zone_group_2025",
    "changed_from_2025",
    "construction_zoning_status",
    "construction_applied_matter_ids",
    "current_check_applied_matter_ids",
    "manual_history_seed_review",
    "history_seed_status",
    "history_seed_evidence",
    "source_url",
    "reviewed_ordinance_event",
    "reviewed_matter_ids",
    "reviewed_source_pdfs",
    "reviewed_event_notes",
]
validation.loc[
    validation["included_in_validation_file"], validation_columns
].sort_values(["dist_to_boundary", "pin"]).to_csv(
    "../output/historical_zoning_paper_sample_validation.csv",
    index=False,
)

summary_rows = []
for sample, sample_data in [
    ("all_within_1500ft", validation),
    ("multifamily_within_1500ft", validation.loc[validation["multifamily"]]),
    ("added_500_to_1500ft", validation.loc[validation["added_beyond_500ft"]]),
    (
        "multifamily_added_500_to_1500ft",
        validation.loc[
            validation["added_beyond_500ft"] & validation["multifamily"]
        ],
    ),
]:
    summary_rows.extend(
        [
            {"sample": sample, "measure": "projects", "value": len(sample_data)},
            {
                "sample": sample,
                "measure": "changed_from_2025",
                "value": int(sample_data["changed_from_2025"].sum()),
            },
            {
                "sample": sample,
                "measure": "manual_history_seed_reviews",
                "value": int(sample_data["manual_history_seed_review"].sum()),
            },
            {
                "sample": sample,
                "measure": "reviewed_ordinance_events",
                "value": int(sample_data["reviewed_ordinance_event"].sum()),
            },
            {
                "sample": sample,
                "measure": "unsupported_assignments",
                "value": int(
                    (~sample_data["construction_zone_group_supported"]).sum()
                ),
            },
        ]
    )

for validation_name, backcast in [
    ("2016_to_2012", backcast_2012),
    ("2016_to_2014", backcast_2014),
]:
    paper_backcast = backcast.loc[
        backcast["pin"].isin(production_1500["pin"])
    ]
    unique_reversals = paper_backcast.loc[
        paper_backcast["backcast_status"].eq("unique_immediate_prior_group")
    ]
    summary_rows.extend(
        [
            {
                "sample": validation_name,
                "measure": "unique_reversals_within_1500ft",
                "value": len(unique_reversals),
            },
            {
                "sample": validation_name,
                "measure": "unique_reversals_correct",
                "value": int(unique_reversals["backcast_agrees"].fillna(False).sum()),
            },
        ]
    )

summary_rows.extend(
    [
        {
            "sample": "production_coverage",
            "measure": "production_projects_within_1500ft",
            "value": len(production_1500),
        },
        {
            "sample": "production_coverage",
            "measure": "reconstructed_geographic_superset",
            "value": len(construction),
        },
        {
            "sample": "production_coverage",
            "measure": "production_projects_missing_from_lookup",
            "value": len(missing_production_pins),
        },
        {
            "sample": "added_500_to_1500ft",
            "measure": "changed_or_manually_reviewed_assignments",
            "value": int(validation["included_in_validation_file"].sum()),
        },
    ]
)
pd.DataFrame(summary_rows).to_csv(
    "../output/historical_zoning_paper_sample_validation_summary.csv",
    index=False,
)
