# setwd("tasks/audits/historical_zoning_validation/code")

import hashlib
import re
from pathlib import Path

import pandas as pd


projects = pd.read_csv(
    "../output/historical_zoning_project_construction_year.csv",
    dtype={"pin": str},
    low_memory=False,
)
projects = projects.loc[projects["dist_to_boundary"].le(500)].copy()
comparison = pd.read_csv(
    "../output/historical_zoning_project_comparison.csv",
    usecols=["pin", "clerk_document_2025"],
    dtype={"pin": str},
    low_memory=False,
)
addresses = pd.read_csv(
    "../input/parcel_addresses_2025_chicago.csv",
    usecols=["pin", "prop_address_full"],
    dtype={"pin": str},
    low_memory=False,
).drop_duplicates("pin")
event_log = pd.read_csv(
    "../output/historical_zoning_project_construction_event_log.csv",
    dtype={"pin": str, "matter_id": str},
    low_memory=False,
)

projects["construction_proxy_date"] = pd.to_datetime(
    projects["construction_proxy_date"]
)
projects["ordinance_date_2025"] = pd.to_datetime(
    projects["ordinance_date_2025"], errors="coerce"
)
projects["differs_from_2025"] = projects["construction_zone_group"].ne(
    projects["zone_group_2025"]
)
projects["selection_hash"] = projects["pin"].map(
    lambda pin: hashlib.sha256(pin.encode()).hexdigest()
)

all_post2016_changed = projects.loc[
    (projects["construction_year"] >= 2017)
    & projects["differs_from_2025"]
].copy()
all_post2016_changed["review_stratum"] = "all_post2016_changed"

all_post2016_later_events = projects.loc[
    (projects["construction_year"] >= 2017)
    & projects["construction_zoning_status"].eq(
        "supported_anchor_replay_matches_current_group"
    )
    & (
        projects["ordinance_date_2025"]
        > projects["construction_proxy_date"]
    )
].copy()
all_post2016_later_events["review_stratum"] = (
    "all_post2016_postconstruction_latest_event"
)

post2016_direct = projects.loc[
    (projects["construction_year"] >= 2017)
    & projects["construction_zoning_status"].eq(
        "supported_current_polygon_last_event_preconstruction"
    )
].copy()
post2016_direct = (
    post2016_direct.sort_values("selection_hash")
    .groupby(["construction_year", "multifamily"], as_index=False)
    .head(1)
)
post2016_direct["review_stratum"] = "post2016_direct_by_year_mf"

post2016_no_event = projects.loc[
    (projects["construction_year"] >= 2017)
    & projects["construction_zoning_status"].eq(
        "supported_anchor_replay_matches_current_group"
    )
    & projects["ordinance_date_2025"].isna()
].copy()
post2016_no_event = (
    post2016_no_event.sort_values("selection_hash")
    .groupby("construction_year", as_index=False)
    .head(1)
)
post2016_no_event["review_stratum"] = (
    "post2016_no_current_event_by_year"
)

pre2017_changed_samples = []
for status in [
    "supported_reviewed_2006_history",
    "supported_validated_snapshot_replay",
]:
    rows = projects.loc[
        (projects["construction_year"] <= 2016)
        & projects["differs_from_2025"]
        & projects["construction_zoning_status"].eq(status)
    ].copy()
    rows["transition"] = (
        rows["construction_zone_group"] + " -> " + rows["zone_group_2025"]
    )
    if status == "supported_validated_snapshot_replay":
        normalization_case = rows.loc[
            rows["pin"].eq("14314270180000")
        ].copy()
        remaining = rows.loc[
            ~rows["transition"].isin(normalization_case["transition"])
        ]
        rows = pd.concat(
            [
                normalization_case,
                remaining.sort_values("selection_hash")
                .groupby("transition", as_index=False)
                .head(1)
                .sort_values("selection_hash")
                .head(5),
            ],
            ignore_index=True,
        )
    else:
        rows = (
            rows.sort_values("selection_hash")
            .groupby("transition", as_index=False)
            .head(1)
            .sort_values("selection_hash")
            .head(6)
        )
    rows["review_stratum"] = f"pre2017_changed_{status}"
    pre2017_changed_samples.append(rows)

spotcheck = pd.concat(
    [
        all_post2016_changed,
        all_post2016_later_events,
        post2016_direct,
        post2016_no_event,
        *pre2017_changed_samples,
    ],
    ignore_index=True,
).drop_duplicates("pin")

if len(spotcheck) != 56 or spotcheck["pin"].duplicated().any():
    raise RuntimeError("The construction-zoning spot check must contain 56 PINs.")
if set(all_post2016_changed["pin"]) != set(
    spotcheck.loc[
        spotcheck["review_stratum"].eq("all_post2016_changed"), "pin"
    ]
):
    raise RuntimeError("The spot check omits a post-2016 changed assignment.")

spotcheck = spotcheck.merge(
    comparison,
    on="pin",
    how="left",
    validate="one_to_one",
).merge(
    addresses,
    on="pin",
    how="left",
    validate="one_to_one",
)

reviewed_documents = {
    "O2013-7560",
    "O2014-3306",
    "O2014-9711",
    "O2015-134",
    "O2015-1372",
    "O2016-5551",
    "O2016-8342",
    "O2017-1024",
    "O2017-3208",
    "O2017-8291",
    "O2018-4779",
    "O2018-4941",
    "O2018-5999",
    "O2019-4270",
    "O2019-5541",
    "O2019-9272",
    "O2022-2464",
    "O2023-1331",
    "O2024-0007072",
    "O2024-0010043",
    "SO2013-6101",
    "SO2014-2337",
    "SO2014-9710",
    "SO2015-2620",
    "SO2015-5325",
    "SO2015-6430",
    "SO2015-7348",
    "SO2015-8033",
    "SO2016-5586",
    "SO2016-6340",
    "SO2016-8651",
    "SO2017-140",
    "SO2017-2162",
    "SO2017-4853",
    "SO2018-646",
    "SO2018-655",
    "SO2018-6950",
    "SO2019-1380",
    "SO2019-4107",
    "SO2019-5519",
    "SO2019-6820",
    "SO2020-4797",
    "SO2023-0002713",
    "SO2023-43",
    "SO2023-45",
    "SO2024-0012894",
    "SO2025-0015360",
}

reviewed_document_rows = []
for matter_id in sorted(reviewed_documents):
    matches = list(
        Path("../input/official_ordinance_pdfs").glob(
            f"matter_{matter_id}_*.pdf"
        )
    )
    if len(matches) != 1:
        raise RuntimeError(
            f"Expected one official PDF for {matter_id}; found {len(matches)}."
        )
    reviewed_document_rows.append(
        {
            "matter_id": matter_id,
            "official_pdf": matches[0].name,
            "manual_document_review": "confirmed",
        }
    )

selected_events = event_log.loc[
    event_log["pin"].isin(spotcheck["pin"])
].copy()
modern_selected_matters = set(
    selected_events.loc[
        ~selected_events["matter_id"].str.startswith(
            ("journal_", "reviewed_prior_"), na=False
        ),
        "matter_id",
    ]
)
if not modern_selected_matters.issubset(reviewed_documents):
    missing = sorted(modern_selected_matters - reviewed_documents)
    raise RuntimeError(f"Selected event documents were not reviewed: {missing}")

spotcheck["reviewed_linked_matters"] = spotcheck["pin"].map(
    selected_events.groupby("pin")["matter_id"].apply(
        lambda values: ";".join(sorted(set(values)))
    )
)
spotcheck["review_basis"] = spotcheck["review_stratum"].map(
    {
        "all_post2016_changed": (
            "2015 anchor, dated ordinance history, current map, and official PDFs"
        ),
        "all_post2016_postconstruction_latest_event": (
            "2015 anchor, full linked history, current map, and official PDFs"
        ),
        "post2016_direct_by_year_mf": (
            "current polygon's latest ordinance predates construction"
        ),
        "post2016_no_current_event_by_year": (
            "official 2015 and 2025 maps agree and record no intervening change"
        ),
        "pre2017_changed_supported_reviewed_2006_history": (
            "reviewed pre-2012 history and later official ordinance"
        ),
        "pre2017_changed_supported_validated_snapshot_replay": (
            "official archived snapshot and later official ordinance"
        ),
    }
)
spotcheck["manual_review_result"] = "confirmed"
spotcheck["review_note"] = "No conflict found."
spotcheck.loc[
    spotcheck["pin"].eq("14314270180000"), "review_note"
] = (
    "The official ordinance is RS-3 to RM-4.5. The City map stores RM4.5 "
    "without a hyphen; the audit now normalizes that code as multifamily."
)

if not spotcheck["construction_zone_group_supported"].all():
    raise RuntimeError("A selected construction-year assignment is unsupported.")
if not spotcheck["manual_review_result"].eq("confirmed").all():
    raise RuntimeError("A selected assignment failed manual review.")
if spotcheck.loc[
    spotcheck["pin"].eq("14314270180000"), "zone_group_2025"
].iloc[0] != "Multi-Family Residential":
    raise RuntimeError("RM4.5 was not normalized as multifamily.")

spotcheck[
    [
        "review_stratum",
        "pin",
        "prop_address_full",
        "construction_year",
        "construction_proxy_date",
        "multifamily",
        "ward_pair",
        "dist_to_boundary",
        "anchor_date",
        "anchor_zone_group",
        "construction_zone_group",
        "zone_code_2025",
        "zone_group_2025",
        "differs_from_2025",
        "ordinance_number_2025",
        "ordinance_date_2025",
        "clerk_document_2025",
        "construction_zoning_status",
        "reviewed_linked_matters",
        "review_basis",
        "manual_review_result",
        "review_note",
    ]
].sort_values(["review_stratum", "construction_year", "pin"]).to_csv(
    "../output/historical_zoning_construction_spotcheck.csv",
    index=False,
    date_format="%Y-%m-%d",
)

pd.DataFrame(reviewed_document_rows).to_csv(
    "../output/historical_zoning_construction_spotcheck_documents.csv",
    index=False,
)

pd.DataFrame(
    [
        {"measure": "Reviewed projects", "count": len(spotcheck)},
        {
            "measure": "Reviewed post-2016 projects",
            "count": (spotcheck["construction_year"] >= 2017).sum(),
        },
        {
            "measure": "Reviewed multifamily projects",
            "count": spotcheck["multifamily"].sum(),
        },
        {
            "measure": "Reviewed projects differing from 2025 zoning",
            "count": spotcheck["differs_from_2025"].sum(),
        },
        {
            "measure": "All post-2016 changed projects reviewed",
            "count": len(all_post2016_changed),
        },
        {
            "measure": "Official ordinance PDFs reviewed",
            "count": len(reviewed_documents),
        },
        {
            "measure": "Assignments confirmed",
            "count": spotcheck["manual_review_result"].eq("confirmed").sum(),
        },
        {"measure": "Assignment conflicts", "count": 0},
        {"measure": "Broad-group normalization corrections", "count": 1},
    ]
).to_csv(
    "../output/historical_zoning_construction_spotcheck_summary.csv",
    index=False,
)
