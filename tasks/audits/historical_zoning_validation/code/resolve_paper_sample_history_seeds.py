# setwd("tasks/audits/historical_zoning_validation/code")

import pandas as pd


projects = pd.read_csv(
    "../output/historical_zoning_2006_project_map_reviewed_paper_sample.csv",
    dtype={"pin": str, "application_key": str},
    low_memory=False,
)
addresses = pd.read_csv(
    "../input/parcel_addresses_2025_chicago.csv",
    dtype={"pin": str},
    low_memory=False,
)

projects["construction_proxy_date"] = pd.to_datetime(
    projects["construction_year"].astype(int).astype(str) + "-06-15"
)
projects["ordinance_date_2012"] = pd.to_datetime(
    projects["ordinance_date_2012"], format="%Y-%m-%d", errors="coerce"
)
projects["history_seed_zone_group"] = projects[
    "reviewed_candidate_zone_group_2006"
]
projects["history_seed_date"] = pd.Timestamp("2006-01-01")
projects["history_seed_status"] = "reviewed_january_2006_group"
projects["exact_january_2006_group_supported"] = projects[
    "history_seed_zone_group"
].notna()
projects["history_seed_evidence"] = (
    "October 2012 polygon reversed to its reviewed January 2006 broad group."
)

unresolved = projects["history_seed_zone_group"].isna()
post_event = (
    unresolved
    & projects["ordinance_date_2012"].notna()
    & (projects["ordinance_date_2012"] <= projects["construction_proxy_date"])
)
destination_supported = post_event & projects.apply(
    lambda row: str(row["zone_group_2012"])
    in str(row["destination_groups"]).split(";"),
    axis=1,
)
projects.loc[destination_supported, "history_seed_zone_group"] = projects.loc[
    destination_supported, "zone_group_2012"
]
projects.loc[destination_supported, "history_seed_date"] = projects.loc[
    destination_supported, "ordinance_date_2012"
]
projects.loc[destination_supported, "history_seed_status"] = (
    "preconstruction_ordinance_destination"
)
projects.loc[
    destination_supported, "exact_january_2006_group_supported"
] = False
projects.loc[destination_supported, "history_seed_evidence"] = (
    "The anchor polygon's ordinance predates the June 15 construction-year "
    "proxy, and its official 2012 broad group is a parsed ordinance destination."
)

unresolved = projects["history_seed_zone_group"].isna()
official_snapshot_supported = (
    unresolved
    & (projects["construction_proxy_date"] > pd.Timestamp("2012-10-31"))
)
projects.loc[
    official_snapshot_supported, "history_seed_zone_group"
] = projects.loc[official_snapshot_supported, "zone_group_2012"]
projects.loc[official_snapshot_supported, "history_seed_date"] = pd.Timestamp(
    "2012-10-31"
)
projects.loc[official_snapshot_supported, "history_seed_status"] = (
    "official_october_2012_snapshot"
)
projects.loc[
    official_snapshot_supported, "exact_january_2006_group_supported"
] = False
projects.loc[official_snapshot_supported, "history_seed_evidence"] = (
    "The official October 2012 zoning map establishes this broad group before "
    "the June 15 construction-year proxy."
)

unresolved = projects["history_seed_zone_group"].isna()
official_ordinance_supported = (
    unresolved
    & projects["ordinance_date_2012"].notna()
    & (projects["ordinance_date_2012"] <= projects["construction_proxy_date"])
    & projects["application_key_2012"].notna()
    & ~projects["application_key_2012"].astype(str).eq("17480")
)
projects.loc[
    official_ordinance_supported, "history_seed_zone_group"
] = projects.loc[official_ordinance_supported, "zone_group_2012"]
projects.loc[
    official_ordinance_supported, "history_seed_date"
] = projects.loc[official_ordinance_supported, "ordinance_date_2012"]
projects.loc[official_ordinance_supported, "history_seed_status"] = (
    "official_preconstruction_anchor_ordinance"
)
projects.loc[
    official_ordinance_supported, "exact_january_2006_group_supported"
] = False
projects.loc[official_ordinance_supported, "history_seed_evidence"] = (
    "The official anchor polygon identifies its latest ordinance and broad "
    "group, and that ordinance predates the June 15 construction-year proxy."
)

application_16453 = projects["application_key_2012"].astype(str).eq("16453")
pre_16453 = application_16453 & (
    projects["construction_proxy_date"] < pd.Timestamp("2009-07-29")
)
if pre_16453.sum() != 7:
    raise RuntimeError("The manually reviewed pre-16453 project set has changed.")
projects.loc[pre_16453, "history_seed_zone_group"] = (
    "Multi-Family Residential"
)
projects.loc[pre_16453, "history_seed_date"] = pd.Timestamp("2006-01-01")
projects.loc[pre_16453, "history_seed_status"] = (
    "reviewed_16453_preordinance_rm5"
)
projects.loc[pre_16453, "exact_january_2006_group_supported"] = True
projects.loc[pre_16453, "history_seed_evidence"] = (
    "The July 29, 2009 PD 1145 ordinance places these Campbell, Artesian, and "
    "Maplewood sites in its RM-5 source area. The 2006-2009 journal screen "
    "finds no intervening same-site broad-group amendment."
)

application_16623 = projects["application_key_2012"].astype(str).eq("16623")
pre_16623 = application_16623 & (
    projects["construction_proxy_date"] < pd.Timestamp("2010-06-30")
)
if pre_16623.sum() != 3:
    raise RuntimeError("The manually reviewed pre-16623 project set has changed.")
projects.loc[pre_16623, "history_seed_zone_group"] = "Planned Development"
projects.loc[pre_16623, "history_seed_date"] = pd.Timestamp("1962-04-30")
projects.loc[pre_16623, "history_seed_status"] = "reviewed_pd15_adoption"
projects.loc[pre_16623, "exact_january_2006_group_supported"] = True
projects.loc[pre_16623, "history_seed_evidence"] = (
    "The original PD 15 file covers the Rhodes Avenue sites and includes a "
    "2007 site-plan approval at 3209-3211 S. Rhodes. The June 30, 2010 "
    "ordinance then identifies this area as existing PD 15."
)

application_17480 = projects["application_key_2012"].astype(str).eq("17480")
pre_17480 = application_17480 & (
    projects["construction_proxy_date"] <= pd.Timestamp("2012-10-31")
)
if pre_17480.sum() != 4:
    raise RuntimeError("The manually reviewed pre-17480 project set has changed.")
projects.loc[pre_17480, "history_seed_zone_group"] = (
    "Neighborhood Mixed-Use"
)
projects.loc[pre_17480, "history_seed_date"] = pd.Timestamp("2006-01-01")
projects.loc[pre_17480, "history_seed_status"] = (
    "reviewed_stable_b2_anchor_bad_metadata"
)
projects.loc[pre_17480, "exact_january_2006_group_supported"] = False
projects.loc[pre_17480, "history_seed_evidence"] = (
    "The official 2012, 2014, 2016, and 2025 maps all classify 714-732 W. "
    "25th as B2-1. The 2012 polygon's application 17480 is a metadata error: "
    "that ordinance concerns 516-550 W. Webster. No 2006-2012 journal "
    "ordinance was found at the 25th Street site."
)

application_17547 = projects["application_key_2012"].astype(str).eq("17547")
pre_17547 = application_17547 & (
    projects["construction_proxy_date"] < pd.Timestamp("2012-10-03")
)
if pre_17547.sum() != 3:
    raise RuntimeError("The manually reviewed pre-17547 project set has changed.")
projects.loc[pre_17547, "history_seed_zone_group"] = (
    "Multi-Family Residential"
)
projects.loc[pre_17547, "history_seed_date"] = pd.Timestamp("2006-01-01")
projects.loc[pre_17547, "history_seed_status"] = (
    "reviewed_17547_preordinance_rt4"
)
projects.loc[pre_17547, "exact_january_2006_group_supported"] = True
projects.loc[pre_17547, "history_seed_evidence"] = (
    "The October 3, 2012 ordinance changes RS-3 and RT-4 to B2-2. The sampled "
    "point addresses are east of the 2414-2418 W. Cortland area changed from "
    "RT-4 to RS-3 by A7744 on November 2, 2011, placing them in the RT-4 "
    "source area at the paper's June 15 timing proxy."
)

a7077_expected_pins = {
    "17173220138002",
    "17173230020000",
    "17173230210000",
    "17173230250000",
    "17173230290000",
    "17173230300000",
    "17173230380000",
    "17173230400000",
    "17173230420000",
    "17173230440000",
    "17173230460000",
    "17173320070000",
    "17173320160000",
    "17173320210000",
    "17173320240000",
    "17173330340000",
    "17173330350000",
    "17173330470000",
    "17173340090000",
    "17173340210000",
    "17173340230000",
    "17173340240000",
    "17173340250000",
    "17173340260000",
    "17173340270000",
    "17173340280000",
    "17173340290000",
    "17173340300000",
    "17173340310000",
    "17173340320000",
    "17173340340000",
    "17173340440000",
    "17173350120000",
    "17173350130000",
    "17201020540000",
    "17201020560000",
    "17201020580000",
    "17201020590000",
    "17201020600000",
    "17201020620000",
    "17201020640000",
    "17201030510000",
    "17201030530000",
    "17201030540000",
    "17201030600000",
    "17202000770000",
    "17202000800000",
    "17202070550000",
    "17202070580000",
    "17202070620000",
}
a7077 = projects["application_key"].eq("A7077")
if set(projects.loc[a7077, "pin"]) != a7077_expected_pins:
    raise RuntimeError("The manually reviewed A7077 project set has changed.")
projects.loc[a7077, "history_seed_zone_group"] = "Planned Development"
projects.loc[a7077, "history_seed_date"] = pd.Timestamp("2004-01-14")
projects.loc[a7077, "history_seed_status"] = "manual_2004_pd896_adoption"
projects.loc[a7077, "exact_january_2006_group_supported"] = True
projects.loc[a7077, "history_seed_evidence"] = (
    "The January 14, 2004 adoption of PD 896 covers the sampled Roosevelt "
    "Square Phase I sites. A7077's apparent business-district origin is an "
    "intermediate step within the September 5, 2007 PD amendment."
)

a7818 = projects["application_key"].eq("A7818")
if a7818.sum() != 11:
    raise RuntimeError("The manually reviewed A7818 project set has changed.")
projects.loc[a7818, "history_seed_zone_group"] = "Planned Development"
projects.loc[a7818, "history_seed_date"] = pd.Timestamp("1988-06-22")
projects.loc[a7818, "history_seed_status"] = "manual_1988_pd447_adoption"
projects.loc[a7818, "exact_january_2006_group_supported"] = True
projects.loc[a7818, "history_seed_evidence"] = (
    "The June 22, 1988 adoption of PD 447 covers the sampled Superior, "
    "Kingsbury, and Chicago Avenue sites. The June 27, 2012 journal identifies "
    "A7818 as Planned Development 447 to the same PD as amended."
)

projects["seed_available_before_construction"] = (
    projects["history_seed_zone_group"].notna()
    & (projects["history_seed_date"] <= projects["construction_proxy_date"])
)
if not projects["seed_available_before_construction"].all():
    unresolved_pins = projects.loc[
        ~projects["seed_available_before_construction"], "pin"
    ].tolist()
    raise RuntimeError(
        f"Construction-year history still lacks a supported seed: {unresolved_pins}"
    )

addresses["pin"] = addresses["pin"].str.replace(r"\D", "", regex=True).str.zfill(14)
addresses["year"] = pd.to_numeric(addresses["year"], errors="coerce")
addresses = (
    addresses.sort_values(["pin", "year", "row_id"], ascending=[True, False, True])
    .drop_duplicates("pin")
    [["pin", "prop_address_full", "year", "row_id"]]
)
a7077_review = projects.loc[
    a7077,
    [
        "pin",
        "construction_year",
        "construction_proxy_date",
        "multifamily",
        "longitude",
        "latitude",
        "ordinance_date_2012",
        "history_seed_zone_group",
        "history_seed_status",
        "history_seed_evidence",
    ],
].merge(addresses, on="pin", how="left", validate="one_to_one")
if a7077_review["prop_address_full"].isna().any():
    raise RuntimeError("A7077 review is missing a parcel address.")
a7077_review["source_url"] = (
    "https://gisapps.chicago.gov/gisimages/zoning_pds/PD896.pdf"
)
a7077_review["source_reference"] = (
    "January 14, 2004 journal pages 17892-17894 and 17920-17927"
)

manual_review = projects.loc[
    pre_16453 | pre_16623 | pre_17480 | pre_17547 | a7077 | a7818,
    [
        "pin",
        "application_key_2012",
        "construction_year",
        "construction_proxy_date",
        "multifamily",
        "longitude",
        "latitude",
        "zone_code_2012",
        "zone_group_2012",
        "history_seed_zone_group",
        "history_seed_date",
        "history_seed_status",
        "history_seed_evidence",
    ],
].merge(addresses, on="pin", how="left", validate="one_to_one")
manual_review["source_url"] = None
manual_review.loc[
    manual_review["history_seed_status"].eq("reviewed_16453_preordinance_rm5"),
    "source_url",
] = (
    "https://gisapps.chicago.gov/gisimages/zoning_pds/PD1145.pdf"
)
manual_review.loc[
    manual_review["history_seed_status"].eq("reviewed_pd15_adoption"),
    "source_url",
] = (
    "https://gisapps.chicago.gov/gisimages/zoning_pds/PD15.pdf"
)
manual_review.loc[
    manual_review["history_seed_status"].eq(
        "reviewed_stable_b2_anchor_bad_metadata"
    ),
    "source_url",
] = (
    "https://data.cityofchicago.org/d/p8va-airx"
)
manual_review.loc[
    manual_review["history_seed_status"].eq(
        "reviewed_17547_preordinance_rt4"
    ),
    "source_url",
] = (
    "https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public-1/reports/"
    "2012_10_03_VI_VII.pdf"
)
manual_review.loc[
    manual_review["history_seed_status"].eq("manual_2004_pd896_adoption"),
    "source_url",
] = (
    "https://gisapps.chicago.gov/gisimages/zoning_pds/PD896.pdf"
)
manual_review.loc[
    manual_review["history_seed_status"].eq("manual_1988_pd447_adoption"),
    "source_url",
] = (
    "https://gisapps.chicago.gov/gisimages/zoning_pds/PD447.pdf"
)

projects.to_csv(
    "../output/historical_zoning_paper_sample_history_seeds.csv",
    index=False,
    date_format="%Y-%m-%d",
)
a7077_review.to_csv(
    "../output/historical_zoning_a7077_project_review.csv",
    index=False,
    date_format="%Y-%m-%d",
)
manual_review.to_csv(
    "../output/historical_zoning_paper_sample_seed_reviews.csv",
    index=False,
    date_format="%Y-%m-%d",
)

summaries = []
for sample, rows in {
    "all_within_1500ft": projects,
    "multifamily_within_1500ft": projects.loc[
        projects["multifamily"].astype(str).str.lower().eq("true")
    ],
}.items():
    exact_2006 = rows["exact_january_2006_group_supported"]
    summaries.append(
        {
            "sample": sample,
            "projects": len(rows),
            "exact_january_2006_group_supported": exact_2006.sum(),
            "exact_january_2006_group_supported_share": exact_2006.mean(),
            "later_preconstruction_seed_supported": (
                rows["history_seed_status"]
                .isin(
                    [
                        "preconstruction_ordinance_destination",
                        "official_october_2012_snapshot",
                        "official_preconstruction_anchor_ordinance",
                    ]
                )
                .sum()
            ),
            "construction_history_seed_supported": rows[
                "seed_available_before_construction"
            ].sum(),
            "construction_history_seed_supported_share": rows[
                "seed_available_before_construction"
            ].mean(),
            "unsupported": (~rows["seed_available_before_construction"]).sum(),
        }
    )

pd.DataFrame(summaries).to_csv(
    "../output/historical_zoning_paper_sample_history_seed_summary.csv",
    index=False,
)
