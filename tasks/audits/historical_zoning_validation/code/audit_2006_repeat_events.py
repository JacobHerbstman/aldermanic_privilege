# setwd("tasks/audits/historical_zoning_validation/code")

import pandas as pd


reviews = pd.DataFrame(
    [
        ("14414", "different_site", "Industrial", "The prior PD amendment is north of 37th Street; the sampled PINs are on 38th Place."),
        ("15685", "different_site", "Neighborhood Mixed-Use", "The prior site is at Kilbourn; the sampled project is at 4719 W Lawrence near Keating."),
        ("15814", "different_site", "Single-Family Residential", "The prior Ohio Street site is at Wood; the sampled project is at 2057 W Ohio near Hoyne."),
        ("15822", "different_site", "Neighborhood Mixed-Use", "The prior Lincoln Avenue site is at Diversey; the sampled project is at 2653 N Lincoln near Kenmore."),
        ("15923", "different_site", "Industrial", "The earlier Lakewood site lies south of Schubert and the Seminary site lies west of Seminary; neither reaches the sampled Diversey frontage."),
        ("15948", "different_sampled_area", "Planned Development", "The prior amendment is at Wabansia and Bosworth within a large multi-area ordinance; the sampled PIN is 1545 W North Avenue."),
        ("16142", "same_site_same_candidate_group", "Commercial", "The earlier Milwaukee and Kedvale ordinance retains the commercial broad group used by the candidate map."),
        ("16294", "adjacent_site", "Multi-Family Residential", "The prior amendment covers Wood to Wolcott; the sampled projects lie in the adjacent block from Wolcott to Damen."),
        ("16333", "adjacent_site", "Industrial", "The Marshfield parcels use nonoverlapping north-of-Wabansia offsets."),
        ("16404", "different_site", "Neighborhood Mixed-Use", "The earlier Montrose parcels are at Drake or Central Park, or on the opposite side of Montrose."),
        ("16550", "adjacent_site", "Single-Family Residential", "The Cortez parcels use nonoverlapping offsets east of Ashland."),
        ("16566", "different_site", "Commercial", "The earlier Ashland parcels lie north of Waveland or farther south; the sampled project is immediately south and west of Waveland."),
        ("16603", "different_site", "Single-Family Residential", "The prior Campbell sites are at Wabansia and Charleston; the sampled project is at Lyndale."),
        ("16606", "different_site", "Single-Family Residential", "The earlier Huron parcels are east of Armour or near Bishop; the sampled project is west of Armour."),
        ("16718", "different_site", "Single-Family Residential", "The prior Augusta site is at Campbell; the sampled project is near Rockwell."),
        ("16742", "different_site", "Neighborhood Mixed-Use", "The earlier Hyde Park amendment begins at 53rd Street; the sampled project is at 5105 S Harper."),
        ("16781", "same_site_same_candidate_group", "Planned Development", "The earlier ordinance changes PD 182 to a commercial district and immediately back to a planned development over the same bounds."),
        ("16812", "same_site_candidate_correction", "Commercial", "The exact site changed from C1-2 to B2-5 and then to a planned development in July 2007."),
        ("16818", "different_site", "Single-Family Residential", "The earlier Augusta parcels are near Greenview or Elston; the sampled project is immediately east of Ashland."),
        ("16821", "different_sampled_area", "Neighborhood Mixed-Use", "The prior 1621 N Washtenaw parcel is in the interior residential portion; the sampled PINs front North Avenue in the business-district portion."),
        ("16871", "different_site", "Single-Family Residential", "The earlier Erie parcel is near Paulina and the other candidate is south at Lake and Washington; the sampled project is at Damen."),
        ("16952", "adjacent_site", "Single-Family Residential", "The Huron parcels use nonoverlapping offsets west of Oakley and opposite sides of the street."),
        ("17078", "same_site_same_candidate_group", "Neighborhood Mixed-Use", "Two earlier ordinances have the same 3431-3435 W Armitage bounds and retain the neighborhood mixed-use group."),
        ("17081", "same_site_same_candidate_group", "Neighborhood Mixed-Use", "The earlier ordinance has the same 3503 W Armitage address and retains the neighborhood mixed-use group."),
        ("17088", "different_sampled_area", "Neighborhood Mixed-Use", "The prior Sheridan Road amendments are away from the sampled PIN at 1209 W Arthur Avenue."),
        ("17218", "same_site_same_candidate_group", "Multi-Family Residential", "The sampled Van Buren block is inside the earlier East Lake/West End ordinance, where the immediate source is RM5."),
        ("17369", "different_site", "Single-Family Residential", "The prior Erie parcels are near Paulina or Damen; the sampled project is immediately east of Wolcott."),
        ("17381", "different_site", "Commercial", "The candidate is a large Lincoln Park ordinance; the sampled project is at Madison and Racine."),
        ("17382", "different_site", "Neighborhood Mixed-Use", "The prior Halsted site is at Addison; the sampled project is at Grace and Bradley."),
        ("17436", "same_site_candidate_correction", "Commercial", "The September 2007 ordinance uses the same Shakespeare and California bounds and changes C1-2 to B2-3."),
        ("17516", "different_site", "Single-Family Residential", "The Hamilton parcels use separated offsets from 33rd and 34th Streets."),
        ("17533", "different_site", "Single-Family Residential", "The earlier Leavitt parcel is at Le Moyne; the sampled project is at Potomac."),
        ("A7380", "same_site_same_candidate_group", "Planned Development", "The July 2006 ordinance has the same Kingsbury, Huron, and Erie footprint and starts from a planned development."),
        ("A7404", "different_site", "Planned Development", "The Stratford parcels use nonoverlapping offsets east of Broadway."),
        ("A7480", "same_site_same_candidate_group", "Multi-Family Residential", "The sampled Pershing and 38th Place PINs were in the RT4 portion changed to RM4.5 before PD 840 was amended."),
        ("A7505", "different_sampled_area", "Neighborhood Mixed-Use", "The prior Rice Street ordinance reaches the opposite side near Hoyne; the sampled PIN lies in the northern B2 portion."),
        ("A7591", "different_site", "Single-Family Residential", "The prior Grace parcel is at Kostner; the sampled project is at Keystone."),
        ("A7764", "same_site_candidate_correction", "Commercial", "Three January 2007 ordinances partition the sampled Ashland frontage and change C1-2 to B2-3."),
        ("A7775", "same_site_candidate_correction", "Neighborhood Mixed-Use", "The sampled 2945 N Milwaukee parcel is in the March 2007 footprint changed from B3-2 to C1-2."),
    ],
    columns=[
        "application_key",
        "review_classification",
        "reviewed_zone_group_2006",
        "review_note",
    ],
)

paper_extension_reviews = pd.DataFrame(
    [
        (
            "15752",
            "candidate_group_invariant_to_overlap",
            "Single-Family Residential",
            "The possible earlier amendment was passed after the June 15, 2006 construction proxy and starts from the same single-family group.",
        ),
        (
            "15753",
            "candidate_group_invariant_to_overlap",
            "Industrial",
            "The possible earlier amendment was passed after the June 15, 2006 construction proxy and starts from the same industrial group.",
        ),
        (
            "16451",
            "different_site",
            "Commercial",
            "The earlier Belmont Avenue amendment is west of Albany Avenue; the sampled project is at Francisco Avenue.",
        ),
        (
            "16651",
            "different_site",
            "Multi-Family Residential",
            "The earlier school planned development runs from 21st Street to Cermak Road; the sampled project is north at 19th Street and Loomis Street.",
        ),
        (
            "16744",
            "different_site",
            "Single-Family Residential",
            "The earlier Superior Street parcel is east of Leavitt Street; the sampled project is at Paulina Street.",
        ),
        (
            "16904",
            "different_site",
            "Single-Family Residential",
            "The earlier Keeler Avenue amendment is at Fullerton Avenue; the sampled project is at Wabansia Avenue.",
        ),
        (
            "17121",
            "same_site_candidate_correction",
            "Industrial",
            "The January 2010 ordinance has the same 2883 South Hillock footprint and changes the site from an industrial district to a multifamily district before the November 2010 amendment.",
        ),
        (
            "17264",
            "different_site",
            "Industrial",
            "The sampled pre-amendment project is at Polk Street; the possible earlier amendments are at Congress Parkway, Fifth Avenue, Adams Street, or Fillmore Street.",
        ),
        (
            "17395",
            "different_site",
            "Commercial",
            "The sampled project is 3211 West Armitage on the south side of the street; the nearby repeated amendments are different parcels, principally 3228-3234 West Armitage on the north side.",
        ),
        (
            "A5888",
            "different_sampled_area",
            "Single-Family Residential",
            "The earlier business-district amendment is the Milwaukee Avenue wedge at Windsor Avenue; the sampled PINs are in the RS block bounded by Lockwood Avenue, Sunnyside Avenue, and the alleys southwest of Milwaukee Avenue.",
        ),
        (
            "A7437",
            "candidate_group_invariant_to_overlap",
            "Single-Family Residential",
            "The possible earlier amendment both starts and ends in the same single-family broad group used by the candidate map.",
        ),
    ],
    columns=[
        "application_key",
        "review_classification",
        "reviewed_zone_group_2006",
        "review_note",
    ],
)
reviews = pd.concat([reviews, paper_extension_reviews], ignore_index=True)

reviewed_prior_dates = {
    "16812": "2007-07-19",
    "17436": "2007-09-05",
    "A7764": "2007-01-11",
    "A7775": "2007-03-14",
    "17121": "2010-01-13",
}
reviews["reviewed_prior_event_date"] = reviews["application_key"].map(
    reviewed_prior_dates
)

screen = pd.read_csv(
    "../output/historical_zoning_2006_repeat_event_screen_paper_sample.csv",
    dtype={"application_key": str, "prior_application_key": str},
    low_memory=False,
)

project_dates = pd.read_csv(
    "../output/historical_zoning_2006_project_map.csv",
    dtype={"pin": str, "application_key": str},
    low_memory=False,
)
project_dates = project_dates.loc[
    pd.to_numeric(project_dates["dist_to_boundary"], errors="coerce").le(1500)
    & project_dates["application_key"].isin(screen["application_key"])
].copy()
project_dates["construction_proxy_date"] = pd.to_datetime(
    project_dates["construction_year"].astype(int).astype(str) + "-06-15"
)
project_dates["ordinance_date_2012"] = pd.to_datetime(
    project_dates["ordinance_date_2012"], errors="coerce"
)

reviewed_applications = set(reviews["application_key"])
preconstruction_target_applications = set(
    project_dates.loc[
        ~project_dates["application_key"].isin(reviewed_applications)
    ]
    .groupby("application_key")
    .filter(
        lambda rows: (
            rows["ordinance_date_2012"] <= rows["construction_proxy_date"]
        ).all()
    )["application_key"]
)
preconstruction_target_reviews = pd.DataFrame(
    {
        "application_key": sorted(preconstruction_target_applications),
        "review_classification": "target_predates_all_sampled_construction",
        "reviewed_zone_group_2006": pd.NA,
        "review_note": (
            "The target amendment establishes an official broad zoning group "
            "before every sampled construction proxy; an exact January 2006 "
            "group is not required for construction-year zoning."
        ),
        "reviewed_prior_event_date": pd.NaT,
    }
)
if len(preconstruction_target_reviews) != 31:
    raise RuntimeError(
        "Expected 31 paper-extension applications with a target amendment "
        "before all sampled construction dates."
    )
reviews = pd.concat(
    [reviews, preconstruction_target_reviews], ignore_index=True
)

screen_applications = set(screen["application_key"])
reviewed_applications = set(reviews["application_key"])
if screen_applications != reviewed_applications:
    missing = sorted(screen_applications - reviewed_applications)
    extra = sorted(reviewed_applications - screen_applications)
    raise RuntimeError(
        f"Repeat-event review is out of date. Missing={missing}; extra={extra}."
    )

projects = pd.read_csv(
    "../output/historical_zoning_2006_project_map.csv",
    dtype={"pin": str, "application_key": str},
    low_memory=False,
)
projects = projects.loc[
    pd.to_numeric(projects["dist_to_boundary"], errors="coerce").le(1500)
    & projects["application_key"].isin(reviewed_applications)
].copy()
projects = projects.merge(
    reviews,
    on="application_key",
    how="left",
    validate="many_to_one",
)
projects["candidate_group_changes_after_review"] = (
    projects["review_classification"].eq("same_site_candidate_correction")
    & projects["candidate_zone_group_2006"].ne(
        projects["reviewed_zone_group_2006"]
    )
)

project_summary = (
    projects.groupby("application_key", as_index=False)
    .agg(
        sampled_projects=("pin", "size"),
        sampled_multifamily_projects=("multifamily", "sum"),
        earliest_construction_year=("construction_year", "min"),
        latest_construction_year=("construction_year", "max"),
        candidate_group_changes_after_review=(
            "candidate_group_changes_after_review",
            "max",
        ),
    )
)
reviews = reviews.merge(
    project_summary,
    on="application_key",
    how="left",
    validate="one_to_one",
)
reviews["earlier_change_predates_sampled_construction"] = False
correction_rows = reviews["review_classification"].eq(
    "same_site_candidate_correction"
)
reviews.loc[correction_rows, "earlier_change_predates_sampled_construction"] = (
    pd.to_datetime(
        reviews.loc[correction_rows, "reviewed_prior_event_date"]
    ).dt.year
    < reviews.loc[correction_rows, "earliest_construction_year"]
)

reviews.to_csv(
    "../output/historical_zoning_2006_repeat_event_review_paper_sample.csv",
    index=False,
)
projects.to_csv(
    "../output/historical_zoning_2006_repeat_event_project_review_paper_sample.csv",
    index=False,
)

reviewed_map = pd.read_csv(
    "../output/historical_zoning_2006_project_map.csv",
    dtype={"pin": str, "application_key": str},
    low_memory=False,
)
reviewed_map = reviewed_map.loc[
    pd.to_numeric(reviewed_map["dist_to_boundary"], errors="coerce").le(1500)
].copy()
reviewed_map = reviewed_map.merge(
    reviews[
        [
            "application_key",
            "review_classification",
            "reviewed_zone_group_2006",
            "review_note",
        ]
    ],
    on="application_key",
    how="left",
    validate="many_to_one",
)
reviewed_map["reviewed_candidate_zone_group_2006"] = reviewed_map[
    "candidate_zone_group_2006"
]
correction_projects = reviewed_map["review_classification"].eq(
    "same_site_candidate_correction"
)
preconstruction_seed_projects = reviewed_map["review_classification"].eq(
    "target_predates_all_sampled_construction"
)
reviewed_map.loc[
    correction_projects, "reviewed_candidate_zone_group_2006"
] = reviewed_map.loc[correction_projects, "reviewed_zone_group_2006"]
reviewed_map.loc[
    preconstruction_seed_projects, "reviewed_candidate_zone_group_2006"
] = pd.NA
reviewed_map["repeat_event_review_status"] = "no_repeat_candidate_flagged"
reviewed_map.loc[
    reviewed_map["review_classification"].notna(),
    "repeat_event_review_status",
] = "reviewed_candidate_supported"
reviewed_map.loc[
    correction_projects, "repeat_event_review_status"
] = "reviewed_candidate_corrected"
reviewed_map.loc[
    preconstruction_seed_projects, "repeat_event_review_status"
] = "target_preconstruction_seed_required"
reviewed_map.loc[
    reviewed_map["reviewed_candidate_zone_group_2006"].isna(),
    "repeat_event_review_status",
] = "initial_reconstruction_unresolved"
reviewed_map.to_csv(
    "../output/historical_zoning_2006_project_map_reviewed_paper_sample.csv",
    index=False,
)

repeat_summary = pd.DataFrame(
    [
        {
            "target_applications_reviewed": len(reviews),
            "candidate_prior_ordinances_reviewed": len(screen),
            "applications_with_same_site_repeat": reviews[
                "review_classification"
            ]
            .str.startswith("same_site")
            .sum(),
            "applications_requiring_2006_group_correction": correction_rows.sum(),
            "sampled_projects_requiring_2006_group_correction": projects[
                "candidate_group_changes_after_review"
            ].sum(),
            "sampled_multifamily_projects_requiring_2006_group_correction": projects.loc[
                projects["candidate_group_changes_after_review"], "multifamily"
            ].sum(),
            "corrections_predating_all_sampled_construction": reviews.loc[
                correction_rows, "earlier_change_predates_sampled_construction"
            ].all(),
            "unresolved_review_applications": reviews[
                "review_classification"
            ]
            .str.startswith("unresolved")
            .sum(),
            "applications_using_preconstruction_target_seed": len(
                preconstruction_target_reviews
            ),
        }
    ]
)
repeat_summary.to_csv(
    "../output/historical_zoning_2006_repeat_event_review_paper_sample_summary.csv",
    index=False,
)

sample_summaries = []
for sample, sample_rows in {
    "all_within_1500ft": reviewed_map,
    "multifamily_within_1500ft": reviewed_map.loc[
        reviewed_map["multifamily"].astype(str).str.lower().eq("true")
    ],
}.items():
    sample_summaries.append(
        {
            "sample": sample,
            "projects": len(sample_rows),
            "initially_resolved": sample_rows[
                "candidate_zone_group_2006"
            ].notna().sum(),
            "repeat_event_corrections": sample_rows[
                "repeat_event_review_status"
            ].eq("reviewed_candidate_corrected").sum(),
            "reviewed_resolved": sample_rows[
                "reviewed_candidate_zone_group_2006"
            ].notna().sum(),
            "reviewed_resolved_share": sample_rows[
                "reviewed_candidate_zone_group_2006"
            ].notna().mean(),
            "unresolved": sample_rows[
                "reviewed_candidate_zone_group_2006"
            ].isna().sum(),
        }
    )

pd.DataFrame(sample_summaries).to_csv(
    "../output/historical_zoning_2006_project_map_reviewed_paper_sample_summary.csv",
    index=False,
)
