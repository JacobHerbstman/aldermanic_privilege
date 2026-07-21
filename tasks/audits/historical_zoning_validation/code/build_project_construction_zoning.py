# setwd("tasks/audits/historical_zoning_validation/code")

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
reviewed_event_keys = pd.DataFrame(
    [
        {"pin": "13262050360000", "matter_id": "O2020-2959"},
        {"pin": "17082050130000", "matter_id": "SO2017-7051"},
        {"pin": "17311180340000", "matter_id": "O2017-7056"},
    ]
)
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

reviewed_project_events = pd.DataFrame(
    [
        {
            "pin": "13262050360000",
            "matter_id": "O2020-2959",
            "matter_passed_date": "2020-07-22",
            "from_groups": "Single-Family Residential",
            "to_group": "Multi-Family Residential",
            "project_match_method": "exact_ordinance_address",
            "parcel_match_method": "reviewed_ordinance_boundary",
            "event_source": "reviewed_official_ordinance",
            "source_pdf": "../input/O2020-2959.pdf",
            "review_note": (
                "Application 20423T1 changes the 3130-3132 N Spaulding "
                "zoning lot from RS-3 to RT-3.5 before the 2022 construction "
                "proxy. The narrative assigns the new single-family house to "
                "3130, but the legal zoning group is RT-3.5."
            ),
        },
        {
            "pin": "17082050130000",
            "matter_id": "SO2017-7051",
            "matter_passed_date": "2018-01-17",
            "from_groups": "Industrial",
            "to_group": "Planned Development",
            "project_match_method": "exact_ordinance_address",
            "parcel_match_method": "reviewed_ordinance_boundary",
            "event_source": "reviewed_official_ordinance",
            "source_pdf": "../input/SO2017-7051.pdf",
            "review_note": (
                "Application 19398 changes M1-3 to DX-5 and then establishes "
                "PD 1396 at 728-738 N Milwaukee before the 2020 construction "
                "proxy. The title parser captured only the intermediate DX-5 "
                "step."
            ),
        },
        {
            "pin": "17092420090000",
            "matter_id": "SO2018-7749",
            "matter_passed_date": "2018-10-31",
            "from_groups": "Downtown",
            "to_group": "Planned Development",
            "project_match_method": "reviewed_ordinance_boundary",
            "parcel_match_method": "official_current_polygon",
            "event_source": "reviewed_official_ordinance",
            "source_pdf": "../input/SO2018-7749.pdf",
            "review_note": (
                "Application 19819 changes DX-7 to PD 1428 before the 2021 "
                "construction proxy. The legal bounds and current official "
                "polygon include the 369 W Grand project."
            ),
        },
        {
            "pin": "17311180340000",
            "matter_id": "O2017-7056",
            "matter_passed_date": "2017-11-21",
            "from_groups": "Single-Family Residential",
            "to_group": "Single-Family Residential",
            "project_match_method": "reviewed_ordinance_boundary",
            "parcel_match_method": "exact_project_point",
            "event_source": "reviewed_official_ordinance",
            "source_pdf": "../input/O2017-7056.pdf",
            "review_note": (
                "Application 19403 changes 2312-2314 W 35th to RT-4 but "
                "explicitly leaves the western 2316 W 35th parcel RS-3. The "
                "range-based link had incorrectly applied the change to 2316."
            ),
        },
        {
            "pin": "25013240390000",
            "matter_id": "SO2018-863",
            "matter_passed_date": "2018-05-25",
            "from_groups": (
                "Single-Family Residential;Neighborhood Mixed-Use;Commercial"
            ),
            "to_group": "Planned Development",
            "project_match_method": "exact_ordinance_address",
            "parcel_match_method": "reviewed_ordinance_boundary",
            "event_source": "reviewed_official_ordinance",
            "source_pdf": "../input/SO2018-863.pdf",
            "review_note": (
                "Application 19526 establishes a planned development at "
                "9329-9429 S Stony Island three weeks before the June 15, 2018 "
                "construction proxy."
            ),
        },
        {
            "pin": "13134080510000",
            "matter_id": "O2024-0010153",
            "matter_passed_date": "2024-10-30",
            "from_groups": "Neighborhood Mixed-Use;Commercial",
            "to_group": "Neighborhood Mixed-Use",
            "project_match_method": "exact_ordinance_address",
            "parcel_match_method": "official_current_polygon",
            "event_source": "reviewed_official_ordinance",
            "source_pdf": "../input/O2024-0010153.pdf",
            "review_note": (
                "Application A-8900 changes the Western Avenue commercial "
                "corridor to B3-3 after the 2019 construction proxy."
            ),
        },
        {
            "pin": "20041100270000",
            "matter_id": "O2024-0008982",
            "matter_passed_date": "2024-06-12",
            "from_groups": (
                "Single-Family Residential;Multi-Family Residential;Industrial"
            ),
            "to_group": "Multi-Family Residential",
            "project_match_method": "reviewed_ordinance_boundary",
            "parcel_match_method": "official_current_polygon",
            "event_source": "reviewed_official_ordinance",
            "source_pdf": "../input/O2024-0008982.pdf",
            "review_note": (
                "Application A-8886 changes the 40th Place, 41st Street, and "
                "Wallace Street site to RT-3.5 after construction."
            ),
        },
        {
            "pin": "20041100350000",
            "matter_id": "O2024-0008982",
            "matter_passed_date": "2024-06-12",
            "from_groups": (
                "Single-Family Residential;Multi-Family Residential;Industrial"
            ),
            "to_group": "Multi-Family Residential",
            "project_match_method": "reviewed_ordinance_boundary",
            "parcel_match_method": "official_current_polygon",
            "event_source": "reviewed_official_ordinance",
            "source_pdf": "../input/O2024-0008982.pdf",
            "review_note": (
                "Application A-8886 changes the 40th Place, 41st Street, and "
                "Wallace Street site to RT-3.5 after construction."
            ),
        },
        {
            "pin": "20041100360000",
            "matter_id": "O2024-0008982",
            "matter_passed_date": "2024-06-12",
            "from_groups": (
                "Single-Family Residential;Multi-Family Residential;Industrial"
            ),
            "to_group": "Multi-Family Residential",
            "project_match_method": "reviewed_ordinance_boundary",
            "parcel_match_method": "official_current_polygon",
            "event_source": "reviewed_official_ordinance",
            "source_pdf": "../input/O2024-0008982.pdf",
            "review_note": (
                "Application A-8886 changes the 40th Place, 41st Street, and "
                "Wallace Street site to RT-3.5 after construction."
            ),
        },
        {
            "pin": "13312140010000",
            "matter_id": "SO2018-4452",
            "matter_passed_date": "2018-09-20",
            "from_groups": (
                "Planned Development;Multi-Family Residential;Industrial"
            ),
            "to_group": "Planned Development",
            "project_match_method": "official_current_polygon",
            "parcel_match_method": "reviewed_ordinance_boundary",
            "event_source": "reviewed_official_ordinance",
            "source_pdf": "../input/SO2018-4452.pdf",
            "review_note": (
                "Application 19687 amends the 10.8-acre PD 1345 site. The "
                "current official polygon assigns this PIN to that application."
            ),
        },
        {
            "pin": "14194350290000",
            "matter_id": "O2022-2464",
            "matter_passed_date": "2022-10-26",
            "from_groups": "Commercial",
            "to_group": "Neighborhood Mixed-Use",
            "project_match_method": "official_current_polygon",
            "parcel_match_method": "reviewed_ordinance_boundary",
            "event_source": "reviewed_official_ordinance",
            "source_pdf": "../input/O2022-2464.pdf",
            "review_note": (
                "Application A8780 changes C1-3 to B3-3 after the 2019 "
                "construction-year proxy."
            ),
        },
        {
            "pin": "17042240390000",
            "matter_id": "SO2019-4107",
            "matter_passed_date": "2020-01-15",
            "from_groups": "Neighborhood Mixed-Use",
            "to_group": "Planned Development",
            "project_match_method": "exact_ordinance_address",
            "parcel_match_method": "reviewed_ordinance_boundary",
            "event_source": "reviewed_official_ordinance",
            "source_pdf": "../input/SO2019-4107.pdf",
            "review_note": (
                "Application 20062 creates PD 1465 at 1200-1212 N State "
                "before the 2022 construction-year proxy."
            ),
        },
        {
            "pin": "17042240390000",
            "matter_id": "SO2023-43",
            "matter_passed_date": "2023-03-15",
            "from_groups": "Planned Development",
            "to_group": "Planned Development",
            "project_match_method": "official_current_polygon",
            "parcel_match_method": "reviewed_ordinance_boundary",
            "event_source": "reviewed_official_ordinance",
            "source_pdf": "../input/SO2023-43.pdf",
            "review_note": "Application 22062 amends PD 1465 without changing its broad group.",
        },
        {
            "pin": "17081340330000",
            "matter_id": "O2023-1331",
            "matter_passed_date": "2023-04-19",
            "from_groups": "Multi-Family Residential",
            "to_group": "Neighborhood Mixed-Use",
            "project_match_method": "exact_ordinance_address",
            "parcel_match_method": "reviewed_ordinance_boundary",
            "event_source": "reviewed_official_ordinance",
            "source_pdf": "../input/O2023-1331.pdf",
            "review_note": (
                "Application 22147 changes RT-4 to B2-2 after the 2022 "
                "construction-year proxy."
            ),
        },
    ]
)
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
application_19687 = projects["ordinance_number_2025"].map(
    normalize_application
).eq("19687")
projects.loc[
    application_19687, "current_ordinance_date_recovered"
] = pd.Timestamp("2018-09-20")
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
exact_preconstruction_pins = {
    "13262050360000",
    "13363160300000",
    "17082050130000",
    "17092420090000",
    "25013240390000",
}
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

event_log = pd.concat(
    [construction_log, current_log], ignore_index=True
).sort_values(["replay", "pin", "matter_passed_date", "matter_id"])
event_log.to_csv(
    "../output/historical_zoning_project_construction_event_log.csv",
    index=False,
    date_format="%Y-%m-%d",
)

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

unresolved = projects.loc[
    ~projects["construction_zone_group_supported"], output_columns
]
unresolved.to_csv(
    "../output/historical_zoning_project_construction_year_unresolved.csv",
    index=False,
    date_format="%Y-%m-%d",
)

summary_rows = []
for sample_name, sample in {
    "all_within_1500ft": projects,
    "multifamily_within_1500ft": projects.loc[
        projects["multifamily"].astype(str).str.lower().eq("true")
    ],
}.items():
    for status, rows in sample.groupby("construction_zoning_status"):
        summary_rows.append(
            {
                "sample": sample_name,
                "construction_zoning_status": status,
                "projects": len(rows),
                "share": len(rows) / len(sample),
                "construction_group_changes_from_anchor": rows[
                    "construction_zone_group"
                ].ne(rows["anchor_zone_group"]).sum(),
            }
        )
    summary_rows.append(
        {
            "sample": sample_name,
            "construction_zoning_status": "TOTAL",
            "projects": len(sample),
            "share": 1.0,
            "construction_group_changes_from_anchor": sample[
                "construction_zone_group"
            ].ne(sample["anchor_zone_group"]).sum(),
        }
    )

pd.DataFrame(summary_rows).to_csv(
    "../output/historical_zoning_project_construction_year_summary.csv",
    index=False,
)

reviewed_matter_check[
    [
        "pin",
        "matter_id",
        "matter_passed_date",
        "matter_title",
        "from_groups",
        "to_group",
        "source_pdf",
        "review_note",
    ]
].to_csv(
    "../output/historical_zoning_project_construction_reviewed_events.csv",
    index=False,
    date_format="%Y-%m-%d",
)
