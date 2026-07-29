#!/usr/bin/env python3
import re

import pandas as pd


# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_pd_ordinance_far_history/code")

matters = pd.read_csv(
    "../output/post_khan_matters_20160901_20201231.csv", dtype=str, low_memory=False
)
candidates = pd.read_csv(
    "../output/post_khan_candidate_final_20160901_20201231.csv",
    dtype=str,
    low_memory=False,
)
fields = pd.read_csv(
    "../output/post_khan_discovery_fields_20160901_20201231.csv",
    dtype=str,
    low_memory=False,
)
fallback_fields = pd.read_csv(
    "../output/post_khan_discovery_fallback_fields_20160901_20201231.csv",
    dtype=str,
    low_memory=False,
)
pages = pd.read_csv(
    "../output/post_khan_discovery_text_20160901_20201231.csv",
    dtype=str,
    low_memory=False,
)
fallback_pages = pd.read_csv(
    "../output/post_khan_discovery_fallback_text_20160901_20201231.csv",
    dtype=str,
    low_memory=False,
)
manual = pd.read_csv(
    "../input/post_khan_pd_queue_manual_classifications.csv", dtype=str
)

final_ids = set(
    candidates.loc[
        candidates["is_final_candidate"].astype(str).str.lower().eq("true"),
        "matter_id",
    ]
)
passed = matters.loc[
    matters["matter_id"].isin(final_ids)
    & matters["matter_status_name"].eq("90-Final")
].copy()

all_fields = pd.concat([fields, fallback_fields], ignore_index=True).drop_duplicates(
    ["matter_id", "attachment_id"], keep="last"
)
all_fields["zoning_parse_quality"] = pd.to_numeric(
    all_fields["zoning_parse_quality"], errors="coerce"
).fillna(0)
all_fields["has_complete_pair"] = (
    all_fields["from_zoning_canonical"].notna()
    & all_fields["to_zoning_canonical"].notna()
)
all_fields["is_pd_to_pd"] = (
    all_fields["from_zoning_canonical"].eq("PD")
    & all_fields["to_zoning_canonical"].eq("PD")
)
all_fields = all_fields.sort_values(
    ["matter_id", "is_pd_to_pd", "has_complete_pair", "zoning_parse_quality"],
    ascending=[True, False, False, False],
)
best_fields = all_fields.groupby("matter_id", as_index=False).first()

all_pages = pd.concat([pages, fallback_pages], ignore_index=True).drop_duplicates(
    ["matter_id", "attachment_id", "page_number"], keep="last"
)
page_text = (
    all_pages.sort_values(["matter_id", "attachment_id", "page_number"])
    .groupby("matter_id", as_index=False)["page_text"]
    .agg(lambda values: "\n".join(values.fillna("")))
)
discovery = passed.merge(best_fields, on="matter_id", how="left", validate="one_to_one")
discovery = discovery.merge(page_text, on="matter_id", how="left", validate="one_to_one")

pd_to_pd = discovery.loc[
    discovery["from_zoning_canonical"].eq("PD")
    & discovery["to_zoning_canonical"].eq("PD")
].copy()

pd_pattern = re.compile(
    r"(?:PLANNED\s+DEVELOPMENT|\bPD)\s*(?:NO\.?|NUMBER|#)?\s*([0-9]{1,4})",
    re.IGNORECASE,
)
pd_numbers = []
pd_number_sources = []
for row in pd_to_pd.itertuples(index=False):
    pd_number = ""
    pd_number_source = "unresolved"
    for column in [
        "to_zoning_raw",
        "to_zoning",
        "from_zoning_raw",
        "from_zoning",
        "page_text",
        "matter_title",
    ]:
        value = getattr(row, column, "")
        match = pd_pattern.search(str(value)) if pd.notna(value) else None
        if match:
            pd_number = match.group(1)
            pd_number_source = column
            break
    pd_numbers.append(pd_number)
    pd_number_sources.append(pd_number_source)

pd_to_pd["pd_number"] = pd_numbers
pd_to_pd["pd_number_source"] = pd_number_sources
pd_to_pd["is_final_candidate"] = True
pd_to_pd["classification"] = "pd_to_pd"
pd_to_pd["queue_source"] = "automatic_parse"

manual["include_in_pd_to_pd_queue"] = manual["include_in_pd_to_pd_queue"].map(
    {"True": True, "False": False}
)
if manual["matter_id"].duplicated().any():
    raise ValueError("Manual post-Khan PD classifications contain duplicate matters")
if manual["include_in_pd_to_pd_queue"].isna().any():
    raise ValueError("Manual post-Khan PD classifications contain an invalid inclusion flag")
if set(manual["matter_id"]) & set(pd_to_pd["matter_id"]):
    raise ValueError("Manual post-Khan PD classification duplicates an automatic queue row")

available_pages = set(
    zip(
        all_pages["matter_id"],
        all_pages["attachment_id"],
        pd.to_numeric(all_pages["page_number"], errors="raise").astype(int),
    )
)
for row in manual.itertuples(index=False):
    citation = (row.matter_id, row.source_attachment, int(row.source_page))
    if citation not in available_pages:
        raise ValueError(f"Missing manual classification source page for {row.matter_id}")

manual_additions = manual.loc[manual["include_in_pd_to_pd_queue"]].merge(
    passed[
        ["matter_id", "matter_title", "matter_intro_date", "matter_passed_date"]
    ],
    on="matter_id",
    how="left",
    validate="one_to_one",
)
if manual_additions["matter_title"].isna().any():
    raise ValueError("Manual PD-to-PD addition is absent from passed post-Khan matters")
manual_additions["pd_number_source"] = "manual_ordinance_review"
manual_additions["from_zoning_raw"] = manual_additions["from_zoning"]
manual_additions["to_zoning_raw"] = manual_additions["to_zoning"]
manual_additions["zoning_parse_method"] = "manual_ordinance_review"
manual_additions["zoning_parse_quality"] = 4
manual_additions["is_final_candidate"] = True
manual_additions["queue_source"] = "manual_ordinance_review"

pd_to_pd = pd.concat([pd_to_pd, manual_additions], ignore_index=True)

if pd_to_pd["matter_id"].duplicated().any():
    raise ValueError("Post-Khan PD-to-PD queue contains duplicate matter_id rows")

pd_to_pd[
    [
        "matter_id",
        "matter_title",
        "matter_intro_date",
        "matter_passed_date",
        "pd_number",
        "pd_number_source",
        "from_zoning",
        "to_zoning",
        "from_zoning_raw",
        "to_zoning_raw",
        "zoning_parse_method",
        "zoning_parse_quality",
        "is_final_candidate",
        "classification",
        "queue_source",
    ]
].to_csv("../output/post_khan_pd_to_pd_queue_20160901_20201231.csv", index=False)

pd.DataFrame(
    [
        {
            "candidate_matters": len(matters),
            "passed_candidate_matters": len(passed),
            "matters_with_attachment_record": int(
                passed["matter_id"].isin(all_fields["matter_id"]).sum()
            ),
            "matters_with_parsed_from_to": int(
                (
                    discovery["from_zoning_canonical"].notna()
                    & discovery["to_zoning_canonical"].notna()
                ).sum()
            ),
            "pd_to_pd_matters": len(pd_to_pd),
            "pd_to_pd_with_pd_number": int(pd_to_pd["pd_number"].ne("").sum()),
            "manual_pd_to_pd_matters": int(
                pd_to_pd["queue_source"].eq("manual_ordinance_review").sum()
            ),
            "manually_excluded_one_sided_pd_matters": int(
                (~manual["include_in_pd_to_pd_queue"]).sum()
            ),
        }
    ]
).to_csv("../output/post_khan_discovery_summary_20160901_20201231.csv", index=False)
