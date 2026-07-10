#!/usr/bin/env python3
import re

import pandas as pd


# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_pd_ordinance_far_history/code")

sample = pd.read_csv("../input/khan_reconciled_sample.csv", low_memory=False)
evidence = pd.read_csv("../input/khan_pd_underlying_evidence.csv", dtype=str)
evidence["underlying_far"] = pd.to_numeric(evidence["underlying_far"], errors="coerce")

resolved_ids = set(evidence.loc[evidence["underlying_far"].notna(), "matter_id"])
queue = sample.loc[
    sample["from_code"].eq("PD")
    & sample["to_code"].eq("PD")
    & ~sample["matter_id"].isin(resolved_ids)
].copy()

pd_numbers = []
for row in queue.itertuples(index=False):
    zoning_text = f"{row.from_zoning} {row.to_zoning}"
    match = re.search(
        r"(?:PLANNED DEVELOPMENT|\bPD)\s*(?:NO\.?|NUMBER|#)?\s*([0-9]{1,4})",
        zoning_text,
        re.IGNORECASE,
    )
    pd_numbers.append(match.group(1) if match else "")

queue["pd_number"] = pd_numbers
manual_pd_numbers = {
    "SO2010-5174": "1107",
    "SO2011-7046": "1116",
    "SO2011-8909": "136",
    "SO2014-4170": "44",
    "SO2015-1419": "368",
    "SO2016-665": "1",
}
queue["pd_number_source"] = "parsed_zoning_text"
for matter_id, pd_number in manual_pd_numbers.items():
    mask = queue["matter_id"].eq(matter_id) & queue["pd_number"].eq("")
    queue.loc[mask, "pd_number"] = pd_number
    queue.loc[mask, "pd_number_source"] = "manual_packet_or_official_search"
queue.loc[queue["pd_number"].eq(""), "pd_number_source"] = "unresolved"
queue["is_final_candidate"] = True

if len(queue) != 85:
    raise ValueError(f"Expected 85 unresolved PD-to-PD rows, found {len(queue)}")
if queue["matter_id"].duplicated().any():
    raise ValueError("Unresolved PD-to-PD queue has duplicate matter_id rows")

queue[
    [
        "matter_id",
        "matter_title",
        "matter_intro_date",
        "matter_passed_date",
        "pd_number",
        "pd_number_source",
        "from_zoning",
        "to_zoning",
        "is_final_candidate",
    ]
].to_csv("../output/unresolved_pd_to_pd_queue.csv", index=False)
