#!/usr/bin/env python3
import pandas as pd


# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_pd_ordinance_far_history/code")

matters = pd.read_csv(
    "../output/post_khan_matters_20160901_20201231.csv", dtype=str, low_memory=False
)
fields = pd.read_csv(
    "../output/post_khan_discovery_fields_20160901_20201231.csv",
    dtype=str,
    low_memory=False,
)

passed = matters.loc[matters["matter_status_name"].eq("90-Final")].copy()
coverage = fields.groupby("matter_id", as_index=False).agg(
    has_from=("from_zoning_canonical", lambda values: values.notna().any()),
    has_to=("to_zoning_canonical", lambda values: values.notna().any()),
)
fallback = passed.merge(coverage, on="matter_id", how="left", validate="one_to_one")
fallback[["has_from", "has_to"]] = fallback[["has_from", "has_to"]].fillna(False)
fallback = fallback.loc[~fallback["has_from"] | ~fallback["has_to"]].copy()
fallback["is_final_candidate"] = True

if fallback["matter_id"].duplicated().any():
    raise ValueError("Post-Khan discovery fallback contains duplicate matter_id rows")

fallback[
    [
        "matter_id",
        "matter_file",
        "matter_title",
        "matter_intro_date",
        "matter_status_name",
        "is_final_candidate",
    ]
].to_csv(
    "../output/post_khan_discovery_fallback_candidates_20160901_20201231.csv",
    index=False,
)
