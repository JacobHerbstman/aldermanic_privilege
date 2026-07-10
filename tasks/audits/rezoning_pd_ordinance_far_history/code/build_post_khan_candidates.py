#!/usr/bin/env python3
import pandas as pd


# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_pd_ordinance_far_history/code")

matters = pd.read_csv(
    "../input/matters_20101201_20260212.csv", dtype=str, low_memory=False
)
seeds = pd.read_csv(
    "../input/candidate_seed_ids_20101201_20260212.csv", dtype=str, low_memory=False
)

matters["matter_intro_date"] = pd.to_datetime(
    matters["matter_intro_date"], errors="coerce"
)
in_period = matters["matter_intro_date"].between("2016-09-01", "2020-12-31")
candidate_ids = set(
    seeds.loc[
        seeds["seed_is_candidate"].astype(str).str.lower().eq("true"), "matter_id"
    ]
)

post_khan_matters = matters.loc[
    in_period & matters["matter_id"].isin(candidate_ids)
].copy()
post_khan_seeds = seeds.loc[
    seeds["matter_id"].isin(post_khan_matters["matter_id"])
].copy()
passed_ids = set(
    post_khan_matters.loc[
        post_khan_matters["matter_status_name"].eq("90-Final"), "matter_id"
    ]
)
post_khan_passed_candidates = post_khan_seeds.loc[
    post_khan_seeds["matter_id"].isin(passed_ids)
].copy()
post_khan_passed_candidates["is_final_candidate"] = True

if post_khan_matters["matter_id"].duplicated().any():
    raise ValueError("Post-Khan matter cohort contains duplicate matter_id rows")
if post_khan_seeds["matter_id"].duplicated().any():
    raise ValueError("Post-Khan candidate cohort contains duplicate matter_id rows")
if set(post_khan_matters["matter_id"]) != set(post_khan_seeds["matter_id"]):
    raise ValueError("Post-Khan matters and candidate seeds do not cover the same matters")

post_khan_matters["matter_intro_date"] = post_khan_matters[
    "matter_intro_date"
].dt.strftime("%Y-%m-%d")
post_khan_matters.to_csv(
    "../output/post_khan_matters_20160901_20201231.csv", index=False
)
post_khan_seeds.to_csv(
    "../output/post_khan_candidate_seeds_20160901_20201231.csv", index=False
)
post_khan_passed_candidates.to_csv(
    "../output/post_khan_passed_candidates_20160901_20201231.csv", index=False
)
