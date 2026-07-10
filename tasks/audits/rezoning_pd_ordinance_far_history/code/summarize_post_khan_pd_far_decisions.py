#!/usr/bin/env python3
import pandas as pd


# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_pd_ordinance_far_history/code")

queue = pd.read_csv(
    "../output/post_khan_pd_to_pd_queue_20160901_20201231.csv", dtype=str
)
decisions = pd.read_csv("../input/post_khan_pd_far_manual_decisions.csv", dtype=str)
elms_pages = pd.read_csv(
    "../output/post_khan_pd_all_attachment_text_20160901_20201231.csv",
    dtype=str,
    low_memory=False,
)
archive_pages = pd.read_csv(
    "../output/post_khan_pd_archive_text_20160901_20201231.csv",
    dtype=str,
    low_memory=False,
)
elms_review = pd.read_csv(
    "../output/post_khan_pd_far_review_queue_20160901_20201231.csv", dtype=str
)
archive_review = pd.read_csv(
    "../output/post_khan_pd_archive_review_queue_20160901_20201231.csv", dtype=str
)
khan_audit = pd.read_csv("../output/pd_far_decisions_audit.csv", dtype=str)
khan_queue = pd.read_csv("../output/unresolved_pd_to_pd_queue.csv", dtype=str)

if decisions["matter_id"].duplicated().any():
    raise ValueError("Post-Khan PD FAR decisions contain duplicate matters")
if not set(decisions["matter_id"]).issubset(set(queue["matter_id"])):
    raise ValueError("Post-Khan PD FAR decision is absent from the queue")
if not decisions["confidence"].eq("high").all():
    raise ValueError("Only high-confidence decisions belong in the accepted table")
if not set(decisions["scope"]).issubset({"whole_pd", "affected_subarea"}):
    raise ValueError("Post-Khan PD FAR decision has an unsupported scope")

queue_pd = queue.set_index("matter_id")["pd_number"]
for row in decisions.itertuples(index=False):
    for source, page in [
        (row.old_source_attachment, row.old_source_page),
        (row.new_source_attachment, row.new_source_page),
    ]:
        if source.startswith("dpd_pd_archive_PD"):
            source_pd = source.removeprefix("dpd_pd_archive_PD")
            if str(queue_pd.loc[row.matter_id]) != source_pd:
                raise ValueError(f"Archive PD citation does not match {row.matter_id}")
            available = set(
                pd.to_numeric(
                    archive_pages.loc[
                        archive_pages["pd_number"].eq(source_pd), "page_number"
                    ],
                    errors="raise",
                ).astype(int)
            )
        elif source.startswith("elms_"):
            available = set(
                pd.to_numeric(
                    elms_pages.loc[
                        elms_pages["matter_id"].eq(row.matter_id)
                        & elms_pages["attachment_id"].eq(source),
                        "page_number",
                    ],
                    errors="raise",
                ).astype(int)
            )
        else:
            raise ValueError(f"Unknown post-Khan decision source for {row.matter_id}")
        if int(page) not in available:
            raise ValueError(f"Missing cited source page for {row.matter_id}")

decisions["old_far"] = pd.to_numeric(decisions["old_far"], errors="raise")
decisions["new_far"] = pd.to_numeric(decisions["new_far"], errors="raise")
decisions["far_change"] = decisions["new_far"] - decisions["old_far"]

audit = queue[["matter_id", "matter_title", "matter_intro_date", "matter_passed_date", "pd_number"]].merge(
    decisions, on="matter_id", how="inner", validate="one_to_one"
)
audit.to_csv(
    "../output/post_khan_pd_far_decisions_audit_20160901_20201231.csv", index=False
)

unresolved = queue.loc[~queue["matter_id"].isin(audit["matter_id"])].copy()
unresolved = unresolved.merge(
    elms_review[["matter_id", "has_far_evidence"]],
    on="matter_id",
    how="left",
    validate="one_to_one",
)
unresolved = unresolved.merge(
    archive_review[["matter_id", "archive_far_labels"]],
    on="matter_id",
    how="left",
    validate="one_to_one",
)
unresolved["archive_far_labels"] = pd.to_numeric(
    unresolved["archive_far_labels"], errors="coerce"
).fillna(0).astype(int)
unresolved["unresolved_reason"] = "no_comparable_old_new_scope"
unresolved.loc[
    unresolved["has_far_evidence"].ne("True")
    & unresolved["archive_far_labels"].eq(0),
    "unresolved_reason",
] = "no_far_statement_found"
unresolved.to_csv(
    "../output/post_khan_pd_far_unresolved_20160901_20201231.csv", index=False
)

summary = pd.DataFrame(
    [
        {
            "pd_to_pd_rows": len(queue),
            "accepted_old_new_pairs": len(audit),
            "accepted_share": len(audit) / len(queue),
            "unresolved_after_review": len(unresolved),
            "introduced_by_2020_passed_after_2020": int(
                pd.to_datetime(queue["matter_passed_date"], errors="coerce")
                .gt("2020-12-31")
                .sum()
            ),
            "whole_pd_pairs": int(audit["scope"].eq("whole_pd").sum()),
            "affected_subarea_pairs": int(
                audit["scope"].eq("affected_subarea").sum()
            ),
            "mean_old_far": audit["old_far"].mean(),
            "mean_new_far": audit["new_far"].mean(),
            "mean_far_change": audit["far_change"].mean(),
            "upzones": int(audit["far_change"].gt(0).sum()),
            "downzones": int(audit["far_change"].lt(0).sum()),
            "no_far_change": int(audit["far_change"].eq(0).sum()),
        }
    ]
)
summary.to_csv(
    "../output/post_khan_pd_far_decision_summary_20160901_20201231.csv", index=False
)

pd.DataFrame(
    [
        {
            "period": "2010-11-01_through_2020-12-31_intro_dates",
            "reviewed_pd_to_pd_rows": len(khan_queue) + len(queue),
            "accepted_old_new_pairs": len(khan_audit) + len(audit),
            "unresolved_after_review": (len(khan_queue) - len(khan_audit))
            + len(unresolved),
            "khan_window_rows": len(khan_queue),
            "khan_window_accepted": len(khan_audit),
            "post_khan_rows": len(queue),
            "post_khan_accepted": len(audit),
            "post_khan_introduced_by_2020_passed_after_2020": int(
                pd.to_datetime(queue["matter_passed_date"], errors="coerce")
                .gt("2020-12-31")
                .sum()
            ),
        }
    ]
).to_csv("../output/pd_far_20101101_20201231_summary.csv", index=False)
