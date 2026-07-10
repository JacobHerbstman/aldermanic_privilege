#!/usr/bin/env python3
import pandas as pd


# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_pd_ordinance_far_history/code")

queue = pd.read_csv("../output/unresolved_pd_to_pd_queue.csv", dtype=str)
decisions = pd.read_csv("../input/pd_far_manual_decisions.csv", dtype=str)
elms_review = pd.read_csv("../output/pd_far_review_queue.csv", dtype=str)
archive_review = pd.read_csv("../output/pd_archive_review_queue.csv", dtype=str)
elms_pages = pd.read_csv("../output/pd_all_attachment_text.csv", dtype=str, low_memory=False)
archive_pages = pd.read_csv("../output/pd_archive_text.csv", dtype=str, low_memory=False)
sample = pd.read_csv("../input/khan_reconciled_sample.csv", low_memory=False)
pd_evidence = pd.read_csv("../input/khan_pd_underlying_evidence.csv", dtype=str)

if decisions["matter_id"].duplicated().any():
    raise ValueError("PD FAR decisions contain duplicate matter_id rows")
if not set(decisions["matter_id"]).issubset(set(queue["matter_id"])):
    raise ValueError("PD FAR decision is absent from the unresolved queue")
if not decisions["confidence"].eq("high").all():
    raise ValueError("Only high-confidence decisions belong in the accepted decision table")
allowed_scopes = {"whole_pd", "affected_subarea"}
if not set(decisions["scope"]).issubset(allowed_scopes):
    raise ValueError("Accepted old/new FAR decisions must use a documented comparable scope")

queue_pd = queue.set_index("matter_id")["pd_number"]
for row in decisions.itertuples(index=False):
    cited_pages = {int(value) for value in str(row.source_pages).split(";")}
    if row.source_attachment.startswith("dpd_pd_archive_PD"):
        source_pd = row.source_attachment.removeprefix("dpd_pd_archive_PD")
        if str(queue_pd.loc[row.matter_id]) != source_pd:
            raise ValueError(f"Archive PD citation does not match {row.matter_id}")
        available_pages = set(
            pd.to_numeric(
                archive_pages.loc[archive_pages["pd_number"].eq(source_pd), "page_number"],
                errors="raise",
            ).astype(int)
        )
    elif row.source_attachment.startswith("elms_"):
        available_pages = set(
            pd.to_numeric(
                elms_pages.loc[
                    elms_pages["matter_id"].eq(row.matter_id)
                    & elms_pages["attachment_id"].eq(row.source_attachment),
                    "page_number",
                ],
                errors="raise",
            ).astype(int)
        )
    else:
        raise ValueError(f"Unknown decision source for {row.matter_id}")
    if not cited_pages.issubset(available_pages):
        raise ValueError(f"Missing cited source page for {row.matter_id}")

decisions["old_far"] = pd.to_numeric(decisions["old_far"], errors="raise")
decisions["new_far"] = pd.to_numeric(decisions["new_far"], errors="raise")
decisions["far_change"] = decisions["new_far"] - decisions["old_far"]

audit = queue[["matter_id", "matter_title", "pd_number"]].merge(
    decisions, on="matter_id", how="inner", validate="one_to_one"
)
audit.to_csv("../output/pd_far_decisions_audit.csv", index=False)

unresolved = queue.loc[~queue["matter_id"].isin(audit["matter_id"])].copy()
unresolved = unresolved.merge(
    elms_review[["matter_id", "has_far_evidence"]],
    on="matter_id",
    how="left",
    validate="one_to_one",
)
unresolved = unresolved.merge(
    archive_review[["matter_id", "download_status", "archive_far_labels"]],
    on="matter_id",
    how="left",
    validate="one_to_one",
)
unresolved["archive_far_labels"] = pd.to_numeric(
    unresolved["archive_far_labels"], errors="coerce"
).fillna(0).astype(int)
unresolved["unresolved_reason"] = "no_comparable_old_new_scope"
unresolved.loc[unresolved["pd_number"].isna(), "unresolved_reason"] = "pd_number_unresolved"
unresolved.loc[
    unresolved["pd_number"].notna()
    & unresolved["has_far_evidence"].ne("True")
    & unresolved["archive_far_labels"].eq(0),
    "unresolved_reason",
] = "no_far_statement_found"
unresolved.to_csv("../output/pd_far_unresolved.csv", index=False)

pd.DataFrame(
    [
        {
            "unresolved_pd_to_pd_rows": len(queue),
            "accepted_old_new_pairs": len(audit),
            "accepted_share": len(audit) / len(queue),
            "unresolved_after_review": len(unresolved),
            "whole_pd_pairs": int(audit["scope"].eq("whole_pd").sum()),
            "affected_subarea_pairs": int(audit["scope"].eq("affected_subarea").sum()),
            "archive_supported_pairs": int(
                audit["source_attachment"].str.startswith("dpd_pd_archive_").sum()
            ),
            "elms_supported_pairs": int(
                audit["source_attachment"].str.startswith("elms_").sum()
            ),
            "mean_old_far": audit["old_far"].mean(),
            "mean_new_far": audit["new_far"].mean(),
            "mean_far_change": audit["far_change"].mean(),
            "upzones": int(audit["far_change"].gt(0).sum()),
            "downzones": int(audit["far_change"].lt(0).sum()),
            "no_far_change": int(audit["far_change"].eq(0).sum()),
        }
    ]
).to_csv("../output/pd_far_decision_summary.csv", index=False)

for column in ["from_far", "to_far"]:
    sample[column] = pd.to_numeric(sample[column], errors="coerce")
sample.loc[sample["from_code"].eq("T") & sample["from_far"].isna(), "from_far"] = 1.5
sample.loc[sample["to_code"].eq("T") & sample["to_far"].isna(), "to_far"] = 1.5
from_pd = sample["from_code"].eq("PD")
to_pd = sample["to_code"].eq("PD")
sample.loc[
    ~from_pd & to_pd & sample["to_far"].isna() & sample["from_far"].notna(), "to_far"
] = sample["from_far"]
sample.loc[
    from_pd & ~to_pd & sample["from_far"].isna() & sample["to_far"].notna(), "from_far"
] = sample["to_far"]
pd_evidence["underlying_far"] = pd.to_numeric(pd_evidence["underlying_far"], errors="coerce")
for row in pd_evidence.loc[pd_evidence["underlying_far"].notna()].itertuples(index=False):
    sample.loc[sample["matter_id"].eq(row.matter_id), ["from_far", "to_far"]] = row.underlying_far

documented = sample.copy()
ordinance = sample.copy()
for row in audit.itertuples(index=False):
    ordinance.loc[
        ordinance["matter_id"].eq(row.matter_id), ["from_far", "to_far"]
    ] = [row.old_far, row.new_far]

comparison_rows = []
for variant, data in [
    ("khan_documented_structural_rules", documented),
    ("documented_plus_ordinance_pairs", ordinance),
]:
    paired = data.loc[data["from_far"].notna() & data["to_far"].notna()].copy()
    comparison_rows.append(
        {
            "variant": variant,
            "rows": len(data),
            "from_far_n": int(data["from_far"].notna().sum()),
            "from_far_mean": data["from_far"].mean(),
            "from_far_max": data["from_far"].max(),
            "from_far_gap_vs_khan": data["from_far"].mean() - 2.344,
            "to_far_n": int(data["to_far"].notna().sum()),
            "to_far_mean": data["to_far"].mean(),
            "to_far_max": data["to_far"].max(),
            "to_far_gap_vs_khan": data["to_far"].mean() - 2.393,
            "paired_far_n": len(paired),
            "paired_mean_change": (paired["to_far"] - paired["from_far"]).mean(),
        }
    )
pd.DataFrame(comparison_rows).to_csv(
    "../output/pd_far_khan_comparison_summary.csv", index=False
)
