#!/usr/bin/env python3
import re

import pandas as pd


# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_pd_ordinance_far_history/code")

queue = pd.read_csv(
    "../output/post_khan_pd_to_pd_queue_20160901_20201231.csv", dtype=str
)
pages = pd.read_csv(
    "../output/post_khan_pd_all_attachment_text_20160901_20201231.csv",
    dtype=str,
    low_memory=False,
)
fields = pd.read_csv(
    "../output/post_khan_pd_all_attachment_fields_20160901_20201231.csv",
    dtype=str,
    low_memory=False,
)

pages["page_number"] = pd.to_numeric(pages["page_number"], errors="coerce")
pages = pages.sort_values(["matter_id", "attachment_id", "page_number"])

far_pattern = re.compile(
    r"\b(?P<label>(?:maximum|overall|total|base)\s+"
    r"(?:floor\s+area\s+ratio|f\.?\s*a\.?\s*r\.?))\b",
    re.IGNORECASE,
)
unchanged_pattern = re.compile(
    r"(?:far|floor\s+area\s+ratio)[^.]{0,120}"
    r"(?:unchanged|remain(?:s|ed)?\s+the\s+same|no\s+change)|"
    r"(?:unchanged|remain(?:s|ed)?\s+the\s+same|no\s+change)[^.]{0,120}"
    r"(?:far|floor\s+area\s+ratio)",
    re.IGNORECASE,
)
prior_pattern = re.compile(
    r"(?:planned\s+development|ordinance)[^.]{0,180}"
    r"(?:approved|passed|dated)[^.]{0,180}(?:19|20)\d{2}|"
    r"(?:approved|passed)\s+(?:by\s+the\s+city\s+council\s+)?"
    r"on\s+[A-Z][a-z]+\s+\d{1,2},?\s+(?:19|20)\d{2}",
    re.IGNORECASE,
)
change_pattern = re.compile(
    r"(?:current|existing|previously\s+approved|prior|proposed|amended|"
    r"increase|decrease|reduc(?:e|ed|tion)|unchanged|remain(?:s|ed)?)"
    r"[^.]{0,220}(?:far|floor\s+area\s+ratio)|"
    r"(?:far|floor\s+area\s+ratio)[^.]{0,220}"
    r"(?:current|existing|previously\s+approved|prior|proposed|amended|"
    r"increase|decrease|reduc(?:e|ed|tion)|unchanged|remain(?:s|ed)?)",
    re.IGNORECASE,
)
number_pattern = re.compile(r"(?<![\d,])(?:\d{1,2}(?:\.\d{1,3})?|\.\d{1,3})(?![\d,])")

evidence_rows = []
for row in pages.itertuples(index=False):
    text = " ".join(str(row.page_text).split())
    for match in far_pattern.finditer(text):
        context = text[max(0, match.start() - 180) : match.end() + 500]
        evidence_rows.append(
            {
                "matter_id": row.matter_id,
                "attachment_id": row.attachment_id,
                "page_number": row.page_number,
                "evidence_type": "far_label",
                "label": match.group("label"),
                "candidate_values": " | ".join(
                    dict.fromkeys(number_pattern.findall(context))
                ),
                "context": context,
            }
        )
    for evidence_type, pattern in [
        ("far_unchanged", unchanged_pattern),
        ("prior_approval", prior_pattern),
        ("change_context", change_pattern),
    ]:
        for match in pattern.finditer(text):
            evidence_rows.append(
                {
                    "matter_id": row.matter_id,
                    "attachment_id": row.attachment_id,
                    "page_number": row.page_number,
                    "evidence_type": evidence_type,
                    "label": "",
                    "candidate_values": "",
                    "context": text[max(0, match.start() - 180) : match.end() + 300],
                }
            )

evidence = pd.DataFrame(
    evidence_rows,
    columns=[
        "matter_id",
        "attachment_id",
        "page_number",
        "evidence_type",
        "label",
        "candidate_values",
        "context",
    ],
)
evidence.to_csv("../output/post_khan_pd_far_evidence_20160901_20201231.csv", index=False)

field_summary = fields.groupby("matter_id", as_index=False).agg(
    attachment_rows=("attachment_id", "size"),
    downloaded_attachments=(
        "download_status",
        lambda values: values.isin(["downloaded", "ok", "exists"]).sum(),
    ),
    parsed_attachments=("parse_status", lambda values: values.eq("parsed").sum()),
)
evidence_summary = evidence.pivot_table(
    index="matter_id",
    columns="evidence_type",
    values="context",
    aggfunc="count",
    fill_value=0,
).reset_index()
for column in ["far_label", "far_unchanged", "prior_approval", "change_context"]:
    if column not in evidence_summary:
        evidence_summary[column] = 0

review = queue.merge(field_summary, on="matter_id", how="left", validate="one_to_one")
review = review.merge(
    evidence_summary[
        ["matter_id", "far_label", "far_unchanged", "prior_approval", "change_context"]
    ],
    on="matter_id",
    how="left",
    validate="one_to_one",
)
for column in [
    "attachment_rows",
    "downloaded_attachments",
    "parsed_attachments",
    "far_label",
    "far_unchanged",
    "prior_approval",
    "change_context",
]:
    review[column] = pd.to_numeric(review[column], errors="coerce").fillna(0).astype(int)
review["has_far_evidence"] = review["far_label"].gt(0) | review["far_unchanged"].gt(0)
review.to_csv("../output/post_khan_pd_far_review_queue_20160901_20201231.csv", index=False)
