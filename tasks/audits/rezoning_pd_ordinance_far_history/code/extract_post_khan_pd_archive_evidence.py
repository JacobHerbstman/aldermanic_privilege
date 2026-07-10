#!/usr/bin/env python3
import re

import pandas as pd


# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_pd_ordinance_far_history/code")

queue = pd.read_csv(
    "../output/post_khan_pd_to_pd_queue_20160901_20201231.csv", dtype=str
)
pages = pd.read_csv(
    "../output/post_khan_pd_archive_text_20160901_20201231.csv",
    dtype=str,
    low_memory=False,
)
fields = pd.read_csv(
    "../output/post_khan_pd_archive_fields_20160901_20201231.csv", dtype=str
)

if fields["pd_number"].duplicated().any():
    raise ValueError("Post-Khan DPD archive fields contain duplicate PD numbers")
if not fields["download_status"].eq("downloaded").all():
    raise ValueError("At least one requested post-Khan DPD archive failed to download")
if pages.duplicated(["pd_number", "page_number"]).any():
    raise ValueError("Post-Khan DPD archive text contains duplicate PD/page rows")
parsed_page_counts = pages.groupby("pd_number").size().astype(int)
expected_page_counts = pd.to_numeric(
    fields.set_index("pd_number")["page_count"], errors="raise"
).astype(int)
if not parsed_page_counts.equals(expected_page_counts.sort_index()):
    raise ValueError("Post-Khan DPD archive text does not match reported page counts")

far_pattern = re.compile(
    r"\b(?P<label>(?:maximum(?:\s+permitted)?|overall|total|base)\s+"
    r"(?:floor\s+area\s+ratio|f\.?\s*a\.?\s*r\.?)|"
    r"floor\s+area\s+ratio\s*\(maximum\s+permitted\))(?=\s|:|$)",
    re.IGNORECASE,
)
date_pattern = re.compile(
    r"\b(?:0?[1-9]|1[0-2])/(?:0?[1-9]|[12]\d|3[01])/(?:(?:19|20)\d{2}|\d{2})\b"
)
number_pattern = re.compile(r"(?<![\d,])(?:\d{1,2}(?:\.\d{1,3})?|\.\d{1,3})(?![\d,])")

evidence_rows = []
for row in pages.itertuples(index=False):
    text = " ".join(str(row.page_text).split())
    page_dates = " | ".join(dict.fromkeys(date_pattern.findall(text)))
    for match in far_pattern.finditer(text):
        context = text[max(0, match.start() - 220) : match.end() + 650]
        evidence_rows.append(
            {
                "pd_number": row.pd_number,
                "page_number": row.page_number,
                "parse_method": row.parse_method,
                "page_dates": page_dates,
                "label": match.group("label"),
                "candidate_values": " | ".join(
                    dict.fromkeys(number_pattern.findall(context))
                ),
                "context": context,
            }
        )

evidence = pd.DataFrame(
    evidence_rows,
    columns=[
        "pd_number",
        "page_number",
        "parse_method",
        "page_dates",
        "label",
        "candidate_values",
        "context",
    ],
)
evidence.to_csv(
    "../output/post_khan_pd_archive_far_evidence_20160901_20201231.csv", index=False
)

archive_summary = fields[["pd_number", "download_status", "page_count", "ocr_pages"]].copy()
evidence_summary = evidence.groupby("pd_number", as_index=False).agg(
    archive_far_labels=("context", "size"),
    dated_far_labels=("page_dates", lambda values: values.fillna("").ne("").sum()),
)
review = queue.merge(archive_summary, on="pd_number", how="left", validate="many_to_one")
review = review.merge(evidence_summary, on="pd_number", how="left", validate="many_to_one")
for column in ["page_count", "ocr_pages", "archive_far_labels", "dated_far_labels"]:
    review[column] = pd.to_numeric(review[column], errors="coerce").fillna(0).astype(int)
review["download_status"] = review["download_status"].fillna("no_pd_number")
review.to_csv(
    "../output/post_khan_pd_archive_review_queue_20160901_20201231.csv", index=False
)
