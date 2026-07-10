#!/usr/bin/env python3
import re

import pandas as pd


KHAN_REFERENCE_COUNTS = {
    "residential": 784,
    "business": 803,
    "commercial": 356,
    "downtown": 86,
    "planned_development": 67,
}


def district_family(code) -> str:
    if pd.isna(code) or not str(code).strip():
        return "missing"
    value = str(code).strip().upper()
    if value == "PD":
        return "planned_development"
    if value.startswith(("RS", "RT", "RM")):
        return "residential"
    if value.startswith(("B", "BL")):
        return "business"
    if value.startswith(("C", "CL")):
        return "commercial"
    if value.startswith(("D", "DX", "DC", "DR", "DS")):
        return "downtown"
    if value.startswith(("M", "ML", "PMD", "PMO")):
        return "manufacturing"
    if value.startswith("POS"):
        return "parks_open_space"
    if value == "T":
        return "transportation"
    return "other"


def classify_pd_subtype(row, subtype_overrides) -> str:
    if row.matter_id in subtype_overrides:
        return subtype_overrides[row.matter_id]
    text = " | ".join(
        str(value) for value in [row.to_zoning, row.to_zoning_raw] if pd.notna(value)
    ).upper()
    if re.search(r"RESIDENTIAL\s*[- ]?\s*BUSINESS|BUSINESS\s*[- ]?\s*RESIDENTIAL", text):
        return "residential_business"
    if "INSTITUTIONAL-RESIDENTIAL" in text:
        return "institutional_residential"
    if "RESIDENTIAL" in text:
        return "residential"
    if "BUSINESS" in text:
        return "business"
    if "COMMERCIAL" in text:
        return "commercial"
    if re.search(r"MANUFACTUR|INDUSTRIAL|WATERWAY", text):
        return "industrial"
    if "INSTITUTIONAL" in text:
        return "institutional"
    if re.search(r"ENTERTAINMENT|SPECTATOR SPORTS", text):
        return "entertainment"
    return "generic"


def allowed_use_family(row, named_pd, mixed_by_origin, institution_by_origin, custom_by_origin, generic_by_origin):
    destination = row.destination_family
    origin = row.origin_family

    if destination == "manufacturing":
        return "commercial"
    if destination in {"parks_open_space", "transportation"}:
        if custom_by_origin and origin in KHAN_REFERENCE_COUNTS:
            return origin
        if custom_by_origin and origin == "manufacturing":
            return "commercial"
        return "planned_development"
    if destination != "planned_development" or not named_pd:
        return destination

    if row.pd_subtype in {"residential", "institutional_residential"}:
        result = "residential"
    elif row.pd_subtype in {"business", "residential_business"}:
        result = "business"
    elif row.pd_subtype in {"commercial", "industrial", "institutional", "entertainment"}:
        result = "commercial"
    else:
        result = "planned_development"

    if mixed_by_origin and row.pd_subtype == "residential_business" and origin in {"residential", "downtown"}:
        result = origin
    elif institution_by_origin and row.pd_subtype == "institutional":
        if origin in KHAN_REFERENCE_COUNTS:
            result = origin
        elif origin == "manufacturing":
            result = "commercial"
        else:
            result = "planned_development"
    elif generic_by_origin and row.pd_subtype == "generic":
        if origin in KHAN_REFERENCE_COUNTS:
            result = origin
        elif origin == "manufacturing":
            result = "commercial"
    return result


sample = pd.read_csv("../output/khan_reconciled_sample.csv", low_memory=False)
overrides = pd.read_csv("../input/khan_pd_subtype_overrides.csv", dtype=str)
decisions = pd.read_csv("../input/khan_mixed_pd_allowed_use_decisions.csv", dtype=str)

if overrides["matter_id"].duplicated().any():
    raise ValueError("Duplicate matter_id in khan_pd_subtype_overrides.csv")
if not set(overrides["matter_id"]).issubset(set(sample["matter_id"])):
    raise ValueError("PD subtype override is absent from the reconciled sample")
if decisions["matter_id"].duplicated().any():
    raise ValueError("Duplicate matter_id in khan_mixed_pd_allowed_use_decisions.csv")
if not set(decisions["matter_id"]).issubset(set(sample["matter_id"])):
    raise ValueError("Mixed-PD allowed-use decision is absent from the reconciled sample")
if not set(decisions["allowed_family"]).issubset(KHAN_REFERENCE_COUNTS):
    raise ValueError("Mixed-PD decision has an unsupported allowed_family")
if not set(decisions["confidence"]).issubset({"high", "medium"}):
    raise ValueError("Mixed-PD decision confidence must be high or medium")

subtype_overrides = overrides.set_index("matter_id")["pd_subtype"].to_dict()
sample["origin_family"] = sample["from_code"].map(district_family)
sample["destination_family"] = sample["to_code"].map(district_family)
sample["pd_subtype"] = [
    classify_pd_subtype(row, subtype_overrides) if row.to_code == "PD" else ""
    for row in sample.itertuples(index=False)
]
mixed_pd_ids = set(
    sample.loc[
        sample["to_code"].eq("PD") & sample["pd_subtype"].eq("residential_business"),
        "matter_id",
    ]
)
if not set(decisions["matter_id"]).issubset(mixed_pd_ids):
    raise ValueError("Allowed-use decision is not a Residential-Business PD row")

variants = [
    ("destination_codes", False, False, False, False, False),
    ("collapse_manufacturing_and_custom", False, False, False, False, False),
    ("named_pd_allowed_type", True, False, False, False, False),
    ("mixed_and_institution_by_origin", True, True, True, False, False),
    ("plus_pos_t_by_origin", True, True, True, True, False),
    ("plus_generic_by_origin", True, True, True, False, True),
]

variant_classifications = {}
for name, named_pd, mixed_by_origin, institution_by_origin, custom_by_origin, generic_by_origin in variants:
    if name == "destination_codes":
        classifications = sample["destination_family"].tolist()
    else:
        classifications = [
            allowed_use_family(
                row,
                named_pd,
                mixed_by_origin,
                institution_by_origin,
                custom_by_origin,
                generic_by_origin,
            )
            for row in sample.itertuples(index=False)
        ]
    variant_classifications[name] = classifications

high_confidence_decisions = decisions.loc[
    decisions["confidence"].eq("high")
].set_index("matter_id")["allowed_family"]
all_decisions = decisions.set_index("matter_id")["allowed_family"]
decision_confidence = decisions.set_index("matter_id")["confidence"]
decision_evidence = decisions.set_index("matter_id")["evidence"]
for name, decision_map in [
    ("source_backed_high_confidence", high_confidence_decisions),
    ("source_backed_all_documented", all_decisions),
]:
    classifications = pd.Series(
        variant_classifications["mixed_and_institution_by_origin"], index=sample.index
    )
    decided = sample["matter_id"].isin(decision_map.index)
    classifications.loc[decided] = sample.loc[decided, "matter_id"].map(decision_map)
    variant_classifications[name] = classifications.tolist()

summary_rows = []
for name, classifications in variant_classifications.items():
    values = pd.Series(classifications)
    summary = {"variant": name, "rows": len(sample)}
    for family_name, benchmark in KHAN_REFERENCE_COUNTS.items():
        count = int(values.eq(family_name).sum())
        summary[f"{family_name}_count"] = count
        summary[f"{family_name}_share"] = count / len(sample)
        summary[f"{family_name}_gap_vs_khan_reference"] = count - benchmark
    summary_rows.append(summary)

pd.DataFrame(summary_rows).to_csv("../output/khan_allowed_use_family_variants.csv", index=False)

sample["review_family"] = variant_classifications["mixed_and_institution_by_origin"]
review = sample.loc[
    sample["to_code"].eq("PD") & sample["pd_subtype"].eq("residential_business")
].copy()
review_ids = set(review["matter_id"])
pages = {matter_id: [] for matter_id in review_ids}
for chunk in pd.read_csv("../input/pdf_text_20101101_20160831.csv", chunksize=20000):
    for row in chunk.loc[chunk["matter_id"].isin(review_ids), ["matter_id", "page_text"]].itertuples(index=False):
        pages[row.matter_id].append(str(row.page_text))

review_rows = []
for row in review.itertuples(index=False):
    text = " ".join(" ".join(pages[row.matter_id]).split())
    dwelling = [match.group(0) for match in re.finditer(
        r"(?:maximum|total)[^.]{0,100}(?:dwelling|residential) units?[^.]{0,140}", text, re.IGNORECASE
    )]
    commercial = [match.group(0) for match in re.finditer(
        r"(?:maximum|total)[^.]{0,120}(?:commercial|retail|office|business)[^.]{0,160}", text, re.IGNORECASE
    )]
    permitted = [match.group(0) for match in re.finditer(
        r"(?:uses shall be permitted|permitted uses)[^.]{0,450}", text, re.IGNORECASE
    )]
    review_rows.append(
        {
            "matter_id": row.matter_id,
            "matter_title": row.matter_title,
            "origin_family": row.origin_family,
            "review_family": row.review_family,
            "decision_family": all_decisions.get(row.matter_id, ""),
            "decision_confidence": decision_confidence.get(row.matter_id, ""),
            "decision_evidence": decision_evidence.get(row.matter_id, ""),
            "text_chars": len(text),
            "dwelling_evidence": " | ".join(dict.fromkeys(dwelling))[:1800],
            "commercial_evidence": " | ".join(dict.fromkeys(commercial))[:1800],
            "permitted_use_evidence": " | ".join(dict.fromkeys(permitted))[:1800],
        }
    )

pd.DataFrame(review_rows).to_csv("../output/khan_mixed_pd_allowed_use_review.csv", index=False)
