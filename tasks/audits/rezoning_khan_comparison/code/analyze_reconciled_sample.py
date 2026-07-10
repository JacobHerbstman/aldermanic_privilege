#!/usr/bin/env python3
import pandas as pd


KHAN_N = 2096
KHAN_FROM_MEAN = 2.344
KHAN_TO_MEAN = 2.393


def family(code) -> str:
    if pd.isna(code) or not str(code).strip():
        return "missing"
    value = str(code).strip().upper()
    if value == "PD":
        return "planned_development"
    if value.startswith(("RS", "RT", "RM")) or (value.startswith("R") and value[1:].isdigit()):
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


def numeric_summary(name: str, df: pd.DataFrame) -> dict:
    out = {"variant": name, "rows": len(df), "row_gap_vs_khan": len(df) - KHAN_N}
    for side, benchmark in [("from_far", KHAN_FROM_MEAN), ("to_far", KHAN_TO_MEAN)]:
        values = pd.to_numeric(df[side], errors="coerce")
        observed = values.dropna()
        missing = int(values.isna().sum())
        out[f"{side}_n"] = len(observed)
        out[f"{side}_mean"] = observed.mean()
        out[f"{side}_sd"] = observed.std()
        out[f"{side}_min"] = observed.min()
        out[f"{side}_max"] = observed.max()
        out[f"{side}_missing"] = missing
        out[f"{side}_mean_gap_vs_khan"] = observed.mean() - benchmark
        out[f"{side}_required_missing_mean_to_match_khan"] = (
            (benchmark * len(df) - observed.sum()) / missing if missing else None
        )
    return out


base = pd.read_csv("../output/khan_zoning_matters_far_20101101_20160831.csv", low_memory=False)
corrections = pd.read_csv("../input/khan_sample_corrections.csv", dtype=str)
fetched = pd.read_csv("../output/councilmatic_missing_ordinance_text.csv", low_memory=False)
pd_evidence = pd.read_csv("../input/khan_pd_underlying_evidence.csv", dtype=str)
pd_transition_corrections = pd.read_csv("../input/khan_pd_transition_corrections.csv", dtype=str)
destination_corrections = pd.read_csv("../input/khan_destination_code_corrections.csv", dtype=str)

if corrections["matter_id"].duplicated().any():
    raise ValueError("Duplicate matter_id in khan_sample_corrections.csv")
if pd_evidence["matter_id"].duplicated().any():
    raise ValueError("Duplicate matter_id in khan_pd_underlying_evidence.csv")
if pd_transition_corrections["matter_id"].duplicated().any():
    raise ValueError("Duplicate matter_id in khan_pd_transition_corrections.csv")
if destination_corrections["matter_id"].duplicated().any():
    raise ValueError("Duplicate matter_id in khan_destination_code_corrections.csv")

base["matter_status_name"] = base["matter_status_name"].astype("string").str.upper()
sample_mask = base["matter_status_name"].eq("90-FINAL")

include_ids = set(corrections.loc[corrections["action"].eq("include"), "matter_id"])
exclude_ids = set(corrections.loc[corrections["action"].eq("exclude"), "matter_id"])
existing_include_ids = include_ids.intersection(set(base["matter_id"]))
missing_include_ids = include_ids.difference(set(base["matter_id"]))

sample_mask = (sample_mask | base["matter_id"].isin(existing_include_ids)) & ~base["matter_id"].isin(exclude_ids)
sample = base.loc[sample_mask].copy()

if missing_include_ids != {"SO2012-643"}:
    raise ValueError(f"Unexpected correction rows absent from the FAR file: {sorted(missing_include_ids)}")

pd_542 = fetched.loc[fetched["matter_id"].eq("SO2012-643")]
if len(pd_542) != 1 or pd_542.iloc[0]["retrieval_status"] != "ok":
    raise ValueError("SO2012-643 ordinance text was not retrieved successfully")

sample = pd.concat(
    [
        sample,
        pd.DataFrame(
            [
                {
                    "matter_id": "SO2012-643",
                    "matter_title": pd_542.iloc[0]["matter_title"],
                    "matter_intro_date": str(pd_542.iloc[0]["matter_intro_date"])[:10],
                    "matter_passed_date": str(pd_542.iloc[0]["matter_passed_date"])[:10],
                    "matter_status_name": "90-FINAL",
                    "from_zoning": "Institutional Planned Development No. 542",
                    "to_zoning": "Institutional Planned Development No. 542",
                    "from_code": "PD",
                    "to_code": "PD",
                    "from_far": None,
                    "to_far": None,
                }
            ]
        ),
    ],
    ignore_index=True,
)

if sample["matter_id"].duplicated().any():
    raise ValueError("Reconciled Khan sample has duplicate matter_id rows")
for _, row in pd_transition_corrections.iterrows():
    mask = sample["matter_id"].eq(row["matter_id"])
    if mask.sum() != 1 or sample.loc[mask, "to_code"].notna().any():
        raise ValueError(f"PD transition correction is not a unique missing to-code: {row['matter_id']}")
    sample.loc[mask, "to_code"] = row["corrected_to_code"]

for col in ["from_far", "to_far"]:
    sample[col] = pd.to_numeric(sample[col], errors="coerce")

destination_corrections["corrected_to_far"] = pd.to_numeric(
    destination_corrections["corrected_to_far"], errors="coerce"
)
for _, row in destination_corrections.iterrows():
    mask = sample["matter_id"].eq(row["matter_id"])
    if mask.sum() != 1:
        raise ValueError(f"Destination correction is not unique in sample: {row['matter_id']}")
    current = sample.loc[mask, "to_code"].iloc[0]
    expected = row["expected_to_code"]
    if not ((pd.isna(current) and pd.isna(expected)) or str(current) == str(expected)):
        raise ValueError(
            f"Unexpected destination code for {row['matter_id']}: {current}, expected {expected}"
        )
    sample.loc[mask, "to_code"] = row["corrected_to_code"]
    sample.loc[mask, "to_far"] = row["corrected_to_far"]

documented = sample.copy()
documented.loc[documented["from_code"].eq("T") & documented["from_far"].isna(), "from_far"] = 1.5
documented.loc[documented["to_code"].eq("T") & documented["to_far"].isna(), "to_far"] = 1.5

from_pd = documented["from_code"].eq("PD")
to_pd = documented["to_code"].eq("PD")
fill_to = ~from_pd & to_pd & documented["to_far"].isna() & documented["from_far"].notna()
fill_from = from_pd & ~to_pd & documented["from_far"].isna() & documented["to_far"].notna()
documented.loc[fill_to, "to_far"] = documented.loc[fill_to, "from_far"]
documented.loc[fill_from, "from_far"] = documented.loc[fill_from, "to_far"]

pd_evidence["underlying_far"] = pd.to_numeric(pd_evidence["underlying_far"], errors="coerce")
for _, row in pd_evidence.loc[pd_evidence["underlying_far"].notna()].iterrows():
    mask = documented["matter_id"].eq(row["matter_id"])
    if mask.sum() != 1:
        raise ValueError(f"PD evidence matter_id is not unique in sample: {row['matter_id']}")
    documented.loc[mask, ["from_far", "to_far"]] = row["underlying_far"]

pd.DataFrame(
    [
        numeric_summary("base_lookup_corrected_sample", sample),
        numeric_summary("khan_documented_structural_rules", documented),
    ]
).to_csv("../output/khan_reconciled_far_summary.csv", index=False)

correction_audit = corrections.copy()
correction_audit["present_in_base_far_file"] = correction_audit["matter_id"].isin(set(base["matter_id"]))
correction_audit["base_status"] = correction_audit["matter_id"].map(base.set_index("matter_id")["matter_status_name"])
correction_audit["included_in_reconciled_sample"] = correction_audit["matter_id"].isin(set(sample["matter_id"]))
correction_audit.to_csv("../output/khan_reconciled_sample_corrections.csv", index=False)

origin_family = sample["from_code"].map(family)
origin_family = origin_family.where(
    ~(sample["from_code"].eq("PD") & ~sample["to_code"].eq("PD")),
    sample["to_code"].map(family),
)
origin_family = origin_family.where(
    ~(~sample["from_code"].eq("PD") & sample["to_code"].eq("PD")),
    sample["from_code"].map(family),
)
origin_family.loc[sample["matter_id"].isin(set(pd_evidence["matter_id"]))] = sample.loc[
    sample["matter_id"].isin(set(pd_evidence["matter_id"])), "matter_id"
].map(pd_evidence.set_index("matter_id")["underlying_code"].map(family))

family_rows = []
for variant, values in [
    ("destination_codes_after_source_corrections", sample["to_code"].map(family)),
    ("original_side_hybrid_preliminary", origin_family),
]:
    family_row = {"variant": variant, "rows": len(sample), "row_gap_vs_khan": len(sample) - KHAN_N}
    for value in [
        "residential",
        "business",
        "commercial",
        "downtown",
        "planned_development",
        "manufacturing",
        "parks_open_space",
        "transportation",
        "missing",
        "other",
    ]:
        family_row[f"{value}_count"] = int(values.eq(value).sum())
        family_row[f"{value}_share"] = float(values.eq(value).mean())
    family_rows.append(family_row)
pd.DataFrame(family_rows).to_csv("../output/khan_reconciled_family_summary.csv", index=False)

pd_rows = sample.loc[sample["from_code"].eq("PD") & sample["to_code"].eq("PD")].copy()
pd_rows = pd_rows.merge(pd_evidence, on="matter_id", how="left", validate="one_to_one")
pd_rows["pure_pd_after_evidence"] = pd_rows["underlying_code"].isna()
pd_rows[
    [
        "matter_id",
        "matter_title",
        "from_zoning",
        "to_zoning",
        "underlying_code",
        "underlying_far",
        "evidence_source",
        "evidence",
        "pure_pd_after_evidence",
    ]
].to_csv("../output/khan_pd_preexisting_far_audit.csv", index=False)

pd_transition_corrections.to_csv("../output/khan_pd_transition_corrections_audit.csv", index=False)
destination_corrections.to_csv("../output/khan_destination_code_corrections_audit.csv", index=False)

sample.to_csv("../output/khan_reconciled_sample.csv", index=False)
