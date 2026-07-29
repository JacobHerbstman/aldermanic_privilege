#!/usr/bin/env python3
import argparse
from pathlib import Path

import pandas as pd


KHAN_TABLE12 = {
    "period_note": "Khan Table 12: November 2010 to August 2016 ordinance-coded sample",
    "n": 2096,
    "from_far_mean": 2.344,
    "from_far_sd": 2.684,
    "from_far_min": 0.500,
    "from_far_max": 19.000,
    "to_far_mean": 2.393,
    "to_far_sd": 1.923,
    "to_far_min": 0.500,
    "to_far_max": 16.000,
    "residential_share": 0.374,
    "business_share": 0.383,
    "commercial_share": 0.170,
    "downtown_share": 0.041,
    "planned_development_share": 0.032,
}

KHAN_FAMILY_COUNTS = {
    "residential": 784,
    "business": 803,
    "commercial": 356,
    "downtown": 86,
    "planned_development": 67,
}

PD_FAR_REVIEW_REQUIRED_COLUMNS = [
    "matter_id",
    "pd_far_update_source",
    "pd_transition_type",
    "from_far_update",
    "to_far_update",
    "far_change_update",
    "far_direction_update",
    "pd_far_value_update",
    "pd_far_role_update",
    "confidence",
    "evidence",
    "notes",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-rezoning-csv", required=True)
    parser.add_argument("--in-pd-far-updates-csv")
    parser.add_argument("--in-pd-to-pd-manual-csv")
    parser.add_argument("--in-review-queue-csv")
    parser.add_argument("--start-date", default="2010-11-01")
    parser.add_argument("--end-date", default="2016-08-31")
    parser.add_argument("--out-summary-csv", required=True)
    parser.add_argument("--out-year-counts-csv", required=True)
    parser.add_argument("--out-pd-transitions-csv", required=True)
    parser.add_argument("--out-sample-csv", required=True)
    parser.add_argument("--out-family-counts-csv")
    parser.add_argument("--out-far-rule-variants-csv")
    parser.add_argument("--out-pd-direction-csv")
    return parser.parse_args()


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def as_bool(series: pd.Series) -> pd.Series:
    return series.astype(str).str.strip().str.lower().isin(["true", "1", "t", "yes", "y"])


def code_family(code) -> str | None:
    if pd.isna(code):
        return None
    text = str(code).strip().upper()
    if not text:
        return None
    if text == "PD" or "PLANNED DEVELOPMENT" in text:
        return "planned_development"
    if text in {"RS", "RT", "RM"} or text.startswith(("R", "RS-", "RT-", "RM-")):
        return "residential"
    if text.startswith(("B", "BL")):
        return "business"
    if text.startswith(("C", "CL")):
        return "commercial"
    if text.startswith(("D", "DX", "DC", "DR", "DS")):
        return "downtown"
    return "other"


def is_pd(series: pd.Series) -> pd.Series:
    return series.fillna("").astype(str).str.strip().str.upper().eq("PD")


def summarize_numeric(series: pd.Series) -> dict:
    x = pd.to_numeric(series, errors="coerce").dropna()
    return {
        "n": int(len(x)),
        "mean": float(x.mean()) if len(x) else None,
        "sd": float(x.std(ddof=1)) if len(x) > 1 else None,
        "min": float(x.min()) if len(x) else None,
        "max": float(x.max()) if len(x) else None,
    }


def far_direction_from_change(value) -> str:
    change = pd.to_numeric(pd.Series([value]), errors="coerce").iloc[0]
    if pd.isna(change):
        return "unknown"
    if change > 0:
        return "upzone"
    if change < 0:
        return "downzone"
    return "same"


def apply_pd_far_updates(df: pd.DataFrame, path: str | None) -> tuple[pd.DataFrame, int, int]:
    if not path or not Path(path).exists():
        return df, 0, 0

    out = df.copy()
    updates = pd.read_csv(path, dtype=str, low_memory=False)
    missing_cols = [col for col in PD_FAR_REVIEW_REQUIRED_COLUMNS if col not in updates.columns]
    if missing_cols:
        raise ValueError(f"PD FAR review update file is missing columns: {', '.join(missing_cols)}")
    if "matter_id" not in out.columns:
        raise ValueError("Rezoning comparison file is missing matter_id")
    if out["matter_id"].duplicated().any():
        duplicated = sorted(out.loc[out["matter_id"].duplicated(), "matter_id"].astype(str).unique())
        raise ValueError(f"Duplicate matter_id rows block PD FAR review updates: {duplicated}")
    if updates["matter_id"].duplicated().any():
        duplicated = sorted(updates.loc[updates["matter_id"].duplicated(), "matter_id"].astype(str).unique())
        raise ValueError(f"Duplicate PD FAR review update matter_id rows: {duplicated}")

    update_ids_in_data = set(out["matter_id"].astype(str))
    updates = updates.loc[updates["matter_id"].astype(str).isin(update_ids_in_data)].copy()
    if updates.empty:
        out["pd_far_review_applied"] = False
        return out, 0, 0

    updates = updates.set_index(updates["matter_id"].astype(str), drop=False)
    from_update = pd.to_numeric(updates["from_far_update"], errors="coerce")
    to_update = pd.to_numeric(updates["to_far_update"], errors="coerce")
    change_update = pd.to_numeric(updates["far_change_update"], errors="coerce")
    bad_numeric = from_update.isna() | to_update.isna() | change_update.isna()
    if bad_numeric.any():
        bad = sorted(updates.loc[bad_numeric, "matter_id"].astype(str).tolist())
        raise ValueError(f"PD FAR review updates have missing numeric FAR fields: {bad}")

    expected_change = to_update - from_update
    bad_change = (expected_change - change_update).abs() > 0.01
    if bad_change.any():
        bad = sorted(updates.loc[bad_change, "matter_id"].astype(str).tolist())
        raise ValueError(f"PD FAR review updates have inconsistent FAR changes: {bad}")

    expected_direction = change_update.map(far_direction_from_change)
    bad_direction = updates["far_direction_update"].astype(str).str.strip().ne(expected_direction)
    if bad_direction.any():
        bad = sorted(updates.loc[bad_direction, "matter_id"].astype(str).tolist())
        raise ValueError(f"PD FAR review updates have inconsistent FAR directions: {bad}")

    out["from_far_pre_pd_review"] = pd.to_numeric(out["from_far"], errors="coerce")
    out["to_far_pre_pd_review"] = pd.to_numeric(out["to_far"], errors="coerce")
    out["far_change_pre_pd_review"] = pd.to_numeric(out["far_change"], errors="coerce")
    out["far_pair_status_pre_pd_review"] = out["far_pair_status"]
    out["pd_far_review_applied"] = False
    out["pd_far_update_source"] = ""
    out["pd_far_update_confidence"] = ""
    out["pd_far_transition_type"] = ""
    out["pd_far_value_update"] = pd.NA
    out["pd_far_role_update"] = ""
    out["pd_far_review_evidence"] = ""
    out["pd_far_review_notes"] = ""

    matter_id = out["matter_id"].astype(str)
    update_mask = matter_id.isin(updates.index)
    update_ids = matter_id.loc[update_mask]
    out.loc[update_mask, "pd_far_review_applied"] = True
    out.loc[update_mask, "pd_far_update_source"] = update_ids.map(updates["pd_far_update_source"])
    out.loc[update_mask, "pd_far_update_confidence"] = update_ids.map(updates["confidence"])
    out.loc[update_mask, "pd_far_transition_type"] = update_ids.map(updates["pd_transition_type"])
    out.loc[update_mask, "pd_far_value_update"] = update_ids.map(pd.to_numeric(updates["pd_far_value_update"], errors="coerce"))
    out.loc[update_mask, "pd_far_role_update"] = update_ids.map(updates["pd_far_role_update"])
    out.loc[update_mask, "pd_far_review_evidence"] = update_ids.map(updates["evidence"])
    out.loc[update_mask, "pd_far_review_notes"] = update_ids.map(updates["notes"])
    out.loc[update_mask, "from_far"] = update_ids.map(from_update)
    out.loc[update_mask, "to_far"] = update_ids.map(to_update)
    out.loc[update_mask, "far_change"] = update_ids.map(change_update)
    out.loc[update_mask, "far_pair_status"] = "RESOLVED_BOTH"
    out["is_upzone"] = [
        None if pd.isna(value) else bool(value > 0)
        for value in pd.to_numeric(out["far_change"], errors="coerce")
    ]
    return out, int(update_mask.sum()), int(len(updates))


def apply_one_sided_pd_nochange(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    from_pd = is_pd(out["from_code"])
    to_pd = is_pd(out["to_code"])
    fill_to = (~from_pd) & to_pd & out["to_far"].isna() & out["from_far"].notna()
    fill_from = from_pd & (~to_pd) & out["from_far"].isna() & out["to_far"].notna()
    out.loc[fill_to, "to_far"] = out.loc[fill_to, "from_far"]
    out.loc[fill_from, "from_far"] = out.loc[fill_from, "to_far"]
    out["far_change"] = out["to_far"] - out["from_far"]
    return out


def apply_pd_to_pd_manual(df: pd.DataFrame, path: str | None) -> pd.DataFrame:
    if not path or not Path(path).exists():
        return df
    manual = pd.read_csv(path, low_memory=False)
    required = ["matter_id", "proposed_from_far", "proposed_to_far"]
    missing = [col for col in required if col not in manual.columns]
    if missing:
        raise ValueError(f"PD-to-PD manual file is missing columns: {', '.join(missing)}")
    if manual["matter_id"].duplicated().any():
        duplicated = sorted(manual.loc[manual["matter_id"].duplicated(), "matter_id"].astype(str).unique())
        raise ValueError(f"Duplicate PD-to-PD manual matter_id rows: {duplicated}")

    out = df.copy()
    manual = manual.set_index(manual["matter_id"].astype(str), drop=False)
    matter_id = out["matter_id"].astype(str)
    update = is_pd(out["from_code"]) & is_pd(out["to_code"]) & matter_id.isin(manual.index)
    out.loc[update, "from_far"] = matter_id.loc[update].map(
        pd.to_numeric(manual["proposed_from_far"], errors="coerce")
    )
    out.loc[update, "to_far"] = matter_id.loc[update].map(
        pd.to_numeric(manual["proposed_to_far"], errors="coerce")
    )
    out["far_change"] = out["to_far"] - out["from_far"]
    return out


def apply_review_queue_from_text(df: pd.DataFrame, path: str | None) -> pd.DataFrame:
    if not path or not Path(path).exists():
        return df
    queue = pd.read_csv(path, low_memory=False)
    if "matter_id" not in queue.columns or "from_far_with_pd_text" not in queue.columns:
        return df
    if queue["matter_id"].duplicated().any():
        queue = queue.drop_duplicates("matter_id", keep="first").copy()

    out = df.copy()
    queue = queue.set_index(queue["matter_id"].astype(str), drop=False)
    matter_id = out["matter_id"].astype(str)
    queue_from = matter_id.map(pd.to_numeric(queue["from_far_with_pd_text"], errors="coerce"))
    update = out["from_far"].isna() & queue_from.notna()
    out.loc[update, "from_far"] = queue_from.loc[update]
    out["far_change"] = out["to_far"] - out["from_far"]
    return out


def variant_summary(name: str, df: pd.DataFrame, mask: pd.Series) -> dict:
    x = df.loc[mask].copy()
    from_stats = summarize_numeric(x["from_far"])
    to_stats = summarize_numeric(x["to_far"])
    change_stats = summarize_numeric(x["far_change"])
    from_family = x["from_code"].map(code_family)
    to_family = x["to_code"].map(code_family)
    pd_transition = (x["from_code"].eq("PD") | x["to_code"].eq("PD")).fillna(False)
    out = {
        "variant": name,
        "rows": int(len(x)),
        "from_far_n": from_stats["n"],
        "from_far_mean": from_stats["mean"],
        "from_far_sd": from_stats["sd"],
        "from_far_min": from_stats["min"],
        "from_far_max": from_stats["max"],
        "to_far_n": to_stats["n"],
        "to_far_mean": to_stats["mean"],
        "to_far_sd": to_stats["sd"],
        "to_far_min": to_stats["min"],
        "to_far_max": to_stats["max"],
        "both_far_n": int((x["from_far"].notna() & x["to_far"].notna()).sum()),
        "far_change_mean": change_stats["mean"],
        "pd_transition_rows": int(pd_transition.sum()),
        "pd_review_applied_rows": int(as_bool(x.get("pd_far_review_applied", pd.Series(False, index=x.index))).sum()),
        "pd_missing_one_side_rows": int(
            (pd_transition & x["far_pair_status"].eq("MISSING_ONE_SIDE")).sum()
        ),
        "pd_missing_both_rows": int(
            (pd_transition & x["far_pair_status"].eq("MISSING_BOTH")).sum()
        ),
        "khan_n": KHAN_TABLE12["n"],
        "row_gap_vs_khan_table12": int(len(x) - KHAN_TABLE12["n"]),
        "from_far_mean_gap_vs_khan": None,
        "to_far_mean_gap_vs_khan": None,
    }
    if out["from_far_mean"] is not None:
        out["from_far_mean_gap_vs_khan"] = out["from_far_mean"] - KHAN_TABLE12["from_far_mean"]
    if out["to_far_mean"] is not None:
        out["to_far_mean_gap_vs_khan"] = out["to_far_mean"] - KHAN_TABLE12["to_far_mean"]
    for family in ["residential", "business", "commercial", "downtown", "planned_development", "other"]:
        out[f"from_{family}_share"] = float((from_family == family).mean()) if len(x) else None
        out[f"to_{family}_share"] = float((to_family == family).mean()) if len(x) else None
    return out


def far_rule_summary(name: str, df: pd.DataFrame, mask: pd.Series) -> dict:
    out = variant_summary(name, df, mask)
    x = df.loc[mask].copy()
    out["from_far_max_gap_vs_khan"] = None
    out["to_far_max_gap_vs_khan"] = None
    if out["from_far_max"] is not None:
        out["from_far_max_gap_vs_khan"] = out["from_far_max"] - KHAN_TABLE12["from_far_max"]
    if out["to_far_max"] is not None:
        out["to_far_max_gap_vs_khan"] = out["to_far_max"] - KHAN_TABLE12["to_far_max"]
    out["missing_from_far_rows"] = int(x["from_far"].isna().sum())
    out["missing_to_far_rows"] = int(x["to_far"].isna().sum())
    return out


def family_definition(series_from: pd.Series, series_to: pd.Series, definition: str) -> pd.Series:
    from_family = series_from.map(code_family)
    to_family = series_to.map(code_family)
    from_pd = is_pd(series_from)
    to_pd = is_pd(series_to)

    if definition == "raw_from":
        out = from_family
    elif definition == "raw_to":
        out = to_family
    elif definition == "resulting_underlying":
        out = to_family.copy()
        out = out.where(~(to_pd & ~from_pd), from_family)
        out = out.where(~(from_pd & to_pd), "planned_development")
    elif definition == "origin_underlying":
        out = from_family.copy()
        out = out.where(~(from_pd & ~to_pd), to_family)
        out = out.where(~(from_pd & to_pd), "planned_development")
    elif definition == "nonpd_side":
        out = to_family.copy()
        out = out.where(~(to_pd & ~from_pd), from_family)
        out = out.where(~(from_pd & ~to_pd), to_family)
        out = out.where(~(from_pd & to_pd), "planned_development")
    else:
        raise ValueError(f"Unknown family definition: {definition}")
    return out.fillna("missing")


def build_family_counts(df: pd.DataFrame, mask: pd.Series) -> pd.DataFrame:
    rows = []
    x = df.loc[mask].copy()
    for definition in ["raw_to", "raw_from", "resulting_underlying", "origin_underlying", "nonpd_side"]:
        family = family_definition(x["from_code"], x["to_code"], definition)
        row = {"definition": definition, "rows": int(len(x)), "row_gap_vs_khan": int(len(x) - KHAN_TABLE12["n"])}
        for value in ["residential", "business", "commercial", "downtown", "planned_development", "other", "missing"]:
            count = int((family == value).sum())
            row[f"{value}_count"] = count
            row[f"{value}_share"] = float(count / len(x)) if len(x) else None
            if value in KHAN_FAMILY_COUNTS:
                row[f"{value}_khan_count"] = KHAN_FAMILY_COUNTS[value]
                row[f"{value}_count_gap"] = count - KHAN_FAMILY_COUNTS[value]
        rows.append(row)
    return pd.DataFrame(rows)


def build_pd_direction_summary(variants: dict[str, pd.DataFrame], mask: pd.Series) -> pd.DataFrame:
    rows = []
    for variant, df in variants.items():
        x = df.loc[mask].copy()
        from_pd = is_pd(x["from_code"])
        to_pd = is_pd(x["to_code"])
        x["pd_direction"] = "non_pd"
        x.loc[(~from_pd) & to_pd, "pd_direction"] = "base_to_pd"
        x.loc[from_pd & (~to_pd), "pd_direction"] = "pd_to_base"
        x.loc[from_pd & to_pd, "pd_direction"] = "pd_to_pd"
        for direction, part in x.groupby("pd_direction", dropna=False):
            from_stats = summarize_numeric(part["from_far"])
            to_stats = summarize_numeric(part["to_far"])
            rows.append(
                {
                    "variant": variant,
                    "pd_direction": direction,
                    "rows": int(len(part)),
                    "from_far_n": from_stats["n"],
                    "from_far_mean": from_stats["mean"],
                    "to_far_n": to_stats["n"],
                    "to_far_mean": to_stats["mean"],
                    "both_far_n": int((part["from_far"].notna() & part["to_far"].notna()).sum()),
                }
            )
    return pd.DataFrame(rows)


def main() -> int:
    args = parse_args()
    df = pd.read_csv(args.in_rezoning_csv, low_memory=False)
    df["matter_intro_date"] = pd.to_datetime(df["matter_intro_date"], errors="coerce")
    for col in ["from_far", "to_far", "far_change"]:
        df[col] = pd.to_numeric(df[col], errors="coerce")
    for col in ["from_code", "to_code", "record_source", "zoning_class", "far_pair_status", "matter_status_name"]:
        if col not in df.columns:
            df[col] = None
        df[col] = df[col].astype("string").str.strip().str.upper()
    if "is_usable_analysis" not in df.columns:
        df["is_usable_analysis"] = False
    usable = as_bool(df["is_usable_analysis"])

    df_base = df.copy()
    df, pd_updates_applied, pd_updates_available = apply_pd_far_updates(df, args.in_pd_far_updates_csv)

    start = pd.Timestamp(args.start_date)
    end = pd.Timestamp(args.end_date)
    in_window = df["matter_intro_date"].between(start, end, inclusive="both")
    sample = df.loc[in_window].copy()
    sample_base = df_base.loc[in_window].copy()
    both_far = sample["from_far"].notna() & sample["to_far"].notna()
    pd_transition = (sample["from_code"].eq("PD") | sample["to_code"].eq("PD")).fillna(False)
    is_elms = sample["record_source"].fillna("").ne("CLERK_JOURNAL_FIRST_PASS")
    is_final = sample["matter_status_name"].eq("90-FINAL").fillna(False)

    variants = [
        ("all_dated_rows", pd.Series(True, index=sample.index)),
        ("final_status_rows", is_final),
        ("final_status_both_far_resolved", is_final & both_far),
        ("both_far_resolved", both_far),
        ("both_far_resolved_non_pd", both_far & ~pd_transition),
        ("usable_analysis", usable.loc[sample.index]),
        ("usable_analysis_non_pd", usable.loc[sample.index] & ~pd_transition),
        ("elms_rows", is_elms),
        ("elms_both_far_resolved", is_elms & both_far),
    ]
    summary = pd.DataFrame([variant_summary(name, sample, mask) for name, mask in variants])
    summary["pd_updates_available_in_input"] = pd_updates_available
    summary["pd_updates_applied_to_input"] = pd_updates_applied
    for key, value in KHAN_TABLE12.items():
        summary[f"khan_table12_{key}"] = value

    sample["year"] = sample["matter_intro_date"].dt.year
    year_counts = (
        sample.assign(
            both_far_resolved=both_far,
            usable_analysis=usable.loc[sample.index],
            pd_transition=pd_transition,
        )
        .groupby("year", dropna=False)
        .agg(
            rows=("matter_id", "size"),
            both_far_resolved=("both_far_resolved", "sum"),
            usable_analysis=("usable_analysis", "sum"),
            pd_transition=("pd_transition", "sum"),
            from_far_mean=("from_far", "mean"),
            to_far_mean=("to_far", "mean"),
        )
        .reset_index()
    )

    pd_transitions = (
        sample.loc[pd_transition]
        .groupby(["from_code", "to_code"], dropna=False)
        .size()
        .reset_index(name="rows")
        .sort_values("rows", ascending=False)
    )

    sample_cols = [
        "matter_id",
        "matter_title",
        "matter_intro_date",
        "record_source",
        "zoning_class",
        "from_code",
        "to_code",
        "from_far",
        "to_far",
        "far_change",
        "far_pair_status",
        "pd_far_review_applied",
        "matter_status_name",
        "latitude",
        "longitude",
        "is_usable_analysis",
        "analysis_exclusion_reason",
    ]
    sample_cols = [col for col in sample_cols if col in sample.columns]

    output_paths = [args.out_summary_csv, args.out_year_counts_csv, args.out_pd_transitions_csv, args.out_sample_csv]
    for path in [args.out_family_counts_csv, args.out_far_rule_variants_csv, args.out_pd_direction_csv]:
        if path:
            output_paths.append(path)
    for path in output_paths:
        ensure_parent(path)
    summary.to_csv(args.out_summary_csv, index=False)
    year_counts.to_csv(args.out_year_counts_csv, index=False)
    pd_transitions.to_csv(args.out_pd_transitions_csv, index=False)
    sample[sample_cols].to_csv(args.out_sample_csv, index=False)

    if args.out_family_counts_csv:
        build_family_counts(sample_base, is_final).to_csv(args.out_family_counts_csv, index=False)

    if args.out_far_rule_variants_csv or args.out_pd_direction_csv:
        far_variants = {
            "base_lookup": sample_base,
            "pd_text_updates": sample,
        }
        far_variants["pd_text_plus_one_sided_nochange"] = apply_one_sided_pd_nochange(far_variants["pd_text_updates"])
        far_variants["pd_text_nochange_plus_pdtopd_manual"] = apply_pd_to_pd_manual(
            far_variants["pd_text_plus_one_sided_nochange"],
            args.in_pd_to_pd_manual_csv,
        )
        far_variants["pd_text_nochange_manual_plus_queue_from_text"] = apply_review_queue_from_text(
            far_variants["pd_text_nochange_plus_pdtopd_manual"],
            args.in_review_queue_csv,
        )
        far_variants["base_lookup_plus_one_sided_nochange"] = apply_one_sided_pd_nochange(sample_base)

        if args.out_far_rule_variants_csv:
            pd.DataFrame(
                [far_rule_summary(name, variant_df, is_final) for name, variant_df in far_variants.items()]
            ).to_csv(args.out_far_rule_variants_csv, index=False)
        if args.out_pd_direction_csv:
            build_pd_direction_summary(far_variants, is_final).to_csv(args.out_pd_direction_csv, index=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
