import argparse
import hashlib
import json
import re
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

YEAR_START = 1999
YEAR_END = 2010
CUTOFF_2010 = pd.Timestamp("2010-11-30")

MODERN_COLUMNS = [
    "matter_id",
    "matter_file",
    "matter_title",
    "zoning_class",
    "matter_status_name",
    "matter_body_name",
    "matter_intro_date",
    "matter_passed_date",
    "map_grid",
    "address_raw",
    "address_raw_loose",
    "app_number",
    "pd_number",
    "map_ref_pdf",
    "from_zoning",
    "to_zoning",
    "ordinance_text_chars",
    "ordinance_text_source",
    "councilmatic_url",
    "councilmatic_pdf_url",
    "councilmatic_status",
    "primary_sponsor_name",
    "primary_sponsor_ward",
    "first_zoning_referral_date",
    "days_intro_to_first_zoning_referral",
    "days_intro_to_passed",
]

PROVENANCE_COLUMNS = [
    "record_source",
    "journal_year",
    "journal_meeting_date",
    "journal_filename",
    "journal_rel_local_path",
    "journal_pdf_url",
    "journal_page_number",
    "journal_text_source",
    "journal_match_type",
    "journal_map_no",
    "journal_has_from_to",
    "journal_addresses_strict",
    "journal_addresses_loose",
    "journal_from_code",
    "journal_to_code",
    "journal_from_far_lookup",
    "journal_to_far_lookup",
    "journal_far_change_lookup",
    "journal_far_values",
    "journal_max_far_value",
    "journal_dwelling_unit_counts",
    "journal_units_per_acre_values",
    "journal_story_counts",
    "journal_height_ft_values",
    "journal_density_mention_count",
    "journal_snippet",
]

MATCH_TYPE_PRIORITY = {
    "ordinance_reclass": 1,
    "committee_reclass": 2,
    "heading_context_reclass": 3,
    "heuristic_reclass": 4,
    "keyword_page": 5,
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", choices=["harmonize", "finalize", "both"], default="harmonize")
    parser.add_argument("--in-modern-csv")
    parser.add_argument("--in-historical-csv")
    parser.add_argument("--in-enriched-csv")
    parser.add_argument("--in-alderman-panel-csv")
    parser.add_argument("--out-harmonized-csv")
    parser.add_argument("--out-all-csv")
    parser.add_argument("--out-usable-csv")
    parser.add_argument("--out-summary-json")
    return parser.parse_args()


def validate_required_args(args: argparse.Namespace) -> None:
    required_by_mode = {
        "harmonize": ["in_modern_csv", "in_historical_csv", "out_harmonized_csv"],
        "finalize": ["in_enriched_csv", "in_alderman_panel_csv", "out_all_csv", "out_usable_csv"],
        "both": [
            "in_modern_csv",
            "in_historical_csv",
            "out_harmonized_csv",
            "in_enriched_csv",
            "in_alderman_panel_csv",
            "out_all_csv",
            "out_usable_csv",
        ],
    }
    missing = [name for name in required_by_mode[args.mode] if not getattr(args, name)]
    if missing:
        raise ValueError(f"Missing required arguments for mode={args.mode}: {', '.join('--' + x.replace('_', '-') for x in missing)}")


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def ensure_columns(df: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    out = df.copy()
    for col in columns:
        if col not in out.columns:
            out[col] = None
    return out


def derive_output_columns(modern_df: pd.DataFrame) -> list[str]:
    passthrough = [col for col in modern_df.columns if col not in MODERN_COLUMNS and col not in PROVENANCE_COLUMNS]
    return MODERN_COLUMNS + passthrough + PROVENANCE_COLUMNS


def parse_bool(value) -> bool:
    if pd.isna(value):
        return False
    if isinstance(value, bool):
        return value
    text = str(value).strip().lower()
    if text in {"1", "true", "t", "yes", "y"}:
        return True
    if text in {"0", "false", "f", "no", "n", ""}:
        return False
    return bool(value)


def clean_text(value) -> str | None:
    if value is None or pd.isna(value):
        return None
    out = re.sub(r"\s+", " ", str(value)).strip(" ,;.-")
    return out or None


def normalize_date(value) -> str | None:
    ts = pd.to_datetime(value, errors="coerce")
    if pd.isna(ts):
        return None
    return ts.strftime("%Y-%m-%d")


def normalize_year_month(series: pd.Series) -> pd.Series:
    text = series.fillna("").astype(str).str.strip()
    out = text.where(text.str.match(r"^\d{4}-\d{2}$"), None)
    try:
        ts = pd.to_datetime(text, errors="coerce", format="mixed")
    except TypeError:
        ts = pd.to_datetime(text, errors="coerce")
    from_ts = ts.dt.strftime("%Y-%m").where(ts.notna())
    return out.fillna(from_ts)


def normalize_ward_series(series: pd.Series) -> pd.Series:
    text = series.fillna("").astype(str).str.strip()
    extracted = text.str.extract(r"(\d+(?:\.\d+)?)", expand=False)
    numeric = pd.to_numeric(extracted, errors="coerce")
    near_int = numeric.notna() & ((numeric - numeric.round()).abs() < 1e-8)
    numeric = numeric.where(near_int)
    wards = numeric.round().astype("Int64")
    return wards.where(wards.between(1, 50))


def prepare_alderman_panel(panel_df: pd.DataFrame) -> tuple[pd.DataFrame, dict]:
    panel = ensure_columns(panel_df, ["ward", "alderman", "year_month", "month_start_date", "month"]).copy()
    panel["panel_ward"] = normalize_ward_series(panel["ward"])
    panel["panel_year_month"] = normalize_year_month(panel["year_month"])
    missing_mask = panel["panel_year_month"].isna()
    panel.loc[missing_mask, "panel_year_month"] = normalize_year_month(panel.loc[missing_mask, "month_start_date"])
    missing_mask = panel["panel_year_month"].isna()
    panel.loc[missing_mask, "panel_year_month"] = normalize_year_month(panel.loc[missing_mask, "month"])
    panel["panel_alderman"] = panel["alderman"].map(clean_text)

    prepared = panel.dropna(subset=["panel_ward", "panel_year_month"]).copy()
    duplicate_key_rows = int(prepared.duplicated(subset=["panel_ward", "panel_year_month"]).sum())
    if duplicate_key_rows > 0:
        raise ValueError("Alderman panel has duplicate keys for ward + year_month.")
    prepared = prepared[["panel_ward", "panel_year_month", "panel_alderman"]]

    diag = {
        "alderman_panel_rows_input": int(len(panel_df)),
        "alderman_panel_rows_prepared": int(len(prepared)),
        "alderman_panel_duplicate_key_rows": duplicate_key_rows,
        "alderman_panel_min_month": prepared["panel_year_month"].min() if len(prepared) else None,
        "alderman_panel_max_month": prepared["panel_year_month"].max() if len(prepared) else None,
    }
    return prepared, diag


def normalize_code(value) -> str | None:
    text = clean_text(value)
    if not text:
        return None
    out = text.upper()
    if out in {"NAN", "NA", "NONE"}:
        return None
    return out


def parse_address_list(value) -> list[str]:
    if value is None or pd.isna(value):
        return []
    out: list[str] = []
    seen = set()
    for piece in str(value).split(";"):
        cleaned = clean_text(piece)
        if not cleaned:
            continue
        key = cleaned.upper()
        if key in seen:
            continue
        seen.add(key)
        out.append(cleaned)
    return out


def first_address(value) -> str | None:
    values = parse_address_list(value)
    return values[0] if values else None


def first_address_from_cols(row: pd.Series, columns: list[str]) -> str | None:
    for col in columns:
        if col not in row.index:
            continue
        found = first_address(row.get(col))
        if found:
            return found
    return None


def match_type_rank(value) -> int:
    text = clean_text(value)
    if not text:
        return 99
    return MATCH_TYPE_PRIORITY.get(text.lower(), 99)


def page_number_rank(value) -> int:
    out = pd.to_numeric(value, errors="coerce")
    if pd.isna(out):
        return 10**9
    return int(out)


def filter_historical_rows(historical_df: pd.DataFrame) -> tuple[pd.DataFrame, dict]:
    out = ensure_columns(
        historical_df,
        [
            "journal_year",
            "journal_meeting_date",
            "has_from_to",
            "map_no",
            "from_district_raw",
            "to_district_raw",
            "from_code",
            "to_code",
        ],
    ).copy()

    out["journal_year_num"] = pd.to_numeric(out["journal_year"], errors="coerce").astype("Int64")
    out["meeting_ts"] = pd.to_datetime(out["journal_meeting_date"], errors="coerce")
    out["meeting_date_norm"] = out["meeting_ts"].map(normalize_date)
    out["map_no_norm"] = out["map_no"].map(clean_text)
    out["from_code_norm"] = out["from_code"].map(normalize_code)
    out["to_code_norm"] = out["to_code"].map(normalize_code)
    out["has_from_to_bool"] = out["has_from_to"].map(parse_bool)

    input_rows = int(len(out))
    has_from_to_rows = int(out["has_from_to_bool"].sum())
    missing_code_rows = int((out["has_from_to_bool"] & (out["from_code_norm"].isna() | out["to_code_norm"].isna())).sum())

    out = out[out["has_from_to_bool"]].copy()
    out = out[out["from_code_norm"].notna() & out["to_code_norm"].notna()].copy()

    outside_window = ~out["journal_year_num"].between(YEAR_START, YEAR_END)
    dropped_outside_window = int(outside_window.fillna(True).sum())
    out = out[~outside_window.fillna(True)].copy()

    in_1999_2009 = out["journal_year_num"].between(YEAR_START, 2009)
    in_2010 = out["journal_year_num"] == 2010
    dropped_2010_missing_date = int((in_2010 & out["meeting_ts"].isna()).sum())
    dropped_2010_post_cutoff = int((in_2010 & out["meeting_ts"].notna() & (out["meeting_ts"] > CUTOFF_2010)).sum())

    keep_mask = in_1999_2009 | (in_2010 & out["meeting_ts"].notna() & (out["meeting_ts"] <= CUTOFF_2010))
    out = out[keep_mask].copy()

    missing_key_mask = (
        out["meeting_date_norm"].isna()
        | out["map_no_norm"].isna()
        | out["from_code_norm"].isna()
        | out["to_code_norm"].isna()
    )
    dropped_missing_dedupe_keys = int(missing_key_mask.sum())
    out = out[~missing_key_mask].copy()

    diag = {
        "historical_rows_input": input_rows,
        "historical_rows_has_from_to_true": has_from_to_rows,
        "historical_rows_missing_from_or_to_code_dropped": missing_code_rows,
        "historical_rows_outside_1999_2010_dropped": dropped_outside_window,
        "historical_rows_2010_missing_date_dropped": dropped_2010_missing_date,
        "historical_rows_2010_post_cutoff_dropped": dropped_2010_post_cutoff,
        "historical_rows_missing_dedupe_keys_dropped": dropped_missing_dedupe_keys,
        "historical_rows_after_filters": int(len(out)),
    }
    return out, diag


def dedupe_historical_rows(filtered_df: pd.DataFrame) -> tuple[pd.DataFrame, dict]:
    if filtered_df.empty:
        return filtered_df.copy(), {
            "historical_rows_after_dedupe": 0,
            "historical_rows_deduped_out": 0,
        }

    out = ensure_columns(
        filtered_df,
        [
            "meeting_date_norm",
            "map_no_norm",
            "from_code_norm",
            "to_code_norm",
            "addresses_strict",
            "addresses_loose",
            "addresses",
            "match_type",
            "page_number",
            "snippet",
            "journal_filename",
            "journal_rel_local_path",
        ],
    ).copy()
    out["first_address_strict"] = out.apply(
        lambda row: first_address_from_cols(row, ["addresses_strict", "addresses"]), axis=1
    )
    out["first_address_loose"] = out.apply(lambda row: first_address_from_cols(row, ["addresses_loose"]), axis=1)
    out["has_address_strict"] = out["first_address_strict"].notna()
    out["has_address_loose"] = out["first_address_loose"].notna()
    out["match_type_rank"] = out["match_type"].map(match_type_rank)
    out["page_number_rank"] = out["page_number"].map(page_number_rank)
    out["snippet_len"] = out["snippet"].fillna("").astype(str).str.len()

    dedupe_keys = ["meeting_date_norm", "map_no_norm", "from_code_norm", "to_code_norm"]
    out = out.sort_values(
        dedupe_keys
        + [
            "has_address_strict",
            "has_address_loose",
            "match_type_rank",
            "page_number_rank",
            "snippet_len",
            "journal_filename",
            "journal_rel_local_path",
        ],
        ascending=[True, True, True, True, False, False, True, True, False, True, True],
        kind="stable",
    )
    deduped = out.drop_duplicates(subset=dedupe_keys, keep="first").copy()

    diag = {
        "historical_rows_after_dedupe": int(len(deduped)),
        "historical_rows_deduped_out": int(len(filtered_df) - len(deduped)),
    }
    return deduped, diag


def build_historical_ids(df: pd.DataFrame) -> pd.Series:
    keys = (
        ensure_columns(df, ["meeting_date_norm", "map_no_norm", "from_code_norm", "to_code_norm"])[
            ["meeting_date_norm", "map_no_norm", "from_code_norm", "to_code_norm"]
        ]
        .fillna("")
        .astype(str)
        .agg("|".join, axis=1)
    )
    hashes = keys.map(lambda value: hashlib.sha1(value.encode("utf-8")).hexdigest()[:14])
    counts = hashes.groupby(hashes).cumcount() + 1
    return pd.Series(
        [
            f"HISTFP_{base}" if int(rank) == 1 else f"HISTFP_{base}_{int(rank)}"
            for base, rank in zip(hashes.tolist(), counts.tolist())
        ],
        index=df.index,
    )


def build_historical_frame(deduped_df: pd.DataFrame, output_columns: list[str]) -> pd.DataFrame:
    hist = ensure_columns(
        deduped_df,
        [
            "journal_year",
            "journal_meeting_date",
            "journal_filename",
            "journal_rel_local_path",
            "journal_pdf_url",
            "page_number",
            "text_source",
            "match_type",
            "map_no",
            "from_district_raw",
            "to_district_raw",
            "has_from_to",
            "from_code",
            "to_code",
            "from_far_lookup",
            "to_far_lookup",
            "far_change_lookup",
            "addresses",
            "addresses_strict",
            "addresses_loose",
            "far_values",
            "max_far_value",
            "dwelling_unit_counts",
            "units_per_acre_values",
            "story_counts",
            "height_ft_values",
            "density_mention_count",
            "snippet",
            "meeting_date_norm",
            "map_no_norm",
            "from_code_norm",
            "to_code_norm",
            "first_address_strict",
            "first_address_loose",
            "page_number_rank",
        ],
    ).copy()

    hist = hist.sort_values(
        ["meeting_date_norm", "map_no_norm", "from_code_norm", "to_code_norm", "page_number_rank"],
        kind="stable",
    ).reset_index(drop=True)
    hist["matter_id"] = build_historical_ids(hist).astype(str)

    address_strict = hist["first_address_strict"]
    address_loose = hist["first_address_loose"]
    address_raw = address_strict.where(address_strict.notna(), address_loose)

    out = pd.DataFrame()
    out["matter_id"] = hist["matter_id"]
    out["matter_file"] = None
    out["matter_title"] = (
        "Journal Rezoning Map No. "
        + hist["map_no_norm"].fillna("UNKNOWN").astype(str)
        + " ("
        + hist["from_district_raw"].fillna("?").astype(str)
        + " to "
        + hist["to_district_raw"].fillna("?").astype(str)
        + ")"
    )
    out["zoning_class"] = "journal_rezoning"
    out["matter_status_name"] = "Passed"
    out["matter_body_name"] = "City Council"
    out["matter_intro_date"] = hist["meeting_date_norm"]
    out["matter_passed_date"] = hist["meeting_date_norm"]
    out["map_grid"] = hist["map_no_norm"]
    out["address_raw"] = address_raw
    out["address_raw_loose"] = address_loose
    out["app_number"] = None
    out["pd_number"] = None
    out["map_ref_pdf"] = hist["map_no_norm"]
    out["from_zoning"] = hist["from_district_raw"].map(clean_text)
    out["to_zoning"] = hist["to_district_raw"].map(clean_text)
    out["ordinance_text_chars"] = hist["snippet"].fillna("").astype(str).str.len().astype("Int64").astype(str)
    out["ordinance_text_source"] = "journal_snippet"
    out["councilmatic_url"] = None
    out["councilmatic_pdf_url"] = None
    out["councilmatic_status"] = None
    out["primary_sponsor_name"] = None
    out["primary_sponsor_ward"] = None
    out["first_zoning_referral_date"] = None
    out["days_intro_to_first_zoning_referral"] = None
    out["days_intro_to_passed"] = None

    out["record_source"] = "clerk_journal_first_pass"
    out["journal_year"] = hist["journal_year"].map(clean_text)
    out["journal_meeting_date"] = hist["meeting_date_norm"]
    out["journal_filename"] = hist["journal_filename"].map(clean_text)
    out["journal_rel_local_path"] = hist["journal_rel_local_path"].map(clean_text)
    out["journal_pdf_url"] = hist["journal_pdf_url"].map(clean_text)
    out["journal_page_number"] = hist["page_number"].map(clean_text)
    out["journal_text_source"] = hist["text_source"].map(clean_text)
    out["journal_match_type"] = hist["match_type"].map(clean_text)
    out["journal_map_no"] = hist["map_no_norm"]
    out["journal_has_from_to"] = hist["has_from_to"].map(clean_text)
    out["journal_addresses_strict"] = hist["addresses_strict"].fillna(hist["addresses"]).map(clean_text)
    out["journal_addresses_loose"] = hist["addresses_loose"].map(clean_text)
    out["journal_from_code"] = hist["from_code_norm"]
    out["journal_to_code"] = hist["to_code_norm"]
    out["journal_from_far_lookup"] = hist["from_far_lookup"].map(clean_text)
    out["journal_to_far_lookup"] = hist["to_far_lookup"].map(clean_text)
    out["journal_far_change_lookup"] = hist["far_change_lookup"].map(clean_text)
    out["journal_far_values"] = hist["far_values"].map(clean_text)
    out["journal_max_far_value"] = hist["max_far_value"].map(clean_text)
    out["journal_dwelling_unit_counts"] = hist["dwelling_unit_counts"].map(clean_text)
    out["journal_units_per_acre_values"] = hist["units_per_acre_values"].map(clean_text)
    out["journal_story_counts"] = hist["story_counts"].map(clean_text)
    out["journal_height_ft_values"] = hist["height_ft_values"].map(clean_text)
    out["journal_density_mention_count"] = hist["density_mention_count"].map(clean_text)
    out["journal_snippet"] = hist["snippet"].map(clean_text)

    out = ensure_columns(out, output_columns)
    return out[output_columns]


def prepare_modern_frame(modern_df: pd.DataFrame, output_columns: list[str]) -> pd.DataFrame:
    out = ensure_columns(modern_df, output_columns).copy()
    out["matter_id"] = out["matter_id"].astype(str)
    out["record_source"] = "elms"
    for col in PROVENANCE_COLUMNS:
        if col != "record_source":
            out[col] = None
    return out[output_columns]


def build_harmonized_dataset(modern_df: pd.DataFrame, historical_df: pd.DataFrame) -> tuple[pd.DataFrame, dict]:
    output_columns = derive_output_columns(modern_df)
    modern = prepare_modern_frame(modern_df, output_columns)
    hist_filtered, hist_filter_diag = filter_historical_rows(historical_df)
    hist_deduped, hist_dedupe_diag = dedupe_historical_rows(hist_filtered)
    historical = build_historical_frame(hist_deduped, output_columns)

    combined = pd.concat([historical, modern], ignore_index=True)
    combined["_sort_date"] = pd.to_datetime(combined["matter_intro_date"], errors="coerce")
    combined = combined.sort_values(["_sort_date", "record_source", "matter_id"], kind="stable").drop(columns=["_sort_date"])

    duplicate_matter_id_rows = int(combined["matter_id"].duplicated(keep=False).sum())
    if duplicate_matter_id_rows > 0:
        examples = combined.loc[combined["matter_id"].duplicated(keep=False), "matter_id"].head(5).tolist()
        raise ValueError(f"Combined output has duplicate matter_id values. Examples: {examples}")

    historical_far_both = int(
        (
            pd.to_numeric(historical["journal_from_far_lookup"], errors="coerce").notna()
            & pd.to_numeric(historical["journal_to_far_lookup"], errors="coerce").notna()
        ).sum()
    )
    historical_loose_fallback_count = int(
        (
            historical["address_raw"].notna()
            & historical["journal_addresses_strict"].fillna("").eq("")
            & historical["journal_addresses_loose"].fillna("").ne("")
        ).sum()
    )

    summary = {
        "status": "ok",
        **hist_filter_diag,
        **hist_dedupe_diag,
        "modern_rows_input": int(len(modern_df)),
        "modern_rows_kept": int(len(modern)),
        "historical_rows_kept": int(len(historical)),
        "historical_rows_with_address_raw": int(historical["address_raw"].notna().sum()),
        "historical_rows_with_address_raw_loose": int(historical["address_raw_loose"].notna().sum()),
        "historical_rows_using_loose_address_fallback": historical_loose_fallback_count,
        "historical_rows_with_far_lookup_both": historical_far_both,
        "combined_rows": int(len(combined)),
        "combined_unique_matter_id_rows": int(combined["matter_id"].nunique()),
        "combined_duplicate_matter_id_rows": duplicate_matter_id_rows,
        "combined_record_source_counts": {
            str(k): int(v) for k, v in combined["record_source"].fillna("NA").value_counts(dropna=False).to_dict().items()
        },
        "combined_min_intro_date": normalize_date(combined["matter_intro_date"].min()),
        "combined_max_intro_date": normalize_date(combined["matter_intro_date"].max()),
    }
    return combined, summary


def _value_counts_dict(series: pd.Series) -> dict:
    if series.empty:
        return {}
    return {str(k): int(v) for k, v in series.fillna("NA").value_counts(dropna=False).to_dict().items()}


def build_finalized_outputs(df: pd.DataFrame, alderman_panel_df: pd.DataFrame | None = None) -> tuple[pd.DataFrame, pd.DataFrame, dict]:
    out = df.copy()
    out["_row_order"] = range(len(out))

    if "record_source" in out.columns:
        out["record_source"] = out["record_source"].replace({"legistar": "elms", "Legistar": "elms"})

    far_change_num = pd.to_numeric(out.get("far_change"), errors="coerce")
    lat_num = pd.to_numeric(out.get("latitude"), errors="coerce")
    lon_num = pd.to_numeric(out.get("longitude"), errors="coerce")
    intro_year_month = normalize_year_month(out.get("matter_intro_date", pd.Series(index=out.index, dtype=object)))
    ward_norm = normalize_ward_series(out.get("ward", pd.Series(index=out.index, dtype=object)))

    has_far = far_change_num.notna()
    has_latlon = lat_num.notna() & lon_num.notna()
    out["is_usable_analysis"] = has_far & has_latlon

    out["analysis_exclusion_reason"] = "usable"
    out.loc[~has_far & has_latlon, "analysis_exclusion_reason"] = "missing_far_change"
    out.loc[has_far & ~has_latlon, "analysis_exclusion_reason"] = "missing_latlon"
    out.loc[~has_far & ~has_latlon, "analysis_exclusion_reason"] = "missing_far_change_and_latlon"

    out["assigned_alderman"] = pd.NA
    out["assigned_alderman_month"] = pd.NA
    out["assigned_alderman_date_source"] = pd.NA

    eligible_for_assignment = ward_norm.notna() & has_latlon & intro_year_month.notna()
    out["_eligible_alderman_assignment"] = eligible_for_assignment
    out.loc[eligible_for_assignment, "assigned_alderman_month"] = intro_year_month[eligible_for_assignment].values
    out.loc[eligible_for_assignment, "assigned_alderman_date_source"] = "matter_intro_date"

    panel_diag = {
        "alderman_panel_min_month": None,
        "alderman_panel_max_month": None,
    }
    if alderman_panel_df is not None:
        panel_lookup, panel_diag = prepare_alderman_panel(alderman_panel_df)
        eligible_idx = out.index[eligible_for_assignment]
        if len(eligible_idx) > 0 and not panel_lookup.empty:
            lookup = panel_lookup.set_index(["panel_ward", "panel_year_month"])["panel_alderman"].to_dict()
            assigned_vals = []
            for ward_val, month_val in zip(
                ward_norm.loc[eligible_idx].astype("Int64").tolist(),
                intro_year_month.loc[eligible_idx].tolist(),
            ):
                assigned_vals.append(lookup.get((ward_val, month_val)))
            out.loc[eligible_idx, "assigned_alderman"] = assigned_vals

    if "matter_intro_date" in out.columns:
        intro_date_text = out["matter_intro_date"].fillna("").astype(str).str.strip()
        try:
            out["_matter_intro_sort"] = pd.to_datetime(intro_date_text, format="mixed", errors="coerce")
        except TypeError:
            out["_matter_intro_sort"] = pd.to_datetime(intro_date_text, errors="coerce")
    else:
        out["_matter_intro_sort"] = pd.NaT

    if "matter_id" in out.columns:
        out["_matter_id_sort"] = out["matter_id"].fillna("").astype(str)
    else:
        out["_matter_id_sort"] = ""

    out = out.sort_values(
        ["_matter_intro_sort", "_matter_id_sort", "_row_order"],
        kind="mergesort",
        na_position="last",
    ).reset_index(drop=True)

    usable = out[out["is_usable_analysis"]].copy().reset_index(drop=True)

    drop_cols = ["_row_order", "_matter_intro_sort", "_matter_id_sort"]
    out = out.drop(columns=drop_cols, errors="ignore")
    usable = usable.drop(columns=drop_cols, errors="ignore")

    missing_far = int((out["analysis_exclusion_reason"] == "missing_far_change").sum())
    missing_latlon = int((out["analysis_exclusion_reason"] == "missing_latlon").sum())
    missing_both = int((out["analysis_exclusion_reason"] == "missing_far_change_and_latlon").sum())
    eligible_rows = int(out["_eligible_alderman_assignment"].sum())
    assigned_rows = int((out["_eligible_alderman_assignment"] & out["assigned_alderman"].notna()).sum())
    out = out.drop(columns=["_eligible_alderman_assignment"], errors="ignore")
    usable = usable.drop(columns=["_eligible_alderman_assignment"], errors="ignore")

    summary = {
        "status": "ok",
        "total_rows_all": int(len(out)),
        "total_rows_usable": int(len(usable)),
        "usable_share": float((int(out["is_usable_analysis"].sum()) / len(out)) if len(out) else 0.0),
        "missing_far_change_count": missing_far,
        "missing_latlon_count": missing_latlon,
        "missing_both_count": missing_both,
        "record_source_counts": _value_counts_dict(out.get("record_source", pd.Series(dtype=object))),
        "geocode_source_counts": _value_counts_dict(out.get("geocode_source", pd.Series(dtype=object))),
        "alderman_assignment_eligible_rows": eligible_rows,
        "alderman_assigned_rows": assigned_rows,
        "alderman_assignment_rate_eligible": float((assigned_rows / eligible_rows) if eligible_rows else 0.0),
        "alderman_unassigned_rows_eligible": int(eligible_rows - assigned_rows),
        "alderman_panel_min_month": panel_diag["alderman_panel_min_month"],
        "alderman_panel_max_month": panel_diag["alderman_panel_max_month"],
    }
    return out, usable, summary


def not_run_section() -> dict:
    return {"status": "not_run"}


def main() -> int:
    args = parse_args()
    validate_required_args(args)

    summary_payload = {
        "generated_at_utc": datetime.now(timezone.utc).isoformat(timespec="seconds").replace("+00:00", "Z"),
        "run_mode": args.mode,
        "harmonize": not_run_section(),
        "finalize": not_run_section(),
    }

    if args.mode in {"harmonize", "both"}:
        modern_df = pd.read_csv(args.in_modern_csv, dtype=str, low_memory=False)
        historical_df = pd.read_csv(args.in_historical_csv, dtype=str, low_memory=False)
        harmonized_df, harmonize_summary = build_harmonized_dataset(modern_df=modern_df, historical_df=historical_df)

        ensure_parent(args.out_harmonized_csv)
        harmonized_df.to_csv(args.out_harmonized_csv, index=False)

        summary_payload["harmonize"] = {
            "status": "ok",
            "input_modern_csv": args.in_modern_csv,
            "input_historical_csv": args.in_historical_csv,
            "output_harmonized_csv": args.out_harmonized_csv,
            **harmonize_summary,
        }

    if args.mode in {"finalize", "both"}:
        enriched_df = pd.read_csv(args.in_enriched_csv, low_memory=False)
        alderman_panel_df = pd.read_csv(args.in_alderman_panel_csv, dtype=str, low_memory=False)
        final_all, final_usable, finalize_summary = build_finalized_outputs(
            enriched_df,
            alderman_panel_df=alderman_panel_df,
        )

        ensure_parent(args.out_all_csv)
        ensure_parent(args.out_usable_csv)
        final_all.to_csv(args.out_all_csv, index=False)
        final_usable.to_csv(args.out_usable_csv, index=False)

        summary_payload["finalize"] = {
            "status": "ok",
            "input_enriched_csv": args.in_enriched_csv,
            "input_alderman_panel_csv": args.in_alderman_panel_csv,
            "output_all_csv": args.out_all_csv,
            "output_usable_csv": args.out_usable_csv,
            **finalize_summary,
        }

    if args.out_summary_json:
        ensure_parent(args.out_summary_json)
        with open(args.out_summary_json, "w", encoding="utf-8") as handle:
            json.dump(summary_payload, handle, indent=2)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
