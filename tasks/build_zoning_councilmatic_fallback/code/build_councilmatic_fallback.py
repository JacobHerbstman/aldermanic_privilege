import argparse
import sys

import pandas as pd

sys.path.append("../../build_zoning_dataset/code")

from assemble_zoning_tables import (  # noqa: E402
    COUNCILMATIC_FIELD_COLUMNS,
    build_pdf_matter_fields,
    build_pdf_matter_text,
    councilmatic_target_rows,
    fetch_councilmatic_fields,
    normalize_candidates,
    normalize_matters,
    normalize_pdf_fields,
    normalize_pdf_text,
    normalize_councilmatic_fields,
    parsed_pair_quality,
    reparse_from_best_text,
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("date_tag")
    parser.add_argument("--sleep-seconds", type=float, default=0.0)
    parser.add_argument("--timeout-seconds", type=int, default=60)
    parser.add_argument("--progress-every", type=int, default=25)
    return parser.parse_args()


def main() -> int:
    args = parse_args()

    matters_raw = pd.read_csv(f"../input/matters_{args.date_tag}.csv", dtype=str, low_memory=False)
    pdf_fields_raw = pd.read_csv(f"../input/pdf_zoning_fields_{args.date_tag}.csv", dtype=str, low_memory=False)
    pdf_text_raw = pd.read_csv(f"../input/pdf_text_{args.date_tag}.csv", dtype=str, low_memory=False)
    candidates_raw = pd.read_csv(f"../input/candidate_final_ids_{args.date_tag}.csv", dtype=str, low_memory=False)

    candidates = normalize_candidates(candidates_raw)
    final_ids = set(
        candidates.loc[candidates["is_final_candidate"].fillna(False), "matter_id"]
        .dropna()
        .tolist()
    )

    matters = normalize_matters(matters_raw)
    pdf_fields = normalize_pdf_fields(pdf_fields_raw)
    pdf_text = normalize_pdf_text(pdf_text_raw)

    if final_ids:
        matters = matters[matters["matter_id"].isin(final_ids)].copy()
        pdf_fields = pdf_fields[pdf_fields["matter_id"].isin(final_ids)].copy()
        pdf_text = pdf_text[pdf_text["matter_id"].isin(final_ids)].copy()
    else:
        matters = matters.iloc[0:0].copy()
        pdf_fields = pdf_fields.iloc[0:0].copy()
        pdf_text = pdf_text.iloc[0:0].copy()

    matters = matters.merge(build_pdf_matter_fields(pdf_fields), on="matter_id", how="left")
    matters = matters.merge(build_pdf_matter_text(pdf_text), on="matter_id", how="left")
    for col in [
        "from_zoning",
        "to_zoning",
        "from_zoning_raw",
        "to_zoning_raw",
        "from_zoning_canonical",
        "to_zoning_canonical",
        "zoning_parse_method",
    ]:
        if col not in matters.columns:
            matters[col] = pd.Series([None] * len(matters), dtype="string")
        else:
            matters[col] = matters[col].astype("string")
    if "zoning_parse_quality" not in matters.columns:
        matters["zoning_parse_quality"] = 0
    matters["zoning_parse_quality"] = (
        pd.to_numeric(matters["zoning_parse_quality"], errors="coerce").fillna(0).astype(int)
    )
    matters["pdf_text"] = matters["pdf_text"].astype("string")

    matters = reparse_from_best_text(matters, text_col="pdf_text", source_tag="pdf_text")

    target = councilmatic_target_rows(matters)
    if target.empty:
        out = pd.DataFrame(
            columns=[
                "pre_councilmatic_from_zoning",
                "pre_councilmatic_to_zoning",
                "pre_councilmatic_parse_quality",
                *COUNCILMATIC_FIELD_COLUMNS,
                "councilmatic_parse_quality",
            ]
        )
        out.to_csv(f"../output/councilmatic_fallback_{args.date_tag}.csv", index=False)
        return 0

    target = target.merge(
        matters[["matter_id", "matter_file", "from_zoning", "to_zoning"]],
        on=["matter_id", "matter_file"],
        how="left",
        validate="one_to_one",
    )
    target = target.rename(
        columns={
            "from_zoning": "pre_councilmatic_from_zoning",
            "to_zoning": "pre_councilmatic_to_zoning",
        }
    )
    target["pre_councilmatic_parse_quality"] = target.apply(
        lambda row: parsed_pair_quality(
            row["pre_councilmatic_from_zoning"], row["pre_councilmatic_to_zoning"]
        ),
        axis=1,
    )

    fields = fetch_councilmatic_fields(
        matters,
        sleep_seconds=args.sleep_seconds,
        timeout_seconds=args.timeout_seconds,
        progress_every=args.progress_every,
    )
    fields = normalize_councilmatic_fields(fields, target[["matter_id", "matter_file"]])
    fields["councilmatic_parse_quality"] = fields.apply(
        lambda row: parsed_pair_quality(row["councilmatic_from_zoning"], row["councilmatic_to_zoning"]),
        axis=1,
    )

    out = target.merge(
        fields.drop(columns=["matter_file"]),
        on="matter_id",
        how="left",
        validate="one_to_one",
    )
    out.to_csv(f"../output/councilmatic_fallback_{args.date_tag}.csv", index=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
