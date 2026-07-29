import argparse
import importlib.util
import re

import pandas as pd


MANUAL_DECISIONS = {
    "O2014-1444": ("non_scalar_multiple_far", None, None),
    "O2014-1522": ("non_scalar_multiple_far", None, None),
    "SO2011-5141": ("non_scalar_multiple_far", None, None),
    "SO2013-3327": ("scalar_ordinance_review", "B3-2", "B2-3"),
    "SO2013-8395": ("non_scalar_multiple_far", None, None),
    "SO2014-2421": ("non_scalar_multiple_far", None, None),
    "SO2014-4190": ("non_scalar_multiple_far", None, None),
    "SO2015-2402": ("non_scalar_multiple_far", None, None),
    "SO2016-2607": ("non_scalar_multiple_far", None, None),
    "SO2016-2664": ("non_scalar_multiple_far", None, None),
    "SO2018-387": ("non_scalar_multiple_far", None, None),
    "SO2020-1899": ("unresolved_destination_scope", None, None),
    "SO2020-3706": ("scalar_ordinance_review", "B3-2", "B3-3"),
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("date_tag")
    return parser.parse_args()


def load_far_module():
    spec = importlib.util.spec_from_file_location(
        "assign_far_pre_geocode",
        "../../../rezoning_far_pre_geocode/code/assign_far_pre_geocode.py",
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def side_values(row, column, far_module, lookup):
    codes = far_module.extract_all_zoning_codes(row[column])
    fars = [far_module.resolve_far(code, row["matter_intro_date"], lookup)[0] for code in codes]
    return codes, fars


def side_status(codes, fars):
    if not codes or any(value is None for value in fars):
        return "incomplete"
    if len(set(fars)) == 1:
        return "same_far"
    return "different_far"


def evidence_excerpt(text):
    compact = re.sub(r"\s+", " ", text or "").strip()
    match = re.search(r"(?i)(amended by changing|change from)", compact)
    if not match:
        return compact[:700]
    return compact[max(0, match.start() - 80) : match.start() + 620]


def main() -> int:
    args = parse_args()
    far_module = load_far_module()
    lookup = far_module.load_far_lookup("../input/zoning_far_lookup_clean.csv")

    matters = pd.read_csv(f"../input/zoning_matters_far_{args.date_tag}.csv", low_memory=False)
    review = matters.loc[
        matters["matter_status_name"].str.contains("Final", case=False, na=False)
        & (matters["from_code_count"].gt(1) | matters["to_code_count"].gt(1))
    ].copy()

    pages = pd.read_csv(f"../input/pdf_text_{args.date_tag}.csv", dtype=str)
    page_text = (
        pages.assign(page_number=pd.to_numeric(pages["page_number"], errors="coerce"))
        .sort_values(["matter_id", "page_number"])
        .groupby("matter_id", as_index=False)["page_text"]
        .agg(lambda values: " ".join(values.fillna("")))
    )
    review = review.merge(page_text, on="matter_id", how="left", validate="one_to_one")

    records = []
    for row in review.to_dict("records"):
        from_codes, from_fars = side_values(row, "from_zoning_raw", far_module, lookup)
        to_codes, to_fars = side_values(row, "to_zoning_raw", far_module, lookup)
        from_status = side_status(from_codes, from_fars)
        to_status = side_status(to_codes, to_fars)

        if row["far_pair_source"] == "ordinance_pd_review":
            decision = "accepted_ordinance_far_review"
        elif from_status == "same_far" and to_status == "same_far":
            decision = "scalar_equivalent_codes"
        elif from_status == "different_far" or to_status == "different_far":
            decision = "non_scalar_multiple_far"
        else:
            decision = "excluded_incomplete_parse"

        reviewed_from_code = None
        reviewed_to_code = None
        if row["matter_id"] in MANUAL_DECISIONS:
            decision, reviewed_from_code, reviewed_to_code = MANUAL_DECISIONS[row["matter_id"]]

        records.append(
            {
                "matter_id": row["matter_id"],
                "matter_intro_date": row["matter_intro_date"],
                "matter_title": row["matter_title"],
                "from_codes": "|".join(from_codes),
                "from_fars": "|".join("" if value is None else f"{value:g}" for value in from_fars),
                "to_codes": "|".join(to_codes),
                "to_fars": "|".join("" if value is None else f"{value:g}" for value in to_fars),
                "from_side_status": from_status,
                "to_side_status": to_status,
                "production_from_far": row["from_far"],
                "production_to_far": row["to_far"],
                "production_pair_status": row["far_pair_status"],
                "production_transition_status": row["far_transition_status"],
                "far_pair_source": row["far_pair_source"],
                "audit_decision": decision,
                "reviewed_from_code": reviewed_from_code,
                "reviewed_to_code": reviewed_to_code,
                "ordinance_excerpt": evidence_excerpt(row.get("page_text")),
            }
        )

    out = pd.DataFrame(records).sort_values("matter_id")
    if len(out) != 346 or out["matter_id"].duplicated().any():
        raise ValueError(f"Expected 346 unique multi-code matters, found {len(out)}")

    out.to_csv("../output/section_review_matters.csv", index=False)
    (
        out.groupby("audit_decision", as_index=False)
        .size()
        .rename(columns={"size": "matter_count"})
        .sort_values("audit_decision")
        .to_csv("../output/section_review_summary.csv", index=False)
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
