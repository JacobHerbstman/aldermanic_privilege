#!/usr/bin/env python3
import argparse
from pathlib import Path

import pandas as pd

from fetch_matter_children import run_pipeline

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--date-tag", default="20101201_20260212")
    parser.add_argument("--sleep-seconds", default="0.2")
    parser.add_argument("--max-retries", default="4")
    parser.add_argument("--timeout-seconds", default="60")
    return parser.parse_args()


def tag_table(path: Path, table_name: str) -> pd.DataFrame:
    if not path.exists():
        return pd.DataFrame({"child_table": []})
    df = pd.read_csv(path, dtype=str, low_memory=False)
    df["child_table"] = table_name
    return df


def main() -> int:
    args = parse_args()
    tag = args.date_tag
    Path("../temp").mkdir(parents=True, exist_ok=True)

    sponsors = Path(f"../temp/matter_sponsors_{tag}.csv")
    histories = Path(f"../temp/matter_histories_{tag}.csv")
    attachments = Path(f"../temp/matter_attachments_{tag}.csv")
    candidates = Path(f"../temp/candidate_final_ids_{tag}.csv")

    rc = run_pipeline(
        argparse.Namespace(
            in_matters_csv=f"../input/matters_{tag}.csv",
            in_seed_csv=f"../input/candidate_seed_ids_{tag}.csv",
            sleep_seconds=float(args.sleep_seconds),
            max_retries=int(args.max_retries),
            timeout_seconds=int(args.timeout_seconds),
            out_sponsors=str(sponsors),
            out_histories=str(histories),
            out_attachments=str(attachments),
            out_indexes=None,
            out_persons=None,
            out_candidates=str(candidates),
            out_report_json=None,
        )
    )
    if rc != 0:
        return rc

    long_df = pd.concat(
        [
            tag_table(sponsors, "sponsors"),
            tag_table(histories, "histories"),
            tag_table(attachments, "attachments"),
            tag_table(candidates, "candidates"),
        ],
        ignore_index=True,
        sort=False,
    )
    long_df.to_csv(f"../temp/matter_children_long_{tag}.csv", index=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
