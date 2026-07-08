import argparse
import json
from datetime import datetime, timezone
from pathlib import Path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-summary-json", required=True)
    parser.add_argument("--out-stamp", required=True)
    return parser.parse_args()


def evaluate_parseable_non_structural_gate(summary: dict) -> tuple[bool, str]:
    key = "parseable_non_structural_missing_count"
    if key not in summary:
        return (
            False,
            f"Missing `{key}` in FAR summary. Re-run rezoning_far_pre_geocode with updated logic.",
        )
    missing_count = summary.get(key)
    try:
        missing_count = int(missing_count)
    except Exception as exc:  # noqa: BLE001
        return (False, f"Could not parse `{key}` as integer: {exc}")

    if missing_count != 0:
        return (
            False,
            "FAR gate failed: "
            f"`{key}` is {missing_count}. Resolve parseable non-structural FAR misses before geocoding.",
        )
    return (True, "FAR gate passed: parseable non-structural FAR complete.")


def main() -> int:
    args = parse_args()
    with open(args.in_summary_json, "r", encoding="utf-8") as handle:
        payload = json.load(handle)

    passed, message = evaluate_parseable_non_structural_gate(payload)
    if not passed:
        raise SystemExit(message)

    stamp_path = Path(args.out_stamp)
    stamp_path.parent.mkdir(parents=True, exist_ok=True)
    stamp_payload = {
        "generated_at_utc": datetime.now(timezone.utc).isoformat(timespec="seconds").replace("+00:00", "Z"),
        "source_summary": str(Path(args.in_summary_json)),
        "status": "pass",
        "message": message,
    }
    stamp_path.write_text(json.dumps(stamp_payload, indent=2), encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
