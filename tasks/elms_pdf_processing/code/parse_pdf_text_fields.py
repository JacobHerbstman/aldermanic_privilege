import argparse
import signal

import pandas as pd

from pdf_parsers import parse_from_to_zoning_detailed, parse_map_ref


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("date_tag")
    parser.add_argument("--parse-timeout-seconds", type=int, default=180)
    return parser.parse_args()


class ParseTimeoutError(TimeoutError):
    pass


def raise_parse_timeout(signum, frame) -> None:
    raise ParseTimeoutError("PDF text zoning parse timed out")


def parse_with_timeout(text: str, timeout_seconds: int) -> dict:
    if timeout_seconds <= 0:
        return parse_from_to_zoning_detailed(text)

    previous_handler = signal.signal(signal.SIGALRM, raise_parse_timeout)
    signal.alarm(timeout_seconds)
    try:
        return parse_from_to_zoning_detailed(text)
    finally:
        signal.alarm(0)
        signal.signal(signal.SIGALRM, previous_handler)


def build_field_rows(pdf_text: pd.DataFrame, parse_timeout_seconds: int) -> list[dict]:
    if pdf_text.empty:
        return []

    pdf_text = pdf_text.copy()
    pdf_text["page_number"] = pd.to_numeric(pdf_text["page_number"], errors="coerce")
    pdf_text = pdf_text.sort_values(["matter_id", "attachment_id", "page_number"], na_position="last")

    rows = []
    for (matter_id, attachment_id), group in pdf_text.groupby(["matter_id", "attachment_id"], dropna=False):
        full_text = "\n".join([text for text in group["page_text"].fillna("") if text])
        text_chars = len(full_text)
        from_zoning = None
        to_zoning = None
        from_zoning_raw = None
        to_zoning_raw = None
        from_zoning_canonical = None
        to_zoning_canonical = None
        zoning_parse_method = None
        zoning_parse_quality = 0
        map_ref = None
        parse_status = "no_text"
        error_message = None

        if full_text.strip():
            try:
                detail = parse_with_timeout(full_text, parse_timeout_seconds)
                from_zoning = detail["from_zoning_clean"]
                to_zoning = detail["to_zoning_clean"]
                from_zoning_raw = detail["from_zoning_raw"]
                to_zoning_raw = detail["to_zoning_raw"]
                from_zoning_canonical = detail["from_zoning_canonical"]
                to_zoning_canonical = detail["to_zoning_canonical"]
                zoning_parse_method = detail["parse_method"]
                zoning_parse_quality = detail["parse_quality"]
                map_ref = parse_map_ref(full_text)
                parse_status = "parsed"
            except Exception as exc:  # noqa: BLE001
                parse_status = "parse_failed"
                error_message = str(exc)

        rows.append(
            {
                "matter_id": matter_id,
                "attachment_id": attachment_id,
                "attachment_url": None,
                "attachment_guid": None,
                "attachment_url_used": None,
                "local_pdf_path": None,
                "download_status": "from_pdf_text",
                "parse_status": parse_status,
                "file_hash": None,
                "text_chars": text_chars,
                "from_zoning": from_zoning,
                "to_zoning": to_zoning,
                "from_zoning_raw": from_zoning_raw,
                "to_zoning_raw": to_zoning_raw,
                "from_zoning_canonical": from_zoning_canonical,
                "to_zoning_canonical": to_zoning_canonical,
                "zoning_parse_method": zoning_parse_method,
                "zoning_parse_quality": zoning_parse_quality,
                "map_ref": map_ref,
                "error_message": error_message,
            }
        )
    return rows


def main() -> int:
    args = parse_args()

    pdf_text = pd.read_csv(f"../output/pdf_text_{args.date_tag}.csv", dtype=str, low_memory=False)
    rows = build_field_rows(pdf_text, args.parse_timeout_seconds)
    pd.DataFrame(
        rows,
        columns=[
            "matter_id",
            "attachment_id",
            "attachment_url",
            "attachment_guid",
            "attachment_url_used",
            "local_pdf_path",
            "download_status",
            "parse_status",
            "file_hash",
            "text_chars",
            "from_zoning",
            "to_zoning",
            "from_zoning_raw",
            "to_zoning_raw",
            "from_zoning_canonical",
            "to_zoning_canonical",
            "zoning_parse_method",
            "zoning_parse_quality",
            "map_ref",
            "error_message",
        ],
    ).to_csv(f"../output/pdf_zoning_fields_{args.date_tag}.csv", index=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
