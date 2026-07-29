import argparse
import hashlib
import json
import time
from datetime import datetime, timezone
from pathlib import Path

import fitz
import pandas as pd
import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

from journal_parsers import normalize_spaces, parse_zoning_section_page, status_from_outcome


FINAL_COLUMNS = [
    "year",
    "year_filter_value",
    "meeting_date",
    "meeting_type_raw",
    "meeting_type_norm",
    "document_title",
    "pdf_url",
    "filename",
    "rel_local_path",
    "downloaded",
    "download_status",
    "http_status",
    "file_size_bytes",
    "server_content_length",
    "file_sha256",
    "is_valid_pdf",
    "page_count",
    "has_selectable_text",
    "toc_text",
    "zoning_section_page",
    "error_message",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-manifest-csv", required=True)
    parser.add_argument("--pdf-root-dir", required=True)
    parser.add_argument("--sleep-seconds", type=float, default=2.5)
    parser.add_argument("--timeout-seconds", type=float, default=120.0)
    parser.add_argument("--max-retries", type=int, default=1)
    parser.add_argument("--out-manifest-csv", required=True)
    parser.add_argument("--out-download-report-json")
    return parser.parse_args()


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def make_session(max_retries: int) -> requests.Session:
    retry = Retry(
        total=max_retries,
        connect=max_retries,
        read=max_retries,
        status=max_retries,
        backoff_factor=0.5,
        status_forcelist=[429, 500, 502, 503, 504],
        allowed_methods=["GET", "HEAD"],
    )
    adapter = HTTPAdapter(max_retries=retry)
    session = requests.Session()
    session.mount("https://", adapter)
    session.mount("http://", adapter)
    return session


def has_pdf_header(path: Path) -> bool:
    if not path.exists() or path.stat().st_size == 0:
        return False
    with path.open("rb") as handle:
        return handle.read(32).lstrip().startswith(b"%PDF")


def sha256_file(path: Path) -> str:
    hasher = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            hasher.update(chunk)
    return hasher.hexdigest()


def content_length_from_head(
    session: requests.Session,
    url: str,
    timeout_seconds: float,
) -> tuple[int | None, int | None, str | None]:
    try:
        response = session.head(url, allow_redirects=True, timeout=timeout_seconds)
        status = int(response.status_code)
        if status >= 400:
            return None, status, f"HEAD returned HTTP {status}"

        raw_length = response.headers.get("Content-Length")
        if raw_length is None:
            return None, status, None

        try:
            return int(raw_length), status, None
        except ValueError:
            return None, status, None
    except requests.Timeout:
        return None, None, "timeout"
    except Exception as exc:  # noqa: BLE001
        return None, None, str(exc)


def stream_download(
    session: requests.Session,
    url: str,
    local_path: Path,
    timeout_seconds: float,
) -> tuple[int | None, str | None]:
    tmp_path = local_path.with_suffix(local_path.suffix + ".part")
    try:
        with session.get(url, stream=True, allow_redirects=True, timeout=timeout_seconds) as response:
            status = int(response.status_code)
            if status >= 400:
                return status, f"GET returned HTTP {status}"

            tmp_path.parent.mkdir(parents=True, exist_ok=True)
            with tmp_path.open("wb") as handle:
                for chunk in response.iter_content(chunk_size=1024 * 1024):
                    if chunk:
                        handle.write(chunk)
        tmp_path.replace(local_path)
        return status, None
    except requests.Timeout:
        return None, "timeout"
    except Exception as exc:  # noqa: BLE001
        return None, str(exc)
    finally:
        if tmp_path.exists():
            tmp_path.unlink(missing_ok=True)


def extract_profile(path: Path) -> dict:
    profile = {
        "is_valid_pdf": False,
        "page_count": None,
        "has_selectable_text": None,
        "toc_text": None,
        "zoning_section_page": None,
        "error_message": None,
    }

    if not has_pdf_header(path):
        profile["error_message"] = "File is missing %PDF header."
        return profile

    doc = None
    try:
        doc = fitz.open(str(path))
        page_count = int(doc.page_count)
        profile["is_valid_pdf"] = page_count > 0
        profile["page_count"] = page_count

        sample_pages = min(3, page_count)
        total_chars = 0
        for idx in range(sample_pages):
            text = doc.load_page(idx).get_text("text") or ""
            total_chars += len(normalize_spaces(text))
        has_selectable = total_chars >= 40
        profile["has_selectable_text"] = has_selectable

        if has_selectable and page_count > 1:
            candidates: list[tuple[float, str]] = []
            for idx in [1, 2, 3]:
                if idx >= page_count:
                    continue
                text = (doc.load_page(idx).get_text("text") or "").strip()
                if not text:
                    continue
                lower = text.lower()
                score = 0.0
                if "table of contents" in lower or "contents" in lower:
                    score += 2.0
                if "zoning" in lower:
                    score += 1.0
                score += min(len(text), 4000) / 4000.0
                candidates.append((score, text))

            if candidates:
                toc_text = max(candidates, key=lambda item: item[0])[1]
                profile["toc_text"] = toc_text
                profile["zoning_section_page"] = parse_zoning_section_page(toc_text)
    except Exception as exc:  # noqa: BLE001
        profile["error_message"] = str(exc)
        profile["is_valid_pdf"] = False
    finally:
        if doc is not None:
            doc.close()

    if profile["is_valid_pdf"] and profile["error_message"] is None:
        return profile

    if profile["error_message"] is None:
        profile["error_message"] = "Unable to open PDF."
    return profile


def int_or_none(value) -> int | None:
    if pd.isna(value):
        return None
    try:
        return int(value)
    except Exception:  # noqa: BLE001
        return None


def str_or_none(value) -> str | None:
    if value is None:
        return None
    if pd.isna(value):
        return None
    text = str(value).strip()
    return text if text else None


def ensure_manifest_columns(manifest: pd.DataFrame) -> pd.DataFrame:
    out = manifest.copy()
    needed = [
        "year",
        "year_filter_value",
        "meeting_date",
        "meeting_type_raw",
        "meeting_type_norm",
        "document_title",
        "pdf_url",
        "filename",
        "rel_local_path",
    ]
    for col in needed:
        if col not in out.columns:
            out[col] = None

    rel_values = out["rel_local_path"].astype("string")
    missing_rel = rel_values.isna() | rel_values.str.strip().eq("")
    if missing_rel.any():
        year_part = out["year"].fillna(-1).astype(int).astype(str)
        file_part = out["filename"].fillna("").astype(str)
        out.loc[missing_rel, "rel_local_path"] = year_part.loc[missing_rel] + "/" + file_part.loc[missing_rel]

    return out


def build_summary_report(final_manifest: pd.DataFrame) -> dict:
    journals_by_year = (
        final_manifest.groupby("year").size().astype(int).to_dict()
        if not final_manifest.empty
        else {}
    )

    total_size_bytes = int(final_manifest["file_size_bytes"].fillna(0).sum()) if not final_manifest.empty else 0
    bytes_by_year = (
        final_manifest.groupby("year")["file_size_bytes"].sum(min_count=1).fillna(0).astype(int).to_dict()
        if not final_manifest.empty
        else {}
    )
    status_counts = (
        final_manifest["download_status"].value_counts(dropna=False).to_dict()
        if not final_manifest.empty
        else {}
    )

    failures = final_manifest[
        ~final_manifest["download_status"].isin(["downloaded", "exists_valid"])
    ].copy()
    failure_rows = failures[
        ["year", "filename", "pdf_url", "download_status", "http_status", "error_message"]
    ].to_dict(orient="records")

    return {
        "generated_at_utc": datetime.now(timezone.utc).isoformat(timespec="seconds").replace("+00:00", "Z"),
        "total_journals": int(len(final_manifest)),
        "journals_by_year": {str(int(key)): int(value) for key, value in journals_by_year.items()},
        "downloaded_count": int(final_manifest["downloaded"].fillna(False).sum()) if not final_manifest.empty else 0,
        "status_counts": {str(key): int(value) for key, value in status_counts.items()},
        "corrupt_count": int(final_manifest["download_status"].eq("corrupt").sum()) if not final_manifest.empty else 0,
        "total_size_bytes": total_size_bytes,
        "bytes_by_year": {str(int(key)): int(value) for key, value in bytes_by_year.items()},
        "failure_reasons": failure_rows,
    }


def main() -> int:
    args = parse_args()
    ensure_parent(args.out_manifest_csv)
    if args.out_download_report_json:
        ensure_parent(args.out_download_report_json)

    input_manifest = ensure_manifest_columns(pd.read_csv(args.in_manifest_csv, dtype=str, low_memory=False))
    pdf_root = Path(args.pdf_root_dir)
    pdf_root.mkdir(parents=True, exist_ok=True)

    session = make_session(args.max_retries)
    rows: list[dict] = []
    total = len(input_manifest)

    for idx, row in input_manifest.iterrows():
        year = int_or_none(row.get("year"))
        year_filter_value = str_or_none(row.get("year_filter_value"))
        meeting_date = str_or_none(row.get("meeting_date"))
        meeting_type_raw = str_or_none(row.get("meeting_type_raw"))
        meeting_type_norm = str_or_none(row.get("meeting_type_norm"))
        document_title = str_or_none(row.get("document_title"))
        pdf_url = str_or_none(row.get("pdf_url"))
        filename = str_or_none(row.get("filename"))
        rel_local_path = str_or_none(row.get("rel_local_path"))

        if rel_local_path is None and year is not None and filename:
            rel_local_path = f"{year}/{filename}"
        if rel_local_path is None:
            rel_local_path = filename or f"unknown_{idx + 1}.pdf"

        local_path = pdf_root / rel_local_path
        local_path.parent.mkdir(parents=True, exist_ok=True)

        downloaded = False
        download_status = "failed"
        http_status = None
        file_size_bytes = None
        server_content_length = None
        file_sha256 = None
        is_valid_pdf = False
        page_count = None
        has_selectable_text = None
        toc_text = None
        zoning_section_page = None
        error_message = None

        if not pdf_url:
            download_status = status_from_outcome(failed=True)
            error_message = "Missing pdf_url."
        else:
            content_length, head_status, head_error = content_length_from_head(
                session=session,
                url=pdf_url,
                timeout_seconds=args.timeout_seconds,
            )
            server_content_length = content_length
            if head_status in {403, 404}:
                http_status = head_status
            if head_error == "timeout":
                error_message = "timeout"

            if local_path.exists() and local_path.stat().st_size > 0:
                profile = extract_profile(local_path)
                local_size = int(local_path.stat().st_size)
                if profile["is_valid_pdf"] and (
                    server_content_length is None or local_size == server_content_length
                ):
                    download_status = status_from_outcome(exists_valid=True)
                    downloaded = True
                    is_valid_pdf = True
                    file_size_bytes = local_size
                    file_sha256 = sha256_file(local_path)
                    page_count = profile["page_count"]
                    has_selectable_text = profile["has_selectable_text"]
                    toc_text = profile["toc_text"]
                    zoning_section_page = profile["zoning_section_page"]
                    error_message = profile["error_message"]

            if not downloaded:
                get_status, get_error = stream_download(
                    session=session,
                    url=pdf_url,
                    local_path=local_path,
                    timeout_seconds=args.timeout_seconds,
                )
                http_status = get_status if get_status is not None else http_status

                if get_error:
                    timed_out = get_error == "timeout"
                    download_status = status_from_outcome(
                        http_status=http_status,
                        timed_out=timed_out,
                        failed=not timed_out,
                    )
                    error_message = get_error
                else:
                    if not has_pdf_header(local_path):
                        download_status = status_from_outcome(non_pdf=True)
                        error_message = "Downloaded file is not a PDF."
                    else:
                        profile = extract_profile(local_path)
                        if profile["is_valid_pdf"]:
                            download_status = status_from_outcome(downloaded=True)
                            downloaded = True
                            is_valid_pdf = True
                            file_size_bytes = int(local_path.stat().st_size)
                            file_sha256 = sha256_file(local_path)
                            page_count = profile["page_count"]
                            has_selectable_text = profile["has_selectable_text"]
                            toc_text = profile["toc_text"]
                            zoning_section_page = profile["zoning_section_page"]
                            error_message = profile["error_message"]
                        else:
                            download_status = status_from_outcome(corrupt=True)
                            error_message = profile["error_message"]
                            is_valid_pdf = False

        if downloaded and file_size_bytes is None and local_path.exists():
            file_size_bytes = int(local_path.stat().st_size)
        if downloaded and file_sha256 is None and local_path.exists():
            file_sha256 = sha256_file(local_path)

        row_out = {
            "year": year,
            "year_filter_value": year_filter_value,
            "meeting_date": meeting_date,
            "meeting_type_raw": meeting_type_raw,
            "meeting_type_norm": meeting_type_norm,
            "document_title": document_title,
            "pdf_url": pdf_url,
            "filename": filename,
            "rel_local_path": rel_local_path,
            "downloaded": bool(downloaded),
            "download_status": download_status,
            "http_status": http_status,
            "file_size_bytes": file_size_bytes,
            "server_content_length": server_content_length,
            "file_sha256": file_sha256,
            "is_valid_pdf": bool(is_valid_pdf),
            "page_count": page_count,
            "has_selectable_text": has_selectable_text,
            "toc_text": toc_text,
            "zoning_section_page": zoning_section_page,
            "error_message": error_message,
        }
        rows.append(row_out)
        print(f"[{idx + 1}/{total}] {rel_local_path} -> {download_status}")

        if args.sleep_seconds > 0:
            time.sleep(args.sleep_seconds)

    final_manifest = pd.DataFrame(rows, columns=FINAL_COLUMNS)
    final_manifest.to_csv(args.out_manifest_csv, index=False)

    if args.out_download_report_json:
        report = build_summary_report(final_manifest)
        with Path(args.out_download_report_json).open("w", encoding="utf-8") as handle:
            json.dump(report, handle, indent=2)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
