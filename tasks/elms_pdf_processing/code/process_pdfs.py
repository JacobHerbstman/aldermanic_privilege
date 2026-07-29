import argparse
import hashlib
import io
import signal
import time
from pathlib import Path

import pandas as pd
import requests
from pypdf import PdfReader
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

from pdf_parsers import parse_from_to_zoning_detailed, parse_map_ref

MAX_PARSE_PAGES = 25
OCR_MAX_PAGES = 6
OCR_MIN_TEXT_CHARS = 32

try:
    import fitz  # type: ignore

    FITZ_AVAILABLE = True
except Exception:  # noqa: BLE001
    FITZ_AVAILABLE = False

try:
    import pytesseract  # type: ignore
    from PIL import Image  # type: ignore

    OCR_AVAILABLE = FITZ_AVAILABLE
except Exception:  # noqa: BLE001
    OCR_AVAILABLE = False


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-attachments-csv", required=True)
    parser.add_argument("--in-candidates-csv", required=True)
    parser.add_argument("--pdf-dir", required=True)
    parser.add_argument("--sleep-seconds", type=float, default=0.2)
    parser.add_argument("--max-retries", type=int, default=4)
    parser.add_argument("--max-attachments-per-matter", type=int, default=1)
    parser.add_argument("--request-timeout-seconds", type=int, default=120)
    parser.add_argument("--parse-timeout-seconds", type=int, default=180)
    parser.add_argument("--progress-every", type=int, default=0)
    parser.add_argument("--out-text-csv", required=True)
    parser.add_argument("--out-fields-csv", required=True)
    return parser.parse_args()


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def make_session(max_retries: int) -> requests.Session:
    session = requests.Session()
    retry = Retry(
        total=max_retries,
        connect=max_retries,
        read=max_retries,
        status=max_retries,
        status_forcelist=[429, 500, 502, 503, 504],
        backoff_factor=0.5,
        allowed_methods=["GET"],
    )
    adapter = HTTPAdapter(max_retries=retry)
    session.mount("https://", adapter)
    session.mount("http://", adapter)
    return session


def choose_column(df: pd.DataFrame, options: list[str]) -> str | None:
    for col in options:
        if col in df.columns:
            return col
    return None


def normalize_matter_id(value) -> str | None:
    if value is None or pd.isna(value):
        return None
    text = str(value).strip()
    if not text:
        return None
    return text


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


def to_bool_series(series: pd.Series) -> pd.Series:
    return series.map(parse_bool)


def normalize_attachments(attachments: pd.DataFrame) -> pd.DataFrame:
    out = attachments.copy()

    matter_col = choose_column(out, ["matter_id", "MatterId"])
    attach_id_col = choose_column(out, ["attachment_id", "MatterAttachmentId", "AttachmentId", "Id"])
    attach_name_col = choose_column(out, ["attachment_name", "MatterAttachmentName", "AttachmentName", "Name"])
    attach_url_col = choose_column(
        out,
        ["attachment_url", "MatterAttachmentHyperlink", "AttachmentHyperlink", "Hyperlink", "Url"],
    )
    attach_guid_col = choose_column(out, ["attachment_guid", "MatterAttachmentGuid", "AttachmentGuid", "Guid"])
    attach_type_col = choose_column(out, ["attachment_type", "AttachmentType", "MatterAttachmentType"])

    if matter_col is None:
        out["matter_id"] = pd.Series(dtype="string")
    else:
        out["matter_id"] = out[matter_col].map(normalize_matter_id).astype("string")

    if attach_id_col is None:
        out["attachment_id"] = pd.Series(dtype="string")
    else:
        out["attachment_id"] = out[attach_id_col].astype("string")

    out["attachment_name"] = out[attach_name_col].astype("string") if attach_name_col else None
    out["attachment_url"] = out[attach_url_col].astype("string") if attach_url_col else None
    out["attachment_guid"] = out[attach_guid_col].astype("string") if attach_guid_col else None
    out["attachment_type"] = out[attach_type_col].astype("string") if attach_type_col else None

    keep = ["matter_id", "attachment_id", "attachment_name", "attachment_url", "attachment_guid", "attachment_type"]
    return out[keep].drop_duplicates().reset_index(drop=True)


def normalize_candidates(candidates: pd.DataFrame) -> set[str]:
    temp = candidates.copy()
    if "matter_id" not in temp.columns and "MatterId" in temp.columns:
        temp["matter_id"] = temp["MatterId"]

    temp["matter_id"] = temp["matter_id"].map(normalize_matter_id)

    if "is_final_candidate" in temp.columns:
        temp = temp[to_bool_series(temp["is_final_candidate"])]

    return set(temp["matter_id"].dropna().tolist())


def build_download_urls(attachment_url: str | None, attachment_id: str | None, attachment_guid: str | None) -> list[str]:
    urls: list[str] = []

    if attachment_url:
        urls.append(str(attachment_url))

    aid = str(attachment_id) if attachment_id else None
    guid = str(attachment_guid) if attachment_guid else None
    if aid and guid:
        fallback = f"https://chicago.legistar.com/View.ashx?M=F&ID={aid}&GUID={guid}"
        if fallback not in urls:
            urls.append(fallback)

    return urls


def attachment_priority(row: pd.Series) -> tuple[int, int]:
    atype = str(row.get("attachment_type") or "").strip().lower()
    aname = str(row.get("attachment_name") or "").strip().lower()

    if "legislation" in atype or "ordinance" in aname:
        return (0, len(aname))
    if "ordinance" in atype:
        return (1, len(aname))
    if "application" in atype or "application" in aname:
        return (3, len(aname))
    return (2, len(aname))


def select_candidate_attachments_per_matter(attachments: pd.DataFrame, max_per_matter: int) -> pd.DataFrame:
    if attachments.empty:
        return attachments.copy()

    keep_n = max(1, int(max_per_matter))
    temp = attachments.copy()
    temp["priority"] = temp.apply(attachment_priority, axis=1)
    temp["priority_rank"] = temp["priority"].map(lambda x: x[0])
    temp["name_len_rank"] = temp["priority"].map(lambda x: -x[1])
    temp = temp.sort_values(
        ["matter_id", "priority_rank", "name_len_rank", "attachment_id"],
        na_position="last",
    )
    out = temp.groupby("matter_id", as_index=False).head(keep_n).reset_index(drop=True)
    return out.drop(columns=["priority", "priority_rank", "name_len_rank"], errors="ignore")


def sanitize_filename(value: str) -> str:
    keep = []
    for ch in value:
        if ch.isalnum() or ch in {"-", "_", "."}:
            keep.append(ch)
        else:
            keep.append("_")
    return "".join(keep)


def sha256_file(path: Path) -> str:
    hasher = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            hasher.update(chunk)
    return hasher.hexdigest()


def is_pdf_bytes(content: bytes) -> bool:
    return content.lstrip().startswith(b"%PDF")


def is_pdf_file(path: Path) -> bool:
    if not path.exists() or path.stat().st_size == 0:
        return False
    with path.open("rb") as handle:
        header = handle.read(32)
    return is_pdf_bytes(header)


def extract_text_ocr(path: Path) -> tuple[list[dict], str]:
    if not OCR_AVAILABLE:
        return [], ""
    rows = []
    pieces = []
    doc = fitz.open(str(path))
    for page_idx in range(min(OCR_MAX_PAGES, len(doc))):
        page = doc.load_page(page_idx)
        # 2x scale keeps OCR readable while controlling runtime.
        pix = page.get_pixmap(matrix=fitz.Matrix(2.0, 2.0), alpha=False)
        image = Image.open(io.BytesIO(pix.tobytes("png")))
        page_text = pytesseract.image_to_string(image, config="--psm 6") or ""
        rows.append({"page_number": page_idx + 1, "page_text": page_text})
        pieces.append(page_text)
    return rows, "\n".join(pieces)


def extract_text_fitz(path: Path) -> tuple[list[dict], str]:
    if not FITZ_AVAILABLE:
        return [], ""
    rows = []
    pieces = []
    doc = fitz.open(str(path))
    for page_idx in range(min(MAX_PARSE_PAGES, len(doc))):
        page_text = doc.load_page(page_idx).get_text() or ""
        rows.append({"page_number": page_idx + 1, "page_text": page_text})
        pieces.append(page_text)
    return rows, "\n".join(pieces)


def extract_text(path: Path) -> tuple[list[dict], str]:
    fitz_rows, fitz_text = extract_text_fitz(path)
    if len(fitz_text.strip()) >= OCR_MIN_TEXT_CHARS:
        return fitz_rows, fitz_text

    reader = PdfReader(str(path), strict=False)
    rows = []
    pieces = []
    for page_idx, page in enumerate(reader.pages[:MAX_PARSE_PAGES], start=1):
        page_text = page.extract_text() or ""
        rows.append({"page_number": page_idx, "page_text": page_text})
        pieces.append(page_text)
    full_text = "\n".join(pieces)
    if len(full_text.strip()) >= OCR_MIN_TEXT_CHARS or not OCR_AVAILABLE:
        return rows, full_text

    try:
        ocr_rows, ocr_text = extract_text_ocr(path)
    except Exception:  # noqa: BLE001
        return rows, full_text

    if len(ocr_text.strip()) > max(len(full_text.strip()), len(fitz_text.strip())):
        return ocr_rows, ocr_text
    if len(fitz_text.strip()) > len(full_text.strip()):
        return fitz_rows, fitz_text
    return rows, full_text


class ParseTimeoutError(TimeoutError):
    pass


def _raise_parse_timeout(signum, frame) -> None:
    raise ParseTimeoutError("PDF text extraction timed out")


def extract_text_with_timeout(path: Path, timeout_seconds: int) -> tuple[list[dict], str]:
    if timeout_seconds <= 0:
        return extract_text(path)

    previous_handler = signal.signal(signal.SIGALRM, _raise_parse_timeout)
    signal.alarm(timeout_seconds)
    try:
        return extract_text(path)
    finally:
        signal.alarm(0)
        signal.signal(signal.SIGALRM, previous_handler)


def sha256_file_with_timeout(path: Path, timeout_seconds: int) -> str:
    if timeout_seconds <= 0:
        return sha256_file(path)

    previous_handler = signal.signal(signal.SIGALRM, _raise_parse_timeout)
    signal.alarm(timeout_seconds)
    try:
        return sha256_file(path)
    finally:
        signal.alarm(0)
        signal.signal(signal.SIGALRM, previous_handler)


def main() -> int:
    args = parse_args()
    ensure_parent(args.out_text_csv)
    ensure_parent(args.out_fields_csv)

    pdf_dir = Path(args.pdf_dir)
    pdf_dir.mkdir(parents=True, exist_ok=True)

    attachments = normalize_attachments(pd.read_csv(args.in_attachments_csv, dtype=str, low_memory=False))
    candidate_ids = normalize_candidates(pd.read_csv(args.in_candidates_csv, dtype=str, low_memory=False))

    if candidate_ids:
        attachments = attachments[attachments["matter_id"].isin(candidate_ids)].copy()
    else:
        attachments = attachments.iloc[0:0].copy()

    candidate_attachment_rows_before_selection = int(len(attachments))
    attachments = select_candidate_attachments_per_matter(
        attachments, max_per_matter=args.max_attachments_per_matter
    )
    attachments = attachments.reset_index(drop=True)

    session = make_session(args.max_retries)

    manifest_rows = []
    text_rows = []
    fields_rows = []

    for idx, row in attachments.iterrows():
        matter_id = row["matter_id"]
        attachment_id = row["attachment_id"]
        if args.progress_every > 0 and (idx + 1 == 1 or (idx + 1) % args.progress_every == 0):
            print(f"processing_pdf {idx + 1}/{len(attachments)} {matter_id} {attachment_id}", flush=True)
        attachment_url = row["attachment_url"] if pd.notna(row["attachment_url"]) else None
        attachment_name = row["attachment_name"] if pd.notna(row["attachment_name"]) else None
        attachment_guid = row["attachment_guid"] if pd.notna(row["attachment_guid"]) else None
        download_urls = build_download_urls(attachment_url, attachment_id, attachment_guid)

        file_key = f"matter_{matter_id}_attachment_{attachment_id or idx + 1}.pdf"
        file_name = sanitize_filename(file_key)
        file_path = pdf_dir / file_name

        download_status = "not_attempted"
        parse_status = "not_attempted"
        error_message = None
        content_type = None
        file_size_bytes = None
        file_hash = None
        attachment_url_used = None
        text_chars = 0
        from_zoning = None
        to_zoning = None
        from_zoning_raw = None
        to_zoning_raw = None
        from_zoning_canonical = None
        to_zoning_canonical = None
        zoning_parse_method = None
        zoning_parse_quality = 0
        map_ref = None

        if not download_urls:
            download_status = "missing_url"
            parse_status = "skipped"
            error_message = "Attachment URL missing"
        else:
            try:
                if file_path.exists() and file_path.stat().st_size > 0:
                    if is_pdf_file(file_path):
                        download_status = "exists"
                        attachment_url_used = download_urls[0]
                    else:
                        file_path.unlink(missing_ok=True)

                if download_status != "exists":
                    last_error = None
                    gone_status = False
                    non_pdf_status = False
                    for candidate_url in download_urls:
                        try:
                            response = session.get(str(candidate_url), timeout=args.request_timeout_seconds)
                            if response.status_code == 401:
                                raise RuntimeError(
                                    "ELMS attachment request returned 401 Unauthorized. A token is required before this pipeline can run."
                                )
                            if response.status_code in (302, 303) and "Confirmation.aspx?M1=Gone" in str(
                                response.headers.get("Location", "")
                            ):
                                gone_status = True
                                last_error = "Attachment URL redirected to Gone page."
                                continue

                            response.raise_for_status()
                            content_bytes = response.content
                            if not is_pdf_bytes(content_bytes):
                                non_pdf_status = True
                                last_error = "URL returned non-PDF content."
                                continue
                            content_type = response.headers.get("Content-Type")
                            with file_path.open("wb") as handle:
                                handle.write(content_bytes)
                            download_status = "downloaded"
                            attachment_url_used = str(candidate_url)
                            break
                        except requests.HTTPError as exc:
                            status = exc.response.status_code if exc.response is not None else None
                            if status in (404, 410):
                                gone_status = True
                            last_error = str(exc)
                            continue
                        except Exception as exc:  # noqa: BLE001
                            last_error = str(exc)
                            continue

                    if download_status != "downloaded":
                        if gone_status:
                            download_status = "gone"
                        elif non_pdf_status:
                            download_status = "non_pdf"
                        else:
                            download_status = "download_failed"
                        parse_status = "skipped"
                        error_message = last_error or "All attachment URL attempts failed."

                if download_status in {"downloaded", "exists"}:
                    file_size_bytes = file_path.stat().st_size
                    file_hash = sha256_file_with_timeout(file_path, args.parse_timeout_seconds)

                    page_rows, full_text = extract_text_with_timeout(
                        file_path, args.parse_timeout_seconds
                    )
                    text_chars = len(full_text)
                    for page in page_rows:
                        text_rows.append(
                            {
                                "matter_id": matter_id,
                                "attachment_id": attachment_id,
                                "page_number": page["page_number"],
                                "page_text": page["page_text"],
                            }
                        )

                    if full_text.strip():
                        parse_status = "parsed"
                        detail = parse_from_to_zoning_detailed(full_text)
                        from_zoning = detail["from_zoning_clean"]
                        to_zoning = detail["to_zoning_clean"]
                        from_zoning_raw = detail["from_zoning_raw"]
                        to_zoning_raw = detail["to_zoning_raw"]
                        from_zoning_canonical = detail["from_zoning_canonical"]
                        to_zoning_canonical = detail["to_zoning_canonical"]
                        zoning_parse_method = detail["parse_method"]
                        zoning_parse_quality = detail["parse_quality"]
                        map_ref = parse_map_ref(full_text)
                    else:
                        parse_status = "no_text"

            except Exception as exc:  # noqa: BLE001
                download_status = "download_failed" if download_status == "not_attempted" else download_status
                parse_status = "parse_failed"
                error_message = str(exc)

        manifest_rows.append(
            {
                "matter_id": matter_id,
                "attachment_id": attachment_id,
                "attachment_name": attachment_name,
                "attachment_url": attachment_url,
                "attachment_guid": attachment_guid,
                "attachment_url_used": attachment_url_used,
                "local_pdf_path": str(file_path),
                "download_status": download_status,
                "parse_status": parse_status,
                "file_size_bytes": file_size_bytes,
                "content_type": content_type,
                "file_hash": file_hash,
                "text_chars": text_chars,
                "error_message": error_message,
            }
        )

        fields_rows.append(
            {
                "matter_id": matter_id,
                "attachment_id": attachment_id,
                "attachment_url": attachment_url,
                "attachment_guid": attachment_guid,
                "attachment_url_used": attachment_url_used,
                "local_pdf_path": str(file_path),
                "download_status": download_status,
                "parse_status": parse_status,
                "file_hash": file_hash,
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

        if args.sleep_seconds > 0:
            time.sleep(args.sleep_seconds)

    manifest = pd.DataFrame(manifest_rows)
    text_df = pd.DataFrame(text_rows)
    fields = pd.DataFrame(fields_rows)

    if manifest.empty:
        manifest = pd.DataFrame(
            columns=[
                "matter_id",
                "attachment_id",
                "attachment_name",
                "attachment_url",
                "attachment_guid",
                "attachment_url_used",
                "local_pdf_path",
                "download_status",
                "parse_status",
                "file_size_bytes",
                "content_type",
                "file_hash",
                "text_chars",
                "error_message",
            ]
        )

    if text_df.empty:
        text_df = pd.DataFrame(columns=["matter_id", "attachment_id", "page_number", "page_text"])

    if fields.empty:
        fields = pd.DataFrame(
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
            ]
        )

    text_df.to_csv(args.out_text_csv, index=False)
    fields.to_csv(args.out_fields_csv, index=False)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
