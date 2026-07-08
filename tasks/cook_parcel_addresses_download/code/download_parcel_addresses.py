import argparse
import os
import shutil
import tempfile
from pathlib import Path
from urllib.parse import quote_plus

import requests


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--dataset-id", default="3723-97qp")
    parser.add_argument("--domain", default="datacatalog.cookcountyil.gov")
    parser.add_argument("--batch-size", type=int, default=500_000)
    parser.add_argument("--order-by", default="pin")
    parser.add_argument("--max-batches", type=int, default=0)
    parser.add_argument("--where-clause")
    parser.add_argument("--request-timeout-seconds", type=int, default=180)
    parser.add_argument("--min-expected-rows", type=int, default=1_000_000)
    parser.add_argument("--out-csv", required=True)
    return parser.parse_args()


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def base_resource_url(domain: str, dataset_id: str) -> str:
    return f"https://{domain}/resource/{dataset_id}"


def count_csv_rows(path: str) -> int:
    with open(path, "r", encoding="utf-8", errors="replace", newline="") as handle:
        line_count = sum(1 for _ in handle)
    return max(0, line_count - 1)


def fetch_json(url: str, timeout: int) -> dict:
    response = requests.get(url, timeout=timeout)
    response.raise_for_status()
    return response.json()


def query_row_count_with_where(domain: str, dataset_id: str, timeout: int, where_clause: str | None) -> int | None:
    select = quote_plus("count(1) as n")
    url = f"{base_resource_url(domain, dataset_id)}.json?$select={select}"
    if where_clause:
        url += f"&$where={quote_plus(where_clause)}"
    try:
        payload = fetch_json(url, timeout=timeout)
        if isinstance(payload, list) and payload:
            value = payload[0].get("n")
            if value is not None:
                return int(value)
    except Exception:  # noqa: BLE001
        return None
    return None


def download_to_file(url: str, out_path: str, timeout: int) -> None:
    with requests.get(url, timeout=timeout, stream=True) as response:
        response.raise_for_status()
        ensure_parent(out_path)
        with open(out_path, "wb") as handle:
            for chunk in response.iter_content(chunk_size=1024 * 1024):
                if chunk:
                    handle.write(chunk)


def append_without_header(src_path: str, dst_path: str) -> int:
    rows_written = 0
    with open(src_path, "r", encoding="utf-8", errors="replace", newline="") as src:
        with open(dst_path, "a", encoding="utf-8", newline="") as dst:
            for idx, line in enumerate(src):
                if idx == 0:
                    continue
                dst.write(line)
                rows_written += 1
    return rows_written


def build_batch_url(
    domain: str,
    dataset_id: str,
    batch_size: int,
    offset: int,
    order_by: str,
    where_clause: str | None,
) -> str:
    order_clause = quote_plus(order_by) if order_by else quote_plus(":id")
    url = (
        f"{base_resource_url(domain, dataset_id)}.csv"
        f"?$limit={batch_size}&$offset={offset}&$order={order_clause}"
    )
    if where_clause:
        url += f"&$where={quote_plus(where_clause)}"
    return url


def download_paginated(
    domain: str,
    dataset_id: str,
    batch_size: int,
    order_by: str,
    timeout: int,
    max_batches: int,
    where_clause: str | None,
    out_path: str,
) -> dict:
    ensure_parent(out_path)
    if os.path.exists(out_path):
        os.remove(out_path)

    attempts: list[dict] = []
    total_rows = 0
    offset = 0
    batch_num = 0

    with tempfile.TemporaryDirectory() as tmpdir:
        while True:
            if max_batches > 0 and batch_num >= max_batches:
                break

            url = build_batch_url(
                domain=domain,
                dataset_id=dataset_id,
                batch_size=batch_size,
                offset=offset,
                order_by=order_by,
                where_clause=where_clause,
            )
            batch_path = os.path.join(tmpdir, f"batch_{batch_num:05d}.csv")

            try:
                download_to_file(url=url, out_path=batch_path, timeout=timeout)
                batch_rows = count_csv_rows(batch_path)
                attempts.append(
                    {
                        "batch": batch_num,
                        "offset": offset,
                        "url": url,
                        "status": "ok",
                        "rows": batch_rows,
                    }
                )
            except Exception as exc:  # noqa: BLE001
                attempts.append(
                    {
                        "batch": batch_num,
                        "offset": offset,
                        "url": url,
                        "status": "error",
                        "error": str(exc),
                    }
                )
                break

            if batch_rows <= 0:
                break

            if batch_num == 0:
                with open(batch_path, "r", encoding="utf-8", errors="replace", newline="") as src:
                    with open(out_path, "w", encoding="utf-8", newline="") as dst:
                        shutil.copyfileobj(src, dst)
                total_rows += batch_rows
            else:
                total_rows += append_without_header(src_path=batch_path, dst_path=out_path)

            if batch_rows < batch_size:
                break

            batch_num += 1
            offset += batch_size

    return {
        "strategy": "paginated_api",
        "rows": total_rows,
        "batches_attempted": len(attempts),
        "batch_log": attempts,
        "path": out_path,
    }


def main() -> int:
    args = parse_args()

    ensure_parent(args.out_csv)
    expected_rows = query_row_count_with_where(
        domain=args.domain,
        dataset_id=args.dataset_id,
        timeout=args.request_timeout_seconds,
        where_clause=args.where_clause,
    )

    with tempfile.TemporaryDirectory() as tmpdir:
        paginated_tmp = os.path.join(tmpdir, "parcel_addresses_paginated.csv")
        result = download_paginated(
            domain=args.domain,
            dataset_id=args.dataset_id,
            batch_size=args.batch_size,
            order_by=args.order_by,
            timeout=args.request_timeout_seconds,
            max_batches=args.max_batches,
            where_clause=args.where_clause,
            out_path=paginated_tmp,
        )

        downloaded_rows = int(result.get("rows", 0))
        if downloaded_rows <= 0:
            print("Download failed: no rows returned by paginated API.")
            return 1

        if expected_rows is not None and downloaded_rows < expected_rows:
            print(
                "Download failed: paginated row count below Socrata count query "
                f"({downloaded_rows} < {expected_rows})."
            )
            return 1

        if downloaded_rows < args.min_expected_rows:
            print(
                "Download failed: paginated row count below minimum expected rows "
                f"({downloaded_rows} < {args.min_expected_rows})."
            )
            return 1

        os.replace(paginated_tmp, args.out_csv)

    print(f"Wrote {downloaded_rows} rows to {args.out_csv} via paginated API")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
