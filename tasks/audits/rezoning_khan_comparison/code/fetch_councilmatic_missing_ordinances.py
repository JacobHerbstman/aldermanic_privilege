#!/usr/bin/env python3
import subprocess
import tempfile
from pathlib import Path

import pandas as pd
import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry


ELMS_RECORD_URL = "https://api.chicityclerkelms.chicago.gov/matter/recordNumber"


def main() -> int:
    corrections = pd.read_csv("../input/khan_sample_corrections.csv", dtype=str)
    pd_transitions = pd.read_csv("../input/khan_pd_transition_corrections.csv", dtype=str)
    zoning = pd.read_csv(
        "../output/khan_zoning_matters_far_20101101_20160831.csv",
        low_memory=False,
    )
    matter_ids = corrections.loc[corrections["action"].eq("include"), "matter_id"].tolist()
    matter_ids += pd_transitions.loc[
        pd_transitions["corrected_to_code"].eq("PD"), "matter_id"
    ].tolist()
    final_status = zoning["matter_status_name"].astype("string").str.upper().eq("90-FINAL")
    suspect_destination = (
        zoning["to_code"].isna()
        | zoning["to_code"].eq("T")
        | zoning["matter_id"].eq("SO2014-2351")
    )
    exclude_ids = set(corrections.loc[corrections["action"].eq("exclude"), "matter_id"])
    matter_ids += zoning.loc[
        final_status & suspect_destination & ~zoning["matter_id"].isin(exclude_ids),
        "matter_id",
    ].tolist()
    matter_ids = list(dict.fromkeys(matter_ids))

    session = requests.Session()
    session.mount(
        "https://",
        HTTPAdapter(
            max_retries=Retry(
                total=4,
                backoff_factor=1,
                status_forcelist=[429, 500, 502, 503, 504],
                allowed_methods=["GET"],
            )
        ),
    )
    rows = []
    with tempfile.TemporaryDirectory() as temp_dir:
        for matter_id in matter_ids:
            record_numbers = [f"S{matter_id}", matter_id]
            detail = None
            for record_number in record_numbers:
                response = session.get(f"{ELMS_RECORD_URL}/{record_number}", timeout=120)
                if response.status_code == 200 and response.content:
                    detail = response.json()
                    break
            if detail is None:
                rows.append({"matter_id": matter_id, "retrieval_status": "missing_elms_record"})
                continue

            candidates = []
            for attachment_number, attachment in enumerate(detail.get("attachments", []), start=1):
                pdf_url = attachment.get("path")
                if not pdf_url:
                    continue
                pdf_response = session.get(pdf_url, timeout=120)
                if pdf_response.status_code >= 400:
                    continue
                pdf_path = Path(temp_dir) / f"{matter_id}_{attachment_number}.pdf"
                pdf_path.write_bytes(pdf_response.content)
                result = subprocess.run(
                    ["pdftotext", "-layout", str(pdf_path), "-"],
                    check=False,
                    capture_output=True,
                    text=True,
                )
                if result.returncode == 0 and result.stdout.strip():
                    candidates.append(
                        {
                            "version_note": attachment.get("fileName", ""),
                            "pdf_url": pdf_url,
                            "ordinance_text": result.stdout,
                        }
                    )

            if not candidates:
                rows.append(
                    {
                        "matter_id": matter_id,
                        "resolved_record_number": detail.get("recordNumber", ""),
                        "matter_title": detail.get("title", ""),
                        "retrieval_status": "no_pdf_text",
                    }
                )
                continue

            candidates.sort(
                key=lambda row: (
                    str(row["version_note"]).upper().startswith("SO"),
                    len(row["ordinance_text"]),
                ),
                reverse=True,
            )
            chosen = candidates[0]
            rows.append(
                {
                    "matter_id": matter_id,
                    "resolved_record_number": detail.get("recordNumber", ""),
                    "matter_title": detail.get("title", ""),
                    "matter_status_name": detail.get("status", ""),
                    "matter_intro_date": detail.get("introductionDate", ""),
                    "matter_passed_date": detail.get("finalActionDate", ""),
                    "version_note": chosen["version_note"],
                    "pdf_url": chosen["pdf_url"],
                    "ordinance_text_chars": len(chosen["ordinance_text"]),
                    "ordinance_text": chosen["ordinance_text"],
                    "retrieval_status": "ok",
                }
            )

    pd.DataFrame(rows).to_csv("../output/councilmatic_missing_ordinance_text.csv", index=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
