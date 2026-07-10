#!/usr/bin/env python3
import hashlib
import io
from pathlib import Path

import fitz
import pandas as pd
import pytesseract
import requests
from PIL import Image
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry


# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_pd_ordinance_far_history/code")

queue = pd.read_csv(
    "../output/post_khan_pd_to_pd_queue_20160901_20201231.csv", dtype=str
)
pd_numbers = sorted(queue["pd_number"].dropna().unique(), key=int)

prior_pages = pd.read_csv("../output/pd_archive_text.csv", dtype=str, low_memory=False)
prior_fields = pd.read_csv("../output/pd_archive_fields.csv", dtype=str)
prior_pd_numbers = set(
    prior_fields.loc[prior_fields["download_status"].eq("downloaded"), "pd_number"]
)

session = requests.Session()
session.mount(
    "https://",
    HTTPAdapter(
        max_retries=Retry(
            total=4,
            connect=4,
            read=4,
            status=4,
            backoff_factor=0.5,
            status_forcelist=[429, 500, 502, 503, 504],
            allowed_methods=["GET"],
        )
    ),
)

field_rows = []
page_rows = []
for pd_number in pd_numbers:
    if pd_number in prior_pd_numbers:
        field_rows.extend(
            prior_fields.loc[prior_fields["pd_number"].eq(pd_number)].to_dict("records")
        )
        page_rows.extend(
            prior_pages.loc[prior_pages["pd_number"].eq(pd_number)].to_dict("records")
        )
        print(f"PD {pd_number}: reused prior audit snapshot")
        continue

    url = f"https://gisapps.chicago.gov/gisimages/zoning_pds/PD{pd_number}.pdf"
    pdf_path = Path(f"../output/pd_archive_pdfs/PD{pd_number}.pdf")
    try:
        response = session.get(url, timeout=180)
        response.raise_for_status()
        content = response.content
        if not content.lstrip().startswith(b"%PDF"):
            raise ValueError("Response is not a PDF")
        pdf_path.write_bytes(content)

        document = fitz.open(stream=content, filetype="pdf")
        ocr_pages = 0
        for page_index in range(len(document)):
            page = document.load_page(page_index)
            page_text = page.get_text("text") or ""
            parse_method = "embedded_text"
            if len(" ".join(page_text.split())) < 32:
                pixmap = page.get_pixmap(matrix=fitz.Matrix(1.5, 1.5), alpha=False)
                image = Image.open(io.BytesIO(pixmap.tobytes("png")))
                page_text = pytesseract.image_to_string(image, config="--psm 6") or ""
                parse_method = "ocr"
                ocr_pages += 1
            page_rows.append(
                {
                    "pd_number": pd_number,
                    "page_number": page_index + 1,
                    "parse_method": parse_method,
                    "page_text": page_text,
                }
            )

        field_rows.append(
            {
                "pd_number": pd_number,
                "archive_url": url,
                "download_status": "downloaded",
                "page_count": len(document),
                "ocr_pages": ocr_pages,
                "file_size_bytes": len(content),
                "file_sha256": hashlib.sha256(content).hexdigest(),
                "error_message": "",
            }
        )
        document.close()
        print(
            f"PD {pd_number}: downloaded {field_rows[-1]['page_count']} pages "
            f"({field_rows[-1]['ocr_pages']} OCR)"
        )
    except Exception as error:
        field_rows.append(
            {
                "pd_number": pd_number,
                "archive_url": url,
                "download_status": "failed",
                "page_count": 0,
                "ocr_pages": 0,
                "file_size_bytes": 0,
                "file_sha256": "",
                "error_message": str(error),
            }
        )
        print(f"PD {pd_number}: failed: {error}")

pd.DataFrame(page_rows).to_csv(
    "../output/post_khan_pd_archive_text_20160901_20201231.csv", index=False
)
pd.DataFrame(field_rows).to_csv(
    "../output/post_khan_pd_archive_fields_20160901_20201231.csv", index=False
)
