import unittest

import pandas as pd

from parse_pdf_text_fields import build_field_rows


class TestParsePdfTextFields(unittest.TestCase):
    def test_build_field_rows_parses_grouped_pages(self) -> None:
        pdf_text = pd.DataFrame(
            [
                {
                    "matter_id": "O2011-1",
                    "attachment_id": "a1",
                    "page_number": "2",
                    "page_text": "to those of a B3-2 Community Shopping District. Section 2.",
                },
                {
                    "matter_id": "O2011-1",
                    "attachment_id": "a1",
                    "page_number": "1",
                    "page_text": (
                        "Section 1. The Chicago Zoning Ordinance is amended by changing all of "
                        "the RS-3 Residential Single-Unit District symbols and indications "
                    ),
                },
            ]
        )

        rows = build_field_rows(pdf_text, parse_timeout_seconds=10)

        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]["matter_id"], "O2011-1")
        self.assertEqual(rows[0]["parse_status"], "parsed")
        self.assertEqual(rows[0]["from_zoning_canonical"], "RS-3")
        self.assertEqual(rows[0]["to_zoning_canonical"], "B3-2")
        self.assertEqual(rows[0]["download_status"], "from_pdf_text")

    def test_build_field_rows_handles_empty_text(self) -> None:
        pdf_text = pd.DataFrame(
            [
                {
                    "matter_id": "O2011-2",
                    "attachment_id": "a2",
                    "page_number": "1",
                    "page_text": "",
                }
            ]
        )

        rows = build_field_rows(pdf_text, parse_timeout_seconds=10)

        self.assertEqual(rows[0]["parse_status"], "no_text")
        self.assertEqual(rows[0]["zoning_parse_quality"], 0)


if __name__ == "__main__":
    unittest.main()
