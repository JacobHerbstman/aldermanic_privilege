import unittest

import pandas as pd

from process_pdfs import normalize_attachments, normalize_candidates


class ProcessPdfsTests(unittest.TestCase):
    def test_normalize_attachments_keeps_string_matter_id(self) -> None:
        df = pd.DataFrame(
            [
                {
                    "matter_id": "SO2011-2192",
                    "attachment_id": "elms_abc",
                    "attachment_name": "Ord.pdf",
                    "attachment_url": "https://example.test/ord.pdf",
                }
            ]
        )
        out = normalize_attachments(df)
        self.assertEqual(out["matter_id"].iloc[0], "SO2011-2192")

    def test_normalize_candidates_returns_string_set(self) -> None:
        df = pd.DataFrame(
            [
                {"matter_id": "SO2011-2192", "is_final_candidate": "true"},
                {"matter_id": "O2011-1369", "is_final_candidate": "false"},
            ]
        )
        out = normalize_candidates(df)
        self.assertEqual(out, {"SO2011-2192"})


if __name__ == "__main__":
    unittest.main()
