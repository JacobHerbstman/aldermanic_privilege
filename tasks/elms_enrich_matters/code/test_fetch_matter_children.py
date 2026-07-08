import unittest

import pandas as pd

from fetch_matter_children import (
    build_candidates,
    extract_sponsor_ward,
    normalize_attachment,
)


class FetchMatterChildrenTests(unittest.TestCase):
    def test_extract_sponsor_ward_from_office(self) -> None:
        self.assertEqual(extract_sponsor_ward("Alderman, 44th Ward", None), "44")
        self.assertIsNone(extract_sponsor_ward("City Clerk", "Jane Doe"))

    def test_build_candidates_keeps_string_matter_id(self) -> None:
        seed = pd.DataFrame(
            [
                {
                    "matter_id": "SO2011-2192",
                    "seed_is_candidate": "true",
                    "flag_keyword_reclassification": "true",
                    "flag_keyword_planned_development": "false",
                    "flag_keyword_map_amendment": "false",
                    "flag_keyword_lakefront": "false",
                    "flag_title_map_reclassification": "true",
                    "flag_body_zoning_committee": "true",
                    "flag_title_or_body_zoning": "true",
                    "zoning_class": None,
                }
            ]
        )
        out = build_candidates(seed)
        self.assertEqual(out["matter_id"].iloc[0], "SO2011-2192")
        self.assertTrue(bool(out["is_final_candidate"].iloc[0]))
        self.assertEqual(out["zoning_class"].iloc[0], "reclassification")

    def test_normalize_attachment_has_stable_id(self) -> None:
        row = {
            "fileName": "O2011-2192 Ordinance.pdf",
            "path": "https://example.test/file.pdf",
            "attachmentType": "Legislation",
        }
        first = normalize_attachment("SO2011-2192", 1, row)
        second = normalize_attachment("SO2011-2192", 1, row)
        self.assertEqual(first["attachment_id"], second["attachment_id"])
        self.assertEqual(first["matter_id"], "SO2011-2192")


if __name__ == "__main__":
    unittest.main()
