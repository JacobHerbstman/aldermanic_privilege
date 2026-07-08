import unittest

import pandas as pd

from fetch_matters import (
    build_file_year_filter,
    coerce_matter_row,
    has_reclass_title,
    make_seed_table,
)


class FetchMattersTests(unittest.TestCase):
    def test_build_file_year_filter_uses_active_rule(self) -> None:
        out = build_file_year_filter(2024)
        self.assertIn("type eq 'Ordinance'", out)
        self.assertIn("supersededBy eq null", out)
        self.assertIn("supersededBy eq ''", out)
        self.assertIn("fileYear eq 2024", out)

    def test_coerce_row_imputes_intro_date_from_final_action(self) -> None:
        raw = {
            "matterId": "GUID-1",
            "recordNumber": "O2023-100",
            "title": "Zoning Reclassification Map No. 1-A at 1 N State St",
            "type": "Ordinance",
            "status": "Passed",
            "controllingBody": "Committee on Zoning, Landmarks and Building Standards",
            "introductionDate": None,
            "finalActionDate": "2023-06-21T10:00:00+00:00",
        }
        out = coerce_matter_row(raw)
        self.assertEqual(out["matter_id"], "O2023-100")
        self.assertEqual(out["matter_guid"], "GUID-1")
        self.assertEqual(out["matter_intro_date"], "2023-06-21")
        self.assertTrue(out["date_imputed_from_final_action"])

    def test_seed_table_flags_ocr_map_title_variants(self) -> None:
        df = pd.DataFrame(
            [
                {
                    "matter_id": "O2011-1",
                    "matter_file": "O2011-1",
                    "matter_title": "Zoning Reclassifiction Map No, 2-F at 100 W Test",
                    "matter_body_name": "Committee on Zoning",
                },
                {
                    "matter_id": "O2011-2",
                    "matter_file": "O2011-2",
                    "matter_title": "Amendment of Municipal Code Chapter 17",
                    "matter_body_name": "Committee on Finance",
                },
            ]
        )
        out = make_seed_table(df)
        self.assertTrue(bool(out.loc[out["matter_id"] == "O2011-1", "flag_title_map_reclassification"].iloc[0]))
        self.assertEqual(out.loc[out["matter_id"] == "O2011-1", "zoning_class"].iloc[0], "reclassification")
        self.assertFalse(has_reclass_title("Parking meters at 100 W Test"))


if __name__ == "__main__":
    unittest.main()
