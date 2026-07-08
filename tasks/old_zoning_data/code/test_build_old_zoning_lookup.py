import tempfile
import unittest
from pathlib import Path

import pandas as pd

from build_old_zoning_lookup import (
    build_old_lookup,
    build_target_universe,
    normalize_code,
)


SAMPLE_TEXT = """
7.5-1 Minimum Lot Area-R1 Single-Family Residence District.
In an R1 District, there shall be provided not less than 6,250 square feet of lot area per dwelling unit.
7.6-1 Maximum Floor Area Ratio R1 Single-Family Residence District.
In an R1 District, the floor area ratio shall not exceed 0.5.

8.5-6 Maximum Floor Area Ratio Restricted Central Business Districts.
B6-6 and B6-7
(1) In a B6-6 District, the floor area ratio shall not exceed 12.0 except as provided in paragraphs (3) and (4) hereinafter.
(2) In a B6-7 District, the floor area ratio shall not exceed 16.0 except as provided in paragraphs (3) and (5) hereinafter.
(3) ... may be increased by 15 per cent.
(4) ... floor area ratio premiums may be added.

8.6-1 Minimum Lot Area B1-1 To B1-5 Local Retail Districts.
(1) In a B1-1 District, there shall be provided not less than 2,500 square feet of lot area per dwelling unit.
(5) In a B1-5 District, there shall be provided not less than 145 square feet of lot area per dwelling unit.
8.6-2 Minimum Lot Area B2-1 To B2-5 Restricted Retail Districts.
(1) In a B2-1 District, the minimum requirements for lot area per dwelling unit shall be the same as in a B1-1 District.
(5) In a B2-5 District, the minimum requirements for lot area per dwelling unit shall be the same as in a B1-5 District.

9.5-4 Maximum Floor Area Ratio-C4 Motor Freight Terminal District.
In a C4 District, the floor area ratio shall not exceed 1.2.
9.5-3 Maximum Floor Area Ratio C3-1 to C3-7 Commercial-Manufacturing Districts.
(1) In a C3-1 District, the floor area ratio shall not exceed 1.2.

10.12-3 Maximum Floor Area Ratio Heavy Manufacturing Districts.
(5) In an M3-5 District, the floor area ratio shall not exceed 7.0.

ARTICLE 12
ZONING MAPS
"""

SAMPLE_CROSSWALK = """old_code,new_code,conversion_scope,notes
C5-1,C3-1,outside_downtown,
C5-2,C3-2,outside_downtown,
C5-3,C3-3,outside_downtown,
C5-4,C3-5,outside_downtown,
"""


class TestBuildOldZoningLookup(unittest.TestCase):
    def test_normalize_code_ocr(self):
        self.assertEqual(normalize_code("B1.4"), "B1-4")
        self.assertEqual(normalize_code("M1.-4"), "M1-4")
        self.assertEqual(normalize_code("B24"), "B2-4")

    def test_target_universe_contains_expected(self):
        targets = build_target_universe()
        self.assertIn("R1", targets)
        self.assertIn("B6-7", targets)
        self.assertIn("C4", targets)
        self.assertIn("C5-5", targets)
        self.assertIn("M3-5", targets)

    def test_core_extraction_and_crosswalk(self):
        with tempfile.TemporaryDirectory() as td:
            txt_path = Path(td) / "old.txt"
            xwalk_path = Path(td) / "xwalk.csv"
            txt_path.write_text(SAMPLE_TEXT, encoding="utf-8")
            xwalk_path.write_text(SAMPLE_CROSSWALK, encoding="utf-8")

            bulk_df, mention_df, summary = build_old_lookup(
                in_old_zoning_txt=str(txt_path),
                in_crosswalk_csv=str(xwalk_path),
            )

            by_code = bulk_df.set_index("district_code")

            self.assertEqual(float(by_code.loc["R1", "basic_far"]), 0.5)
            self.assertEqual(float(by_code.loc["B6-7", "basic_far"]), 16.0)
            self.assertEqual(float(by_code.loc["C4", "basic_far"]), 1.2)
            self.assertEqual(float(by_code.loc["M3-5", "basic_far"]), 7.0)

            self.assertEqual(int(by_code.loc["R1", "min_lot_area_per_dwelling_unit_sqft"]), 6250)
            self.assertEqual(int(by_code.loc["B2-5", "min_lot_area_per_dwelling_unit_sqft"]), 145)

            self.assertTrue(pd.notna(by_code.loc["C5-1", "basic_far"]))
            self.assertEqual(by_code.loc["C5-1", "source_method"], "crosswalk_inferred")

            self.assertTrue(pd.isna(by_code.loc["C5-5", "basic_far"]))
            self.assertTrue(pd.isna(by_code.loc["C5-5", "min_lot_area_per_dwelling_unit_sqft"]))

            self.assertIn("C5-5", summary["unresolved_codes"])
            self.assertTrue(summary["c5_5_unresolved"])

            self.assertIn("district_code", mention_df.columns)
            self.assertTrue((mention_df["is_target_universe"] == True).any())


if __name__ == "__main__":
    unittest.main()
