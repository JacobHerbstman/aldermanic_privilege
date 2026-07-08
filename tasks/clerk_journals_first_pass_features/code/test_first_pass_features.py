import unittest

import pandas as pd

from parse_journal_first_pass_features import (
    extract_addresses,
    extract_addresses_loose,
    extract_addresses_strict,
    extract_metric_lists,
    far_for_code,
    normalize_address_candidate,
    normalize_address_candidate_loose,
    normalize_address_candidate_strict,
    parse_zoning_code,
    pick_page_window,
)


class TestFirstPassFeatures(unittest.TestCase):
    def test_extract_addresses(self):
        text = "The property at 1756 W Cornelia Ave and 1800 N Milwaukee Avenue is rezoned."
        addrs = extract_addresses(text)
        self.assertIn("1756 W Cornelia Ave", addrs)
        self.assertIn("1800 N Milwaukee Avenue", addrs)

    def test_extract_addresses_filters_noise(self):
        self.assertIsNone(normalize_address_candidate_strict("88052 Committee On Transportation And Public Way"))
        self.assertIsNone(normalize_address_candidate_strict("92532 Reports Of Committees"))
        self.assertIsNone(normalize_address_candidate_strict("175 feet north of the C.M. St"))
        self.assertEqual(
            normalize_address_candidate_strict("16-1 for the property located at 6333 - 6339 South Kedzie Avenue"),
            "6333 - 6339 South Kedzie Avenue",
        )
        self.assertEqual(
            normalize_address_candidate("16-1 for the property located at 6333 - 6339 South Kedzie Avenue"),
            "6333 - 6339 South Kedzie Avenue",
        )

    def test_extract_addresses_loose_adds_coverage(self):
        text = "Bounded by 100 feet north of 200 W Main St and 300 W Main St."
        strict_addrs = extract_addresses_strict(text)
        loose_addrs = extract_addresses_loose(text)
        self.assertEqual(strict_addrs, ["300 W Main St"])
        self.assertIn("200 W Main St", loose_addrs)
        self.assertIn("300 W Main St", loose_addrs)
        self.assertIsNotNone(normalize_address_candidate_loose("200 feet north of 200 W Main St"))
        self.assertIsNone(normalize_address_candidate_loose("200 feet north of W Main St"))

    def test_extract_metric_lists(self):
        text = "Floor area ratio of 3.5 with 120 dwelling units, 8 stories, and 95 feet in height."
        metrics = extract_metric_lists(text)
        self.assertIn(3.5, metrics["far_values"])
        self.assertIn(120, metrics["dwelling_unit_counts"])
        self.assertIn(8, metrics["story_counts"])
        self.assertIn(95.0, metrics["height_ft_values"])
        self.assertGreater(metrics["density_mention_count"], 0)

    def test_pick_page_window_with_zoning_start(self):
        start, end, mode = pick_page_window(
            doc_page_count=300,
            zoning_section_page=120,
            use_window=True,
            buffer_pages=3,
            max_pages_after_start=80,
            fallback_max_pages_no_start=120,
        )
        self.assertEqual(mode, "zoning_start_window")
        self.assertEqual(start, 116)
        self.assertEqual(end, 196)

    def test_pick_page_window_fallback(self):
        start, end, mode = pick_page_window(
            doc_page_count=90,
            zoning_section_page=None,
            use_window=True,
            buffer_pages=3,
            max_pages_after_start=80,
            fallback_max_pages_no_start=120,
        )
        self.assertEqual(mode, "fallback_front_window")
        self.assertEqual(start, 0)
        self.assertEqual(end, 90)

    def test_parse_zoning_code_legacy_and_ocr(self):
        self.assertEqual(parse_zoning_code("R-3 General Residence District"), "R3")
        self.assertEqual(parse_zoning_code("Rl Single-Family Residence District"), "R1")
        self.assertEqual(parse_zoning_code("a 83-2 Community Shopping District"), "B3-2")
        self.assertEqual(parse_zoning_code("an Ml-S Restricted Manufacturing District"), "M1-5")
        self.assertEqual(parse_zoning_code("DXS Downtown Mixed-Use District"), "DX-5")
        self.assertEqual(parse_zoning_code("AB2-3 Neighborhood Mixed-Use District"), "B2-3")
        self.assertEqual(parse_zoning_code("DX1-2 Downtown Mixed-Use District"), "DX-12")
        self.assertEqual(parse_zoning_code("RS-3 Residential Single-Unit District"), "RS-3")

    def test_far_for_code_date_aware(self):
        lookup = {
            "B1-5": [
                {"far": 7.0, "effective_start": pd.Timestamp("1957-05-29"), "effective_end": pd.Timestamp("2004-10-31")},
                {"far": 5.0, "effective_start": pd.Timestamp("2004-11-01"), "effective_end": None},
            ]
        }
        self.assertEqual(far_for_code("B1-5", lookup, "2004-10-31"), 7.0)
        self.assertEqual(far_for_code("B1-5", lookup, "2004-11-01"), 5.0)


if __name__ == "__main__":
    unittest.main()
