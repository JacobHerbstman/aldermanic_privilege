import unittest

from title_parsers import parse_title_fields


class TestTitleParsers(unittest.TestCase):
    def test_reclassification_title(self):
        title = "Zoning Reclassification Map No. 5-G at 2031-2033 N Kingsbury St - App No. 21128"
        parsed = parse_title_fields(title)
        self.assertEqual(parsed["map_grid"], "5-G")
        self.assertEqual(parsed["address_raw"], "2031-2033 N Kingsbury St")
        self.assertEqual(parsed["app_number"], "21128")
        self.assertIsNone(parsed["pd_number"])

    def test_reclassification_with_lettered_app(self):
        title = "Zoning Reclassification Map No. 7-I at 2550 N Milwaukee Ave - App No. A8745"
        parsed = parse_title_fields(title)
        self.assertEqual(parsed["map_grid"], "7-I")
        self.assertEqual(parsed["app_number"], "A8745")

    def test_planned_development_title(self):
        title = "Residential-Business Planned Development No. 1458"
        parsed = parse_title_fields(title)
        self.assertEqual(parsed["pd_number"], "1458")


if __name__ == "__main__":
    unittest.main()
