import unittest

import pandas as pd

from stage1_parcel_geocode import (
    extract_title_address,
    run_stage1,
    split_address_variants,
    standardize_address,
)


class TestStage1ParcelGeocode(unittest.TestCase):
    def test_extract_title_address(self):
        title = "Zoning Reclassification Map No. 5-G at 2031-2033 N Kingsbury St - App No. 21128"
        self.assertEqual(extract_title_address(title), "2031-2033 N Kingsbury St")

    def test_extract_title_address_without_dash_before_application(self):
        title = "Zoning Reclassification Map No. 14-J at 2611 W 23rd Pl App No. 17439"
        self.assertEqual(extract_title_address(title), "2611 W 23rd Pl")

    def test_extract_title_address_with_trailing_application_number(self):
        title = "Zoning Reclassification Map No. 8-H at 2000 W 34th St - 19612T1"
        self.assertEqual(extract_title_address(title), "2000 W 34th St")

    def test_standardize_range_address(self):
        parsed = standardize_address("3120-3132 W Lake St")
        self.assertEqual(parsed["standardized"], "3120 WEST LAKE STREET")
        self.assertTrue(parsed["is_range"])
        self.assertEqual(parsed["range_start"], 3120)
        self.assertEqual(parsed["range_end"], 3132)

    def test_standardize_unit_suffix(self):
        parsed = standardize_address("1756 W Cornelia Ave Apt 2")
        self.assertEqual(parsed["standardized"], "1756 WEST CORNELIA AVENUE")
        self.assertEqual(parsed["house_number"], 1756)

    def test_standardize_bare_parcel_unit_suffix(self):
        parsed = standardize_address("858 W Diversey Pky 2E")
        self.assertEqual(parsed["standardized"], "858 WEST DIVERSEY PARKWAY")
        self.assertEqual(parsed["house_number"], 858)
        self.assertEqual(parsed["street_type"], "PARKWAY")

    def test_split_multi_address(self):
        parts = split_address_variants("3120-3132 W Lake St and 212-220 N Albany Ave")
        self.assertEqual(parts, ["3120-3132 W Lake St", "212-220 N Albany Ave"])

    def test_split_slash_multi_address(self):
        parts = split_address_variants("3600-3618 W 15th St/1446-1448 S Central Park Ave")
        self.assertEqual(parts, ["3600-3618 W 15th St", "1446-1448 S Central Park Ave"])

    def test_standardize_ocr_street_fix(self):
        parsed = standardize_address("2900 North PauUna Street")
        self.assertEqual(parsed["standardized"], "2900 NORTH PAULINA STREET")

    def test_external_row_id_with_string_matter_id(self):
        rezoning_df = pd.DataFrame(
            [
                {
                    "matter_id": "HIST_abc123",
                    "address_raw": "1756 W Cornelia Ave",
                    "matter_title": None,
                }
            ]
        )

        parcel_df = pd.DataFrame(
            [
                {
                    "pin": "01234567890123",
                    "property_address": "1756 W Cornelia Ave",
                    "std_address": "1756 WEST CORNELIA AVENUE",
                    "house_number": 1756,
                    "street_key_full": "WEST|CORNELIA|AVENUE|",
                    "street_key_loose": "CORNELIA|AVENUE|",
                    "latitude": 41.943,
                    "longitude": -87.673,
                }
            ]
        )

        stage1_df, unmatched_df, _, _ = run_stage1(
            rezoning_df=rezoning_df,
            parcel_df=parcel_df,
            nearest_max_diff=50,
        )

        self.assertEqual(stage1_df.loc[0, "external_row_id"], "HIST_abc123")
        self.assertEqual(stage1_df.loc[0, "geocode_source"], "parcel_match")
        self.assertEqual(len(unmatched_df), 0)

    def test_uses_address_raw_loose_when_address_raw_missing(self):
        rezoning_df = pd.DataFrame(
            [
                {
                    "matter_id": "HISTFP_test_1",
                    "address_raw": None,
                    "address_raw_loose": "1756 W Cornelia Ave",
                    "matter_title": None,
                }
            ]
        )

        parcel_df = pd.DataFrame(
            [
                {
                    "pin": "01234567890123",
                    "property_address": "1756 W Cornelia Ave",
                    "std_address": "1756 WEST CORNELIA AVENUE",
                    "house_number": 1756,
                    "street_key_full": "WEST|CORNELIA|AVENUE|",
                    "street_key_loose": "CORNELIA|AVENUE|",
                    "latitude": 41.943,
                    "longitude": -87.673,
                }
            ]
        )

        stage1_df, unmatched_df, _, _ = run_stage1(
            rezoning_df=rezoning_df,
            parcel_df=parcel_df,
            nearest_max_diff=50,
        )

        self.assertEqual(stage1_df.loc[0, "external_row_id"], "HISTFP_test_1")
        self.assertEqual(stage1_df.loc[0, "geocode_source"], "parcel_match")
        self.assertEqual(stage1_df.loc[0, "address_source_used"], "address_raw_loose")
        self.assertEqual(len(unmatched_df), 0)

    def test_exact_house_street_name_fallback_requires_unambiguous_type(self):
        rezoning_df = pd.DataFrame(
            [
                {
                    "matter_id": "O2012-28",
                    "address_raw": "1226 W Altgeld Ave",
                    "matter_title": None,
                }
            ]
        )

        parcel_df = pd.DataFrame(
            [
                {
                    "pin": "01234567890123",
                    "property_address": "1226 W Altgeld St",
                    "std_address": "1226 WEST ALTGELD STREET",
                    "house_number": 1226,
                    "street_key_full": "WEST|ALTGELD|STREET|",
                    "street_key_name": "WEST|ALTGELD|",
                    "street_key_loose": "ALTGELD|STREET|",
                    "latitude": 41.926,
                    "longitude": -87.659,
                }
            ]
        )

        stage1_df, unmatched_df, _, _ = run_stage1(
            rezoning_df=rezoning_df,
            parcel_df=parcel_df,
            nearest_max_diff=50,
        )

        self.assertEqual(stage1_df.loc[0, "geocode_source"], "parcel_match")
        self.assertEqual(stage1_df.loc[0, "match_method"], "house_and_street_name")
        self.assertEqual(len(unmatched_df), 0)

    def test_nearest_house_allows_tied_unit_rows_at_same_address(self):
        rezoning_df = pd.DataFrame(
            [
                {
                    "matter_id": "O2018-894",
                    "address_raw": "1756 W Cornelia Ave",
                    "matter_title": None,
                }
            ]
        )

        parcel_df = pd.DataFrame(
            [
                {
                    "pin": "01234567890123",
                    "property_address": "1757 W Cornelia Ave 1",
                    "std_address": "1757 WEST CORNELIA AVENUE",
                    "house_number": 1757,
                    "street_key_full": "WEST|CORNELIA|AVENUE|",
                    "street_key_name": "WEST|CORNELIA|",
                    "street_key_loose": "CORNELIA|AVENUE|",
                    "latitude": 41.9448,
                    "longitude": -87.6734,
                },
                {
                    "pin": "01234567890124",
                    "property_address": "1757 W Cornelia Ave 2",
                    "std_address": "1757 WEST CORNELIA AVENUE",
                    "house_number": 1757,
                    "street_key_full": "WEST|CORNELIA|AVENUE|",
                    "street_key_name": "WEST|CORNELIA|",
                    "street_key_loose": "CORNELIA|AVENUE|",
                    "latitude": 41.9448,
                    "longitude": -87.6734,
                },
            ]
        )

        stage1_df, unmatched_df, _, _ = run_stage1(
            rezoning_df=rezoning_df,
            parcel_df=parcel_df,
            nearest_max_diff=50,
        )

        self.assertEqual(stage1_df.loc[0, "geocode_source"], "parcel_match")
        self.assertEqual(stage1_df.loc[0, "match_method"], "house_and_street_nearest")
        self.assertEqual(len(unmatched_df), 0)


if __name__ == "__main__":
    unittest.main()
