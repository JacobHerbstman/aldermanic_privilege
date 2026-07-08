import unittest
from tempfile import NamedTemporaryFile

import pandas as pd

from enrich_geocoded_rezonings import (
    apply_pd_far_review_updates,
    assign_far_columns,
    choose_panel_year,
    far_for_code,
    parse_zoning_code,
    ward_era_for_date,
)


def minimal_far_rows() -> pd.DataFrame:
    return pd.DataFrame(
        [
            {
                "matter_id": "SO1",
                "from_far": "",
                "to_far": "",
                "far_change": "",
                "far_pair_status": "missing_both",
            }
        ]
    )


def valid_pd_far_update(**overrides) -> dict:
    row = {
        "matter_id": "SO1",
        "pd_far_update_source": "chatgpt_review",
        "pd_transition_type": "base_to_pd",
        "from_far_update": "2.2",
        "to_far_update": "3.0",
        "far_change_update": "0.8",
        "far_direction_update": "upzone",
        "pd_far_value_update": "3.0",
        "pd_far_role_update": "maximum_far",
        "confidence": "high",
        "evidence": "Maximum FAR 3.0.",
        "notes": "",
    }
    row.update(overrides)
    return row


class TestEnrichGeocodedRezonings(unittest.TestCase):
    def test_parse_basic_code(self):
        self.assertEqual(parse_zoning_code("B3-2 Community Shopping District"), "B3-2")

    def test_parse_rm4_fix(self):
        self.assertEqual(parse_zoning_code("RM-4 Residential Multi-Unit District"), "RM-4.5")

    def test_parse_rt4a(self):
        self.assertEqual(parse_zoning_code("RT-4A Residential Two-Flat District"), "RT-4A")

    def test_parse_pd(self):
        self.assertEqual(parse_zoning_code("Planned Development No. 155"), "PD")

    def test_parse_missing_dash(self):
        self.assertEqual(parse_zoning_code("RS3 Residential Single-Unit District"), "RS-3")

    def test_parse_three_part_b_code(self):
        self.assertEqual(parse_zoning_code("B-3-2"), "B3-2")

    def test_parse_three_part_decimal_code(self):
        self.assertEqual(parse_zoning_code("C-1-1.5"), "C1-1.5")

    def test_parse_old_r_code(self):
        self.assertEqual(parse_zoning_code("R3 General Residence District"), "R3")

    def test_parse_c4_code(self):
        self.assertEqual(parse_zoning_code("C4 Motor Freight Terminal District"), "C4")

    def test_parse_open_space_bulk_text_not_pos(self):
        self.assertEqual(
            parse_zoning_code(
                "RM4-5 Residential Multi-Unit Zoning District Use: Residential Building "
                "Rear Yard Open space: 400 square feet"
            ),
            "RM-4.5",
        )

    def test_parse_trailing_ocr_suffix(self):
        self.assertEqual(parse_zoning_code("C-l-2j Neighborhood Commercial District"), "C1-2")
        self.assertEqual(parse_zoning_code("ML-1I Limited Manufacturing/Business Park District"), "ML-1")

    def test_assign_far_columns_preserves_upstream_far(self):
        df = pd.DataFrame(
            [
                {
                    "from_zoning": "",
                    "to_zoning": "",
                    "matter_intro_date": "2011-01-01",
                    "from_code": "RT-4",
                    "to_code": "RM-4.5",
                    "from_far": "1.2",
                    "to_far": "1.7",
                    "from_far_version": "post_2004",
                    "to_far_version": "post_2004",
                    "from_far_effective_start": "2004-11-01",
                    "to_far_effective_start": "2004-11-01",
                }
            ]
        )
        out = assign_far_columns(df, lookup={})
        self.assertEqual(out.loc[0, "from_code"], "RT-4")
        self.assertEqual(out.loc[0, "to_code"], "RM-4.5")
        self.assertAlmostEqual(out.loc[0, "far_change"], 0.5)
        self.assertEqual(out.loc[0, "far_pair_status"], "resolved_both")
        self.assertEqual(out.loc[0, "to_lookup_version"], "post_2004")

    def test_apply_pd_far_review_updates_overrides_far_with_audit_columns(self):
        df = pd.DataFrame(
            [
                {
                    "matter_id": "SO1",
                    "from_far": "7.0",
                    "to_far": "",
                    "far_change": "",
                    "is_upzone": "",
                    "far_pair_status": "missing_one_side",
                },
                {
                    "matter_id": "SO2",
                    "from_far": "1.2",
                    "to_far": "2.2",
                    "far_change": "1.0",
                    "is_upzone": "True",
                    "far_pair_status": "resolved_both",
                },
            ]
        )
        updates = pd.DataFrame(
            [
                valid_pd_far_update(
                    pd_far_update_source="mechanical_text_high",
                    from_far_update="7.0",
                    to_far_update="9.4",
                    far_change_update="2.4",
                    pd_far_value_update="9.4",
                    pd_far_role_update="to_far_text",
                    evidence="Total FAR 9.4.",
                    notes="High-confidence mechanical extraction.",
                )
            ]
        )
        with NamedTemporaryFile(suffix=".csv") as handle:
            updates.to_csv(handle.name, index=False)
            out = apply_pd_far_review_updates(df, handle.name)

        self.assertTrue(out.loc[0, "pd_far_review_applied"])
        self.assertFalse(out.loc[1, "pd_far_review_applied"])
        self.assertAlmostEqual(out.loc[0, "to_far"], 9.4)
        self.assertAlmostEqual(out.loc[0, "far_change"], 2.4)
        self.assertEqual(out.loc[0, "far_pair_status"], "resolved_both")
        self.assertEqual(out.loc[0, "pd_far_update_source"], "mechanical_text_high")
        self.assertEqual(out.loc[0, "far_direction_pre_pd_review"], "unknown")
        self.assertTrue(pd.isna(out.loc[0, "to_far_pre_pd_review"]))
        self.assertAlmostEqual(out.loc[1, "far_change"], 1.0)

    def test_apply_pd_far_review_updates_rejects_duplicate_update_id(self):
        updates = pd.DataFrame([valid_pd_far_update(), valid_pd_far_update()])
        with NamedTemporaryFile(suffix=".csv") as handle:
            updates.to_csv(handle.name, index=False)
            with self.assertRaisesRegex(ValueError, "Duplicate PD FAR review update"):
                apply_pd_far_review_updates(minimal_far_rows(), handle.name)

    def test_apply_pd_far_review_updates_rejects_missing_geocoded_id(self):
        updates = pd.DataFrame([valid_pd_far_update(matter_id="SO2")])
        with NamedTemporaryFile(suffix=".csv") as handle:
            updates.to_csv(handle.name, index=False)
            with self.assertRaisesRegex(ValueError, "missing from geocoded data"):
                apply_pd_far_review_updates(minimal_far_rows(), handle.name)

    def test_apply_pd_far_review_updates_rejects_bad_direction(self):
        updates = pd.DataFrame(
            [
                valid_pd_far_update(
                    to_far_update="1.2",
                    far_change_update="-1.0",
                    pd_far_value_update="1.2",
                    evidence="Maximum FAR 1.2.",
                )
            ]
        )
        with NamedTemporaryFile(suffix=".csv") as handle:
            updates.to_csv(handle.name, index=False)
            with self.assertRaisesRegex(ValueError, "inconsistent FAR directions"):
                apply_pd_far_review_updates(minimal_far_rows(), handle.name)

    def test_ward_era(self):
        self.assertEqual(ward_era_for_date("2014-01-01"), "pre_2015")
        self.assertEqual(ward_era_for_date("2019-10-10"), "2015_to_2023")
        self.assertEqual(ward_era_for_date("2024-01-01"), "post_2023")

    def test_choose_panel_year(self):
        years = [1998, 2003, 2014, 2015, 2023, 2024, 2026]
        self.assertEqual(choose_panel_year(years, preferred=2014, high=2014), 2014)
        self.assertEqual(choose_panel_year(years, preferred=2015, low=2015, high=2023), 2015)
        self.assertEqual(choose_panel_year(years, preferred=2024, low=2024), 2024)

    def test_far_for_code_date_aware_lookup(self):
        lookup = {
            "R3": [
                {
                    "far": 0.7,
                    "effective_start": pd.Timestamp("1957-05-29"),
                    "effective_end": pd.Timestamp("2004-10-31"),
                    "zoning_code_version": "pre_2004",
                },
                {
                    "far": 0.9,
                    "effective_start": pd.Timestamp("2004-11-01"),
                    "effective_end": None,
                    "zoning_code_version": "post_2004",
                },
            ]
        }

        pre_far, pre_version, pre_start, pre_end = far_for_code("R3", lookup, "2004-10-31")
        self.assertEqual(pre_far, 0.7)
        self.assertEqual(pre_version, "pre_2004")
        self.assertEqual(pre_start, "1957-05-29")
        self.assertEqual(pre_end, "2004-10-31")

        post_far, post_version, post_start, post_end = far_for_code("R3", lookup, "2004-11-01")
        self.assertEqual(post_far, 0.9)
        self.assertEqual(post_version, "post_2004")
        self.assertEqual(post_start, "2004-11-01")
        self.assertIsNone(post_end)


if __name__ == "__main__":
    unittest.main()
