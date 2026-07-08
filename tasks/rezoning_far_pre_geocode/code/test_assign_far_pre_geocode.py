import unittest

import pandas as pd

from assign_far_pre_geocode import (
    compute_parseable_non_structural_mask,
    infer_missing_side_code_from_context,
    parse_zoning_code,
    parse_zoning_code_for_side,
    resolve_code,
    resolve_far,
    is_structural_na_code,
    code_status_for_lookup,
)


class AssignFarPreGeocodeTests(unittest.TestCase):
    def test_parse_zoning_code_handles_ocr(self) -> None:
        self.assertEqual(parse_zoning_code("Bl-I Neighborhood Shopping District"), "BL-1")
        self.assertEqual(parse_zoning_code("Ml-I Limited Manufacturing District"), "ML-1")
        self.assertEqual(parse_zoning_code("n RMS Residential Multi-Unit District"), "RM-5")
        self.assertEqual(parse_zoning_code("RSI Residential Single-Unit District"), "RS-1")
        self.assertEqual(parse_zoning_code("02-1 Motor Vehicle-Related Commercial District"), "C2-1")
        self.assertEqual(parse_zoning_code("33-2 Community Shopping District"), "B3-2")
        self.assertEqual(parse_zoning_code("BZ-2 Neighborhood Mixed-Use District"), "B2-2")
        self.assertEqual(parse_zoning_code('n BZ" [Neighborhood Mixed-Use District'), "B2-1")
        self.assertEqual(parse_zoning_code("RS3 Residential Single-Unit District"), "RS-3")
        self.assertEqual(parse_zoning_code("RM4.5 Residential Multi-Unit District"), "RM-4.5")
        self.assertEqual(parse_zoning_code("RT3.5 Residential Two-Flat District"), "RT-3.5")
        self.assertEqual(parse_zoning_code("RT3.S Residential Two-Flat District"), "RT-3.5")
        self.assertEqual(parse_zoning_code("R 13.5 Residential Two-Flat District"), "RT-3.5")
        self.assertEqual(parse_zoning_code("B2?3 Neighborhood Mixed-Use District"), "B2-3")
        self.assertEqual(parse_zoning_code("C l-3 Neighborhood Commercial District"), "C1-3")
        self.assertEqual(parse_zoning_code("B3-3Community Shopping District"), "B3-3")
        self.assertEqual(parse_zoning_code("M2-,1 Light Industry District"), "M2-1")
        self.assertEqual(parse_zoning_code("M?,-3 Light Industry District"), "M2-3")
        self.assertEqual(parse_zoning_code("Bl-lNeighborhood Shopping District"), "B1-1")
        self.assertEqual(parse_zoning_code("B2-lNeighborhood Mixed-Use District"), "B2-1")
        self.assertEqual(parse_zoning_code("C]-2 Neighborhood Commercial District"), "C1-2")
        self.assertEqual(parse_zoning_code("C2-! Motor Vehicle-Related Commercial District"), "C2-1")
        self.assertEqual(parse_zoning_code("B3-S Community Shopping District"), "B3-5")
        self.assertEqual(parse_zoning_code("R-M-4-5 Residential Multi-Unit District"), "RM-4.5")
        self.assertEqual(parse_zoning_code("132-3 Neighborhood Mixed-Use District"), "B2-3")
        self.assertEqual(parse_zoning_code("RS# Residential Single-Unit District"), "RS-3")
        self.assertEqual(parse_zoning_code("T, Transportation District"), "T")
        self.assertEqual(parse_zoning_code("planned development no. 44"), "PD")
        self.assertEqual(parse_zoning_code("Planned Manufacturing District No. 13"), "PMD-13")
        self.assertEqual(parse_zoning_code("C-l-2j Neighborhood Commercial District"), "C1-2")
        self.assertEqual(parse_zoning_code("ML-1I Limited Manufacturing/Business Park District"), "ML-1")
        self.assertIsNone(parse_zoning_code("the owner allowing the application"))
        self.assertEqual(
            parse_zoning_code(
                "RM4-5 Residential Multi-Unit Zoning District Use: Residential Building "
                "Rear Yard Open space: 400 square feet"
            ),
            "RM-4.5",
        )

    def test_resolve_far_is_date_aware(self) -> None:
        lookup = {
            "R3": [
                {
                    "far": 0.7,
                    "start": pd.Timestamp("1957-05-29"),
                    "end": pd.Timestamp("2004-10-31"),
                    "version": "pre_2004",
                },
                {
                    "far": 0.9,
                    "start": pd.Timestamp("2004-11-01"),
                    "end": None,
                    "version": "post_2004",
                },
            ]
        }
        pre_far, pre_ver, _, _, pre_status = resolve_far("R3", "2004-10-31", lookup)
        self.assertEqual(pre_far, 0.7)
        self.assertEqual(pre_ver, "pre_2004")
        self.assertEqual(pre_status, "ok")

        post_far, post_ver, _, _, post_status = resolve_far("R3", "2004-11-01", lookup)
        self.assertEqual(post_far, 0.9)
        self.assertEqual(post_ver, "post_2004")
        self.assertEqual(post_status, "ok")

    def test_resolve_code_historical_precedence(self) -> None:
        lookup = {"B3-3": [{"far": 3.0, "start": pd.Timestamp("2004-11-01"), "end": None, "version": "post_2004"}]}
        row = pd.Series(
            {
                "record_source": "clerk_journal_first_pass",
                "journal_from_code": "B3-3",
                "from_zoning": "the owner allowing the application",
                "from_zoning_canonical": None,
            }
        )
        code, source, status = resolve_code(row, "from", lookup)
        self.assertEqual(code, "B3-3")
        self.assertEqual(source, "journal_from_code")
        self.assertEqual(status, "ok")

    def test_resolve_code_prefers_raw_text_over_stale_canonical(self) -> None:
        lookup = {"RM-4.5": [{"far": 1.7, "start": pd.Timestamp("2004-11-01"), "end": None, "version": "post_2004"}]}
        row = pd.Series(
            {
                "to_zoning_raw": (
                    "RM4-5 Residential Multi-Unit Zoning District Use: Residential Building "
                    "Rear Yard Open space: 400 square feet"
                ),
                "to_zoning_canonical": "POS",
                "to_zoning": "RM4-5 Residential Multi-Unit Zoning District",
            }
        )
        code, source, status = resolve_code(row, "to", lookup)
        self.assertEqual(code, "RM-4.5")
        self.assertEqual(source, "to_zoning_raw")
        self.assertEqual(status, "ok")

    def test_resolve_far_structural_na(self) -> None:
        far, version, start, end, status = resolve_far("PD", "2015-01-01", {})
        self.assertIsNone(far)
        self.assertIsNone(version)
        self.assertIsNone(start)
        self.assertIsNone(end)
        self.assertEqual(status, "structural_na")
        self.assertTrue(is_structural_na_code("PMD-6"))

    def test_resolve_far_infers_district_suffix_when_lookup_missing(self) -> None:
        far, version, start, end, status = resolve_far("DX-10", "2018-01-01", {})
        self.assertEqual(far, 10.0)
        self.assertEqual(version, "inferred_from_code")
        self.assertIsNone(start)
        self.assertIsNone(end)
        self.assertEqual(status, "ok")

    def test_compute_parseable_non_structural_mask(self) -> None:
        df = pd.DataFrame(
            [
                {"from_code": "B3-2", "to_code": "B3-3"},
                {"from_code": "PD", "to_code": "DX-5"},
                {"from_code": None, "to_code": "POS"},
                {"from_code": None, "to_code": None},
            ]
        )
        mask = compute_parseable_non_structural_mask(df)
        self.assertEqual(mask.tolist(), [True, False, False, False])

    def test_infer_missing_side_code_from_context(self) -> None:
        row = pd.Series(
            {
                "from_zoning": (
                    "M2-2 Manufacturing District as shown on Map No. 9-I in the area bounded by ... "
                    "to those of a M1-2 Limited Manufacturing District"
                ),
                "to_zoning": None,
                "matter_title": "Zoning Reclassification Map No. 9-I at 3000 W Addison St",
            }
        )
        inferred_to = infer_missing_side_code_from_context(row, "to", "M2-2")
        self.assertEqual(inferred_to, "M1-2")

    def test_code_status_for_lookup(self) -> None:
        lookup = {"B3-2": [{"far": 2.2, "start": pd.Timestamp("2004-11-01"), "end": None, "version": "post_2004"}]}
        self.assertEqual(code_status_for_lookup("B3-2", lookup), "ok")
        self.assertEqual(code_status_for_lookup("PD", lookup), "ok")
        self.assertEqual(code_status_for_lookup("B7-9", lookup), "lookup_missing")
        self.assertEqual(code_status_for_lookup(None, lookup), "unparseable")

    def test_parse_zoning_code_for_side_uses_directional_phrase(self) -> None:
        text = "change property currently zoned as B3-1, Community Shopping District to C2-1, Motor Vehicle-Related Commercial District"
        self.assertEqual(parse_zoning_code_for_side(text, "from"), "B3-1")
        self.assertEqual(parse_zoning_code_for_side(text, "to"), "C2-1")


if __name__ == "__main__":
    unittest.main()
