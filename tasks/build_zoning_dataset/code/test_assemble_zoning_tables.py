import unittest

import pandas as pd

from assemble_zoning_tables import (
    has_map_reclassification_title,
    is_non_base_zoning_action,
    normalize_candidates,
    normalize_matters,
    parse_from_to_zoning,
    parse_zoning_code_for_quality,
    parsed_pair_quality,
)


class AssembleZoningTablesTests(unittest.TestCase):
    def test_has_map_reclassification_title_handles_variants(self) -> None:
        self.assertTrue(has_map_reclassification_title("Zoning Reclassifiction Map No, 2-F at 100 W Test"))
        self.assertTrue(has_map_reclassification_title("Zoning Reclassification on Map No. 5-A at 1 N State"))
        self.assertFalse(has_map_reclassification_title("Amendment of Municipal Code Chapter 17"))

    def test_normalize_candidates_keeps_string_matter_id(self) -> None:
        df = pd.DataFrame([{"matter_id": "SO2011-2192", "is_final_candidate": "true"}])
        out = normalize_candidates(df)
        self.assertEqual(out["matter_id"].iloc[0], "SO2011-2192")
        self.assertTrue(bool(out["is_final_candidate"].iloc[0]))

    def test_normalize_matters_keeps_elms_metadata(self) -> None:
        df = pd.DataFrame(
            [
                {
                    "matter_id": "SO2011-2192",
                    "matter_guid": "GUID-1",
                    "matter_file": "SO2011-2192",
                    "matter_title": "Zoning Reclassification Map No. 3-F",
                    "matter_status_name": "Passed",
                    "matter_body_name": "Committee on Zoning",
                    "matter_intro_date": "2011-03-09",
                    "matter_passed_date": "2011-03-09",
                    "source_system": "elms",
                    "date_imputed_from_final_action": True,
                    "matter_intro_date_raw": "2011-03-09",
                }
            ]
        )
        out = normalize_matters(df)
        self.assertEqual(out["matter_id"].iloc[0], "SO2011-2192")
        self.assertEqual(out["matter_guid"].iloc[0], "GUID-1")
        self.assertEqual(out["source_system"].iloc[0], "elms")
        self.assertTrue(bool(out["date_imputed_from_final_action"].iloc[0]))

    def test_parse_zoning_code_for_quality_handles_ocr_artifacts(self) -> None:
        self.assertEqual(parse_zoning_code_for_quality("Bl-I Neighborhood Shopping District"), "BL-1")
        self.assertEqual(parse_zoning_code_for_quality("Ml-I Limited Manufacturing District"), "ML-1")
        self.assertEqual(parse_zoning_code_for_quality("BZ-2 Neighborhood Mixed-Use District"), "B2-2")
        self.assertEqual(parse_zoning_code_for_quality("33-2 Community Shopping District"), "B3-2")
        self.assertEqual(parse_zoning_code_for_quality("RT4-5 Residential Two-Flat District"), "RT-4.5")
        self.assertEqual(parse_zoning_code_for_quality("R 13.5 Residential Two-Flat District"), "RT-3.5")
        self.assertEqual(parse_zoning_code_for_quality("RS3 Residential Single-Unit District"), "RS-3")
        self.assertEqual(parse_zoning_code_for_quality("planned development no. 123"), "PD")
        self.assertIsNone(parse_zoning_code_for_quality("the owner allowing the application"))

    def test_parsed_pair_quality_rejects_junk(self) -> None:
        self.assertEqual(parsed_pair_quality("the owner allowing the application", "proceed"), 0)
        self.assertEqual(parsed_pair_quality("RS-3 Residential Single-Unit District", "proceed"), 1)
        self.assertEqual(parsed_pair_quality("RS-3 Residential Single-Unit District", "RT-4"), 2)

    def test_parse_from_to_zoning_sequence_fallback(self) -> None:
        text = (
            "Section 1. changing all of the RT-4 Residential District B1-1 Neighborhood Shopping District "
            "symbols and indications as shown on Map No. 7-H."
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertEqual(parse_zoning_code_for_quality(from_zone), "RT-4")
        self.assertEqual(parse_zoning_code_for_quality(to_zone), "B1-1")

    def test_parse_from_to_zoning_rejects_address_pair(self) -> None:
        text = "Section 1. change from 3535 N Ashland Ave to 3720 N Ashland Ave for the property."
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertIsNone(from_zone)
        self.assertIsNone(to_zone)

    def test_identifies_non_base_zoning_actions(self) -> None:
        self.assertTrue(
            is_non_base_zoning_action(
                "SECTION 1. The public street and alley are hereby vacated and closed. "
                "The restrictive covenant refers to the Chicago Zoning Ordinance."
            )
        )
        self.assertTrue(
            is_non_base_zoning_action(
                "SECTION 1. This ordinance removes all designations of such block as a Pedestrian Street."
            )
        )
        self.assertFalse(
            is_non_base_zoning_action(
                "SECTION 1. The ordinance is amended by changing all B3-1 symbols to those of B2-2."
            )
        )


if __name__ == "__main__":
    unittest.main()
