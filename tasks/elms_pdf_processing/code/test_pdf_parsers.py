import unittest

from pdf_parsers import parse_from_to_zoning, parse_from_to_zoning_detailed, parse_map_ref


class TestPdfParsers(unittest.TestCase):
    def test_parse_from_to_main_pattern(self):
        text = (
            "Section 1. The Chicago Zoning Ordinance is hereby amended by changing all of the "
            "RS-3 Residential Single-Unit District symbols and indications as shown on Map No. 5-G "
            "to those of a RT-4 Residential Two-Flat, Townhouse and Multi-Unit District."
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertEqual(from_zone, "RS-3 Residential Single-Unit District")
        self.assertEqual(to_zone, "RT-4 Residential Two-Flat, Townhouse and Multi-Unit District")

    def test_parse_from_to_fallback_pattern(self):
        text = "Rezone from B1-2 to B3-2 for parcel at issue."
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertEqual(from_zone, "B1-2")
        self.assertEqual(to_zone, "B3-2")

    def test_parse_rejects_junk_capture(self):
        text = "Section 1. the owner allowing the application to proceed."
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertIsNone(from_zone)
        self.assertIsNone(to_zone)

    def test_parse_ocr_artifact_canonicalization(self):
        text = (
            "Changing all of the Bl-I Neighborhood Shopping District symbols "
            "to those of a Ml-I Limited Manufacturing District."
        )
        detail = parse_from_to_zoning_detailed(text)
        self.assertEqual(detail["from_zoning_clean"], "Bl-I Neighborhood Shopping District")
        self.assertEqual(detail["to_zoning_clean"], "Ml-I Limited Manufacturing District")
        self.assertEqual(detail["from_zoning_canonical"], "BL-1")
        self.assertEqual(detail["to_zoning_canonical"], "ML-1")

    def test_parse_ocr_rt_decimal_with_spurious_one(self):
        text = (
            "Changing all of the ML-2 Limited Manufacturing District symbols "
            "to those of a R 13.5 Residential Two-Flat District."
        )
        detail = parse_from_to_zoning_detailed(text)
        self.assertEqual(detail["from_zoning_canonical"], "ML-2")
        self.assertEqual(detail["to_zoning_canonical"], "RT-3.5")

    def test_parse_sequence_fallback_when_to_missing(self):
        text = (
            "Section 1. changing all of the RT-4 Residential District B1-1 Neighborhood Shopping District "
            "symbols and indications as shown on Map No. 7-H."
        )
        detail = parse_from_to_zoning_detailed(text)
        self.assertEqual(detail["from_zoning_canonical"], "RT-4")
        self.assertEqual(detail["to_zoning_canonical"], "B1-1")
        self.assertEqual(detail["parse_quality"], 2)

    def test_parse_ocr_bz_code(self):
        text = (
            "Changing all of the B3-1 Community Shopping District symbols "
            "to those of a BZ-2 Neighborhood Mixed-Use District."
        )
        detail = parse_from_to_zoning_detailed(text)
        self.assertEqual(detail["to_zoning_canonical"], "B2-2")

    def test_parse_ocr_to_those_of_punctuation(self):
        text = (
            "SECTION 1. Title 17 is hereby amended by changing all the RS-3 Single-Unit "
            "Detached House District symbols and indications as shown on Map No. 3-I "
            "to those o!\" a RT-4 Residential Two-Fiat, Townhouse and Multi-Unit Disfrict. "
            "SECTION 2."
        )
        detail = parse_from_to_zoning_detailed(text)
        self.assertEqual(detail["from_zoning_canonical"], "RS-3")
        self.assertEqual(detail["to_zoning_canonical"], "RT-4")

    def test_parse_missing_symbols_word(self):
        text = (
            "SECTION 1. Title 17 is hereby amended by changing all of the B3-1 Community "
            "Shopping District, as shown on Map 5-I in the area bounded by West Fullerton "
            "Avenue, To those of RM-5 Multi-Unit District. SECTION 2."
        )
        detail = parse_from_to_zoning_detailed(text)
        self.assertEqual(detail["from_zoning_canonical"], "B3-1")
        self.assertEqual(detail["to_zoning_canonical"], "RM-5")

    def test_parse_ocr_lo_those_and_83_prefix(self):
        text = (
            "SECTION 1. Title 17 is amended by changing all ofthe District 83-2 Community "
            "Shopping District and indications as shown on Map No. 11-H in the area bounded "
            "by North Western Avenue; lo those of a B2-3 Neighborhood Mi.xed-Use District. "
            "SECTION 2."
        )
        detail = parse_from_to_zoning_detailed(text)
        self.assertEqual(detail["from_zoning_clean"], "B3-2 Community Shopping District")
        self.assertEqual(detail["to_zoning_canonical"], "B2-3")

    def test_parse_ocr_bz_without_suffix(self):
        text = (
            "SECTION 1. Title 17 is amended by changing all of the B3-1 Community Shopping "
            "District symbols and indications as shown on Map No. 12-M to those of an "
            "BZ\" [Neighborhood Mixed-Use District. SECTION 2."
        )
        detail = parse_from_to_zoning_detailed(text)
        self.assertEqual(detail["from_zoning_canonical"], "B3-1")
        self.assertEqual(detail["to_zoning_canonical"], "B2-1")

    def test_parse_current_zoning_designation_changed_to(self):
        text = (
            "The proposed residential use requires that the property's current zoning "
            "designation of CI-2 be changed to B2-3 mixed use district to allow the "
            "proposed dwelling units."
        )
        detail = parse_from_to_zoning_detailed(text)
        self.assertEqual(detail["from_zoning_canonical"], "C1-2")
        self.assertEqual(detail["to_zoning_canonical"], "B2-3")

    def test_parse_current_zoning_designation_to_that_of(self):
        text = (
            "The Applicants seek the change from the property's current zoning designation "
            "of CI-2 to that of a B2-3 mixed use district to allow conversion."
        )
        detail = parse_from_to_zoning_detailed(text)
        self.assertEqual(detail["from_zoning_canonical"], "C1-2")
        self.assertEqual(detail["to_zoning_canonical"], "B2-3")

    def test_parse_keeps_decimal_codes(self):
        text = (
            "Changing all of the RT-4.5 Residential Two-Flat District symbols "
            "to those of a RM-5.5 Residential Multi-Unit District."
        )
        detail = parse_from_to_zoning_detailed(text)
        self.assertEqual(detail["from_zoning_canonical"], "RT-4.5")
        self.assertEqual(detail["to_zoning_canonical"], "RM-5.5")

    def test_parse_rejects_address_from_to(self):
        text = (
            "Section 1. Change from 3535 N Ashland Ave to 3720 N Ashland Ave "
            "for the subject property."
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertIsNone(from_zone)
        self.assertIsNone(to_zone)

    def test_parse_rejects_garbled_ocr_code_sequence_without_change_context(self):
        text = (
            "¡ì,¡¡pfiçr;rl: capii llltrHll 1 Xildrrii 4,* I jpßh "
            "lilltrrni rv ç*rj I 42: R-1V ltr rttlirr i¡ltl FINAL FOR PUBLICATION"
        )
        detail = parse_from_to_zoning_detailed(text)
        self.assertIsNone(detail["from_zoning_canonical"])
        self.assertIsNone(detail["to_zoning_canonical"])
        self.assertEqual(detail["parse_quality"], 0)

    def test_parse_map_ref(self):
        self.assertEqual(parse_map_ref("Map No. 28-F"), "28-F")
        self.assertIsNone(parse_map_ref("No map in this text"))


if __name__ == "__main__":
    unittest.main()
