import unittest

from assemble_zoning_tables import parse_from_to_zoning


class TestZoningParsers(unittest.TestCase):
    def test_parse_ofthe_variant(self):
        text = (
            "ORDINANCE BE IT ORDAINED BY THE CITY COUNCIL OF THE CITY OF CHICAGO: "
            "SECTION 1. The Chicago Zoning Ordinance, be amended by changing all ofthe "
            "B3-2, Community Shopping District symbols and indications as shown on Map No. 4-J "
            "in the area bounded by: ... to those of a B2-1, Neighborhood Mixed-Use District. "
            "SECTION 2. This ordinance shall be effective after its passage."
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertEqual(from_zone, "B3-2, Community Shopping District")
        self.assertEqual(to_zone, "B2-1, Neighborhood Mixed-Use District")

    def test_parse_all_the_variant(self):
        text = (
            "That the Chicago Zoning Ordinance be amended by changing all the "
            "M2-3 Light Industry District symbols and indications as shown on Map No. 10-F "
            "in the area bounded by ... to those of a RS3 Residential Single-Unit (Detached House) District. "
            "SECTION 2. This Ordinance shall be in force and effect."
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertEqual(from_zone, "M2-3 Light Industry District")
        self.assertEqual(to_zone, "RS-3 Residential Single-Unit (Detached House) District")

    def test_parse_zoning_change_from_to(self):
        text = (
            "Project Description: Zoning Change from an Ml-1 Limited Manufacturing/Business Park District "
            "to a B2-2 Neighborhood Mixed-Use District for adaptive reuse of the existing building."
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertEqual(from_zone, "ML-1 Limited Manufacturing/Business Park District")
        self.assertEqual(to_zone, "B2-2 Neighborhood Mixed-Use District")

    def test_ignores_application_phrase_only(self):
        text = (
            "If the Applicant is not the owner of the property, please provide written authorization "
            "from the owner allowing the applicant to proceed."
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertIsNone(from_zone)
        self.assertIsNone(to_zone)

    def test_prefers_ordinance_before_application_form(self):
        text = (
            "The Chicago Zoning Ordinance is hereby amended by changing all ofthe "
            "RS-3 Residential Single-Unit District symbols and indications ... to those of a "
            "RT-4 Residential Two-Flat, Townhouse and Multi-Unit District. SECTION 2. "
            "APPLICATION FOR AN AMENDMENT TO THE CHICAGO ZONING ORDINANCE. "
            "If the applicant is not the owner, attach authorization from the owner allowing the applicant to proceed."
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertEqual(from_zone, "RS-3 Residential Single-Unit District")
        self.assertEqual(to_zone, "RT-4 Residential Two-Flat, Townhouse and Multi-Unit District")

    def test_parse_ocr_to_those_of_punctuation(self):
        text = (
            "SECTION 1. Title 17 is hereby amended by changing all the RS-3 Single-Unit "
            "Detached House District symbols and indications as shown on Map No. 3-I "
            "to those o!\" a RT-4 Residential Two-Fiat, Townhouse and Multi-Unit Disfrict "
            "and a corresponding use district is hereby established. SECTION 2."
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertEqual(from_zone, "RS-3 Single-Unit Detached House District")
        self.assertEqual(to_zone, "RT-4 Residential Two-Fiat, Townhouse and Multi-Unit Disfrict")

    def test_parse_missing_symbols_word(self):
        text = (
            "SECTION 1. Title 17 is hereby amended by changing all of the B3-1 Community "
            "Shopping District, as shown on Map 5-I in the area bounded by West Fullerton "
            "Avenue, To those of RM-5 Multi-Unit District. SECTION 2."
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertEqual(from_zone, "B3-1 Community Shopping District")
        self.assertEqual(to_zone, "RM-5 Multi-Unit District")

    def test_parse_ocr_lo_those_and_83_prefix(self):
        text = (
            "SECTION 1. Title 17 is amended by changing all ofthe District 83-2 Community "
            "Shopping District and indications as shown on Map No. 11-H in the area bounded "
            "by North Western Avenue; lo those of a B2-3 Neighborhood Mi.xed-Use District. "
            "SECTION 2."
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertEqual(from_zone, "B3-2 Community Shopping District")
        self.assertEqual(to_zone, "B2-3 Neighborhood Mi.xed-Use District")

    def test_parse_ocr_bz_without_suffix(self):
        text = (
            "SECTION 1. Title 17 is amended by changing all of the B3-1 Community Shopping "
            "District symbols and indications as shown on Map No. 12-M to those of an "
            "BZ\" [Neighborhood Mixed-Use District. SECTION 2."
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertEqual(from_zone, "B3-1 Community Shopping District")
        self.assertEqual(to_zone, "B2-1 [Neighborhood Mixed-Use District")

    def test_parse_current_zoning_designation_changed_to(self):
        text = (
            "The proposed residential use requires that the property's current zoning "
            "designation of CI-2 be changed to B2-3 mixed use district to allow the "
            "proposed dwelling units."
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertEqual(from_zone, "CI-2")
        self.assertEqual(to_zone, "B2-3 mixed use district")

    def test_parse_current_zoning_designation_to_that_of(self):
        text = (
            "The Applicants seek the change from the property's current zoning designation "
            "of CI-2 to that of a B2-3 mixed use district to allow conversion."
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertEqual(from_zone, "CI-2")
        self.assertEqual(to_zone, "B2-3 mixed use district")

    def test_rejects_garbled_ocr_code_sequence_without_change_context(self):
        text = (
            "¡ì,¡¡pfiçr;rl: capii llltrHll 1 Xildrrii 4,* I jpßh "
            "lilltrrni rv ç*rj I 42: R-1V ltr rttlirr i¡ltl FINAL FOR PUBLICATION"
        )
        from_zone, to_zone = parse_from_to_zoning(text)
        self.assertIsNone(from_zone)
        self.assertIsNone(to_zone)


if __name__ == "__main__":
    unittest.main()
