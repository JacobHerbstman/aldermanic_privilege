import unittest

from build_chicago_upload_workbook import (
    classify_upload_address,
    normalize_upload_address,
    title_fallback_candidates,
)


class TestBuildChicagoUploadWorkbook(unittest.TestCase):
    def test_normalize_upload_address_fixes_ocr(self):
        out = normalize_upload_address("2900 North PauUna Street")
        self.assertEqual(out, "2900 North PAULINA Street, Chicago, IL")

    def test_classify_soft_street_address(self):
        ok, reason = classify_upload_address("5943-5959 N Broadway, Chicago, IL")
        self.assertTrue(ok)
        self.assertIn(reason, {"street_address", "street_address_soft"})

    def test_normalize_upload_address_strips_app_number_suffix(self):
        out = normalize_upload_address("812 N Western Ave App No A-8842")
        self.assertEqual(out, "812 N Western Ave, Chicago, IL")

    def test_reject_suffix_only_soft_street(self):
        ok, reason = classify_upload_address("99 Avenue, Chicago, IL")
        self.assertFalse(ok)
        self.assertEqual(reason, "low_quality_street_name")

    def test_title_fallback_extracts_segment_from_multi_address(self):
        title = (
            "Zoning Reclassification Map No. 3-E at 400-420 W Huron St and 700-708 N Sedgwick St - App No. 18162"
        )
        candidates = title_fallback_candidates(title)
        addresses = [addr for addr, _ in candidates]
        self.assertTrue(any("400-420 W Huron St, Chicago, IL".lower() in a.lower() for a in addresses))


if __name__ == "__main__":
    unittest.main()
