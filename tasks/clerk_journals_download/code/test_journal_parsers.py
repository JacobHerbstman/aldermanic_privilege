import unittest

import pandas as pd

from journal_parsers import (
    assign_collision_suffixes,
    build_base_filename,
    dedupe_manifest,
    parse_meeting_date_iso,
    parse_meeting_type,
    parse_zoning_section_page,
    status_from_outcome,
)


class TestJournalParsers(unittest.TestCase):
    def test_title_parsing_new_and_old_formats(self) -> None:
        new_title = "Journal of the Proceedings 12/29/2025 - Regular Meeting"
        self.assertEqual(parse_meeting_date_iso(new_title), "2025-12-29")
        new_raw, new_norm = parse_meeting_type(new_title)
        self.assertEqual(new_raw, "Regular Meeting")
        self.assertEqual(new_norm, "regular")

        old_title = "Journal Of The Proceedings 1/12/1977"
        self.assertEqual(parse_meeting_date_iso(old_title), "1977-01-12")
        old_raw, old_norm = parse_meeting_type(old_title)
        self.assertIsNone(old_raw)
        self.assertEqual(old_norm, "other")

    def test_filename_generation_with_collision_suffixes(self) -> None:
        base = build_base_filename("1977-01-12", "regular", "fallback_file")
        names = assign_collision_suffixes([base, base, base])
        self.assertEqual(names[0], "1977_01_12_regular.pdf")
        self.assertEqual(names[1], "1977_01_12_regular_v2.pdf")
        self.assertEqual(names[2], "1977_01_12_regular_v3.pdf")

    def test_toc_parser_extracts_zoning_page(self) -> None:
        toc = """
        TABLE OF CONTENTS
        Committee Reports ..................................... 44
        Zoning Ordinance Amendments .......................... 245
        New Business ......................................... 301
        """
        self.assertEqual(parse_zoning_section_page(toc), 245)

    def test_meeting_type_fallback(self) -> None:
        title = "Journal of the Proceedings 03/10/1999"
        _, norm = parse_meeting_type(title)
        self.assertEqual(norm, "other")

    def test_deduping_by_year_and_pdf_url(self) -> None:
        manifest = pd.DataFrame(
            [
                {"year": 1977, "pdf_url": "https://example.com/a.pdf", "document_title": "A"},
                {"year": 1977, "pdf_url": "https://example.com/a.pdf", "document_title": "A duplicate"},
                {"year": 2009, "pdf_url": "https://example.com/b.pdf", "document_title": "B"},
            ]
        )
        deduped = dedupe_manifest(manifest)
        self.assertEqual(len(deduped), 2)

    def test_download_status_mapping(self) -> None:
        self.assertEqual(status_from_outcome(http_status=403, failed=True), "http_403")
        self.assertEqual(status_from_outcome(http_status=404, failed=True), "http_404")
        self.assertEqual(status_from_outcome(timed_out=True), "timeout")
        self.assertEqual(status_from_outcome(non_pdf=True), "non_pdf")
        self.assertEqual(status_from_outcome(corrupt=True), "corrupt")
        self.assertEqual(status_from_outcome(downloaded=True), "downloaded")
        self.assertEqual(status_from_outcome(exists_valid=True), "exists_valid")

    def test_sample_integration_sanity_fixture(self) -> None:
        sample = pd.DataFrame(
            [
                {"year": 1977, "pdf_url": "https://example.com/a.pdf"},
                {"year": 2009, "pdf_url": "https://example.com/b.pdf"},
            ]
        )
        deduped = dedupe_manifest(sample)
        counts = deduped.groupby("year").size().to_dict()
        self.assertGreater(len(deduped), 0)
        self.assertEqual(counts.get(1977), 1)
        self.assertEqual(counts.get(2009), 1)


if __name__ == "__main__":
    unittest.main()
