import json
import subprocess
import tempfile
import unittest
from pathlib import Path

import pandas as pd

from build_rezoning_dataset import (
    MODERN_COLUMNS,
    build_finalized_outputs,
    build_harmonized_dataset,
    dedupe_historical_rows,
    filter_historical_rows,
)


def make_modern_row(matter_id: str) -> dict:
    row = {col: None for col in MODERN_COLUMNS}
    row["matter_id"] = matter_id
    row["matter_title"] = "Modern rezoning"
    row["matter_intro_date"] = "2011-01-05"
    row["matter_passed_date"] = "2011-02-01"
    row["from_zoning"] = "B1-1"
    row["to_zoning"] = "B3-2"
    return row


class TestRezoningDataset(unittest.TestCase):
    def test_filter_historical_cutoff_and_code_requirements(self):
        df = pd.DataFrame(
            [
                {
                    "journal_year": "2009",
                    "journal_meeting_date": "2009-04-01",
                    "has_from_to": "TRUE",
                    "map_no": "2-H",
                    "from_code": "B1-1",
                    "to_code": "B2-2",
                },
                {
                    "journal_year": "2010",
                    "journal_meeting_date": "2010-11-30",
                    "has_from_to": "TRUE",
                    "map_no": "3-H",
                    "from_code": "B1-2",
                    "to_code": "B2-3",
                },
                {
                    "journal_year": "2010",
                    "journal_meeting_date": "2010-12-01",
                    "has_from_to": "TRUE",
                    "map_no": "4-H",
                    "from_code": "B1-3",
                    "to_code": "B2-4",
                },
                {
                    "journal_year": "2010",
                    "journal_meeting_date": "",
                    "has_from_to": "TRUE",
                    "map_no": "5-H",
                    "from_code": "B1-4",
                    "to_code": "B2-5",
                },
                {
                    "journal_year": "2008",
                    "journal_meeting_date": "2008-03-01",
                    "has_from_to": "FALSE",
                    "map_no": "6-H",
                    "from_code": "B1-1",
                    "to_code": "B2-1",
                },
                {
                    "journal_year": "2008",
                    "journal_meeting_date": "2008-03-01",
                    "has_from_to": "TRUE",
                    "map_no": "7-H",
                    "from_code": "",
                    "to_code": "B2-1",
                },
            ]
        )

        kept, diag = filter_historical_rows(df)
        self.assertEqual(len(kept), 2)
        self.assertEqual(diag["historical_rows_2010_post_cutoff_dropped"], 1)
        self.assertEqual(diag["historical_rows_2010_missing_date_dropped"], 1)
        self.assertEqual(diag["historical_rows_missing_from_or_to_code_dropped"], 1)

    def test_dedupe_prefers_strict_address_then_priority(self):
        raw = pd.DataFrame(
            [
                {
                    "journal_year": "2009",
                    "journal_meeting_date": "2009-04-01",
                    "has_from_to": "TRUE",
                    "map_no": "2-H",
                    "from_code": "B1-1",
                    "to_code": "B2-2",
                    "match_type": "ordinance_reclass",
                    "page_number": "20",
                    "addresses_strict": "",
                    "addresses_loose": "100 W TEST ST",
                    "snippet": "short",
                },
                {
                    "journal_year": "2009",
                    "journal_meeting_date": "2009-04-01",
                    "has_from_to": "TRUE",
                    "map_no": "2-H",
                    "from_code": "B1-1",
                    "to_code": "B2-2",
                    "match_type": "heuristic_reclass",
                    "page_number": "30",
                    "addresses_strict": "100 W TEST ST",
                    "addresses_loose": "",
                    "snippet": "longer snippet content",
                },
            ]
        )
        filtered, _ = filter_historical_rows(raw)
        deduped, diag = dedupe_historical_rows(filtered)

        self.assertEqual(len(deduped), 1)
        self.assertEqual(diag["historical_rows_deduped_out"], 1)
        self.assertEqual(deduped.iloc[0]["match_type"], "heuristic_reclass")
        self.assertEqual(deduped.iloc[0]["first_address_strict"], "100 W TEST ST")

    def test_build_harmonized_dataset_address_fallback_and_ids(self):
        modern = make_modern_row("12345")
        modern["from_zoning_canonical"] = "B1-1"
        modern_df = pd.DataFrame([modern])
        historical_df = pd.DataFrame(
            [
                {
                    "journal_year": "2009",
                    "journal_meeting_date": "2009-02-18",
                    "journal_filename": "journal.pdf",
                    "journal_rel_local_path": "2009/journal.pdf",
                    "journal_pdf_url": "https://example.com/journal.pdf",
                    "page_number": "5",
                    "text_source": "native",
                    "match_type": "committee_reclass",
                    "map_no": "8-G",
                    "from_district_raw": "B1-1 District",
                    "to_district_raw": "B3-2 District",
                    "has_from_to": "TRUE",
                    "from_code": "B1-1",
                    "to_code": "B3-2",
                    "from_far_lookup": "1.2",
                    "to_far_lookup": "3.0",
                    "far_change_lookup": "1.8",
                    "addresses_strict": "",
                    "addresses_loose": "1756 W Cornelia Ave",
                    "snippet": "Map No. 8-G at 1756 W Cornelia Ave",
                }
            ]
        )

        combined, summary = build_harmonized_dataset(modern_df=modern_df, historical_df=historical_df)
        self.assertEqual(len(combined), 2)
        self.assertEqual(summary["combined_rows"], 2)
        self.assertEqual(summary["historical_rows_kept"], 1)
        self.assertEqual(summary["historical_rows_using_loose_address_fallback"], 1)
        self.assertEqual(summary["combined_duplicate_matter_id_rows"], 0)

        hist = combined[combined["record_source"] == "clerk_journal_first_pass"].iloc[0]
        modern_row = combined[combined["record_source"] == "elms"].iloc[0]
        self.assertTrue(str(hist["matter_id"]).startswith("HISTFP_"))
        self.assertEqual(hist["address_raw"], "1756 W Cornelia Ave")
        self.assertEqual(hist["address_raw_loose"], "1756 W Cornelia Ave")
        self.assertEqual(hist["journal_from_code"], "B1-1")
        self.assertEqual(hist["journal_to_code"], "B3-2")
        self.assertIn("from_zoning_canonical", combined.columns)
        self.assertEqual(modern_row["from_zoning_canonical"], "B1-1")
        self.assertTrue(pd.isna(hist["from_zoning_canonical"]))

    def test_finalize_usability_and_exclusion_reason(self):
        df = pd.DataFrame(
            [
                {
                    "matter_id": "A",
                    "matter_intro_date": "2020-01-01",
                    "far_change": 1.0,
                    "latitude": 41.9,
                    "longitude": -87.6,
                },
                {
                    "matter_id": "B",
                    "matter_intro_date": "2020-01-02",
                    "far_change": None,
                    "latitude": 41.9,
                    "longitude": -87.6,
                },
                {
                    "matter_id": "C",
                    "matter_intro_date": "2020-01-03",
                    "far_change": 0.5,
                    "latitude": None,
                    "longitude": -87.6,
                },
                {
                    "matter_id": "D",
                    "matter_intro_date": "2020-01-04",
                    "far_change": None,
                    "latitude": None,
                    "longitude": None,
                },
            ]
        )

        final_all, final_usable, summary = build_finalized_outputs(df)
        reasons = final_all.set_index("matter_id")["analysis_exclusion_reason"].to_dict()
        flags = final_all.set_index("matter_id")["is_usable_analysis"].to_dict()

        self.assertTrue(flags["A"])
        self.assertEqual(reasons["A"], "usable")
        self.assertFalse(flags["B"])
        self.assertEqual(reasons["B"], "missing_far_change")
        self.assertFalse(flags["C"])
        self.assertEqual(reasons["C"], "missing_latlon")
        self.assertFalse(flags["D"])
        self.assertEqual(reasons["D"], "missing_far_change_and_latlon")
        self.assertListEqual(final_usable["matter_id"].tolist(), ["A"])
        self.assertEqual(summary["total_rows_all"], 4)

    def test_finalize_usable_exact_filter_and_sorting(self):
        df = pd.DataFrame(
            [
                {"matter_id": "3", "matter_intro_date": "2020-01-02", "far_change": 1, "latitude": 1, "longitude": 1},
                {"matter_id": "1", "matter_intro_date": "2020-01-01", "far_change": 1, "latitude": 1, "longitude": 1},
                {"matter_id": "2", "matter_intro_date": "2020-01-01", "far_change": 1, "latitude": 1, "longitude": 1},
                {"matter_id": "4", "matter_intro_date": "2020-01-03", "far_change": None, "latitude": 1, "longitude": 1},
            ]
        )

        final_all, final_usable, _ = build_finalized_outputs(df)
        expected = final_all[final_all["is_usable_analysis"]].reset_index(drop=True)

        self.assertListEqual(final_all["matter_id"].tolist(), ["1", "2", "3", "4"])
        self.assertListEqual(final_usable["matter_id"].tolist(), expected["matter_id"].tolist())

    def test_finalize_record_source_normalization(self):
        df = pd.DataFrame(
            [
                {
                    "matter_id": "1",
                    "matter_intro_date": "2021-01-01",
                    "far_change": 1,
                    "latitude": 41.9,
                    "longitude": -87.6,
                    "record_source": "legistar",
                }
            ]
        )
        final_all, _, _ = build_finalized_outputs(df)
        self.assertEqual(final_all.loc[0, "record_source"], "elms")

    def test_finalize_assigns_alderman_for_geocoded_ward_month(self):
        df = pd.DataFrame(
            [
                {
                    "matter_id": "A",
                    "matter_intro_date": "7/16/25",
                    "far_change": 1.0,
                    "latitude": 41.9,
                    "longitude": -87.6,
                    "ward": "24.0",
                },
                {
                    "matter_id": "B",
                    "matter_intro_date": "2025-07-10",
                    "far_change": 1.0,
                    "latitude": None,
                    "longitude": -87.6,
                    "ward": "24",
                },
                {
                    "matter_id": "C",
                    "matter_intro_date": "2025-07-10",
                    "far_change": 1.0,
                    "latitude": 41.9,
                    "longitude": -87.6,
                    "ward": None,
                },
                {
                    "matter_id": "D",
                    "matter_intro_date": "not a date",
                    "far_change": 1.0,
                    "latitude": 41.9,
                    "longitude": -87.6,
                    "ward": "24",
                },
            ]
        )
        panel = pd.DataFrame(
            [
                {"ward": "24", "year_month": "2025-07", "alderman": "Monique Scott"},
                {"ward": "24", "year_month": "2025-08", "alderman": "Monique Scott"},
            ]
        )

        final_all, _, summary = build_finalized_outputs(df, alderman_panel_df=panel)
        row_a = final_all.set_index("matter_id").loc["A"]
        row_b = final_all.set_index("matter_id").loc["B"]
        row_c = final_all.set_index("matter_id").loc["C"]
        row_d = final_all.set_index("matter_id").loc["D"]

        self.assertEqual(row_a["assigned_alderman"], "Monique Scott")
        self.assertEqual(row_a["assigned_alderman_month"], "2025-07")
        self.assertEqual(row_a["assigned_alderman_date_source"], "matter_intro_date")
        self.assertTrue(pd.isna(row_b["assigned_alderman"]))
        self.assertTrue(pd.isna(row_c["assigned_alderman"]))
        self.assertTrue(pd.isna(row_d["assigned_alderman"]))
        self.assertEqual(summary["alderman_assignment_eligible_rows"], 1)
        self.assertEqual(summary["alderman_assigned_rows"], 1)
        self.assertEqual(summary["alderman_unassigned_rows_eligible"], 0)

    def test_finalize_cli_requires_panel_and_reports_not_run(self):
        with tempfile.TemporaryDirectory() as td:
            td_path = Path(td)
            enriched_csv = td_path / "enriched.csv"
            panel_csv = td_path / "panel.csv"
            all_csv = td_path / "all.csv"
            usable_csv = td_path / "usable.csv"
            summary_json = td_path / "summary.json"

            pd.DataFrame(
                [
                    {
                        "matter_id": "A",
                        "matter_intro_date": "2025-07-16",
                        "far_change": 1.0,
                        "latitude": 41.9,
                        "longitude": -87.6,
                        "ward": 24,
                    }
                ]
            ).to_csv(enriched_csv, index=False)
            pd.DataFrame(
                [{"ward": 24, "year_month": "2025-07", "alderman": "Monique Scott"}]
            ).to_csv(panel_csv, index=False)

            subprocess.run(
                [
                    "python3",
                    "build_rezoning_dataset.py",
                    "--mode",
                    "finalize",
                    "--in-enriched-csv",
                    str(enriched_csv),
                    "--in-alderman-panel-csv",
                    str(panel_csv),
                    "--out-all-csv",
                    str(all_csv),
                    "--out-usable-csv",
                    str(usable_csv),
                    "--out-summary-json",
                    str(summary_json),
                ],
                check=True,
                cwd=Path(__file__).resolve().parent,
            )

            payload = json.loads(summary_json.read_text(encoding="utf-8"))
            self.assertEqual(payload["run_mode"], "finalize")
            self.assertEqual(payload["finalize"]["status"], "ok")
            self.assertEqual(payload["harmonize"]["status"], "not_run")
            self.assertEqual(payload["finalize"]["alderman_assigned_rows"], 1)

    def test_summary_sections_not_run_by_mode(self):
        with tempfile.TemporaryDirectory() as td:
            td_path = Path(td)
            modern_csv = td_path / "modern.csv"
            historical_csv = td_path / "hist.csv"
            harmonized_csv = td_path / "harm.csv"
            summary_json = td_path / "summary.json"

            pd.DataFrame([make_modern_row("123")]).to_csv(modern_csv, index=False)
            pd.DataFrame(
                [
                    {
                        "journal_year": "2009",
                        "journal_meeting_date": "2009-02-18",
                        "has_from_to": "TRUE",
                        "map_no": "8-G",
                        "from_code": "B1-1",
                        "to_code": "B3-2",
                        "from_district_raw": "B1-1 District",
                        "to_district_raw": "B3-2 District",
                    }
                ]
            ).to_csv(historical_csv, index=False)

            subprocess.run(
                [
                    "python3",
                    "build_rezoning_dataset.py",
                    "--mode",
                    "harmonize",
                    "--in-modern-csv",
                    str(modern_csv),
                    "--in-historical-csv",
                    str(historical_csv),
                    "--out-harmonized-csv",
                    str(harmonized_csv),
                    "--out-summary-json",
                    str(summary_json),
                ],
                check=True,
                cwd=Path(__file__).resolve().parent,
            )

            payload = json.loads(summary_json.read_text(encoding="utf-8"))
            self.assertEqual(payload["run_mode"], "harmonize")
            self.assertEqual(payload["harmonize"]["status"], "ok")
            self.assertEqual(payload["finalize"]["status"], "not_run")


if __name__ == "__main__":
    unittest.main()
