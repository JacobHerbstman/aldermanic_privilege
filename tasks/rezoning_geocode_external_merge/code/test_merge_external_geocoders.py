import tempfile
import unittest
from pathlib import Path

import pandas as pd

from merge_external_geocoders import load_census_results, load_chicago_results, parse_lat_lon_text


class TestMergeExternalGeocoders(unittest.TestCase):
    def test_parse_point_wkt(self):
        lat, lon = parse_lat_lon_text("POINT (-87.645 41.923)")
        self.assertAlmostEqual(lat, 41.923)
        self.assertAlmostEqual(lon, -87.645)

    def test_parse_pair_lat_lon(self):
        lat, lon = parse_lat_lon_text("41.88,-87.63")
        self.assertAlmostEqual(lat, 41.88)
        self.assertAlmostEqual(lon, -87.63)

    def test_parse_pair_lon_lat(self):
        lat, lon = parse_lat_lon_text("-87.63,41.88")
        self.assertAlmostEqual(lat, 41.88)
        self.assertAlmostEqual(lon, -87.63)

    def test_load_chicago_results_row_order_mapping(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            base = Path(tmpdir)
            results_path = base / "chicago_results.csv"
            mapping_path = base / "mapping.csv"

            pd.DataFrame(
                [
                    {"address": "100 W TEST ST", "Latitude": "41.90", "Longitude": "-87.65"},
                    {"address": "200 W TEST ST", "Latitude": "41.91", "Longitude": "-87.66"},
                ]
            ).to_csv(results_path, index=False)

            pd.DataFrame(
                [
                    {"upload_row_number": 1, "external_row_id": "HIST_A", "upload_included": True},
                    {"upload_row_number": 2, "external_row_id": "HIST_B", "upload_included": True},
                ]
            ).to_csv(mapping_path, index=False)

            df, diag = load_chicago_results(str(results_path), str(mapping_path))

            self.assertEqual(len(df), 2)
            self.assertSetEqual(set(df["external_row_id"].tolist()), {"HIST_A", "HIST_B"})
            self.assertTrue(diag["mapping_attach"]["applied"])

    def test_load_chicago_results_prefers_id_when_present(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            base = Path(tmpdir)
            results_path = base / "chicago_results_with_id.csv"
            mapping_path = base / "mapping.csv"

            pd.DataFrame(
                [
                    {"external_row_id": "1001", "Latitude": "41.90", "Longitude": "-87.65"},
                    {"external_row_id": "", "Latitude": "41.91", "Longitude": "-87.66"},
                ]
            ).to_csv(results_path, index=False)

            pd.DataFrame(
                [
                    {"upload_row_number": 1, "external_row_id": "HIST_A", "upload_included": True},
                    {"upload_row_number": 2, "external_row_id": "HIST_B", "upload_included": True},
                ]
            ).to_csv(mapping_path, index=False)

            df, _ = load_chicago_results(str(results_path), str(mapping_path))
            rows = df.set_index("external_row_id").to_dict("index")

            self.assertIn("1001", rows)
            self.assertIn("HIST_B", rows)

    def test_load_census_results_excludes_no_match(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "census_results.csv"
            pd.DataFrame(
                [
                    {
                        "external_row_id": "A",
                        "match_status": "Match",
                        "longitude": "-87.63",
                        "latitude": "41.88",
                    },
                    {
                        "external_row_id": "B",
                        "match_status": "No_Match",
                        "longitude": "-87.64",
                        "latitude": "41.89",
                    },
                ]
            ).to_csv(path, index=False)

            df, _ = load_census_results(str(path))

            self.assertEqual(df["external_row_id"].tolist(), ["A"])


if __name__ == "__main__":
    unittest.main()
