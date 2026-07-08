import argparse
import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch
from urllib.parse import parse_qs, urlparse

import download_parcel_addresses as module


class FakeResponse:
    def __init__(self, *, text=None, json_data=None, status_code=200):
        self._text = text
        self._json_data = json_data
        self.status_code = status_code

    def raise_for_status(self):
        if self.status_code >= 400:
            raise RuntimeError(f"HTTP {self.status_code}")

    def json(self):
        return self._json_data

    def iter_content(self, chunk_size=1024 * 1024):
        payload = (self._text or "").encode("utf-8")
        for i in range(0, len(payload), chunk_size):
            yield payload[i : i + chunk_size]

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        return False


class DownloadParcelAddressesTests(unittest.TestCase):
    @staticmethod
    def _fixture(path):
        return Path(__file__).parent / "smoke" / path

    @staticmethod
    def _make_args(out_csv, min_expected_rows=1):
        return argparse.Namespace(
            dataset_id="fixture-id",
            domain="fixture.test",
            batch_size=2,
            order_by="pin",
            max_batches=0,
            where_clause=None,
            request_timeout_seconds=5,
            min_expected_rows=min_expected_rows,
            out_csv=out_csv,
        )

    def test_download_paginated_uses_single_strategy(self):
        fixture_count = json.loads(self._fixture("count_ok.json").read_text(encoding="utf-8"))
        fixture_0 = self._fixture("batch_00000.csv").read_text(encoding="utf-8")
        fixture_1 = self._fixture("batch_00001.csv").read_text(encoding="utf-8")
        fixture_2 = self._fixture("batch_00002.csv").read_text(encoding="utf-8")

        def fake_get(url, timeout=None, stream=False):
            if ".json" in url:
                return FakeResponse(json_data=fixture_count)

            query = parse_qs(urlparse(url).query)
            offset = int(query.get("$offset", [0])[0])
            if offset == 0:
                return FakeResponse(text=fixture_0)
            if offset == 2:
                return FakeResponse(text=fixture_1)
            if offset == 4:
                return FakeResponse(text=fixture_2)
            return FakeResponse(text="pin,address\n")

        with tempfile.TemporaryDirectory() as tmpdir:
            out_csv = Path(tmpdir) / "download.csv"
            with patch("download_parcel_addresses.requests.get", side_effect=fake_get):
                result = module.download_paginated(
                    domain="fixture.test",
                    dataset_id="fixture-id",
                    batch_size=2,
                    order_by="pin",
                    timeout=5,
                    max_batches=0,
                    where_clause=None,
                    out_path=str(out_csv),
                )

            self.assertEqual(result["strategy"], "paginated_api")
            self.assertEqual(result["rows"], 3)
            rows = module.count_csv_rows(str(out_csv))
            self.assertEqual(rows, 3)

    def test_main_success(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            out_csv = Path(tmpdir) / "parcel.csv"

            def fake_download_paginated(**kwargs):
                Path(kwargs["out_path"]).write_text(
                    "pin,address\n1,alpha\n2,beta\n3,gamma\n",
                    encoding="utf-8",
                )
                return {"rows": 3}

            with patch("download_parcel_addresses.parse_args", return_value=self._make_args(str(out_csv), min_expected_rows=3)):
                with patch("download_parcel_addresses.query_row_count_with_where", return_value=3):
                    with patch("download_parcel_addresses.download_paginated", side_effect=fake_download_paginated):
                        code = module.main()

            self.assertEqual(code, 0)
            self.assertTrue(out_csv.exists())
            self.assertEqual(module.count_csv_rows(str(out_csv)), 3)

    def test_main_fails_when_below_count_query(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            out_csv = Path(tmpdir) / "parcel.csv"

            def fake_download_paginated(**kwargs):
                Path(kwargs["out_path"]).write_text("pin,address\n1,alpha\n", encoding="utf-8")
                return {"rows": 1}

            with patch("download_parcel_addresses.parse_args", return_value=self._make_args(str(out_csv), min_expected_rows=1)):
                with patch("download_parcel_addresses.query_row_count_with_where", return_value=2):
                    with patch("download_parcel_addresses.download_paginated", side_effect=fake_download_paginated):
                        code = module.main()

            self.assertEqual(code, 1)
            self.assertFalse(out_csv.exists())

    def test_main_fails_when_below_min_expected(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            out_csv = Path(tmpdir) / "parcel.csv"

            def fake_download_paginated(**kwargs):
                Path(kwargs["out_path"]).write_text(
                    "pin,address\n1,alpha\n2,beta\n3,gamma\n",
                    encoding="utf-8",
                )
                return {"rows": 3}

            with patch("download_parcel_addresses.parse_args", return_value=self._make_args(str(out_csv), min_expected_rows=4)):
                with patch("download_parcel_addresses.query_row_count_with_where", return_value=None):
                    with patch("download_parcel_addresses.download_paginated", side_effect=fake_download_paginated):
                        code = module.main()

            self.assertEqual(code, 1)
            self.assertFalse(out_csv.exists())


if __name__ == "__main__":
    unittest.main()
