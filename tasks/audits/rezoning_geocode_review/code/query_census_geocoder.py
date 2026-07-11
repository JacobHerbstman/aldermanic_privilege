import sys
import time

import pandas as pd
import requests


if len(sys.argv) != 4:
    raise SystemExit("Usage: query_census_geocoder.py <date_tag> <sleep_seconds> <timeout_seconds>")

date_tag = sys.argv[1]
sleep_seconds = float(sys.argv[2])
timeout_seconds = float(sys.argv[3])

unmatched = pd.read_csv(
    f"../input/rezoning_geocode_stage1_unmatched_{date_tag}.csv",
    dtype=str,
    low_memory=False,
)
frozen = pd.read_csv(
    f"../input/rezoning_external_geocodes_{date_tag}.csv",
    dtype=str,
    low_memory=False,
)

if unmatched["external_row_id"].fillna("").duplicated().any():
    raise ValueError("Unmatched geocode input is not unique by external_row_id.")
if frozen["external_row_id"].fillna("").duplicated().any():
    raise ValueError("Frozen external geocodes are not unique by external_row_id.")

queries = unmatched[~unmatched["external_row_id"].isin(frozen["external_row_id"])].copy()
queries["query_address"] = queries["address_for_external"].fillna("").str.strip()
queries = queries[
    (queries["query_address"] != "")
    & (~queries["query_address"].str.lower().isin({"nan, chicago, il", "none, chicago, il"}))
].copy()

records = []
for row in queries.itertuples(index=False):
    record = {
        "external_row_id": row.external_row_id,
        "matter_id": row.matter_id,
        "query_address": row.query_address,
        "query_status": "request_error",
        "match_count": 0,
        "matched_address": None,
        "latitude": None,
        "longitude": None,
    }

    try:
        response = requests.get(
            "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress",
            params={
                "address": row.query_address,
                "benchmark": "Public_AR_Current",
                "format": "json",
            },
            timeout=timeout_seconds,
        )
        response.raise_for_status()
        matches = response.json().get("result", {}).get("addressMatches", [])
        record["match_count"] = len(matches)
        if len(matches) == 1:
            coordinates = matches[0].get("coordinates", {})
            record.update(
                query_status="single_match",
                matched_address=matches[0].get("matchedAddress"),
                latitude=coordinates.get("y"),
                longitude=coordinates.get("x"),
            )
        elif len(matches) == 0:
            record["query_status"] = "no_match"
        else:
            record["query_status"] = "multiple_matches"
    except Exception as exc:  # noqa: BLE001
        record["error"] = str(exc)

    records.append(record)
    time.sleep(sleep_seconds)

output = pd.DataFrame(records)
output.to_csv(f"../output/census_geocoder_review_{date_tag}.csv", index=False)

print(output["query_status"].value_counts(dropna=False).to_string())
