## download_parcel_universe_data

Purpose: Downloads the City-triad 2025 slice of Cook County Assessor parcel universe data from the Cook County Open Data Socrata API.

The task uses the historical parcel-universe endpoint (`nj4t-kc8j`) rather than the current-year-only endpoint because the current-year-only endpoint advances over time. The output aliases refreshed API column names back to the column names expected by existing downstream tasks.
