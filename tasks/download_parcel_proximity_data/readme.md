## download_parcel_proximity_data

Purpose: Downloads the 2023 Cook County Assessor parcel proximity file from the Cook County Open Data Socrata API.

The task selects the legacy columns used by the existing paper workflow. The live API now includes additional proximity fields, but this output preserves the old schema for reproducible downstream builds.
