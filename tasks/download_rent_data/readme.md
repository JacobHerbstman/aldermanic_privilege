# download_rent_data

Purpose: Downloads RentHub parquet files from Dewey when the rental pipeline is explicitly used.

Requires `DEWEY_API_KEY`.

Produces: Dewey parquet files in `output/`, plus `output/renthub_file_manifest.csv`, `output/renthub_download_metadata.csv`, and `output/download.done`.

Approx. runtime: ~10-60+ minutes.
