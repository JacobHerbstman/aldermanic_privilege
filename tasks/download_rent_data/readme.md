# download_rent_data

Purpose: Downloads the customized Illinois RentHub parquet export from Dewey when the rental pipeline is explicitly used.

Requires `DEWEY_API_KEY`. Uses DeweyPy `speedy-download` when `uvx` is available, otherwise falls back to `deweydatar`. Set `DEWEY_DOWNLOAD_METHOD=deweypy` or `DEWEY_DOWNLOAD_METHOD=deweydatar` to force one path.

Produces: Dewey parquet files in `output/`, plus file manifest, schema audit, local file audit, download metadata, and `output/download.done`.

Approx. runtime: ~10-60+ minutes.
