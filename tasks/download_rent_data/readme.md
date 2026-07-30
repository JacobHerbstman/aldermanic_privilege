# download_rent_data

Purpose: Downloads the Illinois RentHub parquet files used by the rental analysis.

Requires `DEWEY_API_KEY`. The default download covers January 2014 through December 2022, matching the paper sample. The dates can be changed with `START_DATE` and `END_DATE` in the Makefile.

Produces: Dewey parquet files in `output/` and `output/renthub_manifest.csv`. The manifest records the source date, source modification time, byte size, and local hash for every requested file.

Approx. runtime: ~10-60+ minutes.
