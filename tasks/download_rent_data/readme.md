# Download RentHub Listings

This task downloads the Illinois RentHub files used by the rental analysis.

It requires `DEWEY_API_KEY`. The default download covers January 2014 through
December 2022, matching the paper sample. `START_DATE` and `END_DATE` in the
Makefile can be used to request a different period.

The Dewey parquet files and `output/renthub_manifest.csv` are written to
`output/`. The manifest records the source date, modification time, byte size,
and local hash for every requested file, so a changed or incomplete download
cannot silently pass to the cleaning task.
