# Assessed values

`download_assessed_values.R` downloads the Cook County Assessor assessed values
(Socrata `uzyt-m557`) for every parcel in the Chicago townships 70–77, every
class and every tax year 1999–2026, and saves `output/assessed_values.parquet`
(one row per 14-digit PIN and year) with its data report.

Each township-year is one request, and its row count must equal the API's
count; the download stops if the source was updated while it ran. Values are
in three stages: mailed, certified after Assessor appeals, and after Board of
Review appeals. The latest tax year is incomplete while its values are being
mailed and appealed.

This is a live download: the Assessor revises these records about twice a
month, so a later run can differ from the recorded report. Run `make` in
`code/`.
