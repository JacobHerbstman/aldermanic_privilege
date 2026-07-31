# Download Building Permits

This task downloads City of Chicago building-permit records with application
dates from 2006 through 2022 from dataset `ydr8-5enu`. The script requests the
data in ordered batches and verifies the row count before keeping
`output/building_permits_2006_2022.csv`.

`START_YEAR` and `END_YEAR` in the Makefile define the requested period.
