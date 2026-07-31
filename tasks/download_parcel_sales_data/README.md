# Download Parcel Sales

This task downloads 2006--2022 parcel sales for the eight Chicago townships
from the Cook County Assessor's parcel-sales dataset `wvhk-k5uv`. It keeps the
sale and transaction fields needed by the home-price analysis and writes
`output/parcel_sales_city.csv`.

The script downloads ordered batches and verifies the source row count before
keeping the file. `START_YEAR` and `END_YEAR` in the Makefile define the
requested period.
