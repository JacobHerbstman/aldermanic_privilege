# rental_benchmark_table

Purpose: Builds the paper-facing listed-rent benchmark table comparing the cleaned rental panel to Zillow ZORI.

Inputs:
- `process_rent_data/output/chicago_rent_panel.parquet`
- `download_fred_cpi/output/fred_cpi_cuura207sa0.csv`
- `download_zillow_zori/output/zillow_zori_city.csv`

Produces:
- `output/rental_benchmark_growth.tex`

Approx. runtime: ~1-5 minutes.
