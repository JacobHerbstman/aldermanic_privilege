.PHONY: all link-inputs

all: ../output/renthub_dwellsy_benchmark_summary.csv

link-inputs:
	mkdir -p ../input ../output ../temp
	ln -sfn ../../process_rent_data/output/chicago_rent_panel.parquet ../input/chicago_rent_panel.parquet
	ln -sfn ../../download_rent_data/output ../input/renthub_raw
	ln -sfn ../../renthub_data_audit/output/series_variant_benchmark_comparison.csv ../input/series_variant_benchmark_comparison.csv
	ln -sfn ../../process_dwellsy/output/dwellsy_monthly_series_variants.csv ../input/dwellsy_monthly_series_variants.csv
	ln -sfn ../../process_dwellsy/output/dwellsy_benchmark_comparison_real.csv ../input/dwellsy_benchmark_comparison_real.csv
	ln -sfn ../../../data_raw/Boundaries_-_City_20250920.geojson ../input/chicago_boundary.geojson
	ln -sfn ../../process_dwellsy/input/fred_chi_cpi_all_items.csv ../input/fred_chi_cpi_all_items.csv

../output/renthub_dwellsy_benchmark_summary.csv: run_compare_renthub_dwellsy_aggregate.R | link-inputs
	Rscript $< ../input/chicago_rent_panel.parquet ../input/renthub_raw ../input/series_variant_benchmark_comparison.csv ../input/dwellsy_monthly_series_variants.csv ../input/dwellsy_benchmark_comparison_real.csv ../input/chicago_boundary.geojson ../input/fred_chi_cpi_all_items.csv ../output ../temp

../output/renthub_trim_sensitivity_summary.csv: ../output/renthub_dwellsy_benchmark_summary.csv
	test -f "$@"
