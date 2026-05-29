all: ../output/rental_data_validation_growth.tex

../output/rental_data_validation_growth.tex: build_rental_benchmark_table.R ../input/chicago_rent_panel.parquet ../input/fred_cpi_cuura207sa0.csv ../input/zillow_zori_city.csv | ../output
	Rscript $<

../input/chicago_rent_panel.parquet: ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/fred_cpi_cuura207sa0.csv: ../../download_fred_cpi/output/fred_cpi_cuura207sa0.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/zillow_zori_city.csv: ../../download_zillow_zori/output/zillow_zori_city.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/chicago_rent_panel.parquet ../input/fred_cpi_cuura207sa0.csv ../input/zillow_zori_city.csv

include ../../generic.make
