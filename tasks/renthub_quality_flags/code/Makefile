START_DATE ?= 2014-01-01
END_DATE ?= 2022-12-31

all: ../output/chicago_rent_panel_quality_flags.parquet

../output/chicago_rent_panel_quality_flags.parquet: build_renthub_quality_flags.R ../input/manual_verified_address_locations.csv ../../download_rent_data/output/download.done ../input/chicago_rent_panel.parquet | ../output ../input/renthub_raw
	Rscript $< $(START_DATE) $(END_DATE)

../input/renthub_raw: | ../../download_rent_data/output/download.done ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "../../download_rent_data/output" || ln -sfn "../../download_rent_data/output" "$@"

../input/chicago_rent_panel.parquet: ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/renthub_raw ../input/chicago_rent_panel.parquet

include ../../generic.make
