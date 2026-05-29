START_DATE ?= 2014-01-01
END_DATE ?= 2022-12-31

all: ../output/chicago_rent_panel_quality_flags.parquet

../output/chicago_rent_panel_quality_flags.parquet: run_renthub_proxy_consistency_audit.R manual_verified_address_locations.csv ../input/renthub_raw ../input/chicago_rent_panel.parquet | ../output
	Rscript $< $(START_DATE) $(END_DATE)

../input/renthub_raw: ../../download_rent_data/output/download.done | ../input
	@test "$$(readlink "$@")" = "../../download_rent_data/output" || ln -sfn "../../download_rent_data/output" "$@"

../input/chicago_rent_panel.parquet: ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/renthub_raw ../input/chicago_rent_panel.parquet

include ../../generic.make
