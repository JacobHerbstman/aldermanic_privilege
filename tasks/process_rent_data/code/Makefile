START_DATE ?= 2014-01-01
END_DATE ?= 2022-12-31

all: ../output/chicago_rent_panel.parquet

../output/chicago_rent_panel.parquet: build_renthub_clean_panel.R ../../download_rent_data/output/download.done | ../output ../input/renthub_raw
	Rscript $< $(START_DATE) $(END_DATE)

../input/renthub_raw: | ../../download_rent_data/output/download.done ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "../../download_rent_data/output" || ln -sfn "../../download_rent_data/output" "$@"

link-inputs: ../input/renthub_raw

include ../../generic.make
