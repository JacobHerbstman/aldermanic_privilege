BANDWIDTH_M ?= 152.4
BANDWIDTH_LABEL ?= 500ft
BASELINE_YEAR ?= 2014

all: ../output/permit_itt_balance_$(BANDWIDTH_LABEL).tex

../output/permit_itt_balance_$(BANDWIDTH_LABEL).tex: build_permit_itt_balance.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript $< $(BANDWIDTH_M) $(BANDWIDTH_LABEL) $(BASELINE_YEAR)

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/block_group_controls.csv: ../../create_block_group_controls/output/block_group_controls.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv

include ../../generic.make
