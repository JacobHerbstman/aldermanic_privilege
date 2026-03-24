SHELL := bash
include ../../shell_functions.make

PANELS := cohort_2015 stacked_implementation
OUTCOMES := high_discretion new_construction
BW_FT := 500

OUTPUTS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),../output/permit_border_rd_$(panel)_$(outcome)_issue_bw$(BW_FT)_segment_year_clust_pair.pdf))

all: $(OUTPUTS)

define RUN_RD_RULE
../output/permit_border_rd_$(1)_$(2)_issue_bw$(BW_FT)_segment_year_clust_pair.pdf: run_permit_border_rd.R ../input/permit_block_year_panel.parquet ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript run_permit_border_rd.R $(1) $(2) issue $(BW_FT) $$@
endef

$(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(eval $(call RUN_RD_RULE,$(panel),$(outcome)))))

../input/permit_block_year_panel.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel.parquet | ../input
	ln -sf "$<" "$@"

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel.parquet ../input/permit_block_year_panel_2015.parquet

include ../../generic.make
