MAIN_BW_M ?= 152.4
MAIN_BW_LABEL ?= 500ft

all: \
	../output/permit_event_study_high_discretion_stable_signed_$(MAIN_BW_LABEL).pdf \
	../output/permit_event_study_high_discretion_stable_separate_$(MAIN_BW_LABEL).pdf \
	../output/permit_event_study_low_discretion_nosigns_stable_signed_$(MAIN_BW_LABEL).pdf \
	../output/permit_event_study_high_discretion_all_signed_$(MAIN_BW_LABEL).pdf \
	../output/permit_event_study_appendix_$(MAIN_BW_LABEL).tex

../output/permit_event_study_high_discretion_stable_signed_$(MAIN_BW_LABEL).pdf: run_binary_event_study_permit.R ../input/permit_block_year_panel_2015.parquet Makefile | ../output
	$(R) $< high_discretion stable signed $(MAIN_BW_M) $(MAIN_BW_LABEL)

../output/permit_event_study_high_discretion_stable_separate_$(MAIN_BW_LABEL).pdf: run_binary_event_study_permit.R ../input/permit_block_year_panel_2015.parquet Makefile | ../output
	$(R) $< high_discretion stable separate $(MAIN_BW_M) $(MAIN_BW_LABEL)

../output/permit_event_study_low_discretion_nosigns_stable_signed_$(MAIN_BW_LABEL).pdf: run_binary_event_study_permit.R ../input/permit_block_year_panel_2015.parquet Makefile | ../output
	$(R) $< low_discretion_nosigns stable signed $(MAIN_BW_M) $(MAIN_BW_LABEL)

../output/permit_event_study_high_discretion_all_signed_$(MAIN_BW_LABEL).pdf: run_binary_event_study_permit.R ../input/permit_block_year_panel_2015.parquet Makefile | ../output
	$(R) $< high_discretion all signed $(MAIN_BW_M) $(MAIN_BW_LABEL)

../output/permit_event_study_appendix_$(MAIN_BW_LABEL).tex: build_binary_event_study_table.R ../input/permit_block_year_panel_2015.parquet Makefile | ../output
	$(R) $< $(MAIN_BW_M) $(MAIN_BW_LABEL)

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet

include ../../shell_functions.make
include ../../generic.make
