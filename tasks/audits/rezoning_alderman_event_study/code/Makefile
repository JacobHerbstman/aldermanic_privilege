BANDWIDTH ?= 304.8
BANDWIDTH_LABEL ?= 1000ft
MIN_EVENT_YEAR ?= -4
MAX_EVENT_YEAR ?= 5
OUTCOMES ?= any_rezoning any_upzone far_change_total mean_far_change upzone_share
FROZEN_OUTCOMES ?= any_rezoning any_upzone far_change_total

all: \
	../output/rezoning_alderman_block_year_panel.parquet \
	../output/rezoning_alderman_block_year_panel_pre2015.parquet \
	../output/rezoning_alderman_robustness_year0.pdf \
	../output/rezoning_alderman_robustness_estimates.csv \
	../output/rezoning_alderman_pooled_did.pdf \
	../output/rezoning_alderman_pooled_did.csv \
	../output/rezoning_alderman_upzone_bounds.csv \
	$(foreach outcome,$(OUTCOMES),../output/rezoning_alderman_event_study_$(outcome)_$(BANDWIDTH_LABEL).pdf) \
	$(foreach outcome,$(OUTCOMES),../output/rezoning_alderman_event_study_$(outcome)_$(BANDWIDTH_LABEL)_estimates.csv) \
	$(foreach outcome,$(OUTCOMES),../output/rezoning_alderman_event_study_$(outcome)_$(BANDWIDTH_LABEL)_summary.csv) \
	$(foreach outcome,$(FROZEN_OUTCOMES),../output/rezoning_alderman_event_study_pre2015_$(outcome)_$(BANDWIDTH_LABEL).pdf) \
	$(foreach outcome,$(FROZEN_OUTCOMES),../output/rezoning_alderman_event_study_pre2015_$(outcome)_$(BANDWIDTH_LABEL)_estimates.csv) \
	$(foreach outcome,$(FROZEN_OUTCOMES),../output/rezoning_alderman_event_study_pre2015_$(outcome)_$(BANDWIDTH_LABEL)_summary.csv)

../output/rezoning_alderman_block_year_panel.parquet: build_rezoning_alderman_panel.R ../input/permit_block_year_panel_2015.parquet ../input/block_treatment_panel.csv ../input/rezoning_census_blocks_20101101_20201231.csv ../input/rezoning_matter_block_bridge_20101101_20201231.csv | ../output
	$(R) $< through2022

../output/rezoning_alderman_block_year_panel_pre2015.parquet: build_rezoning_alderman_panel.R ../input/permit_block_year_panel_2015.parquet ../input/corrected_permit_block_year_panel.parquet ../input/rezoning_census_blocks_20101101_20201231.csv ../input/rezoning_matter_block_bridge_20101101_20201231.csv | ../output
	$(R) $< pre2015

../output/rezoning_alderman_robustness_year0.pdf: run_rezoning_alderman_robustness.R ../output/rezoning_alderman_block_year_panel.parquet | ../output
	$(R) $<

../output/rezoning_alderman_robustness_estimates.csv: ../output/rezoning_alderman_robustness_year0.pdf
	@test -s "$@"

../output/rezoning_alderman_pooled_did.pdf: run_rezoning_alderman_pooled_did.R ../output/rezoning_alderman_block_year_panel.parquet | ../output
	$(R) $<

../output/rezoning_alderman_pooled_did.csv: ../output/rezoning_alderman_pooled_did.pdf
	@test -s "$@"

../output/rezoning_alderman_upzone_bounds.csv: run_rezoning_alderman_upzone_bounds.R ../output/rezoning_alderman_block_year_panel.parquet | ../output
	$(R) $<

../output/rezoning_alderman_event_study_pre2015_%_$(BANDWIDTH_LABEL).pdf: run_rezoning_alderman_event_study.R ../output/rezoning_alderman_block_year_panel_pre2015.parquet | ../output
	$(R) $< $* $(BANDWIDTH) $(BANDWIDTH_LABEL) $(MIN_EVENT_YEAR) $(MAX_EVENT_YEAR) pre2015

../output/rezoning_alderman_event_study_%_$(BANDWIDTH_LABEL).pdf: run_rezoning_alderman_event_study.R ../output/rezoning_alderman_block_year_panel.parquet | ../output
	$(R) $< $* $(BANDWIDTH) $(BANDWIDTH_LABEL) $(MIN_EVENT_YEAR) $(MAX_EVENT_YEAR) through2022

../output/rezoning_alderman_event_study_%_$(BANDWIDTH_LABEL)_estimates.csv: ../output/rezoning_alderman_event_study_%_$(BANDWIDTH_LABEL).pdf
	@test -s "$@"

../output/rezoning_alderman_event_study_%_$(BANDWIDTH_LABEL)_summary.csv: ../output/rezoning_alderman_event_study_%_$(BANDWIDTH_LABEL).pdf
	@test -s "$@"

../input/corrected_permit_block_year_panel.parquet: ../../../audits/corrected_permit_event_study/output/corrected_permit_block_year_panel.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/block_treatment_panel.csv: ../../../merge_event_study_scores/output/block_treatment_panel_through202604.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rezoning_census_blocks_20101101_20201231.csv: ../../../rezoning_census_block_assignment/output/rezoning_census_blocks_20101101_20201231.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rezoning_matter_block_bridge_20101101_20201231.csv: ../../../rezoning_census_block_assignment/output/rezoning_matter_block_bridge_20101101_20201231.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/block_treatment_panel.csv ../input/corrected_permit_block_year_panel.parquet ../input/rezoning_census_blocks_20101101_20201231.csv ../input/rezoning_matter_block_bridge_20101101_20201231.csv

include ../../../shell_functions.make
include ../../../generic.make
