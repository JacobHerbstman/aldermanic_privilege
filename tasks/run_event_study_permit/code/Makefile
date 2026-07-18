MAIN_BW_M ?= 152.4
MAIN_BW_LABEL ?= 500ft
MIN_REL_YEAR ?= -5
MAX_REL_YEAR ?= 5
OUTCOMES ?= high_discretion low_discretion_nosigns
LEGACY_BW_M := 304.8
LEGACY_BW_LABEL := 1000ft

all: \
	$(foreach outcome,$(OUTCOMES),../output/event_study_permit_2015_$(outcome)_application_frozen2014_itt_preperiod_controls_$(MAIN_BW_LABEL)_clust_ward_pair.pdf) \
	../output/event_study_permit_2015_high_discretion_application_frozen2014_stable_preperiod_controls_$(MAIN_BW_LABEL)_clust_ward_pair.pdf \
	../output/did_table_permit_2015_application_frozen2014_itt_preperiod_controls_$(MAIN_BW_LABEL)_clust_ward_pair.tex

../output/event_study_permit_2015_%_application_frozen2014_itt_preperiod_controls_$(MAIN_BW_LABEL)_clust_ward_pair.pdf: run_selected_event_study_permit.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript $< $* itt $(MAIN_BW_M) $(MAIN_BW_LABEL) $(MIN_REL_YEAR) $(MAX_REL_YEAR)

../output/event_study_permit_2015_%_application_frozen2014_stable_preperiod_controls_$(MAIN_BW_LABEL)_clust_ward_pair.pdf: run_selected_event_study_permit.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript $< $* stable $(MAIN_BW_M) $(MAIN_BW_LABEL) $(MIN_REL_YEAR) $(MAX_REL_YEAR)

../output/did_table_permit_2015_application_frozen2014_itt_preperiod_controls_$(MAIN_BW_LABEL)_clust_ward_pair.tex: did_table_permit_2015.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript $< $(MAIN_BW_M) $(MAIN_BW_LABEL) $(MIN_REL_YEAR) $(MAX_REL_YEAR)

../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(LEGACY_BW_LABEL)_noctrl_geo_wardpair.tex: did_table_permit_2015_legacy.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript $< $(LEGACY_BW_M) $(LEGACY_BW_LABEL) $(MIN_REL_YEAR) $(MAX_REL_YEAR)

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript $< high_discretion continuous $(LEGACY_BW_M) $(LEGACY_BW_LABEL) $(MIN_REL_YEAR) $(MAX_REL_YEAR)

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(LEGACY_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript $< high_discretion continuous_split $(LEGACY_BW_M) $(LEGACY_BW_LABEL) $(MIN_REL_YEAR) $(MAX_REL_YEAR)

../output/event_study_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(LEGACY_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript $< low_discretion_nosigns continuous $(LEGACY_BW_M) $(LEGACY_BW_LABEL) $(MIN_REL_YEAR) $(MAX_REL_YEAR)

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet

include ../../generic.make
