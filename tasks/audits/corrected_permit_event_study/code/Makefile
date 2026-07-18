BANDWIDTH_M ?= 304.8
BANDWIDTH_LABEL ?= 1000ft
SPECS ?= issue_stable low_issue_stable application_stable issue_itt application_itt issue_stable_pre2015 issue_itt_pre2015 application_itt_pre2015 issue_realized_pre2015 low_issue_realized_pre2015 application_realized_pre2015 triple_issue_stable
CLUSTERS ?= block wardpair

all: \
	../output/corrected_permit_block_year_panel.parquet \
	../output/corrected_permit_sample_summary.csv \
	../output/corrected_permit_itt_discretion_difference_500ft.csv \
	../output/corrected_permit_itt_discretion_transition_500ft.csv \
	../output/corrected_permit_stable_discretion_difference_500ft.csv \
	../output/corrected_permit_realized_discretion_difference_500ft.csv \
	../output/corrected_permit_direction_indicator_500ft.pdf \
	../output/corrected_permit_direction_indicator_500ft_estimates.csv \
	../output/corrected_permit_direction_indicator_500ft_summary.csv \
	../output/corrected_permit_direction_indicator_pre2015_500ft.pdf \
	../output/corrected_permit_direction_indicator_pre2015_500ft_stricter.pdf \
	../output/corrected_permit_direction_indicator_pre2015_500ft_lenient.pdf \
	../output/corrected_permit_direction_indicator_pre2015_500ft_estimates.csv \
	../output/corrected_permit_direction_indicator_pre2015_500ft_summary.csv \
	../output/corrected_permit_direction_indicator_pre2015_1000ft.pdf \
	../output/corrected_permit_direction_indicator_pre2015_1000ft_stricter.pdf \
	../output/corrected_permit_direction_indicator_pre2015_1000ft_lenient.pdf \
	../output/corrected_permit_direction_indicator_pre2015_1000ft_estimates.csv \
	../output/corrected_permit_direction_indicator_pre2015_1000ft_summary.csv \
	../output/corrected_permit_direction_continuous_split_500ft.pdf \
	../output/corrected_permit_direction_continuous_split_500ft_estimates.csv \
	../output/corrected_permit_direction_continuous_split_500ft_summary.csv \
	../output/corrected_permit_direction_continuous_split_1000ft.pdf \
	../output/corrected_permit_direction_continuous_split_1000ft_estimates.csv \
	../output/corrected_permit_direction_continuous_split_1000ft_summary.csv \
	../output/corrected_permit_direction_continuous_split_pre2015_500ft.pdf \
	../output/corrected_permit_direction_continuous_split_pre2015_500ft_stricter.pdf \
	../output/corrected_permit_direction_continuous_split_pre2015_500ft_lenient.pdf \
	../output/corrected_permit_direction_continuous_split_pre2015_500ft_estimates.csv \
	../output/corrected_permit_direction_continuous_split_pre2015_500ft_summary.csv \
	../output/corrected_permit_direction_continuous_split_pre2015_1000ft.pdf \
	../output/corrected_permit_direction_continuous_split_pre2015_1000ft_stricter.pdf \
	../output/corrected_permit_direction_continuous_split_pre2015_1000ft_lenient.pdf \
	../output/corrected_permit_direction_continuous_split_pre2015_1000ft_estimates.csv \
	../output/corrected_permit_direction_continuous_split_pre2015_1000ft_summary.csv \
	../output/corrected_permit_direction_continuous_split_realized_pre2015_500ft.pdf \
	../output/corrected_permit_direction_continuous_split_realized_pre2015_500ft_estimates.csv \
	../output/corrected_permit_direction_continuous_split_realized_pre2015_500ft_summary.csv \
	../output/corrected_permit_direction_continuous_split_realized_pre2015_1000ft.pdf \
	../output/corrected_permit_direction_continuous_split_realized_pre2015_1000ft_estimates.csv \
	../output/corrected_permit_direction_continuous_split_realized_pre2015_1000ft_summary.csv \
	../output/corrected_permit_direction_continuous_separate_application_realized_pre2015_500ft_no_controls_stricter.pdf \
	../output/corrected_permit_direction_continuous_separate_application_realized_pre2015_500ft_no_controls_lenient.pdf \
	../output/corrected_permit_direction_continuous_separate_application_realized_pre2015_1000ft_no_controls_stricter.pdf \
	../output/corrected_permit_direction_continuous_separate_application_realized_pre2015_1000ft_no_controls_lenient.pdf \
	../output/corrected_permit_lenient_alder_influence_500ft.csv \
	../output/corrected_permit_lenient_alder_influence_1000ft.csv \
	../output/frozen_score_alder_comparison.csv \
	../output/frozen_score_correlations.csv \
	../output/frozen_score_treatment_sign_switches_1000ft.csv \
	../output/frozen_score_alder_recovery_influence_500ft.csv \
	../output/frozen_score_pair_recovery_influence_500ft.csv \
	../output/frozen_score_alder_recovery_influence_1000ft.csv \
	../output/frozen_score_pair_recovery_influence_1000ft.csv \
	../output/ward_2_42_raw_permits_1000ft.pdf \
	../output/ward_2_42_raw_permits_1000ft.png \
	../output/ward_2_42_raw_permits_1000ft.csv \
	../output/ward_46_48_raw_permits_500ft.pdf \
	../output/ward_46_48_raw_permits_500ft.png \
	../output/ward_46_48_raw_permits_500ft.csv \
	$(foreach cluster,$(CLUSTERS),../output/corrected_permit_iv_issue_clust_$(cluster)_$(BANDWIDTH_LABEL).pdf) \
	$(foreach cluster,$(CLUSTERS),../output/corrected_permit_iv_issue_clust_$(cluster)_$(BANDWIDTH_LABEL)_estimates.csv) \
	$(foreach cluster,$(CLUSTERS),../output/corrected_permit_iv_issue_clust_$(cluster)_$(BANDWIDTH_LABEL)_summary.csv) \
	$(foreach spec,$(SPECS),$(foreach cluster,$(CLUSTERS),../output/corrected_permit_event_study_$(spec)_clust_$(cluster)_$(BANDWIDTH_LABEL).pdf)) \
	$(foreach spec,$(SPECS),$(foreach cluster,$(CLUSTERS),../output/corrected_permit_event_study_$(spec)_clust_$(cluster)_$(BANDWIDTH_LABEL)_estimates.csv)) \
	$(foreach spec,$(SPECS),$(foreach cluster,$(CLUSTERS),../output/corrected_permit_event_study_$(spec)_clust_$(cluster)_$(BANDWIDTH_LABEL)_summary.csv))

../output/corrected_permit_block_year_panel.parquet: build_corrected_permit_panel.R ../input/permit_block_year_panel_2015.parquet ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores_2022.csv ../input/aldermen_uncertainty_scores_2014.csv | ../output
	$(R) $<

../output/corrected_permit_sample_summary.csv: ../output/corrected_permit_block_year_panel.parquet
	@test -s "$@"

../output/corrected_permit_%_discretion_difference_500ft.csv: test_itt_discretion_difference.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< $* 152.4 500ft

../output/corrected_permit_itt_discretion_transition_500ft.csv: test_itt_transition_year.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< 152.4 500ft

../output/corrected_permit_event_study_%_clust_block_$(BANDWIDTH_LABEL).pdf: run_corrected_permit_event_study.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< $* block $(BANDWIDTH_M) $(BANDWIDTH_LABEL)

../output/corrected_permit_event_study_%_clust_wardpair_$(BANDWIDTH_LABEL).pdf: run_corrected_permit_event_study.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< $* wardpair $(BANDWIDTH_M) $(BANDWIDTH_LABEL)

../output/corrected_permit_iv_issue_clust_block_$(BANDWIDTH_LABEL).pdf: run_corrected_permit_iv.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< block $(BANDWIDTH_M) $(BANDWIDTH_LABEL)

../output/corrected_permit_iv_issue_clust_wardpair_$(BANDWIDTH_LABEL).pdf: run_corrected_permit_iv.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< wardpair $(BANDWIDTH_M) $(BANDWIDTH_LABEL)

../output/corrected_permit_direction_indicator_500ft.pdf: run_corrected_permit_direction.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< indicator 152.4 500ft through2022 assigned issue

../output/corrected_permit_direction_indicator_pre2015_500ft.pdf: run_corrected_permit_direction.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< indicator 152.4 500ft pre2015 assigned issue

../output/corrected_permit_direction_indicator_pre2015_1000ft.pdf: run_corrected_permit_direction.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< indicator 304.8 1000ft pre2015 assigned issue

../output/corrected_permit_direction_continuous_split_500ft.pdf: run_corrected_permit_direction.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< continuous_split 152.4 500ft through2022 assigned issue

../output/corrected_permit_direction_continuous_split_1000ft.pdf: run_corrected_permit_direction.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< continuous_split 304.8 1000ft through2022 assigned issue

../output/corrected_permit_direction_continuous_split_pre2015_500ft.pdf: run_corrected_permit_direction.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< continuous_split 152.4 500ft pre2015 assigned issue

../output/corrected_permit_direction_continuous_split_pre2015_1000ft.pdf: run_corrected_permit_direction.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< continuous_split 304.8 1000ft pre2015 assigned issue

../output/corrected_permit_direction_continuous_split_realized_pre2015_500ft.pdf: run_corrected_permit_direction.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< continuous_split 152.4 500ft pre2015 realized issue

../output/corrected_permit_direction_continuous_split_realized_pre2015_1000ft.pdf: run_corrected_permit_direction.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< continuous_split 304.8 1000ft pre2015 realized issue

../output/corrected_permit_direction_continuous_separate_application_realized_pre2015_500ft_no_controls_stricter.pdf: run_corrected_permit_direction_separate.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< 152.4 500ft pre2015 realized application none

../output/corrected_permit_direction_continuous_separate_application_realized_pre2015_500ft_no_controls_lenient.pdf: ../output/corrected_permit_direction_continuous_separate_application_realized_pre2015_500ft_no_controls_stricter.pdf
	@test -s "$@"

../output/corrected_permit_direction_continuous_separate_application_realized_pre2015_1000ft_no_controls_stricter.pdf: run_corrected_permit_direction_separate.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< 304.8 1000ft pre2015 realized application none

../output/corrected_permit_direction_continuous_separate_application_realized_pre2015_1000ft_no_controls_lenient.pdf: ../output/corrected_permit_direction_continuous_separate_application_realized_pre2015_1000ft_no_controls_stricter.pdf
	@test -s "$@"

../output/corrected_permit_direction_%_stricter.pdf: ../output/corrected_permit_direction_%.pdf
	@test -s "$@"

../output/corrected_permit_direction_%_lenient.pdf: ../output/corrected_permit_direction_%.pdf
	@test -s "$@"

../output/corrected_permit_lenient_alder_influence_1000ft.csv: diagnose_lenient_alder_influence.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< 304.8 1000ft

../output/corrected_permit_lenient_alder_influence_500ft.csv: diagnose_lenient_alder_influence.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< 152.4 500ft

../output/frozen_score_alder_comparison.csv: compare_frozen_scores.R ../output/corrected_permit_block_year_panel.parquet ../input/aldermen_uncertainty_scores_2022.csv ../input/aldermen_uncertainty_scores_2014.csv | ../output
	$(R) $<

../output/frozen_score_correlations.csv: ../output/frozen_score_alder_comparison.csv
	@test -s "$@"

../output/frozen_score_treatment_sign_switches_1000ft.csv: ../output/frozen_score_alder_comparison.csv
	@test -s "$@"

../output/frozen_score_alder_recovery_influence_500ft.csv: diagnose_frozen_score_recovery.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< 152.4 500ft

../output/frozen_score_pair_recovery_influence_500ft.csv: ../output/frozen_score_alder_recovery_influence_500ft.csv
	@test -s "$@"

../output/frozen_score_alder_recovery_influence_1000ft.csv: diagnose_frozen_score_recovery.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $< 304.8 1000ft

../output/frozen_score_pair_recovery_influence_1000ft.csv: ../output/frozen_score_alder_recovery_influence_1000ft.csv
	@test -s "$@"

../output/ward_2_42_raw_permits_1000ft.pdf: plot_ward_2_42_raw_permits.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $<

../output/ward_2_42_raw_permits_1000ft.png: ../output/ward_2_42_raw_permits_1000ft.pdf
	@test -s "$@"

../output/ward_2_42_raw_permits_1000ft.csv: ../output/ward_2_42_raw_permits_1000ft.pdf
	@test -s "$@"

../output/ward_46_48_raw_permits_500ft.pdf: plot_ward_46_48_raw_permits.R ../output/corrected_permit_block_year_panel.parquet | ../output
	$(R) $<

../output/ward_46_48_raw_permits_500ft.png: ../output/ward_46_48_raw_permits_500ft.pdf
	@test -s "$@"

../output/ward_46_48_raw_permits_500ft.csv: ../output/ward_46_48_raw_permits_500ft.pdf
	@test -s "$@"

../output/corrected_permit_direction_%_500ft_estimates.csv: ../output/corrected_permit_direction_%_500ft.pdf
	@test -s "$@"

../output/corrected_permit_direction_%_500ft_summary.csv: ../output/corrected_permit_direction_%_500ft.pdf
	@test -s "$@"

../output/corrected_permit_direction_%_1000ft_estimates.csv: ../output/corrected_permit_direction_%_1000ft.pdf
	@test -s "$@"

../output/corrected_permit_direction_%_1000ft_summary.csv: ../output/corrected_permit_direction_%_1000ft.pdf
	@test -s "$@"

../output/corrected_permit_iv_%_$(BANDWIDTH_LABEL)_estimates.csv: ../output/corrected_permit_iv_%_$(BANDWIDTH_LABEL).pdf
	@test -s "$@"

../output/corrected_permit_iv_%_$(BANDWIDTH_LABEL)_summary.csv: ../output/corrected_permit_iv_%_$(BANDWIDTH_LABEL).pdf
	@test -s "$@"

../output/corrected_permit_event_study_%_$(BANDWIDTH_LABEL)_estimates.csv: ../output/corrected_permit_event_study_%_$(BANDWIDTH_LABEL).pdf
	@test -s "$@"

../output/corrected_permit_event_study_%_$(BANDWIDTH_LABEL)_summary.csv: ../output/corrected_permit_event_study_%_$(BANDWIDTH_LABEL).pdf
	@test -s "$@"

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores_2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores_2014.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2014.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores_2022.csv ../input/aldermen_uncertainty_scores_2014.csv

include ../../../shell_functions.make
include ../../../generic.make
