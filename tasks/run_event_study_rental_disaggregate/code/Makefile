SHELL := bash

all: \
	../output/event_study_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_short.pdf \
	../output/event_study_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_short.pdf \
	../output/event_study_combined_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_short.pdf \
	../output/event_study_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_no_hedonics_short.pdf \
	../output/event_study_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_no_hedonics_short.pdf \
	../output/event_study_combined_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_no_hedonics_short.pdf \
	../output/event_study_disaggregate_yearly_cohort_2015_continuous_triangular_250m_mf_short.pdf \
	../output/event_study_disaggregate_yearly_stacked_implementation_continuous_triangular_250m_mf_overlap.pdf \
	../output/event_study_disaggregate_yearly_stacked_implementation_continuous_split_triangular_250m_mf_overlap.pdf \
	../output/event_study_combined_disaggregate_yearly_stacked_implementation_continuous_split_triangular_250m_mf_overlap.pdf

../output/event_study_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_short.pdf: run_event_study_disaggregate.R \
	../../_lib/event_study_plot_helpers.R ../input/rental_listing_panel_2023.parquet | ../output
	Rscript run_event_study_disaggregate.R cohort_2023 continuous TRUE

../output/event_study_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_short.pdf: run_event_study_disaggregate.R \
	../../_lib/event_study_plot_helpers.R ../input/rental_listing_panel_2023.parquet | ../output
	Rscript run_event_study_disaggregate.R cohort_2023 continuous_split TRUE

../output/event_study_combined_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_short.pdf: ../output/event_study_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_short.pdf
	@test -s "$@"

../output/event_study_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_no_hedonics_short.pdf: run_event_study_disaggregate.R \
	../../_lib/event_study_plot_helpers.R ../input/rental_listing_panel_2023.parquet | ../output
	Rscript run_event_study_disaggregate.R cohort_2023 continuous FALSE

../output/event_study_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_no_hedonics_short.pdf: run_event_study_disaggregate.R \
	../../_lib/event_study_plot_helpers.R ../input/rental_listing_panel_2023.parquet | ../output
	Rscript run_event_study_disaggregate.R cohort_2023 continuous_split FALSE

../output/event_study_combined_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_no_hedonics_short.pdf: ../output/event_study_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_no_hedonics_short.pdf
	@test -s "$@"

../output/event_study_disaggregate_yearly_cohort_2015_continuous_triangular_250m_mf_short.pdf: run_event_study_disaggregate.R \
	../../_lib/event_study_plot_helpers.R ../input/rental_listing_panel_2015.parquet | ../output
	Rscript run_event_study_disaggregate.R cohort_2015 continuous TRUE

../output/event_study_disaggregate_yearly_stacked_implementation_continuous_triangular_250m_mf_overlap.pdf: run_event_study_disaggregate.R \
	../../_lib/event_study_plot_helpers.R ../input/rental_listing_panel.parquet | ../output
	Rscript run_event_study_disaggregate.R stacked_implementation continuous TRUE

../output/event_study_disaggregate_yearly_stacked_implementation_continuous_split_triangular_250m_mf_overlap.pdf: run_event_study_disaggregate.R \
	../../_lib/event_study_plot_helpers.R ../input/rental_listing_panel.parquet | ../output
	Rscript run_event_study_disaggregate.R stacked_implementation continuous_split TRUE

../output/event_study_combined_disaggregate_yearly_stacked_implementation_continuous_split_triangular_250m_mf_overlap.pdf: ../output/event_study_disaggregate_yearly_stacked_implementation_continuous_split_triangular_250m_mf_overlap.pdf
	@test -s "$@"

../input/rental_listing_panel.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rental_listing_panel_2015.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rental_listing_panel_2023.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel_2023.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_listing_panel.parquet ../input/rental_listing_panel_2015.parquet ../input/rental_listing_panel_2023.parquet

include ../../generic.make
