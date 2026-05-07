SHELL := bash
include ../../shell_functions.make

MAIN_BW_M ?= 250
MAIN_BW_FT := $(shell Rscript -e 'cat(round($(MAIN_BW_M) / 0.3048))')
MAIN_BW_LABEL ?= $(MAIN_BW_M)m
LEGACY_MAIN_BW_FT ?= 1000
LEGACY_ROBUST_BW_FT ?= 500

PAPER_PDFS := \
	../output/event_study_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.pdf \
	../output/event_study_combined_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.pdf
PAPER_TABLES := ../output/did_table_sales_2015_uniform_$(MAIN_BW_LABEL)_geo_wardpair_clust_block.tex

all: $(PAPER_PDFS) $(PAPER_TABLES)

../output/event_study_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv: run_event_study.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous TRUE yearly strict_pair_x_year uniform $(MAIN_BW_FT) full ward_pair block amenity $(MAIN_BW_LABEL)

../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.pdf \
../output/event_study_combined_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv: run_event_study.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous_split TRUE yearly strict_pair_x_year uniform $(MAIN_BW_FT) full ward_pair block amenity $(MAIN_BW_LABEL)

../output/did_table_sales_2015_uniform_$(MAIN_BW_LABEL)_geo_wardpair_clust_block.tex: did_table_sales_2015.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript did_table_sales_2015.R $(MAIN_BW_FT) uniform $@ amenity

../output/event_study_disaggregate_yearly_cohort_2015_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous TRUE yearly strict_pair_x_year triangular $(LEGACY_MAIN_BW_FT) full segment twoway_block_segment

../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_triangular_$(LEGACY_MAIN_BW_FT)ft_full.pdf \
../output/event_study_combined_disaggregate_yearly_cohort_2015_continuous_split_triangular_$(LEGACY_MAIN_BW_FT)ft_full.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_split_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_split_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_split_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_split_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous_split TRUE yearly strict_pair_x_year triangular $(LEGACY_MAIN_BW_FT) full segment twoway_block_segment

../output/event_study_disaggregate_yearly_cohort_2015_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_no_hedonics_full.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_no_hedonics_full.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_no_hedonics_full.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_no_hedonics_full.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_no_hedonics_full.csv: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous FALSE yearly strict_pair_x_year triangular $(LEGACY_MAIN_BW_FT) full segment twoway_block_segment

../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_triangular_$(LEGACY_MAIN_BW_FT)ft_no_hedonics_full.pdf \
../output/event_study_combined_disaggregate_yearly_cohort_2015_continuous_split_triangular_$(LEGACY_MAIN_BW_FT)ft_no_hedonics_full.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_split_triangular_$(LEGACY_MAIN_BW_FT)ft_no_hedonics_full.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_split_triangular_$(LEGACY_MAIN_BW_FT)ft_no_hedonics_full.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_split_triangular_$(LEGACY_MAIN_BW_FT)ft_no_hedonics_full.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_split_triangular_$(LEGACY_MAIN_BW_FT)ft_no_hedonics_full.csv: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous_split FALSE yearly strict_pair_x_year triangular $(LEGACY_MAIN_BW_FT) full segment twoway_block_segment

../output/event_study_disaggregate_yearly_cohort_2012_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2012_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_support_disaggregate_yearly_cohort_2012_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2012_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2012_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R cohort_2012 continuous TRUE yearly strict_pair_x_year triangular $(LEGACY_MAIN_BW_FT) full segment twoway_block_segment

../output/event_study_disaggregate_yearly_stacked_announcement_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.pdf \
../output/event_study_coefficients_disaggregate_yearly_stacked_announcement_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_support_disaggregate_yearly_stacked_announcement_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_pretrend_disaggregate_yearly_stacked_announcement_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_metadata_disaggregate_yearly_stacked_announcement_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R stacked_announcement continuous TRUE yearly strict_pair_x_year triangular $(LEGACY_MAIN_BW_FT) full segment twoway_block_segment

../output/event_study_disaggregate_yearly_stacked_implementation_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.pdf \
../output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_support_disaggregate_yearly_stacked_implementation_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv \
../output/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_full.csv: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R stacked_implementation continuous TRUE yearly strict_pair_x_year triangular $(LEGACY_MAIN_BW_FT) full segment twoway_block_segment

../output/event_study_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_full_geo_wardpair_clust_block.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_full_geo_wardpair_clust_block.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_full_geo_wardpair_clust_block.csv: run_event_study.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous TRUE yearly strict_pair_x_year uniform $(LEGACY_MAIN_BW_FT) full ward_pair block

../output/event_study_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_amenity_full_geo_wardpair_clust_block.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv: run_event_study.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous TRUE yearly strict_pair_x_year uniform $(LEGACY_MAIN_BW_FT) full ward_pair block amenity

../output/event_study_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_ROBUST_BW_FT)ft_amenity_full_geo_wardpair_clust_block.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_ROBUST_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_ROBUST_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_ROBUST_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_uniform_$(LEGACY_ROBUST_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv: run_event_study.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous TRUE yearly strict_pair_x_year uniform $(LEGACY_ROBUST_BW_FT) full ward_pair block amenity

../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_full_geo_wardpair_clust_block.pdf \
../output/event_study_combined_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_full_geo_wardpair_clust_block.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_full_geo_wardpair_clust_block.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_full_geo_wardpair_clust_block.csv: run_event_study.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous_split TRUE yearly strict_pair_x_year uniform $(LEGACY_MAIN_BW_FT) full ward_pair block

../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_amenity_full_geo_wardpair_clust_block.pdf \
../output/event_study_combined_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_amenity_full_geo_wardpair_clust_block.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv: run_event_study.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous_split TRUE yearly strict_pair_x_year uniform $(LEGACY_MAIN_BW_FT) full ward_pair block amenity

../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_ROBUST_BW_FT)ft_amenity_full_geo_wardpair_clust_block.pdf \
../output/event_study_combined_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_ROBUST_BW_FT)ft_amenity_full_geo_wardpair_clust_block.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_ROBUST_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_ROBUST_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_ROBUST_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(LEGACY_ROBUST_BW_FT)ft_amenity_full_geo_wardpair_clust_block.csv: run_event_study.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous_split TRUE yearly strict_pair_x_year uniform $(LEGACY_ROBUST_BW_FT) full ward_pair block amenity

../output/did_table_sales_clean_uniform_$(LEGACY_MAIN_BW_FT)ft.tex: run_did_sales_disaggregate.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet | ../output
	Rscript run_did_sales_disaggregate.R $(LEGACY_MAIN_BW_FT) uniform strict_pair_x_year segment twoway_block_segment
	cp ../output/did_table_sales_clean.tex "$@"

../output/did_table_sales_2015_uniform_$(LEGACY_MAIN_BW_FT)ft_geo_wardpair_clust_block.tex: did_table_sales_2015.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript did_table_sales_2015.R $(LEGACY_MAIN_BW_FT) uniform $@ amenity

../output/did_table_sales_2015_uniform_$(LEGACY_ROBUST_BW_FT)ft_geo_wardpair_clust_block.tex: did_table_sales_2015.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript did_table_sales_2015.R $(LEGACY_ROBUST_BW_FT) uniform $@ amenity

../input/sales_transaction_panel.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_announcement.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_announcement.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_2012.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2012.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_2022.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2022.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_2023.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2023.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/sales_transaction_panel_2015.parquet

include ../../generic.make
