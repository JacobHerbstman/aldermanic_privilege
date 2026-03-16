SHELL := bash

PANEL_MODES := cohort_2012 cohort_2015 cohort_2022 cohort_2023 stacked_announcement stacked_implementation
APPROACHES := bw1000 corridor1320 citywide_valid
TREATMENT_MODES := continuous continuous_split

all: \
	$(foreach panel,$(PANEL_MODES),$(foreach approach,$(APPROACHES),../output/repeat_sales_event_study_$(panel)_$(approach)_continuous.pdf)) \
	$(foreach panel,$(PANEL_MODES),$(foreach approach,$(APPROACHES),../output/repeat_sales_event_study_$(panel)_$(approach)_continuous_split.pdf)) \
	$(foreach panel,$(PANEL_MODES),$(foreach approach,$(APPROACHES),../output/repeat_sales_event_study_combined_$(panel)_$(approach)_continuous_split.pdf)) \
	../output/repeat_sales_sample_summary.csv \
	../output/repeat_sales_spec_comparison.csv \
	../output/repeat_sales_spec_comparison.tex

define build_panel_rule
../output/repeat_sales_panel_$(1)_$(2).parquet: build_repeat_sales_panels.R ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript $$< $(1) $(2) ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet ../output/repeat_sales_panel_$(1)_$(2).parquet
endef

$(foreach panel,$(PANEL_MODES),$(foreach approach,$(APPROACHES),$(eval $(call build_panel_rule,$(panel),$(approach)))))

define run_continuous_rule
../output/repeat_sales_event_study_$(1)_$(2)_continuous.pdf \
../output/repeat_sales_coefficients_$(1)_$(2)_continuous.csv \
../output/repeat_sales_support_$(1)_$(2)_continuous.csv \
../output/repeat_sales_pretrend_$(1)_$(2)_continuous.csv \
../output/repeat_sales_metadata_$(1)_$(2)_continuous.csv: run_repeat_sales_event_study.R ../output/repeat_sales_panel_$(1)_$(2).parquet | ../output
	Rscript $$< $(1) $(2) continuous ../output/repeat_sales_panel_$(1)_$(2).parquet ../output/repeat_sales_event_study_$(1)_$(2)_continuous.pdf ../output/repeat_sales_event_study_combined_$(1)_$(2)_continuous.pdf ../output/repeat_sales_coefficients_$(1)_$(2)_continuous.csv ../output/repeat_sales_support_$(1)_$(2)_continuous.csv ../output/repeat_sales_pretrend_$(1)_$(2)_continuous.csv ../output/repeat_sales_metadata_$(1)_$(2)_continuous.csv
endef

$(foreach panel,$(PANEL_MODES),$(foreach approach,$(APPROACHES),$(eval $(call run_continuous_rule,$(panel),$(approach)))))

define run_split_rule
../output/repeat_sales_event_study_$(1)_$(2)_continuous_split.pdf \
../output/repeat_sales_event_study_combined_$(1)_$(2)_continuous_split.pdf \
../output/repeat_sales_coefficients_$(1)_$(2)_continuous_split.csv \
../output/repeat_sales_support_$(1)_$(2)_continuous_split.csv \
../output/repeat_sales_pretrend_$(1)_$(2)_continuous_split.csv \
../output/repeat_sales_metadata_$(1)_$(2)_continuous_split.csv: run_repeat_sales_event_study.R ../output/repeat_sales_panel_$(1)_$(2).parquet | ../output
	Rscript $$< $(1) $(2) continuous_split ../output/repeat_sales_panel_$(1)_$(2).parquet ../output/repeat_sales_event_study_$(1)_$(2)_continuous_split.pdf ../output/repeat_sales_event_study_combined_$(1)_$(2)_continuous_split.pdf ../output/repeat_sales_coefficients_$(1)_$(2)_continuous_split.csv ../output/repeat_sales_support_$(1)_$(2)_continuous_split.csv ../output/repeat_sales_pretrend_$(1)_$(2)_continuous_split.csv ../output/repeat_sales_metadata_$(1)_$(2)_continuous_split.csv
endef

$(foreach panel,$(PANEL_MODES),$(foreach approach,$(APPROACHES),$(eval $(call run_split_rule,$(panel),$(approach)))))

../output/repeat_sales_sample_summary.csv \
../output/repeat_sales_spec_comparison.csv \
../output/repeat_sales_spec_comparison.tex: summarize_repeat_sales_results.R \
	$(foreach panel,$(PANEL_MODES),$(foreach approach,$(APPROACHES),../output/repeat_sales_panel_$(panel)_$(approach).parquet)) \
	$(foreach panel,$(PANEL_MODES),$(foreach approach,$(APPROACHES),../output/repeat_sales_coefficients_$(panel)_$(approach)_continuous.csv ../output/repeat_sales_support_$(panel)_$(approach)_continuous.csv ../output/repeat_sales_pretrend_$(panel)_$(approach)_continuous.csv ../output/repeat_sales_metadata_$(panel)_$(approach)_continuous.csv)) \
	$(foreach panel,$(PANEL_MODES),$(foreach approach,$(APPROACHES),../output/repeat_sales_coefficients_$(panel)_$(approach)_continuous_split.csv ../output/repeat_sales_support_$(panel)_$(approach)_continuous_split.csv ../output/repeat_sales_pretrend_$(panel)_$(approach)_continuous_split.csv ../output/repeat_sales_metadata_$(panel)_$(approach)_continuous_split.csv)) | ../output
	Rscript $< ../output/repeat_sales_sample_summary.csv ../output/repeat_sales_spec_comparison.csv ../output/repeat_sales_spec_comparison.tex

../input/sales_transaction_panel_2012.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2012.parquet | ../input
	ln -sf $< $@

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	ln -sf $< $@

../input/sales_transaction_panel_2022.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2022.parquet | ../input
	ln -sf $< $@

../input/sales_transaction_panel_2023.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2023.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet

include ../../generic.make
