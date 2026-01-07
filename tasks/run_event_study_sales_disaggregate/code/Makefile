SHELL := bash
include ../../shell_functions.make

INPUT_PANEL := ../input/sales_transaction_panel.parquet

# Define all specifications
TREATMENT_TYPES := continuous binary_direction
TIME_UNITS := yearly quarterly
HEDONIC_OPTIONS := with_hedonics no_hedonics

# Primary outputs (with hedonics, main specifications)
MAIN_OUTPUTS := \
    ../output/event_study_disaggregate_yearly_continuous.pdf \
    ../output/event_study_disaggregate_yearly_binary.pdf \
    ../output/event_study_disaggregate_quarterly_continuous.pdf \
    ../output/event_study_disaggregate_quarterly_binary.pdf

# Pattern rule for running specifications
# Yearly with hedonics
../output/event_study_disaggregate_yearly_continuous.pdf ../output/did_table_disaggregate_yearly_continuous.tex: run_event_study.R $(INPUT_PANEL) | ../output
	@echo "→ [run_event_study_sales_disaggregate] yearly + continuous + hedonics"
	$(R) $< --treatment_type=continuous --time_unit=yearly --include_hedonics=TRUE

../output/event_study_disaggregate_yearly_binary.pdf ../output/did_table_disaggregate_yearly_binary.tex: run_event_study.R $(INPUT_PANEL) | ../output
	@echo "→ [run_event_study_sales_disaggregate] yearly + binary + hedonics"
	$(R) $< --treatment_type=binary_direction --time_unit=yearly --include_hedonics=TRUE

# Quarterly with hedonics
../output/event_study_disaggregate_quarterly_continuous.pdf ../output/did_table_disaggregate_quarterly_continuous.tex: run_event_study.R $(INPUT_PANEL) | ../output
	@echo "→ [run_event_study_sales_disaggregate] quarterly + continuous + hedonics"
	$(R) $< --treatment_type=continuous --time_unit=quarterly --include_hedonics=TRUE

../output/event_study_disaggregate_quarterly_binary.pdf ../output/did_table_disaggregate_quarterly_binary.tex: run_event_study.R $(INPUT_PANEL) | ../output
	@echo "→ [run_event_study_sales_disaggregate] quarterly + binary + hedonics"
	$(R) $< --treatment_type=binary_direction --time_unit=quarterly --include_hedonics=TRUE

# No hedonics versions (yearly)
../output/event_study_disaggregate_yearly_continuous_no_hedonics.pdf: run_event_study.R $(INPUT_PANEL) | ../output
	@echo "→ [run_event_study_sales_disaggregate] yearly + continuous + no hedonics"
	$(R) $< --treatment_type=continuous --time_unit=yearly --include_hedonics=FALSE

../output/event_study_disaggregate_yearly_binary_no_hedonics.pdf: run_event_study.R $(INPUT_PANEL) | ../output
	@echo "→ [run_event_study_sales_disaggregate] yearly + binary + no hedonics"
	$(R) $< --treatment_type=binary_direction --time_unit=yearly --include_hedonics=FALSE

# No hedonics versions (quarterly)
../output/event_study_disaggregate_quarterly_continuous_no_hedonics.pdf: run_event_study.R $(INPUT_PANEL) | ../output
	@echo "→ [run_event_study_sales_disaggregate] quarterly + continuous + no hedonics"
	$(R) $< --treatment_type=continuous --time_unit=quarterly --include_hedonics=FALSE

../output/event_study_disaggregate_quarterly_binary_no_hedonics.pdf: run_event_study.R $(INPUT_PANEL) | ../output
	@echo "→ [run_event_study_sales_disaggregate] quarterly + binary + no hedonics"
	$(R) $< --treatment_type=binary_direction --time_unit=quarterly --include_hedonics=FALSE

# Convenience targets
.PHONY: main all yearly quarterly with_hedonics no_hedonics

# Main specifications (with hedonics)
main: $(MAIN_OUTPUTS)

# All specifications including no hedonics
all: main \
    ../output/event_study_disaggregate_yearly_continuous_no_hedonics.pdf \
    ../output/event_study_disaggregate_yearly_binary_no_hedonics.pdf \
    ../output/event_study_disaggregate_quarterly_continuous_no_hedonics.pdf \
    ../output/event_study_disaggregate_quarterly_binary_no_hedonics.pdf

# Yearly only (both with and without hedonics)
yearly: \
    ../output/event_study_disaggregate_yearly_continuous.pdf \
    ../output/event_study_disaggregate_yearly_binary.pdf \
    ../output/event_study_disaggregate_yearly_continuous_no_hedonics.pdf \
    ../output/event_study_disaggregate_yearly_binary_no_hedonics.pdf

# Quarterly only (both with and without hedonics)
quarterly: \
    ../output/event_study_disaggregate_quarterly_continuous.pdf \
    ../output/event_study_disaggregate_quarterly_binary.pdf \
    ../output/event_study_disaggregate_quarterly_continuous_no_hedonics.pdf \
    ../output/event_study_disaggregate_quarterly_binary_no_hedonics.pdf

include ../../generic.make
