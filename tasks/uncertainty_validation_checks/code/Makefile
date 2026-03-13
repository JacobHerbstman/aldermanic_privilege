SHELL := bash
include ../../shell_functions.make

UNCERTAINTY_SPEC ?= ptfeFALSE_rtfeFALSE_porchTRUE_cafeFALSE_2stage

all: ../output/permit_validation_results_uncertainty_$(UNCERTAINTY_SPEC).csv \
	../output/permit_validation_table_uncertainty_$(UNCERTAINTY_SPEC).tex

../output/permit_validation_results_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/permit_validation_table_uncertainty_$(UNCERTAINTY_SPEC).tex: build_permit_validation_table.R ../input/building_permits_text_features.csv.gz ../input/chicago_alderman_panel.csv ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../output
	Rscript build_permit_validation_table.R "$(UNCERTAINTY_SPEC)" ../input/building_permits_text_features.csv.gz ../input/chicago_alderman_panel.csv ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../output/permit_validation_results_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/permit_validation_table_uncertainty_$(UNCERTAINTY_SPEC).tex

../input/building_permits_text_features.csv.gz: ../../building_permits_scraping/output/building_permits_text_features.csv.gz | ../input
	ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/building_permits_text_features.csv.gz ../input/chicago_alderman_panel.csv ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv

include ../../generic.make
