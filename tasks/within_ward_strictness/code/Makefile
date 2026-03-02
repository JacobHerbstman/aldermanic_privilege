SHELL := bash
include ../../shell_functions.make

UNCERTAINTY_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH

all: ../output/predecessor_successor_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf \
	../output/ward_alderman_summary_uncertainty_$(UNCERTAINTY_SPEC).csv \
	../output/within_ward_dotplot_uncertainty_$(UNCERTAINTY_SPEC).pdf \
	../output/variance_decomposition_uncertainty_$(UNCERTAINTY_SPEC).csv

../output/predecessor_successor_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf: analyze_within_ward_variation.R ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../input/chicago_alderman_panel.csv | ../output
	Rscript analyze_within_ward_variation.R ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv uncertainty_index "Regulatory Stringency Index" uncertainty_$(UNCERTAINTY_SPEC)

../output/ward_alderman_summary_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/within_ward_dotplot_uncertainty_$(UNCERTAINTY_SPEC).pdf ../output/variance_decomposition_uncertainty_$(UNCERTAINTY_SPEC).csv: ../output/predecessor_successor_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf

../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../input
	ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../input/chicago_alderman_panel.csv

include ../../generic.make
