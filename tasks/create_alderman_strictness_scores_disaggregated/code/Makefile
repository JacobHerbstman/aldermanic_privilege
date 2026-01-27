SHELL := bash
include ../../shell_functions.make

# Primary outputs
OUTPUT_FILES := \
	../output/alderman_strictness_scores_disaggregated.csv \
	../output/strictness_index_plot.pdf \
	../output/strictness_index_plot.png \
	../output/stage1_regression_table.tex \
	../output/strictness_score_map_2014-01.pdf \
	../output/strictness_score_map_2025-01.pdf \
	../output/correlation_comparison.png

all: $(OUTPUT_FILES)

# Main rule: run R script when input data changes
$(OUTPUT_FILES): create_strictness_scores_disaggregated.R \
		../input/alderman_restrictiveness_scores_data.csv \
		../input/ward_panel.gpkg \
		../input/chicago_alderman_panel.csv \
		../input/alderman_restrictiveness_scores_month_FEs.csv | ../output
	$(R) create_strictness_scores_disaggregated.R

# Symbolic links to input data
../input/alderman_restrictiveness_scores_data.csv: ../../data_for_alderman_strictness_scores/output/alderman_restrictiveness_scores_data.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/alderman_restrictiveness_scores_month_FEs.csv: ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/alderman_restrictiveness_scores_data.csv \
	../input/ward_panel.gpkg \
	../input/chicago_alderman_panel.csv \
	../input/alderman_restrictiveness_scores_month_FEs.csv

