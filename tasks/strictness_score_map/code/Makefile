SHELL := bash
include ../../shell_functions.make

DATE := 2025-01

all: ../output/strictness_score_map_$(DATE).pdf

../output/strictness_score_map_$(DATE).pdf: strictness_score_map.R ../input/alderman_restrictiveness_scores_month_FEs.csv ../input/chicago_alderman_panel.csv ../input/ward_panel.gpkg | ../output
	Rscript strictness_score_map.R $(DATE)

../input/alderman_restrictiveness_scores_month_FEs.csv: ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/alderman_restrictiveness_scores_month_FEs.csv ../input/chicago_alderman_panel.csv ../input/ward_panel.gpkg
