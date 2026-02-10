SHELL := bash
include ../../shell_functions.make

DATE := 2025-01
UNCERTAINTY_SPEC := ptfeFALSE_rtfeFALSE_porchTRUE_cafeFALSE_2stage

all: ../output/uncertainty_score_map_$(UNCERTAINTY_SPEC)_$(DATE).pdf

../output/uncertainty_score_map_$(UNCERTAINTY_SPEC)_$(DATE).pdf: strictness_score_map.R ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../input/chicago_alderman_panel.csv ../input/ward_panel.gpkg | ../output
	Rscript strictness_score_map.R $(DATE) ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv uncertainty_index ../output/uncertainty_score_map_$(UNCERTAINTY_SPEC)_$(DATE).pdf "Regulatory uncertainty index" "Regulatory Uncertainty Index by Ward ($(DATE))"

../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../input/chicago_alderman_panel.csv ../input/ward_panel.gpkg
