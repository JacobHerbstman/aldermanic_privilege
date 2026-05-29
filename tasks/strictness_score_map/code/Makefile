DATE ?= 2022-01
UNCERTAINTY_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022

all: ../output/uncertainty_score_map_$(UNCERTAINTY_SPEC)_$(DATE).pdf

../output/uncertainty_score_map_$(UNCERTAINTY_SPEC)_$(DATE).pdf: strictness_score_map.R ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../input/chicago_alderman_panel.csv ../input/ward_panel.gpkg | ../output
	Rscript $< $(DATE) $(UNCERTAINTY_SPEC)

../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../input/chicago_alderman_panel.csv ../input/ward_panel.gpkg

include ../../generic.make
