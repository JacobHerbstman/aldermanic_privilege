DATE ?= 2022-01
UNCERTAINTY_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022

all: ../output/uncertainty_score_map_$(UNCERTAINTY_SPEC)_$(DATE).pdf \
	../output/uncertainty_score_distribution_$(UNCERTAINTY_SPEC).pdf \
	../output/uncertainty_score_predecessor_successor_$(UNCERTAINTY_SPEC).pdf

../output/uncertainty_score_map_$(UNCERTAINTY_SPEC)_$(DATE).pdf: strictness_score_map.R ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../input/chicago_alderman_panel.csv ../input/ward_panel.gpkg | ../output
	$(R) $< $(DATE) $(UNCERTAINTY_SPEC)

../output/uncertainty_score_distribution_$(UNCERTAINTY_SPEC).pdf: plot_score_distribution_and_turnover.R ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../input/chicago_alderman_panel.csv | ../output
	$(R) $< $(UNCERTAINTY_SPEC)

../output/uncertainty_score_predecessor_successor_$(UNCERTAINTY_SPEC).pdf: ../output/uncertainty_score_distribution_$(UNCERTAINTY_SPEC).pdf
	@test -s "$@"

../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../input/chicago_alderman_panel.csv ../input/ward_panel.gpkg

include ../../shell_functions.make
include ../../generic.make
