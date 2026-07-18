BANDWIDTH_M ?= 152.4
MIN_CONSTRUCTION_YEAR ?= 2006
MAX_CONSTRUCTION_YEAR ?= 2022
MAX_SPATIAL_MATCH_FT ?= 100
MAX_PERMIT_LEAD_YEARS ?= 8
MAX_PERMIT_LAG_YEARS ?= 1

all: ../output/density_timing_audit_summary.csv ../output/density_timing_project_matches.csv ../output/density_timing_robustness.csv

../output/density_timing_audit_summary.csv: audit_density_treatment_timing.R ../../../_lib/border_pair_helpers.R ../../../_lib/canonical_geometry_helpers.R ../input/parcels_with_ward_distances.csv ../input/parcels_with_geometry.gpkg ../input/building_permits_clean.gpkg ../input/building_permits_text_features.csv.gz ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores.csv ../input/ward_panel.gpkg | ../output
	Rscript $< $(BANDWIDTH_M) $(MIN_CONSTRUCTION_YEAR) $(MAX_CONSTRUCTION_YEAR) $(MAX_SPATIAL_MATCH_FT) $(MAX_PERMIT_LEAD_YEARS) $(MAX_PERMIT_LAG_YEARS)

../output/density_timing_project_matches.csv: ../output/density_timing_audit_summary.csv

../output/density_timing_robustness.csv: ../output/density_timing_audit_summary.csv

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_geometry.gpkg: ../../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/building_permits_clean.gpkg: ../../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/building_permits_text_features.csv.gz: ../../../building_permits_scraping/output/building_permits_text_features.csv.gz | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/parcels_with_geometry.gpkg ../input/building_permits_clean.gpkg ../input/building_permits_text_features.csv.gz ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores.csv ../input/ward_panel.gpkg

include ../../../generic.make
