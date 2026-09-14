include ../../shared/code/shell_functions.make

BOUNDARY_WINDOW_FT := 1500
ADJACENT_YEAR_WINDOW := 1
COINCIDENT_DISTANCE_FT := 1

all: ../output/construction_regressors.csv \
	../output/new_construction_analysis_data.csv

../output/construction_regressors.csv: attach_construction_regressors.R Makefile \
	../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R \
	../input/preferred_new_construction_project_ledger.csv \
	../input/preferred_new_construction_boundary_scope.csv \
	../input/boundary_segments_1320ft.gpkg \
	../input/chicago_alderman_terms.csv \
	../input/ward_controls_2006_2022.csv \
	../input/alderman_uncertainty_index_through2022.csv ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $< $(BOUNDARY_WINDOW_FT)

../output/new_construction_analysis_data.csv: build_new_construction_analysis_data.R Makefile \
	../../setup_environment/code/packages.R ../output/construction_regressors.csv \
	../input/historical_zoning_project_construction_year.csv ../input/historical_zoning_2006_candidate.gpkg \
	../input/zoning_nov2012.zip ../input/zoning_sep2014.zip ../input/zoning_jan2016.zip ../input/zoning_sep2025.geojson \
	../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $< $(ADJACENT_YEAR_WINDOW) $(COINCIDENT_DISTANCE_FT)

../input/preferred_new_construction_project_ledger.csv: ../../prepare_new_construction/output/preferred_new_construction_project_ledger.csv | ../input
	ln -sf $< $@

../input/preferred_new_construction_boundary_scope.csv: ../../prepare_new_construction/output/preferred_new_construction_boundary_scope.csv | ../input
	ln -sf $< $@

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@

../input/chicago_alderman_terms.csv: ../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	ln -sf $< $@

../input/ward_controls_2006_2022.csv: ../../create_ward_controls/output/ward_controls_2006_2022.csv | ../input
	ln -sf $< $@

../input/alderman_uncertainty_index_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@

../input/historical_zoning_project_construction_year.csv: ../../construction_zoning_history/output/historical_zoning_project_construction_year.csv | ../input
	ln -sf $< $@

../input/historical_zoning_2006_candidate.gpkg: ../../construction_zoning_history/output/historical_zoning_2006_candidate.gpkg | ../input
	ln -sf $< $@

../input/zoning_nov2012.zip: ../../../data_raw/construction_review/zoning_nov2012.zip | ../input
	ln -sf $< $@

../input/zoning_sep2014.zip: ../../../data_raw/construction_review/zoning_sep2014.zip | ../input
	ln -sf $< $@

../input/zoning_jan2016.zip: ../../../data_raw/construction_review/zoning_jan2016.zip | ../input
	ln -sf $< $@

../input/zoning_sep2025.geojson: ../../../data_raw/construction_review/zoning_sep2025.geojson | ../input
	ln -sf $< $@

link-inputs: ../input/historical_zoning_project_construction_year.csv \
	../input/historical_zoning_2006_candidate.gpkg \
	../input/zoning_nov2012.zip \
	../input/zoning_sep2014.zip \
	../input/zoning_jan2016.zip \
	../input/zoning_sep2025.geojson \
	../input/preferred_new_construction_project_ledger.csv \
	../input/preferred_new_construction_boundary_scope.csv \
	../input/boundary_segments_1320ft.gpkg \
	../input/chicago_alderman_terms.csv \
	../input/ward_controls_2006_2022.csv \
	../input/alderman_uncertainty_index_through2022.csv

include ../../shared/code/generic.make
