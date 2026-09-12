include ../../shared/code/shell_functions.make

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
	$(R) $<

../output/new_construction_analysis_data.csv: build_new_construction_analysis_data.R Makefile ../../setup_environment/code/packages.R ../output/construction_regressors.csv ../input/preferred_new_construction_zoning.csv ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../input/preferred_new_construction_project_ledger.csv: ../../construction_boundary_distances/output/preferred_new_construction_project_ledger.csv | ../input
	ln -sf $< $@

../input/preferred_new_construction_boundary_scope.csv: ../../construction_boundary_distances/output/preferred_new_construction_boundary_scope.csv | ../input
	ln -sf $< $@

../input/preferred_new_construction_zoning.csv: ../../construction_boundary_distances/output/preferred_new_construction_zoning.csv | ../input
	ln -sf $< $@

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@

../input/chicago_alderman_terms.csv: ../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	ln -sf $< $@

../input/ward_controls_2006_2022.csv: ../../create_ward_controls/output/ward_controls_2006_2022.csv | ../input
	ln -sf $< $@

../input/alderman_uncertainty_index_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@

link-inputs: ../input/preferred_new_construction_project_ledger.csv \
	../input/preferred_new_construction_boundary_scope.csv \
	../input/preferred_new_construction_zoning.csv \
	../input/boundary_segments_1320ft.gpkg \
	../input/chicago_alderman_terms.csv \
	../input/ward_controls_2006_2022.csv \
	../input/alderman_uncertainty_index_through2022.csv

include ../../shared/code/generic.make
