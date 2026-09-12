include ../../shared/code/shell_functions.make

all: ../output/density_sample_summary.csv \
	../output/density_rd.pdf \
	../output/density_sample_summary.tex

../%/density_sample_summary.csv \
	../%/density_sample_summary.tex: build_density_summary.R \
	Makefile ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R ../input/boundary_segments_1320ft.gpkg \
	../input/new_construction_analysis_data.csv ../input/preferred_new_construction_project_ledger.csv ../input/preferred_new_construction_boundary_scope.csv ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/density_rd.pdf: \
	build_density_main_results.R Makefile ../../setup_environment/code/packages.R \
	../input/new_construction_analysis_data.csv \
	../input/alderman_uncertainty_index_through2022.csv | ../output ../report
	$(R) $<

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../input/alderman_uncertainty_index_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@

../input/preferred_new_construction_project_ledger.csv: ../../construction_boundary_distances/output/preferred_new_construction_project_ledger.csv | ../input
	ln -sf $< $@

../input/preferred_new_construction_boundary_scope.csv: ../../construction_boundary_distances/output/preferred_new_construction_boundary_scope.csv | ../input
	ln -sf $< $@

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/new_construction_analysis_data.csv ../input/alderman_uncertainty_index_through2022.csv \
	../input/preferred_new_construction_project_ledger.csv ../input/preferred_new_construction_boundary_scope.csv \
	../input/boundary_segments_1320ft.gpkg

include ../../shared/code/generic.make
