include ../../shared/code/shell_functions.make

START_YEAR = 2006
END_YEAR = 2022
BANDWIDTH_FT = 500
BIN_WIDTH_FT = 100
CONTROLS = share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own
FIXED_EFFECTS = zone_group + segment_id + construction_year
CLUSTER = ward_pair

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
	../input/new_construction_analysis_data.csv | ../output ../report
	$(R) $< $(START_YEAR) $(END_YEAR) $(BANDWIDTH_FT) $(BIN_WIDTH_FT) "$(CONTROLS)" "$(FIXED_EFFECTS)" "$(CLUSTER)"

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../input/preferred_new_construction_project_ledger.csv: ../../prepare_new_construction/output/preferred_new_construction_project_ledger.csv | ../input
	ln -sf $< $@

../input/preferred_new_construction_boundary_scope.csv: ../../prepare_new_construction/output/preferred_new_construction_boundary_scope.csv | ../input
	ln -sf $< $@

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/new_construction_analysis_data.csv  \
	../input/preferred_new_construction_project_ledger.csv ../input/preferred_new_construction_boundary_scope.csv \
	../input/boundary_segments_1320ft.gpkg

include ../../shared/code/generic.make
