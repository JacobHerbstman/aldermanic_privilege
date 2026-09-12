include ../../shared/code/shell_functions.make

WORKERS := 8
GAP_THRESHOLDS := 0.25,0.50

all: ../output/density_score_robustness.tex \
	../output/project_permit_matches.csv

../output/density_score_robustness.tex: \
	build_density_score_robustness.R Makefile ../../setup_environment/code/packages.R \
	../../shared/code/alderman_uncertainty_helpers.R \
	../output/project_permit_matches.csv \
	../input/new_construction_analysis_data.csv \
	../input/permits_for_uncertainty_index.csv \
	../input/alderman_uncertainty_index_through2022.csv | ../output ../report
	$(R) $< $(WORKERS) $(GAP_THRESHOLDS)

../output/project_permit_matches.csv: build_project_permit_matches.R Makefile ../../setup_environment/code/packages.R ../adjudication/project_permit_matches.csv ../input/new_construction_analysis_data.csv ../input/building_permits_for_verification.gpkg ../input/preferred_project_year_geometry.gpkg ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf $< $@

../input/alderman_uncertainty_index_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@

../input/building_permits_for_verification.gpkg: ../../construction_project_permits/output/building_permits_for_verification.gpkg | ../input
	ln -sf $< $@

../input/preferred_project_year_geometry.gpkg: ../../construction_project_locations/output/preferred_project_year_geometry.gpkg | ../input
	ln -sf $< $@

link-inputs: \
	../input/new_construction_analysis_data.csv \
	../input/permits_for_uncertainty_index.csv \
	../input/alderman_uncertainty_index_through2022.csv \
	../input/building_permits_for_verification.gpkg \
	../input/preferred_project_year_geometry.gpkg

include ../../shared/code/generic.make
