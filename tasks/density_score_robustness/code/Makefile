include ../../shared/code/shell_functions.make

WORKERS := 8
GAP_THRESHOLDS := 0.25,0.50

all: ../output/density_score_robustness.tex ../report/project_permit_matches.csv.log

../output/density_score_robustness.tex: \
	build_density_score_robustness.R Makefile ../../setup_environment/code/packages.R \
	../../shared/code/alderman_uncertainty_helpers.R \
	../output/project_permit_matches.csv \
	../input/new_construction_analysis_data.csv \
	../input/permits_for_uncertainty_index.csv \
	../input/alderman_uncertainty_index_through2022.csv | ../output
	$(R) $< $(WORKERS) $(GAP_THRESHOLDS)

../output/project_permit_matches.csv: build_project_permit_matches.R Makefile ../../setup_environment/code/packages.R ../adjudication/project_permit_matches.csv ../input/new_construction_analysis_data.csv ../input/building_permits_for_verification.gpkg ../input/preferred_project_year_geometry.gpkg | ../output
	$(R) $<

../report/project_permit_matches.csv.log: ../../shared/code/report.py ../output/project_permit_matches.csv | ../report
	$(PYTHON) $< ../output/project_permit_matches.csv $@ project_id permit_id

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/new_construction_analysis_data.csv \
	../input/permits_for_uncertainty_index.csv \
	../input/alderman_uncertainty_index_through2022.csv

../input/building_permits_for_verification.gpkg: ../../new_construction_cleaning/output/building_permits_for_verification.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/preferred_project_year_geometry.gpkg: ../../new_construction_cleaning/output/preferred_project_year_geometry.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

include ../../shared/code/generic.make
