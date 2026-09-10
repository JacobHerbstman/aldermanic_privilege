include ../../shared/code/shell_functions.make

all: ../report/construction_regressors.csv.log ../output/new_construction_analysis_data.csv ../report/new_construction_analysis_data.csv.log

../output/construction_regressors.csv: attach_construction_regressors.R Makefile \
	../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R \
	../../shared/code/assessor_classification.R \
	../input/preferred_new_construction_project_ledger.csv \
	../input/preferred_new_construction_boundary_scope.csv \
	../input/residential_selected_buildings.csv \
	../input/multicard_external_web_reviews.csv \
	../input/project_manual_reviews.csv \
	../input/boundary_segments_1320ft.gpkg \
	../input/chicago_alderman_terms.csv \
	../input/ward_controls_2006_2022.csv \
	../input/alderman_uncertainty_index_through2022.csv | ../output
	$(R) $<

../output/new_construction_analysis_data.csv: build_new_construction_analysis_data.R Makefile ../../setup_environment/code/packages.R ../output/construction_regressors.csv ../input/preferred_new_construction_zoning.csv | ../output
	$(R) $<

../report/construction_regressors.csv.log: ../../shared/code/report.py ../output/construction_regressors.csv | ../report
	$(PYTHON) $< ../output/construction_regressors.csv $@ project_id

../report/new_construction_analysis_data.csv.log: ../../shared/code/report.py ../output/new_construction_analysis_data.csv | ../report
	$(PYTHON) $< ../output/new_construction_analysis_data.csv $@ project_id

../input/preferred_new_construction_project_ledger.csv: ../../new_construction_cleaning/output/preferred_new_construction_project_ledger.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/preferred_new_construction_boundary_scope.csv: ../../new_construction_cleaning/output/preferred_new_construction_boundary_scope.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/preferred_new_construction_zoning.csv: ../../new_construction_cleaning/output/preferred_new_construction_zoning.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/residential_selected_buildings.csv: ../../new_construction_cleaning/output/residential_selected_buildings.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/multicard_external_web_reviews.csv: ../../new_construction_cleaning/adjudication/multicard_external_web_reviews.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/project_manual_reviews.csv: ../../new_construction_cleaning/adjudication/project_manual_reviews.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_terms.csv: ../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_controls_2006_2022.csv: ../../create_ward_controls/output/ward_controls_2006_2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/preferred_new_construction_project_ledger.csv \
	../input/preferred_new_construction_boundary_scope.csv \
	../input/preferred_new_construction_zoning.csv \
	../input/residential_selected_buildings.csv \
	../input/multicard_external_web_reviews.csv \
	../input/project_manual_reviews.csv \
	../input/boundary_segments_1320ft.gpkg \
	../input/chicago_alderman_terms.csv \
	../input/ward_controls_2006_2022.csv \
	../input/alderman_uncertainty_index_through2022.csv

include ../../shared/code/generic.make
