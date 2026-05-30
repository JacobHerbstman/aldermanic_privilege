MAX_PERMIT_YEAR ?= 2022

all: ../output/alderman_uncertainty_index_low_discretion_residualized.csv

../output/alderman_uncertainty_index_low_discretion_residualized.csv: build_low_discretion_score.R ../input/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../output
	Rscript $<

../output/alderman_uncertainty_index_new_construction.csv: build_new_construction_score.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript $< new_construction new_construction $(MAX_PERMIT_YEAR)

../output/alderman_uncertainty_index_new_construction_demolition.csv: build_new_construction_score.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript $< new_construction_demolition new_construction,demolition $(MAX_PERMIT_YEAR)

../output/alderman_uncertainty_index_restricted_renovation.csv: build_restricted_renovation_score.R ../../_lib/alderman_uncertainty_helpers.R ../../_lib/restricted_renovation_classification.R ../input/permits_for_uncertainty_index.csv ../input/building_permits_clean.gpkg | ../output
	Rscript $< $(MAX_PERMIT_YEAR)

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv: ../../within_ward_strictness/output/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv

include ../../generic.make
