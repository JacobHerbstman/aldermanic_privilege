all: ../output/alderman_uncertainty_index_low_discretion_residualized.csv

../output/alderman_uncertainty_index_low_discretion_residualized.csv: build_low_discretion_score.R ../input/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../output
	Rscript $<

../input/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv: ../../within_ward_strictness/output/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv

include ../../generic.make
