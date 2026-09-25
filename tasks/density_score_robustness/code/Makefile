include ../../shared/code/shell_functions.make

all: ../output/density_score_robustness.tex

../output/density_score_robustness.tex: build_density_score_robustness.R ../input/new_construction_analysis_data.csv \
	../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_through2022.csv | ../output ../report
	$(R) $<

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@
../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf $< $@
../input/alderman_uncertainty_index_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@

link-inputs: ../input/new_construction_analysis_data.csv ../input/permits_for_uncertainty_index.csv \
	../input/alderman_uncertainty_index_through2022.csv

include ../../shared/code/generic.make
