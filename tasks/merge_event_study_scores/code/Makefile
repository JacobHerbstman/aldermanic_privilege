include ../../shared/code/shell_functions.make

all: ../output/block_treatment_panel_frozen2014.csv

../output/block_treatment_panel_frozen2014.csv: merge_event_study_scores.R ../input/block_treatment_pre_scores.csv ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores_through2014.csv ../../setup_environment/code/packages.R Makefile ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../input/block_treatment_pre_scores.csv: ../../create_block_treatment_panel/output/block_treatment_pre_scores.csv | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/aldermen_uncertainty_scores_through2014.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2014.csv | ../input
	ln -sf $< $@

link-inputs: ../input/block_treatment_pre_scores.csv ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores_through2014.csv

include ../../shared/code/generic.make
