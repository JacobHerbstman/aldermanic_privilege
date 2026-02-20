script := create_rezoning_strictness_score.R

candidate_csv := ../output/alderman_strictness_candidates.csv
winner_csv := ../output/alderman_rezoning_strictness_scores.csv
rank_plot := ../output/strictness_candidates_rank_plot.pdf
qc_csv := ../output/strictness_candidates_qc.csv
qc_txt := ../output/strictness_candidates_qc.txt

rezoning_input := ../input/rezoning_dataset_usable_19990101_20260212.csv
panel_input := ../input/chicago_alderman_panel.csv
trusted_summary_input := ../input/alderman_rezoning_summary_19990101_20260212.csv

all: $(candidate_csv) $(winner_csv) $(rank_plot) $(qc_csv) $(qc_txt)

$(candidate_csv): $(script) $(rezoning_input) $(panel_input) $(trusted_summary_input) | ../output
	Rscript $(script)

$(winner_csv): $(candidate_csv)
$(rank_plot): $(candidate_csv)
$(qc_csv): $(candidate_csv)
$(qc_txt): $(candidate_csv)

$(rezoning_input): /Users/jacobherbstman/Desktop/alderman_data/tasks/rezoning_dataset/output/rezoning_dataset_usable_19990101_20260212.csv | ../input
	ln -sf "$<" "$@"

$(panel_input): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

$(trusted_summary_input): /Users/jacobherbstman/Desktop/alderman_data/tasks/rezoning_summary_stats/output/alderman_rezoning_summary_19990101_20260212.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: $(rezoning_input) $(panel_input) $(trusted_summary_input)
