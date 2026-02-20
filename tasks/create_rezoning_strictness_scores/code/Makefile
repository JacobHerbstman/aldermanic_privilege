script := create_rezoning_strictness_score.R

score_csv := ../output/alderman_rezoning_strictness_scores.csv
score_plot := ../output/rezoning_strictness_index.pdf
qc_csv := ../output/strictness_qc.csv
qc_txt := ../output/strictness_qc.txt

rezoning_input := ../input/rezoning_dataset_usable_19990101_20260212.csv
alderman_panel_input := ../input/chicago_alderman_panel.csv

all: $(score_csv) $(score_plot) $(qc_csv) $(qc_txt)

$(score_csv): $(script) $(rezoning_input) $(alderman_panel_input) | ../output
	Rscript $(script)

$(score_plot): $(score_csv)
$(qc_csv): $(score_csv)
$(qc_txt): $(score_csv)

$(rezoning_input): /Users/jacobherbstman/Desktop/alderman_data/tasks/rezoning_dataset/output/rezoning_dataset_usable_19990101_20260212.csv | ../input
	ln -sf "$<" "$@"

$(alderman_panel_input): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: $(rezoning_input) $(alderman_panel_input)
