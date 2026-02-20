script := create_rezoning_pca_scores.R

score_csv := ../output/alderman_rezoning_pca_scores.csv
loadings_csv := ../output/rezoning_pca_loadings.csv
variance_csv := ../output/rezoning_pca_variance.csv
corr_csv := ../output/rezoning_pca_vs_permit_correlation.csv
score_plot := ../output/rezoning_pca_strictness_index.pdf
qc_txt := ../output/rezoning_pca_qc.txt

rezoning_input := ../input/rezoning_dataset_usable_19990101_20260212.csv
alderman_panel_input := ../input/chicago_alderman_panel.csv
permit_month_input := ../input/alderman_restrictiveness_scores_month_FEs.csv
permit_ward_month_input := ../input/alderman_restrictiveness_scores_ward_month_FEs.csv

all: $(score_csv) $(loadings_csv) $(variance_csv) $(corr_csv) $(score_plot) $(qc_txt)

$(score_csv): $(script) $(rezoning_input) $(alderman_panel_input) $(permit_month_input) $(permit_ward_month_input) | ../output
	Rscript $(script)

$(loadings_csv): $(score_csv)
$(variance_csv): $(score_csv)
$(corr_csv): $(score_csv)
$(score_plot): $(score_csv)
$(qc_txt): $(score_csv)

$(rezoning_input): /Users/jacobherbstman/Desktop/alderman_data/tasks/rezoning_dataset/output/rezoning_dataset_usable_19990101_20260212.csv | ../input
	ln -sf "$<" "$@"

$(alderman_panel_input): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

$(permit_month_input): ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	ln -sf "$<" "$@"

$(permit_ward_month_input): ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_ward_month_FEs.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: $(rezoning_input) $(alderman_panel_input) $(permit_month_input) $(permit_ward_month_input)
