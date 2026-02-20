script := screen_strictness_candidates.R

results_csv := ../output/candidate_screen_results.csv
rd_results_csv := ../output/candidate_screen_rd_results.csv
unmatched_csv := ../output/candidate_unmatched_names.csv
summary_txt := ../output/candidate_screen_summary.txt
winner_local_csv := ../output/alderman_rezoning_strictness_scores.csv
winner_upstream_csv := ../../create_rezoning_strictness_scores/output/alderman_rezoning_strictness_scores.csv

candidate_input := ../input/alderman_strictness_candidates.csv
parcel_input := ../input/parcels_pre_scores.csv

all: $(results_csv) $(rd_results_csv) $(unmatched_csv) $(summary_txt) $(winner_local_csv) $(winner_upstream_csv)

$(results_csv): $(script) $(candidate_input) $(parcel_input) | ../output
	Rscript $(script)

$(rd_results_csv): $(results_csv)
$(unmatched_csv): $(results_csv)
$(summary_txt): $(results_csv)
$(winner_local_csv): $(results_csv)
$(winner_upstream_csv): $(results_csv)

$(candidate_input): ../../create_rezoning_strictness_scores/output/alderman_strictness_candidates.csv | ../input
	ln -sf "$<" "$@"

$(parcel_input): ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: $(candidate_input) $(parcel_input)
