script := merge_scores.R

parcels_out := ../output/parcels_with_ward_distances.csv
boundary_summary_out := ../output/boundary_distance_summary.csv
merge_qc_csv := ../output/merge_qc.csv
merge_qc_txt := ../output/merge_qc.txt

parcels_input := ../input/parcels_pre_scores.csv
score_input := ../input/aldermen_strictness_scores.csv

all: $(parcels_out) $(boundary_summary_out) $(merge_qc_csv) $(merge_qc_txt)

$(parcels_out): $(script) $(parcels_input) $(score_input) | ../output
	Rscript $(script)

$(boundary_summary_out): $(parcels_out)
$(merge_qc_csv): $(parcels_out)
$(merge_qc_txt): $(parcels_out)

$(parcels_input): ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf "$<" "$@"

$(score_input): ../../create_rezoning_strictness_scores/output/alderman_rezoning_strictness_scores.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: $(parcels_input) $(score_input)
