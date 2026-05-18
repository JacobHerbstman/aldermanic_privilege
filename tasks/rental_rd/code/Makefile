SHELL := bash

BANDWIDTH_FT ?= 500
BINS_PER_SIDE ?= 15
SAMPLE ?= all
SAMPLES := all clean_location no_modal_pair_change no_modal_ward_change no_questionable_address

all: \
	../output/rd_sample_flag_balance.csv \
	../output/rd_segment_contract_audit.csv \
	../output/rd_segment_month_flag_imbalance.csv \
	../output/rd_sample_definition_summary.csv \
	$(foreach s,$(SAMPLES),../output/rental_rd_flat_bw$(BANDWIDTH_FT)_2014_2022_$(s)_no_controls.pdf) \
	$(foreach s,$(SAMPLES),../output/rental_rd_flat_bw$(BANDWIDTH_FT)_2014_2022_$(s)_no_controls.csv) \
	$(foreach s,$(SAMPLES),../output/rental_rd_bins_bw$(BANDWIDTH_FT)_2014_2022_$(s)_no_controls.csv) \
	$(foreach s,$(SAMPLES),../output/rental_rd_diagnostics_bw$(BANDWIDTH_FT)_2014_2022_$(s)_no_controls.csv) \
	$(foreach s,$(SAMPLES),../output/rental_rd_flat_bw$(BANDWIDTH_FT)_2014_2022_$(s)_controls.pdf) \
	$(foreach s,$(SAMPLES),../output/rental_rd_flat_bw$(BANDWIDTH_FT)_2014_2022_$(s)_controls.csv) \
	$(foreach s,$(SAMPLES),../output/rental_rd_bins_bw$(BANDWIDTH_FT)_2014_2022_$(s)_controls.csv) \
	$(foreach s,$(SAMPLES),../output/rental_rd_diagnostics_bw$(BANDWIDTH_FT)_2014_2022_$(s)_controls.csv)

../output/rental_rd_flat_bw$(BANDWIDTH_FT)_2014_2022_%_no_controls.pdf: run_rental_rd.R ../input/rent_with_ward_distances.parquet ../output/rd_sample_flag_balance.csv | ../output
	# arg order: bandwidth_ft sample use_controls bins_per_side
	$(R) $< $(BANDWIDTH_FT) $* FALSE $(BINS_PER_SIDE)

../output/rental_rd_flat_bw$(BANDWIDTH_FT)_2014_2022_%_no_controls.csv \
../output/rental_rd_bins_bw$(BANDWIDTH_FT)_2014_2022_%_no_controls.csv \
../output/rental_rd_diagnostics_bw$(BANDWIDTH_FT)_2014_2022_%_no_controls.csv: ../output/rental_rd_flat_bw$(BANDWIDTH_FT)_2014_2022_%_no_controls.pdf
	@test -s "$@"

../output/rental_rd_flat_bw$(BANDWIDTH_FT)_2014_2022_%_controls.pdf: run_rental_rd.R ../input/rent_with_ward_distances.parquet ../output/rd_sample_flag_balance.csv | ../output
	# arg order: bandwidth_ft sample use_controls bins_per_side
	$(R) $< $(BANDWIDTH_FT) $* TRUE $(BINS_PER_SIDE)

../output/rental_rd_flat_bw$(BANDWIDTH_FT)_2014_2022_%_controls.csv \
../output/rental_rd_bins_bw$(BANDWIDTH_FT)_2014_2022_%_controls.csv \
../output/rental_rd_diagnostics_bw$(BANDWIDTH_FT)_2014_2022_%_controls.csv: ../output/rental_rd_flat_bw$(BANDWIDTH_FT)_2014_2022_%_controls.pdf
	@test -s "$@"

../output/rd_sample_flag_balance.csv: run_rental_rd_quality_audit.R ../../_lib/canonical_geometry_helpers.R ../input/rent_with_ward_distances.parquet ../input/boundary_segments_1320ft.gpkg | ../output
	# arg order: bandwidth_ft sample
	$(R) $< $(BANDWIDTH_FT) $(SAMPLE)

../output/rd_segment_contract_audit.csv \
../output/rd_segment_month_flag_imbalance.csv \
../output/rd_sample_definition_summary.csv: ../output/rd_sample_flag_balance.csv
	@test -s "$@"

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet ../input/boundary_segments_1320ft.gpkg

include ../../shell_functions.make
include ../../generic.make
