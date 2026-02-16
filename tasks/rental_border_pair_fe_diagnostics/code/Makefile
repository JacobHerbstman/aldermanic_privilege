SHELL := bash
include ../../shell_functions.make

OUTPUTS := \
	../output/rent_fe_driver_main_summary.csv \
	../output/rent_fe_driver_main_summary.md \
	../output/rent_fe_boundary_influence_fast.csv \
	../output/rent_fe_ward_side_influence_fast.csv \
	../output/rent_fe_building_influence_fast.csv \
	../output/rent_fe_boundary_influence_exact_loo.csv \
	../output/rent_fe_ward_side_influence_exact_loo.csv \
	../output/rent_fe_building_influence_exact_loo.csv \
	../output/rent_fe_lop_pair_month_metrics.csv \
	../output/rent_fe_lop_boundary_summary.csv \
	../output/rent_fe_lop_flags_top_boundaries.csv \
	../output/rent_fe_driver_maps.pdf \
	../output/rent_fe_driver_takeaways.md

FLAGGED_ZOOM_OUTPUTS := \
	../output/rent_fe_flagged_boundary_selection.csv \
	../output/rent_fe_flagged_boundary_side_summary.csv \
	../output/rent_fe_flagged_boundary_pair_summary.csv \
	../output/rent_fe_flagged_boundary_type_shares.csv \
	../output/rent_fe_flagged_boundary_unit_maps.pdf \
	../output/rent_fe_flagged_boundary_characteristics.pdf \
	../output/rent_fe_flagged_boundary_zoom_summary.md

FULL_STAMP := ../output/.rental_fe_driver_forensics_full.stamp
SMOKE_STAMP := ../output/smoke/.rental_fe_driver_forensics_smoke.stamp
SMOKE_OUTPUTS := $(subst ../output/,../output/smoke/,$(OUTPUTS))
FLAGGED_ZOOM_STAMP := ../output/.rental_fe_flagged_boundary_zoom_full.stamp
FLAGGED_ZOOM_SMOKE_STAMP := ../output/smoke/.rental_fe_flagged_boundary_zoom_smoke.stamp
FLAGGED_ZOOM_SMOKE_OUTPUTS := $(subst ../output/,../output/smoke/,$(FLAGGED_ZOOM_OUTPUTS))

INPUTS := \
	../input/rent_with_ward_distances.parquet \
	../input/ward_panel.gpkg \
	../input/border_certification_gates.csv \
	../input/border_pair_anomaly_samples.csv \
	../input/reference_fe_table_rental_bw500_pre_2023.csv

all: $(FULL_STAMP)

$(FULL_STAMP): run_rental_fe_driver_forensics.R $(INPUTS) | ../output
	$(R) run_rental_fe_driver_forensics.R \
		--input=../input/rent_with_ward_distances.parquet \
		--ward_panel=../input/ward_panel.gpkg \
		--bw_ft=500 \
		--window=pre_2023 \
		--sample_filter=all \
		--top_n_fast=200 \
		--top_n_exact_boundary=30 \
		--top_n_exact_ward_side=30 \
		--top_n_exact_building=50 \
		--building_round=5 \
		--lop_bins=0.05,0.10,0.20 \
		--near_boundary_ft=250,500 \
		--reference_csv=../input/reference_fe_table_rental_bw500_pre_2023.csv \
		--cert_gates=../input/border_certification_gates.csv \
		--anomaly_samples=../input/border_pair_anomaly_samples.csv \
		--output_dir=../output
	@for f in $(OUTPUTS); do \
		test -f $$f || (echo "Missing expected output: $$f" && exit 1); \
	done
	@touch $@

diagnostics-smoke: $(SMOKE_STAMP)

$(SMOKE_STAMP): run_rental_fe_driver_forensics.R $(INPUTS) | ../output
	@mkdir -p ../output/smoke
	$(R) run_rental_fe_driver_forensics.R \
		--input=../input/rent_with_ward_distances.parquet \
		--ward_panel=../input/ward_panel.gpkg \
		--bw_ft=500 \
		--window=pre_2023 \
		--sample_filter=all \
		--top_n_fast=40 \
		--top_n_exact_boundary=8 \
		--top_n_exact_ward_side=8 \
		--top_n_exact_building=12 \
		--building_round=5 \
		--lop_bins=0.05,0.10,0.20 \
		--near_boundary_ft=250,500 \
		--reference_csv=../input/reference_fe_table_rental_bw500_pre_2023.csv \
		--cert_gates=../input/border_certification_gates.csv \
		--anomaly_samples=../input/border_pair_anomaly_samples.csv \
		--output_dir=../output/smoke \
		--smoke=TRUE
	@for f in $(SMOKE_OUTPUTS); do \
		test -f $$f || (echo "Missing expected output: $$f" && exit 1); \
	done
	@touch $@

flagged-zoom: $(FLAGGED_ZOOM_STAMP)

$(FLAGGED_ZOOM_STAMP): run_rental_fe_flagged_boundary_zoom.R $(INPUTS) $(FULL_STAMP) | ../output
	$(R) run_rental_fe_flagged_boundary_zoom.R \
		--input=../input/rent_with_ward_distances.parquet \
		--ward_panel=../input/ward_panel.gpkg \
		--flags_csv=../output/rent_fe_lop_flags_top_boundaries.csv \
		--bw_ft=500 \
		--window=pre_2023 \
		--top_n_per_spec=8 \
		--map_year=2015 \
		--zoom_margin_ft=1500 \
		--point_sample_per_boundary=6000 \
		--output_dir=../output
	@for f in $(FLAGGED_ZOOM_OUTPUTS); do \
		test -f $$f || (echo "Missing expected output: $$f" && exit 1); \
	done
	@touch $@

flagged-zoom-smoke: $(FLAGGED_ZOOM_SMOKE_STAMP)

$(FLAGGED_ZOOM_SMOKE_STAMP): run_rental_fe_flagged_boundary_zoom.R $(INPUTS) $(SMOKE_STAMP) | ../output
	@mkdir -p ../output/smoke
	$(R) run_rental_fe_flagged_boundary_zoom.R \
		--input=../input/rent_with_ward_distances.parquet \
		--ward_panel=../input/ward_panel.gpkg \
		--flags_csv=../output/smoke/rent_fe_lop_flags_top_boundaries.csv \
		--bw_ft=500 \
		--window=pre_2023 \
		--top_n_per_spec=3 \
		--map_year=2015 \
		--zoom_margin_ft=1500 \
		--point_sample_per_boundary=3000 \
		--output_dir=../output/smoke \
		--smoke=TRUE
	@for f in $(FLAGGED_ZOOM_SMOKE_OUTPUTS); do \
		test -f $$f || (echo "Missing expected output: $$f" && exit 1); \
	done
	@touch $@

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/border_certification_gates.csv: ../../border_verification/output/border_certification_gates.csv | ../input
	ln -sf "$<" "$@"

../input/border_pair_anomaly_samples.csv: ../../border_verification/output/border_pair_anomaly_samples.csv | ../input
	ln -sf "$<" "$@"

../input/reference_fe_table_rental_bw500_pre_2023.csv: ../../rental_border_pair_fe/output/fe_table_rental_bw500_pre_2023.csv | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUTS)

include ../../generic.make
