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
	# arg order: input ward_panel bw_ft window sample_filter top_n_fast top_n_exact_boundary top_n_exact_ward_side top_n_exact_building building_round lop_bins near_boundary_ft reference_csv cert_gates anomaly_samples output_dir smoke
	$(R) run_rental_fe_driver_forensics.R \
		../input/rent_with_ward_distances.parquet \
		../input/ward_panel.gpkg \
		500 \
		pre_2023 \
		all \
		200 \
		30 \
		30 \
		50 \
		5 \
		0.05,0.10,0.20 \
		250,500 \
		../input/reference_fe_table_rental_bw500_pre_2023.csv \
		../input/border_certification_gates.csv \
		../input/border_pair_anomaly_samples.csv \
		../output \
		FALSE
	@for f in $(OUTPUTS); do \
		test -f $$f || (echo "Missing expected output: $$f" && exit 1); \
	done
	@touch $@

diagnostics-smoke: $(SMOKE_STAMP)

$(SMOKE_STAMP): run_rental_fe_driver_forensics.R $(INPUTS) | ../output
	@mkdir -p ../output/smoke
	# arg order: input ward_panel bw_ft window sample_filter top_n_fast top_n_exact_boundary top_n_exact_ward_side top_n_exact_building building_round lop_bins near_boundary_ft reference_csv cert_gates anomaly_samples output_dir smoke
	$(R) run_rental_fe_driver_forensics.R \
		../input/rent_with_ward_distances.parquet \
		../input/ward_panel.gpkg \
		500 \
		pre_2023 \
		all \
		40 \
		8 \
		8 \
		12 \
		5 \
		0.05,0.10,0.20 \
		250,500 \
		../input/reference_fe_table_rental_bw500_pre_2023.csv \
		../input/border_certification_gates.csv \
		../input/border_pair_anomaly_samples.csv \
		../output/smoke \
		TRUE
	@for f in $(SMOKE_OUTPUTS); do \
		test -f $$f || (echo "Missing expected output: $$f" && exit 1); \
	done
	@touch $@

flagged-zoom: $(FLAGGED_ZOOM_STAMP)

$(FLAGGED_ZOOM_STAMP): run_rental_fe_flagged_boundary_zoom.R $(INPUTS) $(FULL_STAMP) | ../output
	# arg order: input ward_panel flags_csv bw_ft window top_n_per_spec map_year zoom_margin_ft point_sample_per_boundary output_dir smoke
	$(R) run_rental_fe_flagged_boundary_zoom.R \
		../input/rent_with_ward_distances.parquet \
		../input/ward_panel.gpkg \
		../output/rent_fe_lop_flags_top_boundaries.csv \
		500 \
		pre_2023 \
		8 \
		2015 \
		1500 \
		6000 \
		../output \
		FALSE
	@for f in $(FLAGGED_ZOOM_OUTPUTS); do \
		test -f $$f || (echo "Missing expected output: $$f" && exit 1); \
	done
	@touch $@

flagged-zoom-smoke: $(FLAGGED_ZOOM_SMOKE_STAMP)

$(FLAGGED_ZOOM_SMOKE_STAMP): run_rental_fe_flagged_boundary_zoom.R $(INPUTS) $(SMOKE_STAMP) | ../output
	@mkdir -p ../output/smoke
	# arg order: input ward_panel flags_csv bw_ft window top_n_per_spec map_year zoom_margin_ft point_sample_per_boundary output_dir smoke
	$(R) run_rental_fe_flagged_boundary_zoom.R \
		../input/rent_with_ward_distances.parquet \
		../input/ward_panel.gpkg \
		../output/smoke/rent_fe_lop_flags_top_boundaries.csv \
		500 \
		pre_2023 \
		3 \
		2015 \
		1500 \
		3000 \
		../output/smoke \
		TRUE
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
