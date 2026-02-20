SHELL := bash
include ../../shell_functions.make

OUTPUTS := \
	../output/border_verification_dataset_overview.csv \
	../output/border_verification_summary.csv \
	../output/border_verification_panel_checks.csv \
	../output/border_verification_invalid_pairs_sales.csv \
	../output/border_verification_invalid_pairs_rent.csv \
	../output/border_verification_findings.md \
	../output/border_certification_gates.csv \
	../output/border_certification_report.md \
	../output/border_pair_anomaly_samples.csv \
	../output/border_pair_anomaly_maps.pdf

CERT_OUTPUTS := \
	../output/border_certification_gates.csv \
	../output/border_certification_report.md \
	../output/border_pair_anomaly_samples.csv \
	../output/border_pair_anomaly_maps.pdf

COMPARE_OUTPUTS := \
	../output/density_rental_change_summary.csv \
	../output/density_rental_change_summary.md \
	../output/rental_effect_concentration_report.md

FORENSICS_OUTPUTS := \
	../output/geometry_forensics_maps_before_after.pdf \
	../output/geometry_forensics_summary_before_after.csv \
	../output/geometry_forensics_summary_before_after.md \
	../output/geometry_forensics_pair_flow_before_after.csv \
	../output/geometry_forensics_code_change_ledger.csv \
	../output/geometry_forensics_code_change_ledger.md \
	../output/geometry_forensics_approval_checklist.md

INPUTS := \
	../input/parcels_pre_scores.csv \
	../input/parcels_with_ward_distances.csv \
	../input/sales_pre_scores.csv \
	../input/sales_with_ward_distances.csv \
	../input/rent_pre_scores_full.parquet \
	../input/rent_with_ward_distances_full.parquet \
	../input/block_treatment_pre_scores.csv \
	../input/block_treatment_panel.csv \
	../input/sales_transaction_panel.parquet \
	../input/rental_listing_panel.parquet \
	../input/ward_panel.gpkg \
	../input/geocoded_residential_data.gpkg

SNAPSHOT_DIR ?=
SKIP_THRESHOLDS ?= FALSE

all: $(OUTPUTS)

$(OUTPUTS): run_border_verification.R $(INPUTS) | ../output
	$(R) run_border_verification.R

certify: run_border_verification.R $(INPUTS) | ../output
	# arg order: certify_only skip_thresholds output_dir
	$(R) run_border_verification.R TRUE $(SKIP_THRESHOLDS) ../output

compare: compare_density_rental_before_after.R $(CERT_OUTPUTS) | ../output
	@if [ -z "$(SNAPSHOT_DIR)" ]; then echo "SNAPSHOT_DIR is required for compare target"; exit 1; fi
	# arg order: snapshot_dir output_dir
	$(R) compare_density_rental_before_after.R "$(SNAPSHOT_DIR)" ../output

forensics-pack: build_geometry_forensics_pack.R $(INPUTS) $(CERT_OUTPUTS) | ../output
	# arg order: mode_before mode_after top_n_pairs top_n_points output_dir smoke seed
	$(R) build_geometry_forensics_pack.R legacy_before certified_after 20 50000 ../output FALSE 20260216

forensics-smoke: build_geometry_forensics_pack.R $(INPUTS) $(CERT_OUTPUTS) | ../output
	# arg order: mode_before mode_after top_n_pairs top_n_points output_dir smoke seed
	$(R) build_geometry_forensics_pack.R legacy_before certified_after 10 8000 ../output TRUE 20260216

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf $< $@

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf $< $@

../input/sales_pre_scores.csv: ../../calculate_sale_distances/output/sales_pre_scores.csv | ../input
	ln -sf $< $@

../input/sales_with_ward_distances.csv: ../../merge_event_study_scores/output/sales_with_ward_distances.csv | ../input
	ln -sf $< $@

../input/rent_pre_scores_full.parquet: ../../calculate_rent_distances/output/rent_pre_scores_full.parquet | ../input
	ln -sf $< $@

../input/rent_with_ward_distances_full.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf $< $@

../input/block_treatment_pre_scores.csv: ../../create_block_treatment_panel/output/block_treatment_pre_scores.csv | ../input
	ln -sf $< $@

../input/block_treatment_panel.csv: ../../merge_event_study_scores/output/block_treatment_panel.csv | ../input
	ln -sf $< $@

../input/sales_transaction_panel.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel.parquet | ../input
	ln -sf $< $@

../input/rental_listing_panel.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel.parquet | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/geocoded_residential_data.gpkg: ../../geocode_ccao_data/output/geocoded_residential_data.gpkg | ../input
	ln -sf $< $@

link-inputs: $(INPUTS)

include ../../generic.make
