include ../../../shared/code/shell_functions.make

all: ../output/density_results_through_2026.csv

../output/density_results_through_2026.csv: density_results_through_2026.R ../output/new_construction_analysis_data_through_2026.csv \
	../input/scores_through_2026.csv ../input/alderman_uncertainty_index_through2022.csv \
	../input/new_construction_analysis_data.csv | ../output
	$(R) $<

../output/new_construction_analysis_data_through_2026.csv: build_new_construction_analysis_data.R \
	../output/permit_construction.csv ../output/construction_permits.csv ../output/boundary_segments_1320ft.gpkg \
	../output/chicago_alderman_terms.csv ../output/ward_controls_2023_map.csv ../input/ward_controls_2006_2022.csv \
	../input/scores_through_2026.csv ../input/historical_zoning_2006_candidate.gpkg ../input/zoning_nov2012.zip \
	../input/zoning_sep2014.zip ../input/zoning_jan2016.zip ../input/zoning_sep2025.geojson | ../output ../report
	$(R) $<

../output/ward_controls_2023_map.csv: build_ward_controls_2023_map.R ../output/ward_panel_through_2026.gpkg \
	../input/ward_panel.gpkg ../input/census_acs.csv ../input/census_block_groups.gpkg \
	../input/ward_controls_2006_2022.csv | ../output
	$(R) $<

../output/permit_construction.csv: build_ledger.R ../output/construction_buildings.csv ../output/ward_panel_through_2026.gpkg \
	../output/ward_pair_boundaries.gpkg | ../output ../report
	$(R) $<

../output/construction_buildings.csv: build_construction_buildings.R construction_rules.R ../output/permit_buildings.csv \
	../output/construction_permits.csv ../input/manual_decisions.csv ../input/building_permits_full.csv \
	../input/residential_assessor_history.parquet ../input/condominium_characteristics.csv ../input/commercial_valuation_data.csv \
	../input/parcel_addresses_2025_chicago.csv ../input/parcel_centroids.csv ../input/building_footprints_2022_chicago.gpkg | ../output ../report
	$(R) $<

../output/permit_buildings.csv: measure_buildings.R construction_rules.R ../output/construction_permits.csv \
	../input/manual_decisions.csv ../input/residential_assessor_history.parquet ../input/condominium_characteristics.csv \
	../input/commercial_valuation_data.csv ../input/parcel_addresses_2025_chicago.csv ../input/parcel_centroids.csv | ../output ../report
	$(R) $<

../output/construction_permits.csv: select_permits.R ../input/building_permits_full.csv | ../output ../report
	$(R) $<

../%/boundary_segments_1320ft.gpkg ../%/ward_pair_boundaries.gpkg: build_boundary_segments.R \
	../output/ward_panel_through_2026.gpkg | ../output ../report
	$(R) $<

../output/ward_panel_through_2026.gpkg: build_ward_panel_through_2026.R ../input/ward_panel.gpkg ../input/Wards_2024.geojson | ../output
	$(R) $<

../%/chicago_alderman_panel.csv ../%/chicago_alderman_terms.csv: create_alderman_panel.R \
	../input/alderman_terms_through_2026.csv | ../output ../report
	$(R) $<

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@
../input/Wards_2024.geojson: ../../../../data_raw/Wards_2024.geojson | ../input
	ln -sf $< $@
../input/alderman_terms_through_2026.csv: ../../scores_through_2026/sources/alderman_terms_through_2026.csv | ../input
	ln -sf $< $@
../input/scores_through_2026.csv: ../../scores_through_2026/output/scores_through_2026.csv | ../input
	ln -sf $< $@
../input/alderman_uncertainty_index_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@
../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@
../input/building_permits_full.csv: ../../../../data_raw/construction_review/building_permits_full.csv | ../input
	ln -sf $< $@
../input/commercial_valuation_data.csv: ../../../../data_raw/construction_review/commercial_valuation_data.csv | ../input
	ln -sf $< $@
../input/manual_decisions.csv: ../../../prepare_permit_construction/adjudication/manual_decisions.csv | ../input
	ln -sf $< $@
../input/residential_assessor_history.parquet: ../../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	ln -sf $< $@
../input/condominium_characteristics.csv: ../../../download_condominium_characteristics/output/condominium_characteristics.csv | ../input
	ln -sf $< $@
../input/parcel_addresses_2025_chicago.csv: ../../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv | ../input
	ln -sf $< $@
../input/parcel_centroids.csv: ../../../download_parcel_centroids/output/parcel_centroids.csv | ../input
	ln -sf $< $@
../input/building_footprints_2022_chicago.gpkg: ../../../download_building_footprints_2022/output/building_footprints_2022_chicago.gpkg | ../input
	ln -sf $< $@
../input/ward_controls_2006_2022.csv: ../../../create_ward_controls/output/ward_controls_2006_2022.csv | ../input
	ln -sf $< $@
../input/census_acs.csv: ../../../create_ward_controls/output/census_acs.csv | ../input
	ln -sf $< $@
../input/census_block_groups.gpkg: ../../../create_ward_controls/output/census_block_groups.gpkg | ../input
	ln -sf $< $@
../input/historical_zoning_2006_candidate.gpkg: ../../../construction_zoning_history/output/historical_zoning_2006_candidate.gpkg | ../input
	ln -sf $< $@
../input/zoning_%.zip: ../../../../data_raw/construction_review/zoning_%.zip | ../input
	ln -sf $< $@
../input/zoning_sep2025.geojson: ../../../../data_raw/construction_review/zoning_sep2025.geojson | ../input
	ln -sf $< $@

link-inputs: ../input/ward_panel.gpkg ../input/Wards_2024.geojson ../input/alderman_terms_through_2026.csv \
	../input/scores_through_2026.csv ../input/alderman_uncertainty_index_through2022.csv \
	../input/new_construction_analysis_data.csv ../input/building_permits_full.csv ../input/commercial_valuation_data.csv \
	../input/manual_decisions.csv ../input/residential_assessor_history.parquet ../input/condominium_characteristics.csv \
	../input/parcel_addresses_2025_chicago.csv ../input/parcel_centroids.csv ../input/building_footprints_2022_chicago.gpkg \
	../input/ward_controls_2006_2022.csv ../input/census_acs.csv ../input/census_block_groups.gpkg \
	../input/historical_zoning_2006_candidate.gpkg ../input/zoning_nov2012.zip ../input/zoning_sep2014.zip \
	../input/zoning_jan2016.zip ../input/zoning_sep2025.geojson

include ../../../shared/code/generic.make
