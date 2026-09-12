include ../../shared/code/shell_functions.make

START_YEAR ?= 2006
END_YEAR ?= 2022
BANDWIDTH_FT ?= 500
BIN_WIDTH_FT ?= 100
HIGH_PPSF ?= 5000
REVIEW_PPSF ?= 2000
LOW_PPSF ?= 5
OUTLIER_ROW_ID ?= 96240099
DROP_INCONSISTENT_ROOMS ?= FALSE

all: ../output/quality_summary.csv \
	../output/quality_records.parquet \
	../output/property_review.csv \
	../output/property_sale_history.csv \
	../output/property_improvement_history.csv \
	../output/property_history_spells.csv \
	../output/apartment_count_evidence.csv \
	../output/apartment_count_summary.csv \
	../output/apartment_count_by_year.csv \
	../output/apartment_count_composition.csv \
	../output/date_summary.csv \
	../output/date_assignment_review.csv \
	../output/date_assignment_possibilities.csv \
	../output/date_cta_review.csv \
	../output/current_county_comparison.csv \
	../output/current_county_sales.csv \
	../output/current_county_improvements.csv \
	../output/current_characteristic_comparison.csv \
	../output/quality_rd_sensitivity.csv \
	../output/flagged_price_influence.csv \
	../output/date_rd_sensitivity.csv \
	../output/date_row_diagnosis.csv \
	../output/date_diagnosis_by_month.csv

../output/quality_summary.csv: build_sales_quality_audit.R Makefile ../../shared/code/canonical_geometry_helpers.R ../input/sales_with_hedonics_amenities.parquet ../input/clean_sales_with_hedonics_amenities.parquet ../input/residential_sales_clean.parquet ../input/parcel_sales_city.csv ../input/residential_improvement_characteristics_full.csv ../input/price_boundary_property_type_fe_estimates.csv | ../output
	$(R) $< $(START_YEAR) $(END_YEAR) $(BANDWIDTH_FT) $(HIGH_PPSF) $(REVIEW_PPSF) $(LOW_PPSF)
	touch -r "$@" ../output/quality_records.parquet ../output/property_review.csv ../output/property_sale_history.csv ../output/property_improvement_history.csv ../output/property_history_spells.csv ../output/apartment_count_evidence.csv ../output/apartment_count_summary.csv ../output/apartment_count_by_year.csv ../output/apartment_count_composition.csv

../output/quality_records.parquet ../output/property_review.csv ../output/property_sale_history.csv ../output/property_improvement_history.csv ../output/property_history_spells.csv ../output/apartment_count_evidence.csv ../output/apartment_count_summary.csv ../output/apartment_count_by_year.csv ../output/apartment_count_composition.csv: ../output/quality_summary.csv
	@test -s "$@"

../output/date_summary.csv: audit_sales_dates.R ../../shared/code/canonical_geometry_helpers.R ../output/quality_records.parquet ../input/chicago_alderman_terms.csv ../input/aldermen_uncertainty_scores.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/cta_stops.gpkg | ../output
	$(R) $<
	touch -r "$@" ../output/date_assignment_review.csv ../output/date_assignment_possibilities.csv ../output/date_cta_review.csv

../output/date_assignment_review.csv ../output/date_assignment_possibilities.csv ../output/date_cta_review.csv: ../output/date_summary.csv
	@test -s "$@"

../output/date_rd_sensitivity.csv: diagnose_sales_dates.R Makefile ../output/quality_records.parquet ../output/date_assignment_review.csv ../output/date_assignment_possibilities.csv ../output/current_county_comparison.csv ../input/price_boundary_property_type_fe_estimates.csv | ../output
	$(R) $< $(BANDWIDTH_FT) $(BIN_WIDTH_FT)
	touch -r "$@" ../output/date_row_diagnosis.csv ../output/date_diagnosis_by_month.csv

../output/date_row_diagnosis.csv ../output/date_diagnosis_by_month.csv: ../output/date_rd_sensitivity.csv
	@test -s "$@"

../output/current_county_comparison.csv: check_current_county_records.R ../output/quality_records.parquet ../output/property_review.csv ../output/date_assignment_review.csv | ../output
	$(R) $<
	touch -r "$@" ../output/current_county_sales.csv ../output/current_county_improvements.csv ../output/current_characteristic_comparison.csv

../output/current_county_sales.csv ../output/current_county_improvements.csv ../output/current_characteristic_comparison.csv: ../output/current_county_comparison.csv
	@test -s "$@"

../output/quality_rd_sensitivity.csv: estimate_quality_sensitivity.R Makefile ../output/quality_records.parquet ../input/price_boundary_property_type_fe_estimates.csv | ../output
	$(R) $< $(REVIEW_PPSF) $(OUTLIER_ROW_ID)
	touch -r "$@" ../output/flagged_price_influence.csv

../output/flagged_price_influence.csv: ../output/quality_rd_sensitivity.csv
	@test -s "$@"

../output/sales_with_hedonics.parquet: ../../prep_sales_border_data/code/prep_sales_data.R Makefile ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet | ../output
	$(R) $< $(DROP_INCONSISTENT_ROOMS)

../output/sales_with_hedonics_amenities.parquet: ../../sales_border_pair_fe/code/enrich_sales_with_amenity_distances.R ../../shared/code/amenity_distance_helpers.R ../input/sales_with_hedonics.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	$(R) $<

../input/sales_with_ward_distances.csv: ../../merge_sales_scores/output/sales_with_ward_distances_through2022.csv | ../input
	ln -sf $< $@

../input/residential_improvements_panel.parquet: ../../process_residential_improvements_full/output/residential_improvements_panel.parquet | ../input
	ln -sf $< $@

../input/sales_with_hedonics.parquet: ../output/sales_with_hedonics.parquet | ../input
	ln -sf $< $@

../input/sales_with_hedonics_amenities.parquet: ../output/sales_with_hedonics_amenities.parquet | ../input
	ln -sf $< $@

../input/clean_sales_with_hedonics_amenities.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	ln -sf $< $@

../input/residential_sales_clean.parquet: ../../clean_residential_sales/output/residential_sales_clean.parquet | ../input
	ln -sf $< $@

../input/parcel_sales_city.csv: ../../download_parcel_sales_data/output/parcel_sales_city.csv | ../input
	ln -sf $< $@

../input/residential_improvement_characteristics_full.csv: ../../download_residential_improvements_full/output/residential_improvement_characteristics_full.csv | ../input
	ln -sf $< $@

../input/price_boundary_property_type_fe_estimates.csv: ../../price_boundary_results/output/price_boundary_property_type_fe_estimates.csv | ../input
	ln -sf $< $@

../input/chicago_alderman_terms.csv: ../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	ln -sf $< $@

../input/aldermen_uncertainty_scores.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

../input/cta_stops.gpkg: ../../prepare_amenities_data/output/cta_stops.gpkg | ../input
	ln -sf $< $@

../input/schools_2015.gpkg: ../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	ln -sf $< $@

../input/parks.gpkg: ../../prepare_amenities_data/output/parks.gpkg | ../input
	ln -sf $< $@

../input/major_streets.gpkg: ../../prepare_amenities_data/output/major_streets.gpkg | ../input
	ln -sf $< $@

../input/gis_osm_water_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	ln -sf $< $@

link-inputs: ../input/sales_with_hedonics_amenities.parquet ../input/clean_sales_with_hedonics_amenities.parquet ../input/residential_sales_clean.parquet ../input/parcel_sales_city.csv ../input/residential_improvement_characteristics_full.csv ../input/price_boundary_property_type_fe_estimates.csv ../input/chicago_alderman_terms.csv ../input/aldermen_uncertainty_scores.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/cta_stops.gpkg ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet ../input/sales_with_hedonics.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../shared/code/generic.make
