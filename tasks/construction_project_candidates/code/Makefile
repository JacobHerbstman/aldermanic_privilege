include ../../shared/code/shell_functions.make

PREFERRED_ASSESSMENT_YEAR ?= 2022
FALLBACK_ASSESSMENT_YEAR ?= 2025

all: ../output/residential_project_candidate_inventory.csv \
	../output/commercial_entity_version_candidates.csv

../%/residential_project_candidate_inventory.csv \
	../%/residential_project_history_summary.csv \
	../%/residential_fractional_base_groups.csv \
	../%/residential_tieback_groups_full.csv \
	../%/residential_tieback_members_full.csv \
	../%/residential_multicard_cards.csv: build_residential_project_candidates.R \
	../input/residential_reviewed_card_selections.csv \
	../input/residential_assessor_history.parquet \
	../input/geocoded_residential_data.gpkg \
	../input/multifamily_data_cleaned.csv \
	../input/construction_parcel_boundary_distances.csv \
	../input/residential_cross_section.csv \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	Makefile | ../output ../report
	$(R) $< $(PREFERRED_ASSESSMENT_YEAR) $(FALLBACK_ASSESSMENT_YEAR)

../%/commercial_entity_version_candidates.csv \
	../%/commercial_address_family_candidates.csv \
	../%/commercial_entity_component_candidates.csv \
	../%/commercial_family_vintage_summary.csv \
	../%/commercial_production_family_members.csv \
	../%/commercial_project_family_review.csv: build_commercial_project_candidates.R \
	../input/commercial_value_raw.csv \
	../input/construction_parcel_boundary_distances.csv \
	../input/multifamily_data_cleaned.csv \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	Makefile | ../output ../report
	$(R) $<

../input/commercial_value_raw.csv: ../../../data_raw/construction_review/commercial_valuation_data.csv | ../input
	ln -sf $< $@

../input/construction_parcel_boundary_distances.csv: ../../construction_candidate_sites/output/construction_parcel_boundary_distances.csv | ../input
	ln -sf $< $@

../input/geocoded_residential_data.gpkg: ../../construction_assessor_records/output/geocoded_residential_data.gpkg | ../input
	ln -sf $< $@

../input/multifamily_data_cleaned.csv: ../../construction_assessor_records/output/multifamily_data_cleaned.csv | ../input
	ln -sf $< $@

../input/residential_assessor_history.parquet: ../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	ln -sf $< $@

../input/residential_cross_section.csv: ../../construction_assessor_records/output/residential_cross_section.csv | ../input
	ln -sf $< $@

../input/residential_reviewed_card_selections.csv: ../../new_construction_cleaning/output/residential_reviewed_card_selections.csv | ../input
	ln -sf $< $@

link-inputs: ../input/commercial_value_raw.csv \
	../input/construction_parcel_boundary_distances.csv \
	../input/geocoded_residential_data.gpkg \
	../input/multifamily_data_cleaned.csv \
	../input/residential_assessor_history.parquet \
	../input/residential_cross_section.csv \
	../input/residential_reviewed_card_selections.csv

include ../../shared/code/generic.make
