include ../../../shared/code/shell_functions.make

PLOTTED_MEASURES := new_construction new_units

all: ../output/ward_election_changes.csv ../output/turnover_change_tests.csv \
	../output/own_amendments_by_alderman.csv ../output/current_term_permits_by_ward.csv \
	$(foreach measure,$(PLOTTED_MEASURES),../output/turnover_$(measure).png)

../%/ward_election_changes.csv ../%/turnover_change_tests.csv: test_turnover_changes.R \
	../output/ward_term_counts.csv | ../output ../report
	$(R) $<

../output/turnover_%.png: plot_turnover_changes.R ../output/ward_election_changes.csv | ../output
	$(R) $< $*

../output/ward_term_counts.csv: count_ward_terms.R ../input/zoning_amendments.csv \
	../input/permits_for_uncertainty_index.csv ../input/permit_construction.csv ../input/alderman_terms.csv \
	../input/community_area_sides.csv | ../output ../report
	$(R) $<

../output/own_amendments_by_alderman.csv: summarize_own_amendments.R ../input/zoning_amendments.csv \
	../input/alderman_terms.csv | ../output ../report
	$(R) $<

../output/current_term_permits_by_ward.csv: compare_current_term_permits.R \
	../input/building_permits_clean_2006_2022.gpkg ../input/building_permits_clean_2023_2026.gpkg \
	../input/Wards_2024.geojson ../input/alderman_terms.csv | ../output ../report
	$(R) $<

../input/zoning_amendments.csv: ../../zoning_stalls_and_delays/output/zoning_amendments.csv | ../input
	ln -sf $< $@
../input/permits_for_uncertainty_index.csv: \
	../../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf $< $@
../input/permit_construction.csv: ../../../prepare_permit_construction/output/permit_construction.csv | ../input
	ln -sf $< $@
../input/alderman_terms.csv: ../../../create_alderman_data/adjudication/alderman_terms.csv | ../input
	ln -sf $< $@
../input/community_area_sides.csv: ../sources/community_area_sides.csv | ../input
	ln -sf $< $@
../input/building_permits_clean_%.gpkg: ../../../clean_building_permits/output/building_permits_clean_%.gpkg | ../input
	ln -sf $< $@
../input/Wards_2024.geojson: ../../../../data_raw/Wards_2024.geojson | ../input
	ln -sf $< $@

link-inputs: ../input/zoning_amendments.csv ../input/permits_for_uncertainty_index.csv ../input/permit_construction.csv \
	../input/alderman_terms.csv \
	../input/community_area_sides.csv ../input/building_permits_clean_2006_2022.gpkg \
	../input/building_permits_clean_2023_2026.gpkg ../input/Wards_2024.geojson

include ../../../shared/code/generic.make
