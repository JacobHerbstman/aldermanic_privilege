include ../../shared/code/shell_functions.make

all: ../output/new_construction_analysis_data.csv

../output/new_construction_analysis_data.csv: build_new_construction_analysis_data.R ../input/permit_construction.csv \
	../input/construction_permits.csv ../input/boundary_segments_1320ft.gpkg ../input/chicago_alderman_terms.csv \
	../input/ward_controls_2006_2022.csv ../input/alderman_uncertainty_index_through2022.csv \
	../input/historical_zoning_2006_candidate.gpkg ../input/zoning_nov2012.zip ../input/zoning_sep2014.zip \
	../input/zoning_jan2016.zip ../input/zoning_sep2025.geojson | ../output ../report
	$(R) $<

../input/permit_construction.csv: ../../prepare_permit_construction/output/permit_construction.csv | ../input
	ln -sf $< $@
../input/construction_permits.csv: ../../prepare_permit_construction/output/construction_permits.csv | ../input
	ln -sf $< $@
../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@
../input/chicago_alderman_terms.csv: ../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	ln -sf $< $@
../input/ward_controls_2006_2022.csv: ../../create_ward_controls/output/ward_controls_2006_2022.csv | ../input
	ln -sf $< $@
../input/alderman_uncertainty_index_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@
../input/historical_zoning_2006_candidate.gpkg: ../../construction_zoning_history/output/historical_zoning_2006_candidate.gpkg | ../input
	ln -sf $< $@
../input/zoning_nov2012.zip: ../../../data_raw/construction_review/zoning_nov2012.zip | ../input
	ln -sf $< $@
../input/zoning_sep2014.zip: ../../../data_raw/construction_review/zoning_sep2014.zip | ../input
	ln -sf $< $@
../input/zoning_jan2016.zip: ../../../data_raw/construction_review/zoning_jan2016.zip | ../input
	ln -sf $< $@
../input/zoning_sep2025.geojson: ../../../data_raw/construction_review/zoning_sep2025.geojson | ../input
	ln -sf $< $@

link-inputs: ../input/permit_construction.csv ../input/construction_permits.csv ../input/boundary_segments_1320ft.gpkg \
	../input/chicago_alderman_terms.csv ../input/ward_controls_2006_2022.csv ../input/alderman_uncertainty_index_through2022.csv \
	../input/historical_zoning_2006_candidate.gpkg ../input/zoning_nov2012.zip ../input/zoning_sep2014.zip \
	../input/zoning_jan2016.zip ../input/zoning_sep2025.geojson

include ../../shared/code/generic.make
