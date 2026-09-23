include ../../../shared/code/shell_functions.make

START_YEAR := 2006
END_YEAR := 2022
BANDWIDTH_FT := 500
BIN_WIDTH_FT := 100
CONTROLS := share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own
FIXED_EFFECTS := zone_group + segment_id + construction_year
CLUSTER := ward_pair
SEGMENT_WINDOW_FT := 1500

all: ../output/density_comparison.csv

../output/density_comparison.csv: compare_density.R ../input/permit_construction.csv \
	../input/new_construction_analysis_data.csv ../input/boundary_segments_1320ft.gpkg ../input/chicago_alderman_terms.csv \
	../input/alderman_uncertainty_index_through2022.csv ../input/ward_controls_2006_2022.csv ../input/historical_zoning_2006_candidate.gpkg \
	../input/zoning_nov2012.zip ../input/zoning_sep2014.zip ../input/zoning_jan2016.zip \
	../../../shared/code/canonical_geometry_helpers.R ../../../shared/code/save_data.R | ../output ../report
	$(R) $< $(START_YEAR) $(END_YEAR) $(BANDWIDTH_FT) $(BIN_WIDTH_FT) "$(CONTROLS)" "$(FIXED_EFFECTS)" "$(CLUSTER)" $(SEGMENT_WINDOW_FT)

../input/permit_construction.csv: ../../../prepare_permit_construction/output/permit_construction.csv | ../input
	ln -sf $< $@
../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@
../input/boundary_segments_1320ft.gpkg: ../../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@
../input/chicago_alderman_terms.csv: ../../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	ln -sf $< $@
../input/alderman_uncertainty_index_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@
../input/ward_controls_2006_2022.csv: ../../../create_ward_controls/output/ward_controls_2006_2022.csv | ../input
	ln -sf $< $@
../input/historical_zoning_2006_candidate.gpkg: ../../../construction_zoning_history/output/historical_zoning_2006_candidate.gpkg | ../input
	ln -sf $< $@
../input/zoning_nov2012.zip: ../../../../data_raw/construction_review/zoning_nov2012.zip | ../input
	ln -sf $< $@
../input/zoning_sep2014.zip: ../../../../data_raw/construction_review/zoning_sep2014.zip | ../input
	ln -sf $< $@
../input/zoning_jan2016.zip: ../../../../data_raw/construction_review/zoning_jan2016.zip | ../input
	ln -sf $< $@

link-inputs: ../input/permit_construction.csv \
	../input/new_construction_analysis_data.csv ../input/boundary_segments_1320ft.gpkg ../input/chicago_alderman_terms.csv \
	../input/alderman_uncertainty_index_through2022.csv ../input/ward_controls_2006_2022.csv ../input/historical_zoning_2006_candidate.gpkg \
	../input/zoning_nov2012.zip ../input/zoning_sep2014.zip ../input/zoning_jan2016.zip

include ../../../shared/code/generic.make
