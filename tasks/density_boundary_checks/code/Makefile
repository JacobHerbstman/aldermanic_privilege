include ../../shared/code/shell_functions.make

START_YEAR = 2006
END_YEAR = 2022
BANDWIDTH_FT = 500
BIN_WIDTH_FT = 100
CONTROLS = share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own
FIXED_EFFECTS = zone_group + segment_id + construction_year
CLUSTER = ward_pair
CONTINUITY_FIXED_EFFECTS = segment_id + construction_year

all: ../output/density_boundary_robustness.tex \
	../output/density_location_continuity.tex

../%/density_boundary_robustness.tex \
	../%/density_location_continuity.tex: \
	build_density_boundary_checks.R Makefile ../../setup_environment/code/packages.R \
	../input/new_construction_analysis_data.csv \
	../input/density_boundary_characteristics.csv | ../output
	$(R) $< $(START_YEAR) $(END_YEAR) $(BANDWIDTH_FT) $(BIN_WIDTH_FT) "$(CONTROLS)" "$(FIXED_EFFECTS)" "$(CLUSTER)" "$(CONTINUITY_FIXED_EFFECTS)"

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../input/density_boundary_characteristics.csv: ../../density_boundary_characteristics/output/density_boundary_characteristics.csv | ../input
	ln -sf $< $@

link-inputs: \
	../input/new_construction_analysis_data.csv \
	../input/density_boundary_characteristics.csv

include ../../shared/code/generic.make
