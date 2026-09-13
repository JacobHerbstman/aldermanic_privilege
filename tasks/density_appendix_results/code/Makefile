include ../../shared/code/shell_functions.make

START_YEAR = 2006
END_YEAR = 2022
BANDWIDTH_FT = 500
BIN_WIDTH_FT = 100
CONTROLS = share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own
FIXED_EFFECTS = zone_group + segment_id + construction_year
CLUSTER = ward_pair
PLACEBO_FT = 1000
DONUT_INNER_FT = 25
DONUT_OUTER_FT = 50

all: ../output/density_placebo_neg$(PLACEBO_FT)ft.pdf \
	../output/density_placebo_pos$(PLACEBO_FT)ft.pdf \
	../output/density_donut$(DONUT_INNER_FT)ft.pdf \
	../output/density_donut$(DONUT_OUTER_FT)ft.pdf

../%/density_placebo_neg$(PLACEBO_FT)ft.pdf \
	../%/density_placebo_pos$(PLACEBO_FT)ft.pdf \
	../%/density_donut$(DONUT_INNER_FT)ft.pdf \
	../%/density_donut$(DONUT_OUTER_FT)ft.pdf: \
	build_density_appendix_results.R Makefile ../../setup_environment/code/packages.R \
	../input/new_construction_analysis_data.csv | ../output
	$(R) $< $(START_YEAR) $(END_YEAR) $(BANDWIDTH_FT) $(BIN_WIDTH_FT) "$(CONTROLS)" "$(FIXED_EFFECTS)" "$(CLUSTER)" $(PLACEBO_FT) $(DONUT_INNER_FT) $(DONUT_OUTER_FT)

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

link-inputs: ../input/new_construction_analysis_data.csv

include ../../shared/code/generic.make
