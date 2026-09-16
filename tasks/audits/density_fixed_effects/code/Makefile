include ../../../shared/code/shell_functions.make

START_YEAR := 2006
END_YEAR := 2022
BANDWIDTH_FT := 500
BIN_WIDTH_FT := 100
SAMPLES := all,multifamily
OUTCOMES := density_far,density_dupac
SPATIAL_FE := segment,border_pair
TIME_FE := year,joint_service,none
ZONING_FE := yes,no
CONTROLS := share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own
CLUSTER := ward_pair

all: ../output/density_fixed_effects.csv ../output/density_fixed_effects.html ../output/density_fixed_effects.tex

../output/density_fixed_effects.csv ../output/density_fixed_effects.html ../output/density_fixed_effects.tex: estimate_fixed_effects.R Makefile ../input/construction.csv ../input/alderman_terms.csv ../input/baseline_all.csv ../input/baseline_multifamily.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< $(START_YEAR) $(END_YEAR) $(BANDWIDTH_FT) $(BIN_WIDTH_FT) $(SAMPLES) $(OUTCOMES) $(SPATIAL_FE) $(TIME_FE) $(ZONING_FE) "$(CONTROLS)" $(CLUSTER)

../input/construction.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@
../input/alderman_terms.csv: ../../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	ln -sf $< $@
../input/baseline_all.csv: ../../raw_log_score_sensitivity/output/density_all_results.csv | ../input
	ln -sf $< $@
../input/baseline_multifamily.csv: ../../raw_log_score_sensitivity/output/density_multifamily_results.csv | ../input
	ln -sf $< $@

link-inputs: ../input/construction.csv ../input/alderman_terms.csv ../input/baseline_all.csv ../input/baseline_multifamily.csv

include ../../../shared/code/generic.make
