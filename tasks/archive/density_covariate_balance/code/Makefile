SHELL := bash
include ../../shell_functions.make

all: ../output/density_covariate_balance_main_bw500_multifamily.csv \
	../output/density_covariate_balance_main_bw500_multifamily.tex \
	../output/density_covariate_balance_snapshot2014_bw500_multifamily.csv

../output/density_covariate_balance_main_bw500_multifamily.tex: build_density_covariate_balance.R ../input/parcels_with_ward_distances.csv ../input/ward_controls_2000_2023.csv | ../output
	Rscript build_density_covariate_balance.R ../input/parcels_with_ward_distances.csv ../input/ward_controls_2000_2023.csv ../output/density_covariate_balance_main_bw500_multifamily.csv ../output/density_covariate_balance_main_bw500_multifamily.tex ../output/density_covariate_balance_snapshot2014_bw500_multifamily.csv

../output/density_covariate_balance_main_bw500_multifamily.csv ../output/density_covariate_balance_snapshot2014_bw500_multifamily.csv: ../output/density_covariate_balance_main_bw500_multifamily.tex

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

../input/ward_controls_2000_2023.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/ward_controls_2000_2023.csv

include ../../generic.make
