SHELL := bash
include ../../shell_functions.make

all: ../output/density_bg_border_balance_snapshot2014.csv \
	../output/density_bg_border_balance_snapshot2014_drop_straddle.csv \
	../output/density_bg_border_straddle_summary_snapshot2014.csv \
	../output/density_bg_border_balance_snapshot2014.tex

../output/density_bg_border_balance_snapshot2014.tex: build_density_bg_border_balance.R ../input/parcels_with_ward_distances.csv ../input/block_group_controls.csv | ../output
	Rscript build_density_bg_border_balance.R ../input/parcels_with_ward_distances.csv ../input/block_group_controls.csv ../output/density_bg_border_balance_snapshot2014.csv ../output/density_bg_border_balance_snapshot2014_drop_straddle.csv ../output/density_bg_border_straddle_summary_snapshot2014.csv ../output/density_bg_border_balance_snapshot2014.tex

../output/density_bg_border_balance_snapshot2014.csv \
../output/density_bg_border_balance_snapshot2014_drop_straddle.csv \
../output/density_bg_border_straddle_summary_snapshot2014.csv: ../output/density_bg_border_balance_snapshot2014.tex

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

../input/block_group_controls.csv: ../../create_block_group_controls/output/block_group_controls.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/block_group_controls.csv

include ../../generic.make
