all: ../output/sales_block_year_panel.csv ../output/sales_block_month_panel.csv ../output/sales_stacked_panel.csv ../output/sales_stacked_quarterly_panel.csv ../output/sales_stacked_monthly_panel.csv ../output/sales_unstacked_2015_panel.csv ../output/sales_unstacked_2023_panel.csv

../output/sales_block_year_panel.csv: create_event_study_data.R ../input/sales_with_ward_distances.csv ../input/ward_panel.gpkg ../input/census_blocks_2010.csv ../input/block_treatment_panel.csv ../input/block_group_controls.csv | ../output
	Rscript create_event_study_data.R

../output/sales_block_month_panel.csv ../output/sales_stacked_panel.csv ../output/sales_stacked_quarterly_panel.csv ../output/sales_stacked_monthly_panel.csv ../output/sales_unstacked_2015_panel.csv ../output/sales_unstacked_2023_panel.csv: ../output/sales_block_year_panel.csv
	@test -s "$@"
../input/sales_with_ward_distances.csv: ../../../merge_sales_scores/output/sales_with_ward_distances_through2022.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/block_treatment_panel.csv: ../../merge_extended_event_study_scores/output/block_treatment_panel_through202604.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/block_group_controls.csv: ../../../create_block_group_controls/output/block_group_controls.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/sales_with_ward_distances.csv ../input/ward_panel.gpkg ../input/census_blocks_2010.csv ../input/block_treatment_panel.csv ../input/block_group_controls.csv

include ../../../generic.make
