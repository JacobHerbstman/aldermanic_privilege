include ../../../shared/code/shell_functions.make

all: ../output/processing_time_event_study.csv ../output/processing_time_event_study.pdf \
	../output/discretion_difference_event_study.csv ../output/discretion_difference_event_study.pdf \
	../output/continuous_stringency.csv ../output/lenient_size_by_size_group.csv ../output/lenient_size_by_move.csv \
	../output/density_continuous_suite.csv ../output/reassignment_cost_pooled.csv ../output/reassignment_cost_event_study.pdf \
	../output/reassignment_cost_binary_event_study.pdf ../output/reassignment_by_direction_pooled.csv \
	../output/reassignment_by_direction_event_study.pdf

../%/processing_time_event_study.csv ../%/processing_time_event_study.pdf: processing_time_event_study.R \
	../input/permit_block_year_panel_2015.parquet ../input/building_permits_clean.gpkg ../input/census_blocks_2010.csv \
	../input/manual_permit_block_assignments.csv | ../output
	$(R) $<

../%/discretion_difference_event_study.csv ../%/discretion_difference_event_study.pdf: discretion_difference_event_study.R \
	../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $<

../output/continuous_stringency.csv: continuous_stringency.R ../input/permit_block_year_panel_2015.parquet \
	../input/new_construction_analysis_data.csv ../input/rental_rd_characteristics_panel_bw1500.parquet \
	../input/sales_with_hedonics_amenities.parquet | ../output
	$(R) $<

../%/lenient_size_by_size_group.csv ../%/lenient_size_by_move.csv: lenient_size_by_boundary.R \
	../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $<

../output/density_continuous_suite.csv: density_continuous_suite.R ../input/new_construction_analysis_data.csv \
	../input/density_boundary_characteristics.csv | ../output
	$(R) $<

../%/reassignment_cost_pooled.csv ../%/reassignment_cost_implied.csv ../%/reassignment_cost_event.csv \
	../%/reassignment_cost_leave_one_move_out.csv ../%/reassignment_cost_event_study.pdf \
	../%/reassignment_cost_binary_event_study.pdf: reassignment_cost.R \
	../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $<

../%/reassignment_by_direction_pooled.csv ../%/reassignment_by_direction_event.csv \
	../%/reassignment_by_direction_event_study.pdf: reassignment_by_direction.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $<

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	ln -sf $< $@
../input/building_permits_clean.gpkg: ../../../clean_building_permits/output/building_permits_clean_2006_2022.gpkg | ../input
	ln -sf $< $@
../input/census_blocks_2010.csv: ../../../download_chicago_spatial_data/output/census_blocks_2010.csv | ../input
	ln -sf $< $@
../input/manual_permit_block_assignments.csv: ../../../permit_block_hand_adjudications/output/manual_permit_block_assignments.csv | ../input
	ln -sf $< $@
../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@
../input/density_boundary_characteristics.csv: ../../../density_boundary_characteristics/output/density_boundary_characteristics.csv | ../input
	ln -sf $< $@
../input/rental_rd_characteristics_panel_bw1500.parquet: ../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw1500.parquet | ../input
	ln -sf $< $@
../input/sales_with_hedonics_amenities.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/building_permits_clean.gpkg \
	../input/census_blocks_2010.csv ../input/manual_permit_block_assignments.csv \
	../input/new_construction_analysis_data.csv ../input/density_boundary_characteristics.csv \
	../input/rental_rd_characteristics_panel_bw1500.parquet ../input/sales_with_hedonics_amenities.parquet

include ../../../shared/code/generic.make
