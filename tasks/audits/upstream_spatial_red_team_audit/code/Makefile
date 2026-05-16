STAMP = ../output/.upstream_spatial_red_team_complete

all: $(STAMP)

$(STAMP): run_upstream_spatial_red_team_audit.R \
	../../../download_building_permits/output/building_permits.csv \
	../../../download_parcel_sales_data/output/parcel_sales_city.csv \
	../../../calculate_sale_distances/output/sales_pre_scores.csv \
	../../../clean_building_permits/output/building_permits_clean.gpkg \
	../../../border_segment_creation/output/boundary_segments_1320ft.gpkg \
	../../../border_segment_creation/output/ward_pair_boundaries.gpkg \
	../../../ward_panel_create/output/ward_panel.gpkg \
	../../../census_block_2020_cleaning/output/census_blocks_2020.csv | ../output
	Rscript run_upstream_spatial_red_team_audit.R
	touch $(STAMP)

include ../../../generic.make
