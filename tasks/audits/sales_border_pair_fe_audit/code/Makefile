SHELL := bash

all: \
	../output/sales_amenity_distance_diagnostics.csv \
	../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.csv \
	../output/year_diagnostics_sales_bw500_year_quarter_amenity_clust_ward_pair.csv \
	../output/fe_table_sales_right_bw500_year_quarter_amenity_clust_ward_pair.csv \
	../output/year_diagnostics_sales_right_bw500_year_quarter_amenity_clust_ward_pair.csv \
	../output/fe_table_sales_right_bw500_year_quarter_amenity_clust_segment.csv \
	../output/year_diagnostics_sales_right_bw500_year_quarter_amenity_clust_segment.csv

../output/sales_amenity_distance_diagnostics.csv: build_sales_border_pair_fe_audit.R ../input/sales_with_hedonics_amenities.parquet | ../output
	Rscript build_sales_border_pair_fe_audit.R

../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.csv \
../output/year_diagnostics_sales_bw500_year_quarter_amenity_clust_ward_pair.csv \
../output/fe_table_sales_right_bw500_year_quarter_amenity_clust_ward_pair.csv \
../output/year_diagnostics_sales_right_bw500_year_quarter_amenity_clust_ward_pair.csv \
../output/fe_table_sales_right_bw500_year_quarter_amenity_clust_segment.csv \
../output/year_diagnostics_sales_right_bw500_year_quarter_amenity_clust_segment.csv: ../output/sales_amenity_distance_diagnostics.csv
	@test -s "$@"

../input/sales_with_hedonics_amenities.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/sales_with_hedonics_amenities.parquet

include ../../../shell_functions.make
include ../../../generic.make
