BANDWIDTHS_FT ?= 100,200,300,400,500,600,700,800,900,1000

all: \
	../output/sales_rd_bandwidth_path.pdf \
	../output/sales_rd_bandwidth_path.png \
	../output/sales_rd_bandwidth_path_fe_specs.pdf \
	../output/sales_rd_bandwidth_path_fe_specs.png \
	../output/sales_rd_cluster_robustness.pdf \
	../output/sales_rd_cluster_robustness.png \
	../output/sales_rd_cluster_robustness.csv \
	../output/sales_rd_local_linear_path.pdf \
	../output/sales_rd_local_linear_path.png \
	../output/sales_rd_local_linear_path.csv \
	../output/sales_rd_bandwidth_path.csv \
	../output/sales_rd_bandwidth_support.csv

../output/sales_rd_bandwidth_path.pdf: build_sales_rd_bandwidth_path.R ../input/sales_with_hedonics_amenities.parquet | ../output
	Rscript $< $(BANDWIDTHS_FT)

../output/sales_rd_bandwidth_path.png \
../output/sales_rd_bandwidth_path_fe_specs.pdf \
../output/sales_rd_bandwidth_path_fe_specs.png \
../output/sales_rd_cluster_robustness.pdf \
../output/sales_rd_cluster_robustness.png \
../output/sales_rd_cluster_robustness.csv \
../output/sales_rd_local_linear_path.pdf \
../output/sales_rd_local_linear_path.png \
../output/sales_rd_local_linear_path.csv \
../output/sales_rd_bandwidth_path.csv \
../output/sales_rd_bandwidth_support.csv: ../output/sales_rd_bandwidth_path.pdf
	@test -s "$@"

../input/sales_with_hedonics_amenities.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/sales_with_hedonics_amenities.parquet

include ../../generic.make
