BANDWIDTHS_FT ?= 100,200,300,400,500,600,700,800,900,1000
SAMPLES ?= all,clean_location,no_questionable_address

all: \
	../output/rental_rd_bandwidth_path.pdf \
	../output/rental_rd_bandwidth_path.png \
	../output/rental_rd_bandwidth_path_fe_specs.pdf \
	../output/rental_rd_bandwidth_path_fe_specs.png \
	../output/rental_rd_cluster_robustness.pdf \
	../output/rental_rd_cluster_robustness.png \
	../output/rental_rd_cluster_robustness.csv \
	../output/rental_rd_local_linear_path.pdf \
	../output/rental_rd_local_linear_path.png \
	../output/rental_rd_local_linear_path.csv \
../output/rental_rd_bandwidth_path.csv

../output/rental_rd_bandwidth_path.pdf: build_rental_rd_bandwidth_path.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript $< $(BANDWIDTHS_FT) $(SAMPLES)

../output/rental_rd_bandwidth_path.png \
../output/rental_rd_bandwidth_path_fe_specs.pdf \
../output/rental_rd_bandwidth_path_fe_specs.png \
../output/rental_rd_cluster_robustness.pdf \
../output/rental_rd_cluster_robustness.png \
../output/rental_rd_cluster_robustness.csv \
../output/rental_rd_local_linear_path.pdf \
../output/rental_rd_local_linear_path.png \
../output/rental_rd_local_linear_path.csv \
../output/rental_rd_bandwidth_path.csv: ../output/rental_rd_bandwidth_path.pdf
	@test -s "$@"

../input/rent_with_ward_distances.parquet: ../../../merge_rent_scores/output/rent_with_ward_distances_full_through2022.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../../generic.make
