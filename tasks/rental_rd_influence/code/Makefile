all: \
	../output/rental_rd_influence_summary.csv \
	../output/rental_rd_leave_one_pair.csv \
	../output/rental_rd_leave_one_ward.csv \
	../output/rental_rd_leave_one_alderman.csv \
	../output/rental_rd_leave_one_top_segment.csv \
	../output/rental_rd_pair_estimates.csv \
	../output/rental_rd_pair_support.csv \
	../output/rental_rd_leave_one_pair.png \
	../output/rental_rd_pair_estimates.png

../output/rental_rd_influence_summary.csv: build_rental_rd_influence.R ../input/rental_rd_characteristics_panel_bw500.parquet | ../output
	Rscript $<

../output/rental_rd_leave_one_pair.csv \
../output/rental_rd_leave_one_ward.csv \
../output/rental_rd_leave_one_alderman.csv \
../output/rental_rd_leave_one_top_segment.csv \
../output/rental_rd_pair_estimates.csv \
../output/rental_rd_pair_support.csv \
../output/rental_rd_leave_one_pair.png \
../output/rental_rd_pair_estimates.png: ../output/rental_rd_influence_summary.csv
	@test -s "$@"

../input/rental_rd_characteristics_panel_bw500.parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_rd_characteristics_panel_bw500.parquet

include ../../generic.make
