BANDWIDTH_FT ?= 500
BINS_PER_SIDE ?= 15

all: \
	../output/rental_rd_supply_levels_bw$(BANDWIDTH_FT).pdf \
	../output/rental_rd_supply_levels_clean_location_bw$(BANDWIDTH_FT).pdf

../output/rental_rd_supply_levels_bw$(BANDWIDTH_FT).pdf: build_rental_rd_supply.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript $< $(BANDWIDTH_FT) $(BINS_PER_SIDE)

../output/rental_rd_supply_levels_clean_location_bw$(BANDWIDTH_FT).pdf: ../output/rental_rd_supply_levels_bw$(BANDWIDTH_FT).pdf
	@test -s "$@" || Rscript build_rental_rd_supply.R $(BANDWIDTH_FT) $(BINS_PER_SIDE)

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../generic.make
