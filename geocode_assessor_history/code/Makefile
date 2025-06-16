###############################################################################
# geocode_assessor_history / Makefile
###############################################################################

SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Build *both* outputs
# ─────────────────────────────────────────────────────────────────────────────
.PHONY: all
all: ../output/attom_historical_with_address.shp \
     ../output/attom_historical_missing_address.parquet


# ─────────────────────────────────────────────────────────────────────────────
# 1) Symlink upstream inputs
# ─────────────────────────────────────────────────────────────────────────────
.PHONY: setup_input
setup_input: | ../input
	@echo "→ Refreshing input symlinks from upstream tasks…"
	rm -rf ../input/*
	ln -sf ../../process_attom_assessor_historical/output/* ../input/
	ln -sf ../../process_attom_assessor/output/*               ../input/

# ─────────────────────────────────────────────────────────────────────────────
# 2) Run the R script once → produces shapefile & parquet
# ─────────────────────────────────────────────────────────────────────────────
# NB: the recipe moves *all* pieces of the shapefile (.shp/.dbf/.shx/.prj/.cpg)
../output/attom_historical_with_address.shp \
../output/attom_historical_missing_address.parquet: \
        link_assessor_history.R \
        | setup_input
	@echo "→ Ensuring ../output exists…"
	mkdir -p ../output
	@echo "→ Running link_assessor_history.R …"
	Rscript $<
	@echo "→ Moving results into central storage…"
	mkdir -p ../../data_link/geocode_assessor_history_output
	# move shapefile component files
	mv ../output/attom_historical_with_address.* \
	   ../../data_link/geocode_assessor_history_output/
	# move the parquet of addresses without coords
	mv ../output/attom_historical_missing_address.parquet \
	   ../../data_link/geocode_assessor_history_output/
	@echo "→ Symlinking primary files back into this task’s output folder…"
	ln -sf ../../data_link/geocode_assessor_history_output/attom_historical_with_address.shp \
	       ../output/attom_historical_with_address.shp
	ln -sf ../../data_link/geocode_assessor_history_output/attom_historical_missing_address.parquet \
	       ../output/attom_historical_missing_address.parquet

# ─────────────────────────────────────────────────────────────────────────────
# 3) Generic helpers (../input ../output ../temp, etc.)
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make
