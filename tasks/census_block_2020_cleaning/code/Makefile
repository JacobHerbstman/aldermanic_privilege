SHELL := bash
include ../../shell_functions.make

SCRIPT := clean_blocks.R

# Inputs - shapefile components
INPUT_SHP := ../input/tl_2025_17_tabblock20.shp
INPUT_DBF := ../input/tl_2025_17_tabblock20.dbf
INPUT_SHX := ../input/tl_2025_17_tabblock20.shx
INPUT_PRJ := ../input/tl_2025_17_tabblock20.prj
INPUT_BLOCKS_2010 := ../input/census_blocks_2010.csv

ALL_INPUTS := $(INPUT_SHP) $(INPUT_DBF) $(INPUT_SHX) $(INPUT_PRJ) $(INPUT_BLOCKS_2010)

# Output
OUTPUT := ../output/census_blocks_2020.csv

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(OUTPUT)

$(OUTPUT): $(SCRIPT) $(ALL_INPUTS) | ../output
	$(R) $(SCRIPT)

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

# Link all shapefile components
$(INPUT_SHP): ../../../data_raw/tl_2025_17_tabblock20.shp | ../input
	ln -sf $< $@

$(INPUT_DBF): ../../../data_raw/tl_2025_17_tabblock20.dbf | ../input
	ln -sf $< $@

$(INPUT_SHX): ../../../data_raw/tl_2025_17_tabblock20.shx | ../input
	ln -sf $< $@

$(INPUT_PRJ): ../../../data_raw/tl_2025_17_tabblock20.prj | ../input
	ln -sf $< $@

$(INPUT_BLOCKS_2010): ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf $< $@

include ../../generic.make
