# Makefile — Bunching at Ward Borders
SHELL := bash
.DEFAULT_GOAL := all

# ---------------- Tunable grids (override via `make VAR="..."`) ----------------
MAX_DISTS     ?= 1000 1500 2000
BIN_WIDTHS    ?= 50 100
INNER_WINDOWS ?= 0 200 300
POLY_DEGREES  ?= 2 4
BOOT_B        ?= 250

# ---------------- Files ----------------
SCRIPT_FILE   := bunching_analysis.R
SETUP_PKGS    := ../../setup_environment/code/packages.R
INPUT_PARCELS := ../input/parcels_with_ward_distances.csv

# ---------------- Outputs ----------------
define GEN_FILENAMES
$(foreach md,$(MAX_DISTS), \
  $(foreach bw,$(BIN_WIDTHS), \
    $(foreach h,$(INNER_WINDOWS), \
      $(foreach deg,$(POLY_DEGREES), \
        ../output/rd_plot_max$(md)_bin$(bw)_h$(h)_deg$(deg).png \
))))
endef
ALL_PLOTS := $(call GEN_FILENAMES)

.PHONY: all
all: $(ALL_PLOTS)

# ---------------- Build rules ----------------
define GEN_RULE
../output/rd_plot_max$(md)_bin$(bw)_h$(h)_deg$(deg).png: $(SCRIPT_FILE) $(SETUP_PKGS) $(INPUT_PARCELS) | ../output
	@echo "→ Bunching: max=$(md) bin=$(bw) donut=$(h) deg=$(deg) B=$(BOOT_B)"
	Rscript $(SCRIPT_FILE) $(md) $(bw) $(h) $(deg) $(BOOT_B) $$@
endef

$(foreach md,$(MAX_DISTS), \
  $(foreach bw,$(BIN_WIDTHS), \
    $(foreach h,$(INNER_WINDOWS), \
      $(foreach deg,$(POLY_DEGREES), \
        $(eval $(GEN_RULE)) \
))))

# Upstream parcels symlink
$(INPUT_PARCELS): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@ln -sf $< $@

link-inputs: $(INPUT_PARCELS)

# Directories, etc.
include ../../generic.make
