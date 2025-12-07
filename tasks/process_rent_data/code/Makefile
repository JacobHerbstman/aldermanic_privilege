SHELL := bash
include ../../shell_functions.make
# ─────────────────────────────────────────────────────────────────────────────
# Directories & Config
# ─────────────────────────────────────────────────────────────────────────────
SCRIPT_FILE := process_rent_data.R
INPUT_DIR   := ../input
OUTPUT_DIR  := ../output
RAW_SOURCE  := ../../download_rent_data/output
# ─────────────────────────────────────────────────────────────────────────────
# File Lists (Calculated at parse time)
# ─────────────────────────────────────────────────────────────────────────────

RAW_FILES := $(wildcard $(RAW_SOURCE)/*.parquet)

SOURCES := $(patsubst $(RAW_SOURCE)/%, $(INPUT_DIR)/%, $(RAW_FILES))

DATES := $(sort $(foreach f,$(RAW_FILES),$(firstword $(subst --, ,$(notdir $f)))))
TARGETS := $(patsubst %,$(OUTPUT_DIR)/%.csv,$(DATES))
# ─────────────────────────────────────────────────────────────────────────────
# Goals
# ─────────────────────────────────────────────────────────────────────────────
.DEFAULT_GOAL := all
.PHONY: all link-input
all: $(TARGETS)
link-inputs: $(SOURCES)
# ─────────────────────────────────────────────────────────────────────────────
# Processing Rule (dynamically generated)
# ─────────────────────────────────────────────────────────────────────────────
# Maps: ../input/2025-11-11--data_xxx.parquet → ../output/2025-11-11.csv
define make_rule
$(OUTPUT_DIR)/$(firstword $(subst --, ,$(notdir $(1)))).csv: $(1) $(SCRIPT_FILE) | $(OUTPUT_DIR)
	@echo "→ Processing $$(notdir $$<) → $$(notdir $$@)"
	$$(R) $$(SCRIPT_FILE) "$$<" "$$@"
endef
$(foreach f,$(SOURCES),$(eval $(call make_rule,$f)))
# ─────────────────────────────────────────────────────────────────────────────
# Input Symlinks (created on-demand)
# ─────────────────────────────────────────────────────────────────────────────
$(INPUT_DIR)/%.parquet: $(RAW_SOURCE)/%.parquet | $(INPUT_DIR)
	@echo "→ Linking input: $@ ← $<"
	ln -sf "$<" "$@"
# ─────────────────────────────────────────────────────────────────────────────
# Maintenance Rules
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make