SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Directories & Config
# ─────────────────────────────────────────────────────────────────────────────
SINGLE_SCRIPT := process_rent_data.R
AGG_SCRIPT    := aggregate_rents.R

# ─────────────────────────────────────────────────────────────────────────────
# File Lists
# ─────────────────────────────────────────────────────────────────────────────

# 1. Identify raw files in the upstream task
RAW_FILES := $(wildcard ../../download_rent_data/output/*.parquet)

# 2. Define Symlinks needed in ../input
SOURCES := $(patsubst ../../download_rent_data/output/%, ../input/%, $(RAW_FILES))

# 3. Extract Dates for intermediate targets
#    (e.g., 2025-11-11--data_xx.parquet -> 2025-11-11)
DATES := $(sort $(foreach f,$(RAW_FILES),$(firstword $(subst --, ,$(notdir $f)))))

# 4. Define Intermediate Targets (Daily CSVs in ../temp)
TEMP_TARGETS := $(patsubst %,../temp/%.csv,$(DATES))

# 5. Define Final Output
FINAL_TARGET := ../output/chicago_rent_panel.parquet

# ─────────────────────────────────────────────────────────────────────────────
# Goals
# ─────────────────────────────────────────────────────────────────────────────
.DEFAULT_GOAL := all
.PHONY: all link-inputs clean-temp

all: $(FINAL_TARGET)

link-inputs: $(SOURCES)

# ─────────────────────────────────────────────────────────────────────────────
# 1. Aggregation Rule
#    Depends on ALL temp CSVs. If any CSV changes, re-run aggregation.
# ─────────────────────────────────────────────────────────────────────────────
$(FINAL_TARGET): $(AGG_SCRIPT) $(TEMP_TARGETS) | ../output
	@echo "→ [Aggregation] Combining $(words $(TEMP_TARGETS)) daily files into panel..."
	$(R) $(AGG_SCRIPT)

# ─────────────────────────────────────────────────────────────────────────────
# 2. Single Day Processing Rules (Dynamically Generated)
#    Maps: ../input/2025-11-11--data_xxx.parquet → ../temp/2025-11-11.csv
# ─────────────────────────────────────────────────────────────────────────────
define make_rule
../temp/$(firstword $(subst --, ,$(notdir $(1)))).csv: $(1) $(SINGLE_SCRIPT) | ../temp
	@echo "→ [Single Day] Processing $$(notdir $$<)"
	$$(R) $$(SINGLE_SCRIPT) "$$<" "$$@"
endef

$(foreach f,$(SOURCES),$(eval $(call make_rule,$f)))

# ─────────────────────────────────────────────────────────────────────────────
# 3. Input Symlinks - Explicit path pattern rule
# ─────────────────────────────────────────────────────────────────────────────
../input/%.parquet: ../../download_rent_data/output/%.parquet | ../input
	ln -sf "$<" "$@"

# ─────────────────────────────────────────────────────────────────────────────
# Maintenance Rules
# ─────────────────────────────────────────────────────────────────────────────

include ../../generic.make