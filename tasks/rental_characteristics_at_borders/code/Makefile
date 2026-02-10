SHELL := bash

WINDOW    ?= pre_2023
TAG       ?=
BWS       := 500 1000
OUTCOMES  := sqft log_sqft beds baths multifamily laundry gym

LEVEL_PDFS   := $(foreach bw,$(BWS),$(foreach o,$(OUTCOMES),../output/char_level_$(o)_bw$(bw)$(TAG).pdf))
FE_TEX       := $(foreach bw,$(BWS),../output/char_fe_table_bw$(bw)$(TAG).tex)
DENSITY_WINDOWS ?= pre_2023
DENSITY_SAMPLES := all
DENSITY_PCTILES := 0 50
DSAMP_all := all

DENSITY_PDFS := $(foreach bw,$(BWS),$(foreach w,$(DENSITY_WINDOWS),$(foreach s,$(DENSITY_SAMPLES),$(foreach p,$(DENSITY_PCTILES),../output/listing_density_bw$(bw)_$(w)_$(DSAMP_$(s))_pct$(p).pdf))))

all: $(LEVEL_PDFS) $(FE_TEX) $(DENSITY_PDFS)

# ── Level plots (BW × outcome) ──
define LEVEL_TEMPLATE
../output/char_level_$(2)_bw$(1)$(TAG).pdf: characteristic_level_plot.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript characteristic_level_plot.R --bw_ft=$(1) --window=$(WINDOW) \
		--outcome=$(2) --bins_per_side=15 --output_pdf="$$@"
endef
$(foreach bw,$(BWS),$(foreach o,$(OUTCOMES),$(eval $(call LEVEL_TEMPLATE,$(bw),$(o)))))

# ── FE tables (one per BW, all outcomes in one table) ──
define FE_TEMPLATE
../output/char_fe_table_bw$(1)$(TAG).tex: characteristic_fe_table.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript characteristic_fe_table.R --bw_ft=$(1) --window=$(WINDOW) \
		--output_tex="$$@" --output_csv="../output/char_fe_table_bw$(1)$(TAG).csv"
endef
$(foreach bw,$(BWS),$(eval $(call FE_TEMPLATE,$(bw))))

# ── Listing density plots (BW × window × sample × pctile) ──
define DENSITY_TEMPLATE
../output/listing_density_bw$(1)_$(2)_$(DSAMP_$(3))_pct$(4).pdf: listing_density_plot.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript listing_density_plot.R --bw_ft=$(1) --window=$(2) --sample_filter=$(3) \
		--min_strictness_diff_pctile=$(4) --bins_per_side=15 --output_pdf="$$@"
endef
$(foreach bw,$(BWS),$(foreach w,$(DENSITY_WINDOWS),$(foreach s,$(DENSITY_SAMPLES),$(foreach p,$(DENSITY_PCTILES),$(eval $(call DENSITY_TEMPLATE,$(bw),$(w),$(s),$(p)))))))

# ── Input symlink ──
../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../generic.make
