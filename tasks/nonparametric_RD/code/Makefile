SHELL := bash
include ../../shell_functions.make

# ───────── Parameters (feet) ─────────
# Pairs are bw_ft:bins_ft (bins_ft shown literally in filenames)
PAIRS     := 528:52.8 792:79.2 1056:105.6 1320:132 1584:158.4 2640:264
YVARS     := density_dupac density_far density_lapu density_bcr density_lps
LOG_STATUS:= FALSE

LOG_PREFIX_TRUE  := log_
LOG_PREFIX_FALSE :=

INPUT_PARCELS := ../input/parcels_with_ward_distances.csv
SCRIPT_FILE   := nonparametric_RD.R

# ───────── Build list of PDF outputs ─────────
define GEN_FILENAMES
$(foreach pair,$(PAIRS), \
  $(eval bw   := $(word 1,$(subst :, ,$(pair)))) \
  $(eval bins := $(word 2,$(subst :, ,$(pair)))) \
  $(foreach yvar,$(YVARS), \
  $(foreach log_stat,$(LOG_STATUS), \
    ../output/nonparametric_rd_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw).pdf \
  )) \
)
endef
ALL_FIGS := $(call GEN_FILENAMES)

.PHONY: all link-inputs
all: $(ALL_FIGS)

# ───────── Rule template: call Rscript with positional args ─────────
# Args: <yvar> <use_log> <bw_ft> <bins_ft> <output_pdf>
define GEN_RULE
../output/nonparametric_rd_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw).pdf: $(SCRIPT_FILE) $(INPUT_PARCELS) | ../output
	@echo "→ [npRD] y=$(yvar) | log=$(log_stat) | bw=$(bw) ft"
	Rscript $(SCRIPT_FILE) $(yvar) $(log_stat) $(bw)
	@echo "✓ wrote $$@"
endef

# ───────── Expand concrete rules ─────────
$(foreach pair,$(PAIRS), \
  $(eval bw   := $(word 1,$(subst :, ,$(pair)))) \
  $(eval bins := $(word 2,$(subst :, ,$(pair)))) \
  $(foreach yvar,$(YVARS), \
    $(foreach log_stat,$(LOG_STATUS), \
      $(eval $(GEN_RULE)) \
    ) \
  ) \
)

# ───────── Link input from upstream task ─────────
$(INPUT_PARCELS): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@ln -sf $< $@

link-inputs: $(INPUT_PARCELS)
include ../../generic.make
