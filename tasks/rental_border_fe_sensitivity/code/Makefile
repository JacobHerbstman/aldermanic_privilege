SHELL := bash
include ../../shell_functions.make

INPUT_FILE := ../input/rent_with_ward_distances.parquet

# ── Specifications ──────────────────────────────────────────
PLACEBO_BWS   := 250 500
VISUAL_BWS    := 250 500
MAIN_SPEC_BWS := 500 1000
PERM_BWS      := 500 1000
PERM_CONTROLS := TRUE FALSE
CTRL_SUFFIX_TRUE  := ctrl
CTRL_SUFFIX_FALSE := noctrl

# ── Output file lists (primary outputs only) ────────────────
# COMPOSITION := ../output/composition_checks_pre_2021_all_bw500.tex
DECAY       := ../output/distance_decay_pre_2021_all.pdf
PLACEBO     := $(foreach bw,$(PLACEBO_BWS),../output/placebo_shifted_pre_2021_all_bw$(bw).pdf)
VISUALS     := $(foreach bw,$(VISUAL_BWS),../output/shifted_cutoff_visuals_pre_2021_all_bw$(bw).pdf)
MAIN_SPEC   := $(foreach bw,$(MAIN_SPEC_BWS),../output/placebo_main_spec_pre_2021_all_bw$(bw).pdf)
PERM        := $(foreach bw,$(PERM_BWS),$(foreach c,$(PERM_CONTROLS),../output/permutation_test_pre_2021_all_bw$(bw)_$(CTRL_SUFFIX_$(c)).pdf))

all: $(DECAY) $(PLACEBO) $(VISUALS) $(MAIN_SPEC) $(PERM)

# ── Composition checks (commented out for now) ──────────────
# $(COMPOSITION): run_composition_checks.R $(INPUT_FILE) | ../output
# 	Rscript $< --input=$(INPUT_FILE) --bw_ft=500 --window=pre_2021 --sample_filter=all \
# 		--output_tex=$@ --output_csv=$(subst .tex,.csv,$@)

# ── Distance decay (single spec) ────────────────────────────
$(DECAY): run_distance_decay.R $(INPUT_FILE) | ../output
	Rscript $< --input=$(INPUT_FILE) --window=pre_2021 --sample_filter=all \
		--rings="0-250,250-500,500-1000,1000-1500" --use_controls=TRUE \
		--output_pdf=$@

# ── Placebo shifted boundary ───────────────────────────────
define PLACEBO_TEMPLATE
../output/placebo_shifted_pre_2021_all_bw$(1).pdf: run_placebo_shifted_boundary.R $$(INPUT_FILE) | ../output
	Rscript $$< --input=$$(INPUT_FILE) --bw_ft=$(1) --window=pre_2021 --sample_filter=all \
		--shifts="-750,750" --use_controls=TRUE \
		--output_pdf=$$@
endef
$(foreach bw,$(PLACEBO_BWS),$(eval $(call PLACEBO_TEMPLATE,$(bw))))

# ── Shifted cutoff visuals ─────────────────────────────────
define VISUAL_TEMPLATE
../output/shifted_cutoff_visuals_pre_2021_all_bw$(1).pdf: run_shifted_cutoff_visuals.R $$(INPUT_FILE) | ../output
	Rscript $$< --input=$$(INPUT_FILE) --bw_ft=$(1) --window=pre_2021 --sample_filter=all \
		--shifts="-750,0,750" --use_controls=TRUE \
		--bins_per_side=$(if $(filter 250,$(1)),6,8) \
		--output_pdf=$$@
endef
$(foreach bw,$(VISUAL_BWS),$(eval $(call VISUAL_TEMPLATE,$(bw))))

# ── Placebo main spec ──────────────────────────────────────
define MAIN_SPEC_TEMPLATE
../output/placebo_main_spec_pre_2021_all_bw$(1).pdf: run_placebo_shifted_main_spec.R $$(INPUT_FILE) | ../output
	Rscript $$< --input=$$(INPUT_FILE) --bw_ft=$(1) --window=pre_2021 --sample_filter=all \
		--shifts="-750,-500,-250,0,250,500,750" --use_controls=TRUE \
		--output_pdf=$$@
endef
$(foreach bw,$(MAIN_SPEC_BWS),$(eval $(call MAIN_SPEC_TEMPLATE,$(bw))))

# ── Permutation test (BW × controls) ─────────────────────────
define PERM_TEMPLATE
../output/permutation_test_pre_2021_all_bw$(1)_$(CTRL_SUFFIX_$(2)).pdf: run_permutation_test.R $$(INPUT_FILE) | ../output
	Rscript $$< --input=$$(INPUT_FILE) --bw_ft=$(1) --window=pre_2021 --sample_filter=all \
		--use_controls=$(2) --n_perms=500 --seed=42 \
		--output_csv=../output/permutation_test_pre_2021_all_bw$(1)_$(CTRL_SUFFIX_$(2)).csv --output_pdf=$$@
endef
$(foreach bw,$(PERM_BWS),$(foreach c,$(PERM_CONTROLS),$(eval $(call PERM_TEMPLATE,$(bw),$(c)))))

# ── Input symlinks ──────────────────────────────────────────
$(INPUT_FILE): ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUT_FILE)

include ../../generic.make
