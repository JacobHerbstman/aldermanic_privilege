SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

# All the files this task will create.
OUTPUT_FILES := \
	../output/alderman_restrictiveness_scores_month_FEs.csv \
	../output/alderman_restrictiveness_scores_ward_month_FEs.csv \
	../output/Month_FEs_final_strictness_index.png \
	../output/Month_FEs_log_mean_total_fee.png \
	../output/Month_FEs_log_median_processing_time.png \
	../output/Month_FEs_log_n_permits_issued.png \
	../output/Month_FEs_permit_approval_rate.png \
	../output/Ward_Month_FEs_final_strictness_index.png \
	../output/Ward_Month_FEs_log_mean_total_fee.png \
	../output/Ward_Month_FEs_log_median_processing_time.png \
	../output/Ward_Month_FEs_log_n_permits_issued.png \
	../output/Ward_Month_FEs_permit_approval_rate.png \
	../output/stage1_residualization_models.tex

# The R script that does the work.
SCRIPT_FILE := create_restrictiveness_score.R

INPUT_WARD_PANEL := ../input/ward_monthly_panel_for_alderman_fe.csv

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

all: $(OUTPUT_FILES)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output
# ─────────────────────────────────────────────────────────────────────────────
$(OUTPUT_FILES): $(SCRIPT_FILE) $(INPUT_WARD_PANEL) | ../output
	@echo "→ [create_strictness_score] Running R script to create all strictness scores and plots"
	$(R) $(SCRIPT_FILE)


# ─────────────────────────────────────────────────────────────────────────────
# Rules to create the input symbolic links
# -----------------------------------------------------------------------------

# Rule for linking ward monthly panel
$(INPUT_WARD_PANEL): ../../data_for_alderman_strictness_scores/output/ward_monthly_panel_for_alderman_fe.csv | ../input
	@echo "→ [create_strictness_score] Linking: $@ ← $<"
	ln -sf $< $@

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make

# ─────────────────────────────────────────────────────────────────────────────
# Utility Targets
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(INPUT_WARD_PANEL)
