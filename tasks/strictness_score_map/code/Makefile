SHELL := bash
include ../../shell_functions.make

# ───────── User-settable month (YYYY-MM) ─────────
# Override on the command line, e.g.:  make DATE=2014-05
DATE := 2014-01

.PHONY: all link-inputs

# --- Main Goal: Create the strictness score map for the chosen month ---
all: ../output/strictness_score_map_$(DATE).pdf

# --- Rule to generate the map from the R script ---
../output/strictness_score_map_$(DATE).pdf: strictness_score_map.R \
  ../input/alderman_restrictiveness_scores_month_FEs.csv \
  ../input/chicago_alderman_panel.csv \
  ../input/ward_panel.gpkg | ../output
	@echo "→ Generating strictness score map for $(DATE)..."
	Rscript strictness_score_map.R $(DATE)

# --- Rules for linking input files from other tasks ---
../input/alderman_restrictiveness_scores_month_FEs.csv: ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	@ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@ln -sf $< $@

# --- Helper to create all input links at once ---
link-inputs: ../input/alderman_restrictiveness_scores_month_FEs.csv \
             ../input/chicago_alderman_panel.csv \
             ../input/ward_panel.gpkg

# --- Include generic rules for directory creation etc. ---
include ../../generic.make
