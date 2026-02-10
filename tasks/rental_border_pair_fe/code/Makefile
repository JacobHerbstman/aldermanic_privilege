SHELL := bash
include ../../shell_functions.make

# ── Grid specifications ───────────────────────────────────
PLOT_BWS      := 500 1000
PLOT_PCTILES  := 0 50 75
PLOT_CONTROLS := TRUE FALSE
CTRL_SUFFIX_TRUE  := ctrl
CTRL_SUFFIX_FALSE := noctrl

# ── FE table specifications ───────────────────────────────
FE_BWS     := 500 1000
FE_WINDOWS := pre_2023

# ── Output file lists ────────────────────────────────────
FE_TEX       := $(foreach bw,$(FE_BWS),$(foreach w,$(FE_WINDOWS),../output/fe_table_rental_bw$(bw)_$(w).tex))
LEVEL_PDFS   := $(foreach bw,$(PLOT_BWS),$(foreach p,$(PLOT_PCTILES),$(foreach c,$(PLOT_CONTROLS),../output/level_plot_bw$(bw)_pct$(p)_$(CTRL_SUFFIX_$(c)).pdf)))
RD_PDFS      := $(foreach bw,$(PLOT_BWS),$(foreach p,$(PLOT_PCTILES),$(foreach c,$(PLOT_CONTROLS),../output/rd_plot_bw$(bw)_pct$(p)_$(CTRL_SUFFIX_$(c)).pdf)))
BINSCAT_PDFS := $(foreach bw,$(PLOT_BWS),$(foreach p,$(PLOT_PCTILES),$(foreach c,$(PLOT_CONTROLS),../output/binscatter_bw$(bw)_pct$(p)_$(CTRL_SUFFIX_$(c)).pdf)))
CONTINUOUS_TREAT_PDF := ../output/continuous_treatment_by_signed_distance_bw500_pre_2023_ctrl.pdf

all: $(FE_TEX) $(LEVEL_PDFS) $(RD_PDFS) $(CONTINUOUS_TREAT_PDF)

# ── FE regression tables ──────────────────────────────────
define FE_TEMPLATE
../output/fe_table_rental_bw$(1)_$(2).tex: rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_border_pair_fe.R --bw_ft=$(1) --window=$(2) --sample_filter=all \
		--output_tex="$$@" \
		--output_csv="../output/fe_table_rental_bw$(1)_$(2).csv" \
		--output_year_diag="../output/year_diagnostics_bw$(1)_$(2).csv"
endef
$(foreach bw,$(FE_BWS),$(foreach w,$(FE_WINDOWS),$(eval $(call FE_TEMPLATE,$(bw),$(w)))))

# ── Level plots (BW × percentile × controls, pre_2023) ───
define LEVEL_TEMPLATE
../output/level_plot_bw$(1)_pct$(2)_$(CTRL_SUFFIX_$(3)).pdf: rental_level_plot.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_level_plot.R --bw_ft=$(1) --window=pre_2023 --sample_filter=all \
		--use_controls=$(3) --bins_per_side=15 --min_strictness_diff_pctile=$(2) \
		--output_pdf="$$@"
endef
$(foreach bw,$(PLOT_BWS),$(foreach p,$(PLOT_PCTILES),$(foreach c,$(PLOT_CONTROLS),$(eval $(call LEVEL_TEMPLATE,$(bw),$(p),$(c))))))

# ── RD-style plots (BW × percentile × controls, pre_2023) ─
define RD_TEMPLATE
../output/rd_plot_bw$(1)_pct$(2)_$(CTRL_SUFFIX_$(3)).pdf: rental_bfm_rd_plot.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_bfm_rd_plot.R --bw_ft=$(1) --window=pre_2023 --sample_filter=all \
		--use_controls=$(3) --bins_per_side=15 --min_strictness_diff_pctile=$(2) \
		--output_pdf="$$@"
endef
$(foreach bw,$(PLOT_BWS),$(foreach p,$(PLOT_PCTILES),$(foreach c,$(PLOT_CONTROLS),$(eval $(call RD_TEMPLATE,$(bw),$(p),$(c))))))

# ── Binscatter plots (BW × percentile × controls) ────────
define BINSCAT_TEMPLATE
../output/binscatter_bw$(1)_pct$(2)_$(CTRL_SUFFIX_$(3)).pdf: rental_binscatter.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_binscatter.R --bw_ft=$(1) --window=pre_2023 --sample_filter=all \
		--use_controls=$(3) --n_bins=40 --min_strictness_diff_pctile=$(2) \
		--output_pdf="$$@"
endef
$(foreach bw,$(PLOT_BWS),$(foreach p,$(PLOT_PCTILES),$(foreach c,$(PLOT_CONTROLS),$(eval $(call BINSCAT_TEMPLATE,$(bw),$(p),$(c))))))

# ── Continuous-treatment-by-distance plot (bw500, pre_2023, controls) ─
../output/continuous_treatment_by_signed_distance_bw500_pre_2023_ctrl.pdf: rental_continuous_treatment_by_distance_plot.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_continuous_treatment_by_distance_plot.R --bw_ft=500 --bin_width_ft=50 --window=pre_2023 --sample_filter=all \
		--use_controls=TRUE --output_pdf="$@" \
		--output_csv="../output/continuous_treatment_by_signed_distance_bw500_pre_2023_ctrl.csv"

# ── Input symlinks ────────────────────────────────────────
../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../generic.make
