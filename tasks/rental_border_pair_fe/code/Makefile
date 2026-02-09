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
FE_WINDOWS := pre_2021 drop_mid

# ── Output file lists ────────────────────────────────────
FE_TEX       := $(foreach bw,$(FE_BWS),$(foreach w,$(FE_WINDOWS),../output/fe_table_rental_bw$(bw)_$(w).tex))
LEVEL_PDFS   := $(foreach bw,$(PLOT_BWS),$(foreach p,$(PLOT_PCTILES),$(foreach c,$(PLOT_CONTROLS),../output/level_plot_bw$(bw)_pct$(p)_$(CTRL_SUFFIX_$(c)).pdf)))
RD_PDFS      := $(foreach bw,$(PLOT_BWS),$(foreach p,$(PLOT_PCTILES),$(foreach c,$(PLOT_CONTROLS),../output/rd_plot_bw$(bw)_pct$(p)_$(CTRL_SUFFIX_$(c)).pdf)))
BINSCAT_PDFS := $(foreach bw,$(PLOT_BWS),$(foreach p,$(PLOT_PCTILES),$(foreach c,$(PLOT_CONTROLS),../output/binscatter_bw$(bw)_pct$(p)_$(CTRL_SUFFIX_$(c)).pdf)))

# drop_mid window plots (pct0 only, both BWs and controls)
LEVEL_DM   := $(foreach bw,$(PLOT_BWS),$(foreach c,$(PLOT_CONTROLS),../output/level_plot_bw$(bw)_pct0_$(CTRL_SUFFIX_$(c))_drop_mid.pdf))
RD_DM      := $(foreach bw,$(PLOT_BWS),$(foreach c,$(PLOT_CONTROLS),../output/rd_plot_bw$(bw)_pct0_$(CTRL_SUFFIX_$(c))_drop_mid.pdf))

all: $(FE_TEX) $(LEVEL_PDFS) $(RD_PDFS) $(LEVEL_DM) $(RD_DM)

# ── FE regression tables ──────────────────────────────────
define FE_TEMPLATE
../output/fe_table_rental_bw$(1)_$(2).tex: rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_border_pair_fe.R --bw_ft=$(1) --window=$(2) --sample_filter=all \
		--output_tex="$$@" \
		--output_csv="../output/fe_table_rental_bw$(1)_$(2).csv" \
		--output_year_diag="../output/year_diagnostics_bw$(1)_$(2).csv"
endef
$(foreach bw,$(FE_BWS),$(foreach w,$(FE_WINDOWS),$(eval $(call FE_TEMPLATE,$(bw),$(w)))))

# ── Level plots (BW × percentile × controls, pre_2021) ───
define LEVEL_TEMPLATE
../output/level_plot_bw$(1)_pct$(2)_$(CTRL_SUFFIX_$(3)).pdf: rental_level_plot.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_level_plot.R --bw_ft=$(1) --window=pre_2021 --sample_filter=all \
		--use_controls=$(3) --bins_per_side=15 --min_strictness_diff_pctile=$(2) \
		--output_pdf="$$@"
endef
$(foreach bw,$(PLOT_BWS),$(foreach p,$(PLOT_PCTILES),$(foreach c,$(PLOT_CONTROLS),$(eval $(call LEVEL_TEMPLATE,$(bw),$(p),$(c))))))

# ── Level plots (drop_mid window, pct0) ──────────────────
define LEVEL_DM_TEMPLATE
../output/level_plot_bw$(1)_pct0_$(CTRL_SUFFIX_$(2))_drop_mid.pdf: rental_level_plot.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_level_plot.R --bw_ft=$(1) --window=drop_mid --sample_filter=all \
		--use_controls=$(2) --bins_per_side=15 --min_strictness_diff_pctile=0 \
		--output_pdf="$$@"
endef
$(foreach bw,$(PLOT_BWS),$(foreach c,$(PLOT_CONTROLS),$(eval $(call LEVEL_DM_TEMPLATE,$(bw),$(c)))))

# ── RD-style plots (BW × percentile × controls, pre_2021) ─
define RD_TEMPLATE
../output/rd_plot_bw$(1)_pct$(2)_$(CTRL_SUFFIX_$(3)).pdf: rental_bfm_rd_plot.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_bfm_rd_plot.R --bw_ft=$(1) --window=pre_2021 --sample_filter=all \
		--use_controls=$(3) --bins_per_side=15 --min_strictness_diff_pctile=$(2) \
		--output_pdf="$$@"
endef
$(foreach bw,$(PLOT_BWS),$(foreach p,$(PLOT_PCTILES),$(foreach c,$(PLOT_CONTROLS),$(eval $(call RD_TEMPLATE,$(bw),$(p),$(c))))))

# ── RD-style plots (drop_mid window, pct0) ───────────────
define RD_DM_TEMPLATE
../output/rd_plot_bw$(1)_pct0_$(CTRL_SUFFIX_$(2))_drop_mid.pdf: rental_bfm_rd_plot.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_bfm_rd_plot.R --bw_ft=$(1) --window=drop_mid --sample_filter=all \
		--use_controls=$(2) --bins_per_side=15 --min_strictness_diff_pctile=0 \
		--output_pdf="$$@"
endef
$(foreach bw,$(PLOT_BWS),$(foreach c,$(PLOT_CONTROLS),$(eval $(call RD_DM_TEMPLATE,$(bw),$(c)))))

# ── Binscatter plots (BW × percentile × controls) ────────
define BINSCAT_TEMPLATE
../output/binscatter_bw$(1)_pct$(2)_$(CTRL_SUFFIX_$(3)).pdf: rental_binscatter.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_binscatter.R --bw_ft=$(1) --window=pre_2021 --sample_filter=all \
		--use_controls=$(3) --n_bins=40 --min_strictness_diff_pctile=$(2) \
		--output_pdf="$$@"
endef
$(foreach bw,$(PLOT_BWS),$(foreach p,$(PLOT_PCTILES),$(foreach c,$(PLOT_CONTROLS),$(eval $(call BINSCAT_TEMPLATE,$(bw),$(p),$(c))))))

# ── Input symlinks ────────────────────────────────────────
../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../generic.make
