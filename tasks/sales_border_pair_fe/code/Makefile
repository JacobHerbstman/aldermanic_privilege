SHELL := bash
include ../../shell_functions.make

# ── Grid specifications ───────────────────────────────────
PLOT_BWS      := 500 1000
PLOT_PCTILES  := 0 50 75
PLOT_CONTROLS := TRUE FALSE
CTRL_SUFFIX_TRUE  := ctrl
CTRL_SUFFIX_FALSE := noctrl

FE_BWS   := 500 1000
FE_TIMES := year_quarter year

# ── Output file lists ────────────────────────────────────
FE_TEX     := $(foreach bw,$(FE_BWS),$(foreach t,$(FE_TIMES),../output/fe_table_sales_bw$(bw)_$(t).tex))
LEVEL_PDFS := $(foreach bw,$(PLOT_BWS),$(foreach p,$(PLOT_PCTILES),$(foreach c,$(PLOT_CONTROLS),../output/level_plot_bw$(bw)_pct$(p)_$(CTRL_SUFFIX_$(c)).pdf)))

all: $(FE_TEX) $(LEVEL_PDFS)

# ── FE regression tables (BW × time FE) ──────────────────
define FE_TEMPLATE
../output/fe_table_sales_bw$(1)_$(2).tex: sales_border_pair_fe.R ../input/sales_with_hedonics.parquet | ../output
	Rscript sales_border_pair_fe.R --bw_ft=$(1) --fe_time=$(2) \
		--output_tex="$$@" \
		--output_csv="../output/fe_table_sales_bw$(1)_$(2).csv" \
		--output_year_diag="../output/year_diagnostics_sales_bw$(1)_$(2).csv"
endef
$(foreach bw,$(FE_BWS),$(foreach t,$(FE_TIMES),$(eval $(call FE_TEMPLATE,$(bw),$(t)))))

# ── Level plots (BW × percentile × controls) ─────────────
define LEVEL_TEMPLATE
../output/level_plot_bw$(1)_pct$(2)_$(CTRL_SUFFIX_$(3)).pdf: sales_level_plot.R ../input/sales_with_hedonics.parquet | ../output
	Rscript sales_level_plot.R --bw_ft=$(1) \
		--use_controls=$(3) --bins_per_side=15 --min_strictness_diff_pctile=$(2) \
		--output_pdf="$$@"
endef
$(foreach bw,$(PLOT_BWS),$(foreach p,$(PLOT_PCTILES),$(foreach c,$(PLOT_CONTROLS),$(eval $(call LEVEL_TEMPLATE,$(bw),$(p),$(c))))))

# ── Input symlinks ────────────────────────────────────────
../input/sales_with_hedonics.parquet: ../../prep_sales_border_data/output/sales_with_hedonics.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/sales_with_hedonics.parquet

include ../../generic.make
