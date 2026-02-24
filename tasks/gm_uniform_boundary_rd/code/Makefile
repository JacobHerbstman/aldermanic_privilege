SHELL := bash
include ../../shell_functions.make

OUTPUTS := \
	../output/gm_uniform_boundary_rd_detail_all.csv \
	../output/gm_uniform_boundary_rd_detail_pruned.csv \
	../output/gm_uniform_boundary_rd_table_all.csv \
	../output/gm_uniform_boundary_rd_table_pruned.csv \
	../output/gm_uniform_boundary_rd_table_all.tex \
	../output/gm_uniform_boundary_rd_table_pruned.tex \
	../output/gm_uniform_distance_gradients_bw1000m_all.pdf \
	../output/gm_uniform_distance_gradients_bw1000m_pruned.pdf \
	../output/gm_uniform_distance_gradients_bw1000m_all_bins.csv \
	../output/gm_uniform_distance_gradients_bw1000m_pruned_bins.csv

INPUTS := \
	../input/sales_with_hedonics.parquet \
	../input/confounded_pair_era_flags.csv

all: $(OUTPUTS)

$(OUTPUTS): run_gm_uniform_boundary_rd.R $(INPUTS) | ../output
	Rscript run_gm_uniform_boundary_rd.R

../input/sales_with_hedonics.parquet: ../../prep_sales_border_data/output/sales_with_hedonics.parquet | ../input
	ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUTS)

include ../../generic.make
