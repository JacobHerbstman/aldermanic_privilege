SHELL := bash

all: ../output/fe_table_bw250_density_main_panel.tex

../output/fe_table_bw250_density_main_panel.tex: build_density_main_panel_table.R ../input/fe_summary_bw250_all_zonegroup_segment_year_additive_clust_ward_pair.csv ../input/fe_summary_bw250_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv | ../output
	Rscript build_density_main_panel_table.R ../input/fe_summary_bw250_all_zonegroup_segment_year_additive_clust_ward_pair.csv ../input/fe_summary_bw250_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv "$@"

../input/fe_summary_bw250_all_zonegroup_segment_year_additive_clust_ward_pair.csv: ../../border_pair_FE_regressions/output/fe_summary_bw250_all_zonegroup_segment_year_additive_clust_ward_pair.csv | ../input
	ln -sf "$<" "$@"

../input/fe_summary_bw250_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv: ../../border_pair_FE_regressions/output/fe_summary_bw250_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/fe_summary_bw250_all_zonegroup_segment_year_additive_clust_ward_pair.csv ../input/fe_summary_bw250_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv

include ../../generic.make
