SHELL := bash
include ../../shell_functions.make

all: \
	../output/did_coefficients_land_transaction_incidence_cohort_2015_sold_any_continuous_uniform_1000ft_geo_segment_clust_block_post5.csv \
	../output/did_metadata_land_transaction_incidence_cohort_2015_sold_any_continuous_uniform_1000ft_geo_segment_clust_block_post5.csv

../output/did_coefficients_land_transaction_incidence_cohort_2015_sold_any_continuous_uniform_1000ft_geo_segment_clust_block_post5.csv \
../output/did_metadata_land_transaction_incidence_cohort_2015_sold_any_continuous_uniform_1000ft_geo_segment_clust_block_post5.csv: run_did_land_transaction_incidence.R ../input/land_transaction_incidence_model_panel.parquet | ../output
	$(R) run_did_land_transaction_incidence.R cohort_2015 sold_any continuous uniform 1000 segment 5 none

../input/land_transaction_incidence_model_panel.parquet: ../../prepare_land_transaction_model_data/output/land_transaction_incidence_model_panel.parquet | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/land_transaction_incidence_model_panel.parquet
