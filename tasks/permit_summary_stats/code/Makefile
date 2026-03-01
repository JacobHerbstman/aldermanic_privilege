SHELL := bash
include ../../shell_functions.make

all: \
	../output/high_discretion_permit_counts.csv \
	../output/high_discretion_permit_counts.tex \
	../output/processing_time_raw_high_discretion.pdf \
	../output/processing_time_raw_new_construction.pdf \
	../output/processing_time_residualized_high_discretion.pdf \
	../output/processing_time_residualized_new_construction.pdf \
	../output/processing_time_distribution_by_alderman.csv \
	../output/processing_time_distribution_by_permit_type.csv \
	../output/processing_time_distribution_by_alderman_type.csv \
	../output/processing_time_variation_across_aldermen_by_type.csv \
	../output/permit_processing_convergence_monthly_cross_alderman.csv \
	../output/permit_processing_convergence_monthly_overall.csv \
	../output/permit_processing_convergence_prepost_tests.csv \
	../output/permit_processing_convergence_july2019.pdf

../output/high_discretion_permit_counts.csv \
../output/high_discretion_permit_counts.tex \
../output/processing_time_raw_high_discretion.pdf \
../output/processing_time_raw_new_construction.pdf \
../output/processing_time_residualized_high_discretion.pdf \
../output/processing_time_residualized_new_construction.pdf \
../output/processing_time_distribution_by_alderman.csv \
../output/processing_time_distribution_by_permit_type.csv \
../output/processing_time_distribution_by_alderman_type.csv \
../output/processing_time_variation_across_aldermen_by_type.csv \
../output/permit_processing_convergence_monthly_cross_alderman.csv \
../output/permit_processing_convergence_monthly_overall.csv \
../output/permit_processing_convergence_prepost_tests.csv \
../output/permit_processing_convergence_july2019.pdf: permit_summary_stats.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript permit_summary_stats.R ../input/permits_for_uncertainty_index.csv ../output

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/permits_for_uncertainty_index.csv

include ../../generic.make
