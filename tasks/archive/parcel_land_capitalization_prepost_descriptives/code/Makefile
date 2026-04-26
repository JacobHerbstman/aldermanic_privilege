SHELL := bash
include ../../shell_functions.make

all: \
	../output/parcel_land_capitalization_prepost_treatment_summary.csv \
	../output/parcel_land_capitalization_prepost_distribution_quantiles.csv \
	../output/parcel_land_capitalization_prepost_level_summary.csv \
	../output/parcel_land_capitalization_prepost_delta_log_land_psf.pdf \
	../output/parcel_land_capitalization_prepost_delta_log_land_psf.png \
	../output/parcel_land_capitalization_prepost_delta_log_land_sum.pdf \
	../output/parcel_land_capitalization_prepost_delta_log_land_sum.png \
	../output/parcel_land_capitalization_prepost_median_land_psf_levels.pdf \
	../output/parcel_land_capitalization_prepost_median_land_psf_levels.png \
	../output/parcel_land_capitalization_prepost_transition_rates.pdf \
	../output/parcel_land_capitalization_prepost_transition_rates.png

../output/parcel_land_capitalization_prepost_treatment_summary.csv: build_parcel_land_capitalization_prepost_descriptives.R ../input/parcel_land_capitalization_prepost_2014_2016.parquet | ../output
	$(R) build_parcel_land_capitalization_prepost_descriptives.R ../input/parcel_land_capitalization_prepost_2014_2016.parquet ../output

../output/parcel_land_capitalization_prepost_distribution_quantiles.csv \
../output/parcel_land_capitalization_prepost_level_summary.csv \
../output/parcel_land_capitalization_prepost_delta_log_land_psf.pdf \
../output/parcel_land_capitalization_prepost_delta_log_land_psf.png \
../output/parcel_land_capitalization_prepost_delta_log_land_sum.pdf \
../output/parcel_land_capitalization_prepost_delta_log_land_sum.png \
../output/parcel_land_capitalization_prepost_median_land_psf_levels.pdf \
../output/parcel_land_capitalization_prepost_median_land_psf_levels.png \
../output/parcel_land_capitalization_prepost_transition_rates.pdf \
../output/parcel_land_capitalization_prepost_transition_rates.png: ../output/parcel_land_capitalization_prepost_treatment_summary.csv
	test -f $@

../input/parcel_land_capitalization_prepost_2014_2016.parquet: ../../prepare_parcel_land_capitalization_prepost_dataset/output/parcel_land_capitalization_prepost_2014_2016.parquet | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcel_land_capitalization_prepost_2014_2016.parquet
