SHELL := bash
include ../../shell_functions.make

SAMPLE_SCOPES := all_baseline_empty no_ex developable_loose developable_core history_vacant_loose history_vacant_core history_vacant_urban_core current_vacant_loose current_vacant_core current_vacant_private_core
YEAR_SCOPES := admin_only admin_plus_fallback
BANDWIDTHS := 1000ft 500ft
TREATMENT_SPECS := continuous sign
OUTCOMES := delta_log_land_psf_2016_minus_2014 delta_log_land_sum_2016_minus_2014 delta_land_share_2016_minus_2014 building_positive_2016 gained_positive_building_2014_2016
MODEL_SPECS := $(foreach SAMPLE_SCOPE,$(SAMPLE_SCOPES),$(foreach YEAR_SCOPE,$(YEAR_SCOPES),$(foreach BANDWIDTH,$(BANDWIDTHS),$(foreach TREATMENT_SPEC,$(TREATMENT_SPECS),$(foreach OUTCOME,$(OUTCOMES),$(SAMPLE_SCOPE)__$(YEAR_SCOPE)__$(BANDWIDTH)__$(TREATMENT_SPEC)__$(OUTCOME))))))
HISTORY_VACANT_SAMPLE_SCOPES := history_vacant_loose history_vacant_core history_vacant_urban_core
HISTORY_VACANT_OUTCOMES := delta_log_land_psf_2016_minus_2014 delta_log_land_sum_2016_minus_2014 building_positive_2016 gained_positive_building_2014_2016
ANNOUNCEMENT_SAMPLE_SCOPES := history_vacant_core developable_core
ANNOUNCEMENT_OUTCOMES := delta_log_land_psf_2014_minus_2011 delta_log_land_sum_2014_minus_2011 building_positive_2014 gained_positive_building_2011_2014
ANNOUNCEMENT_MODEL_SPECS := $(foreach SAMPLE_SCOPE,$(ANNOUNCEMENT_SAMPLE_SCOPES),$(foreach BANDWIDTH,$(BANDWIDTHS),$(foreach TREATMENT_SPEC,$(TREATMENT_SPECS),$(foreach OUTCOME,$(ANNOUNCEMENT_OUTCOMES),$(SAMPLE_SCOPE)__admin_only__$(BANDWIDTH)__$(TREATMENT_SPEC)__$(OUTCOME)))))

all: \
	../output/parcel_land_capitalization_prepost_coefficients.csv \
	../output/parcel_land_capitalization_prepost_metadata.csv \
	../output/parcel_land_capitalization_prepost_sign_coefficients.pdf \
	../output/parcel_land_capitalization_prepost_sign_coefficients.png \
	../output/parcel_land_capitalization_prepost_continuous_coefficients.pdf \
	../output/parcel_land_capitalization_prepost_continuous_coefficients.png \
	$(foreach MODEL_SPEC,$(ANNOUNCEMENT_MODEL_SPECS),../output/announcement_model_coefficients_$(MODEL_SPEC).csv ../output/announcement_model_metadata_$(MODEL_SPEC).csv)

announcement-checks: $(foreach MODEL_SPEC,$(ANNOUNCEMENT_MODEL_SPECS),../output/announcement_model_coefficients_$(MODEL_SPEC).csv ../output/announcement_model_metadata_$(MODEL_SPEC).csv)

../output/announcement_model_coefficients_%.csv ../output/announcement_model_metadata_%.csv: run_parcel_land_capitalization_prepost_model.R ../input/parcel_land_capitalization_prepost_2011_2014.parquet | ../output
	stem="$*"; \
	sample_scope="$${stem%%__*}"; \
	rest="$${stem#*__}"; \
	year_scope="$${rest%%__*}"; \
	rest="$${rest#*__}"; \
	bandwidth="$${rest%%__*}"; \
	rest="$${rest#*__}"; \
	treatment_spec="$${rest%%__*}"; \
	outcome="$${rest#*__}"; \
	$(patsubst @%,%,$(R)) run_parcel_land_capitalization_prepost_model.R ../input/parcel_land_capitalization_prepost_2011_2014.parquet "$$sample_scope" "$$year_scope" "$$bandwidth" "$$outcome" "$$treatment_spec" ../output/announcement_model_coefficients_$*.csv ../output/announcement_model_metadata_$*.csv

../output/model_coefficients_%.csv ../output/model_metadata_%.csv: run_parcel_land_capitalization_prepost_model.R ../input/parcel_land_capitalization_prepost_2014_2016.parquet | ../output
	stem="$*"; \
	sample_scope="$${stem%%__*}"; \
	rest="$${stem#*__}"; \
	year_scope="$${rest%%__*}"; \
	rest="$${rest#*__}"; \
	bandwidth="$${rest%%__*}"; \
	rest="$${rest#*__}"; \
	treatment_spec="$${rest%%__*}"; \
	outcome="$${rest#*__}"; \
	$(patsubst @%,%,$(R)) run_parcel_land_capitalization_prepost_model.R ../input/parcel_land_capitalization_prepost_2014_2016.parquet "$$sample_scope" "$$year_scope" "$$bandwidth" "$$outcome" "$$treatment_spec" ../output/model_coefficients_$*.csv ../output/model_metadata_$*.csv

../output/parcel_land_capitalization_prepost_coefficients.csv \
../output/parcel_land_capitalization_prepost_metadata.csv \
../output/parcel_land_capitalization_prepost_sign_coefficients.pdf \
../output/parcel_land_capitalization_prepost_sign_coefficients.png \
../output/parcel_land_capitalization_prepost_continuous_coefficients.pdf \
../output/parcel_land_capitalization_prepost_continuous_coefficients.png: combine_parcel_land_capitalization_prepost_models.R $(foreach MODEL_SPEC,$(MODEL_SPECS),../output/model_coefficients_$(MODEL_SPEC).csv ../output/model_metadata_$(MODEL_SPEC).csv) | ../output
	$(patsubst @%,%,$(R)) combine_parcel_land_capitalization_prepost_models.R ../output ../output/parcel_land_capitalization_prepost_coefficients.csv ../output/parcel_land_capitalization_prepost_metadata.csv ../output/parcel_land_capitalization_prepost_sign_coefficients.pdf ../output/parcel_land_capitalization_prepost_sign_coefficients.png ../output/parcel_land_capitalization_prepost_continuous_coefficients.pdf ../output/parcel_land_capitalization_prepost_continuous_coefficients.png

../output/parcel_land_history_vacant_segment_effect_sizes_continuous.pdf \
../output/parcel_land_history_vacant_segment_effect_sizes_continuous.png \
../output/parcel_land_history_vacant_segment_effect_sizes_sign.pdf \
../output/parcel_land_history_vacant_segment_effect_sizes_sign.png \
../output/parcel_land_history_vacant_core_fe_comparison.pdf \
../output/parcel_land_history_vacant_core_fe_comparison.png: plot_history_vacant_effect_sizes.R \
$(foreach SAMPLE_SCOPE,$(HISTORY_VACANT_SAMPLE_SCOPES),$(foreach BANDWIDTH,$(BANDWIDTHS),$(foreach TREATMENT_SPEC,$(TREATMENT_SPECS),$(foreach OUTCOME,$(HISTORY_VACANT_OUTCOMES),../output/model_coefficients_$(SAMPLE_SCOPE)__admin_only__$(BANDWIDTH)__$(TREATMENT_SPEC)__$(OUTCOME).csv)))) \
../output/model_coefficients_history_vacant_core__admin_only__1000ft__continuous__delta_log_land_psf_2016_minus_2014__ward_pair_id.csv \
../output/model_coefficients_history_vacant_core__admin_only__500ft__continuous__delta_log_land_psf_2016_minus_2014__ward_pair_id.csv \
../output/model_coefficients_history_vacant_core__admin_only__1000ft__sign__delta_log_land_psf_2016_minus_2014__ward_pair_id.csv \
../output/model_coefficients_history_vacant_core__admin_only__500ft__sign__delta_log_land_psf_2016_minus_2014__ward_pair_id.csv \
../output/model_coefficients_history_vacant_core__admin_only__1000ft__continuous__delta_log_land_sum_2016_minus_2014__ward_pair_id.csv \
../output/model_coefficients_history_vacant_core__admin_only__500ft__continuous__delta_log_land_sum_2016_minus_2014__ward_pair_id.csv \
../output/model_coefficients_history_vacant_core__admin_only__1000ft__sign__delta_log_land_sum_2016_minus_2014__ward_pair_id.csv \
../output/model_coefficients_history_vacant_core__admin_only__500ft__sign__delta_log_land_sum_2016_minus_2014__ward_pair_id.csv | ../output
	$(patsubst @%,%,$(R)) plot_history_vacant_effect_sizes.R ../output ../output/parcel_land_history_vacant_segment_effect_sizes_continuous.pdf ../output/parcel_land_history_vacant_segment_effect_sizes_continuous.png ../output/parcel_land_history_vacant_segment_effect_sizes_sign.pdf ../output/parcel_land_history_vacant_segment_effect_sizes_sign.png ../output/parcel_land_history_vacant_core_fe_comparison.pdf ../output/parcel_land_history_vacant_core_fe_comparison.png

../input/parcel_land_capitalization_prepost_2014_2016.parquet: ../../prepare_parcel_land_capitalization_prepost_dataset/output/parcel_land_capitalization_prepost_2014_2016.parquet | ../input
	ln -sf $< $@

../input/parcel_land_capitalization_prepost_2011_2014.parquet: ../../prepare_parcel_land_capitalization_prepost_dataset/output/parcel_land_capitalization_prepost_2011_2014.parquet | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcel_land_capitalization_prepost_2014_2016.parquet ../input/parcel_land_capitalization_prepost_2011_2014.parquet
