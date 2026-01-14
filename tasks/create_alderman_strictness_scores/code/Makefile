SHELL := bash
include ../../shell_functions.make

OUTPUT_FILES := \
	../output/alderman_restrictiveness_scores_month_FEs.csv \
	../output/alderman_restrictiveness_scores_ward_month_FEs.csv \
	../output/Month_FEs_final_strictness_index.png \
	../output/Month_FEs_log_mean_total_fee.png \
	../output/Month_FEs_log_median_processing_time.png \
	../output/Month_FEs_log_n_permits_issued.png \
	../output/Month_FEs_permit_approval_rate.png \
	../output/Ward_Month_FEs_final_strictness_index.png \
	../output/Ward_Month_FEs_log_mean_total_fee.png \
	../output/Ward_Month_FEs_log_median_processing_time.png \
	../output/Ward_Month_FEs_log_n_permits_issued.png \
	../output/Ward_Month_FEs_permit_approval_rate.png \
	../output/stage1_residualization_models.tex

all: $(OUTPUT_FILES)

$(OUTPUT_FILES): create_restrictiveness_score.R ../input/ward_monthly_panel_for_alderman_fe.csv | ../output
	$(R) create_restrictiveness_score.R

../input/ward_monthly_panel_for_alderman_fe.csv: ../../data_for_alderman_strictness_scores/output/ward_monthly_panel_for_alderman_fe.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/ward_monthly_panel_for_alderman_fe.csv
