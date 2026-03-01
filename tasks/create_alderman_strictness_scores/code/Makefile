SHELL := bash
include ../../shell_functions.make

REF_ALDERMAN ?= Andre Vasquez

all: \
	../output/alderman_restrictiveness_scores_month_FEs.csv \
	../output/alderman_restrictiveness_scores_ward_month_FEs.csv \
	../output/Month_FEs_final_strictness_index.pdf \
	../output/Ward_Month_FEs_final_strictness_index.pdf \
	../output/stage1_residualization_models.tex \
	../output/stage1_residualization_models_ward_month.tex \
	../output/stage2_alderman_dummies_month_FEs.tex \
	../output/stage2_alderman_dummies_ward_month_FEs.tex

../output/alderman_restrictiveness_scores_month_FEs.csv \
../output/Month_FEs_final_strictness_index.pdf \
../output/stage1_residualization_models.tex \
../output/stage2_alderman_dummies_month_FEs.tex: create_restrictiveness_score.R ../input/ward_monthly_panel_for_alderman_fe.csv | ../output
	Rscript create_restrictiveness_score.R month "$(REF_ALDERMAN)" ../input/ward_monthly_panel_for_alderman_fe.csv ../output/alderman_restrictiveness_scores_month_FEs.csv ../output/Month_FEs_final_strictness_index.pdf ../output/stage1_residualization_models.tex ../output/stage2_alderman_dummies_month_FEs.tex

../output/alderman_restrictiveness_scores_ward_month_FEs.csv \
../output/Ward_Month_FEs_final_strictness_index.pdf \
../output/stage1_residualization_models_ward_month.tex \
../output/stage2_alderman_dummies_ward_month_FEs.tex: create_restrictiveness_score.R ../input/ward_monthly_panel_for_alderman_fe.csv | ../output
	Rscript create_restrictiveness_score.R ward_month "$(REF_ALDERMAN)" ../input/ward_monthly_panel_for_alderman_fe.csv ../output/alderman_restrictiveness_scores_ward_month_FEs.csv ../output/Ward_Month_FEs_final_strictness_index.pdf ../output/stage1_residualization_models_ward_month.tex ../output/stage2_alderman_dummies_ward_month_FEs.tex

../input/ward_monthly_panel_for_alderman_fe.csv: ../../data_for_alderman_strictness_scores/output/ward_monthly_panel_for_alderman_fe.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/ward_monthly_panel_for_alderman_fe.csv

include ../../generic.make
