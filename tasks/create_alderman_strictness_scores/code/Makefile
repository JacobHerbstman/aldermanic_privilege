SHELL := bash
include ../../shell_functions.make

SCRIPT := create_restrictiveness_score.R
INPUT := ../input/ward_monthly_panel_for_alderman_fe.csv

FE_SPECS := month ward_month

CSV_month := ../output/alderman_restrictiveness_scores_month_FEs.csv
CSV_ward_month := ../output/alderman_restrictiveness_scores_ward_month_FEs.csv

PLOT_month := ../output/Month_FEs_final_strictness_index.pdf
PLOT_ward_month := ../output/Ward_Month_FEs_final_strictness_index.pdf

STAGE1_month := ../output/stage1_residualization_models.tex
STAGE1_ward_month := ../output/stage1_residualization_models_ward_month.tex

STAGE2_month := ../output/stage2_alderman_dummies_month_FEs.tex
STAGE2_ward_month := ../output/stage2_alderman_dummies_ward_month_FEs.tex

ALL_OUTPUTS := \
	$(CSV_month) $(CSV_ward_month) \
	$(PLOT_month) $(PLOT_ward_month) \
	$(STAGE1_month) $(STAGE1_ward_month) \
	$(STAGE2_month) $(STAGE2_ward_month)

all: $(ALL_OUTPUTS)

$(CSV_month) $(PLOT_month) $(STAGE1_month) $(STAGE2_month): $(SCRIPT) $(INPUT) | ../output
	$(R) $(SCRIPT) --fe_spec=month --input_csv=$(INPUT) --output_csv=$(CSV_month) --output_plot=$(PLOT_month) --output_stage1_tex=$(STAGE1_month) --output_stage2_tex=$(STAGE2_month)

$(CSV_ward_month) $(PLOT_ward_month) $(STAGE1_ward_month) $(STAGE2_ward_month): $(SCRIPT) $(INPUT) | ../output
	$(R) $(SCRIPT) --fe_spec=ward_month --input_csv=$(INPUT) --output_csv=$(CSV_ward_month) --output_plot=$(PLOT_ward_month) --output_stage1_tex=$(STAGE1_ward_month) --output_stage2_tex=$(STAGE2_ward_month)

$(INPUT): ../../data_for_alderman_strictness_scores/output/ward_monthly_panel_for_alderman_fe.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: $(INPUT)
