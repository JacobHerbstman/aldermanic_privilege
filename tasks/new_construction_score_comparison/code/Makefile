SHELL := bash

BASELINE_SPEC := ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022

all: \
	../output/score_comparison_new_construction_vs_baseline.csv \
	../output/score_comparison_new_construction_demolition_vs_baseline.csv \
	../output/score_comparison_restricted_renovation_vs_baseline.csv \
	../output/score_rankings_restricted_renovation.tex

../output/score_comparison_new_construction_vs_baseline.csv: compare_new_construction_scores.R ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../input/alderman_uncertainty_index_new_construction.csv | ../output
	Rscript $< ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../input/alderman_uncertainty_index_new_construction.csv ../output/score_comparison_new_construction_vs_baseline.csv ../output/score_comparison_new_construction_vs_baseline_summary.csv ../output/score_comparison_new_construction_vs_baseline_summary.tex ../output/score_comparison_new_construction_vs_baseline_scatter.pdf new_construction "New construction only"

../output/score_comparison_new_construction_demolition_vs_baseline.csv: compare_new_construction_scores.R ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../input/alderman_uncertainty_index_new_construction_demolition.csv | ../output
	Rscript $< ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../input/alderman_uncertainty_index_new_construction_demolition.csv ../output/score_comparison_new_construction_demolition_vs_baseline.csv ../output/score_comparison_new_construction_demolition_vs_baseline_summary.csv ../output/score_comparison_new_construction_demolition_vs_baseline_summary.tex ../output/score_comparison_new_construction_demolition_vs_baseline_scatter.pdf new_construction_demolition "New construction + demolition"

../output/score_comparison_restricted_renovation_vs_baseline.csv: compare_new_construction_scores.R ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../input/alderman_uncertainty_index_restricted_renovation.csv | ../output
	Rscript $< ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../input/alderman_uncertainty_index_restricted_renovation.csv ../output/score_comparison_restricted_renovation_vs_baseline.csv ../output/score_comparison_restricted_renovation_vs_baseline_summary.csv ../output/score_comparison_restricted_renovation_vs_baseline_summary.tex ../output/score_comparison_restricted_renovation_vs_baseline_scatter.pdf restricted_renovation "Restricted renovation"

../output/score_rankings_restricted_renovation.tex: export_score_rankings_tex.R ../output/score_comparison_restricted_renovation_vs_baseline.csv | ../output
	Rscript $< ../output/score_comparison_restricted_renovation_vs_baseline.csv ../output/score_rankings_restricted_renovation.tex restricted_renovation

../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(BASELINE_SPEC).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_new_construction.csv: ../../new_construction_score_variants/output/alderman_uncertainty_index_new_construction.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_new_construction_demolition.csv: ../../new_construction_score_variants/output/alderman_uncertainty_index_new_construction_demolition.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_restricted_renovation.csv: ../../new_construction_score_variants/output/alderman_uncertainty_index_restricted_renovation.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../input/alderman_uncertainty_index_new_construction.csv ../input/alderman_uncertainty_index_new_construction_demolition.csv ../input/alderman_uncertainty_index_restricted_renovation.csv

include ../../shell_functions.make
include ../../generic.make
