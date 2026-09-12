include ../../../shared/code/shell_functions.make

all: ../report/density_common_sample_estimates.csv.log

../output/density_common_sample_estimates.csv: compare_density_samples.R Makefile \
	../../../setup_environment/code/packages.R \
	../../../density_main_results/code/build_density_main_results.R \
	../../../density_appendix_results/code/build_density_appendix_results.R \
	../../../density_boundary_checks/code/build_density_boundary_checks.R \
	../../../density_score_robustness/code/build_density_score_robustness.R \
	../input/new_construction_analysis_data.csv ../input/alderman_uncertainty_index_through2022.csv \
	../input/density_boundary_characteristics.csv | ../output
	$(R) $<

../report/density_common_sample_estimates.csv.log: ../../../shared/code/report.py ../output/density_common_sample_estimates.csv | ../report
	$(PYTHON) $< ../output/density_common_sample_estimates.csv $@ check sample outcome eligibility

../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_boundary_characteristics.csv: ../../../density_boundary_characteristics/output/density_boundary_characteristics.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/new_construction_analysis_data.csv ../input/alderman_uncertainty_index_through2022.csv ../input/density_boundary_characteristics.csv

include ../../../shared/code/generic.make
