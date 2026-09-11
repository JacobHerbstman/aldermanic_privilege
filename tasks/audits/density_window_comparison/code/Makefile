include ../../../shared/code/shell_functions.make

all: ../output/density_windows.pdf ../output/density_windows.png ../report/density_window_estimates.csv.log

../output/density_window_estimates.csv: estimate_density_windows.R Makefile ../../../setup_environment/code/packages.R ../input/new_construction_analysis_data.csv ../input/alderman_uncertainty_index_through2022.csv | ../output
	$(R) $<

../output/density_windows.pdf: plot_density_windows.R Makefile ../../../setup_environment/code/packages.R ../output/density_window_estimates.csv | ../output
	$(R) $< pdf

../output/density_windows.png: plot_density_windows.R Makefile ../../../setup_environment/code/packages.R ../output/density_window_estimates.csv | ../output
	$(R) $< png

../report/density_window_estimates.csv.log: ../../../shared/code/report.py ../output/density_window_estimates.csv | ../report
	$(PYTHON) $< ../output/density_window_estimates.csv $@ sample outcome window_ft

../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

include ../../../shared/code/generic.make
