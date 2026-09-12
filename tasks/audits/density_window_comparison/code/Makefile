include ../../../shared/code/shell_functions.make

all: ../output/density_windows.pdf ../output/density_windows.png ../report/density_window_estimates.csv.log ../output/density_windows_levels.pdf ../output/density_windows_levels.png ../report/density_window_estimates_levels.csv.log

all: ../report/density_window_estimates_straight.csv.log

../output/density_window_estimates_straight.csv: estimate_density_windows.R Makefile ../../../setup_environment/code/packages.R ../input/new_construction_analysis_data.csv ../input/alderman_uncertainty_index_through2022.csv ../input/density_boundary_characteristics.csv | ../output
	$(R) $< log straight

../report/density_window_estimates_straight.csv.log: ../../../shared/code/report.py ../output/density_window_estimates_straight.csv | ../report
	$(PYTHON) $< ../output/density_window_estimates_straight.csv $@ sample outcome window_ft

../output/density_window_estimates.csv: estimate_density_windows.R Makefile ../../../setup_environment/code/packages.R ../input/new_construction_analysis_data.csv ../input/alderman_uncertainty_index_through2022.csv | ../output
	$(R) $< log all

../output/density_windows.pdf: plot_density_windows.R Makefile ../../../setup_environment/code/packages.R ../output/density_window_estimates.csv | ../output
	$(R) $< log pdf

../output/density_windows.png: plot_density_windows.R Makefile ../../../setup_environment/code/packages.R ../output/density_window_estimates.csv | ../output
	$(R) $< log png

../report/density_window_estimates.csv.log: ../../../shared/code/report.py ../output/density_window_estimates.csv | ../report
	$(PYTHON) $< ../output/density_window_estimates.csv $@ sample outcome window_ft

../output/density_window_estimates_levels.csv: estimate_density_windows.R Makefile ../../../setup_environment/code/packages.R ../input/new_construction_analysis_data.csv ../input/alderman_uncertainty_index_through2022.csv | ../output
	$(R) $< levels all

../output/density_windows_levels.pdf: plot_density_windows.R Makefile ../../../setup_environment/code/packages.R ../output/density_window_estimates_levels.csv | ../output
	$(R) $< levels pdf

../output/density_windows_levels.png: plot_density_windows.R Makefile ../../../setup_environment/code/packages.R ../output/density_window_estimates_levels.csv | ../output
	$(R) $< levels png

../report/density_window_estimates_levels.csv.log: ../../../shared/code/report.py ../output/density_window_estimates_levels.csv | ../report
	$(PYTHON) $< ../output/density_window_estimates_levels.csv $@ sample outcome window_ft

../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_boundary_characteristics.csv: ../../../density_boundary_characteristics/output/density_boundary_characteristics.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

include ../../../shared/code/generic.make
