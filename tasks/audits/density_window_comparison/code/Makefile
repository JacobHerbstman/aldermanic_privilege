include ../../../shared/code/shell_functions.make

all: ../output/density_windows.pdf ../output/density_windows.png ../output/density_window_estimates.csv ../output/density_windows_levels.pdf ../output/density_windows_levels.png ../output/density_window_estimates_levels.csv ../output/density_window_estimates_straight.csv

../output/density_window_estimates_straight.csv: estimate_density_windows.R Makefile ../../../setup_environment/code/packages.R ../input/new_construction_analysis_data.csv ../input/density_boundary_characteristics.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< log straight

../output/density_window_estimates.csv: estimate_density_windows.R Makefile ../../../setup_environment/code/packages.R ../input/new_construction_analysis_data.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< log all

../output/density_windows.pdf: plot_density_windows.R Makefile ../../../setup_environment/code/packages.R ../output/density_window_estimates.csv | ../output ../report
	$(R) $< log pdf

../output/density_windows.png: plot_density_windows.R Makefile ../../../setup_environment/code/packages.R ../output/density_window_estimates.csv | ../output ../report
	$(R) $< log png

../output/density_window_estimates_levels.csv: estimate_density_windows.R Makefile ../../../setup_environment/code/packages.R ../input/new_construction_analysis_data.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< levels all

../output/density_windows_levels.pdf: plot_density_windows.R Makefile ../../../setup_environment/code/packages.R ../output/density_window_estimates_levels.csv | ../output ../report
	$(R) $< levels pdf

../output/density_windows_levels.png: plot_density_windows.R Makefile ../../../setup_environment/code/packages.R ../output/density_window_estimates_levels.csv | ../output ../report
	$(R) $< levels png

../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../input/density_boundary_characteristics.csv: ../../../density_boundary_characteristics/output/density_boundary_characteristics.csv | ../input
	ln -sf $< $@

link-inputs: ../input/new_construction_analysis_data.csv ../input/density_boundary_characteristics.csv

include ../../../shared/code/generic.make
