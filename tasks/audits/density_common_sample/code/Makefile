include ../../../shared/code/shell_functions.make

all: ../output/density_common_sample_estimates.csv

../output/density_common_sample_estimates.csv: compare_density_samples.R Makefile \
	../../../setup_environment/code/packages.R \
	../input/new_construction_analysis_data.csv \
	../input/density_boundary_characteristics.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $<

../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../input/density_boundary_characteristics.csv: ../../../density_boundary_characteristics/output/density_boundary_characteristics.csv | ../input
	ln -sf $< $@

link-inputs: ../input/new_construction_analysis_data.csv ../input/density_boundary_characteristics.csv

include ../../../shared/code/generic.make
