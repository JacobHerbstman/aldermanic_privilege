include ../../shared/code/shell_functions.make

all: ../output/new_construction_measurements.csv

../output/new_construction_measurements.csv: calculate_construction_density.R \
	../input/residential_selected_buildings.csv \
	../input/preferred_commercial_projects.csv \
	../input/construction_building_types.csv \
	../../shared/code/assessor_classification.R \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	Makefile | ../output ../report
	$(R) $<

../input/construction_building_types.csv: ../../new_construction_cleaning/output/construction_building_types.csv | ../input
	ln -sf $< $@

../input/preferred_commercial_projects.csv: ../../construction_commercial_measurements/output/preferred_commercial_projects.csv | ../input
	ln -sf $< $@

../input/residential_selected_buildings.csv: ../../construction_residential_measurements/output/residential_selected_buildings.csv | ../input
	ln -sf $< $@

link-inputs: ../input/construction_building_types.csv \
	../input/preferred_commercial_projects.csv \
	../input/residential_selected_buildings.csv

include ../../shared/code/generic.make
