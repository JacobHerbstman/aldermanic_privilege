include ../../../shared/code/shell_functions.make

all: ../output/commercial_completion_evidence.csv

../output/commercial_completion_evidence.csv: build_commercial_completion_evidence.R ../output/commercial_residential_history_evidence.csv ../input/commercial_address_permit_history.csv ../input/commercial_ground_up_evidence.csv ../input/new_construction_spatial_permit_matches.csv ../input/preferred_commercial_project_candidates.csv ../input/project_permit_chain_links.csv ../../../setup_environment/code/packages.R ../../../shared/code/save_data.R ../../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../output/commercial_residential_history_evidence.csv: build_commercial_residential_history_evidence.R ../input/residential_improvement_characteristics_full.csv ../input/preferred_commercial_project_candidates.csv ../input/preferred_commercial_project_components.csv ../../../setup_environment/code/packages.R ../../../shared/code/save_data.R ../../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../input/commercial_address_permit_history.csv: ../../../construction_commercial_permits/output/commercial_address_permit_history.csv | ../input
	ln -sf $< $@

../input/commercial_ground_up_evidence.csv: ../../../construction_commercial_permits/output/commercial_ground_up_evidence.csv | ../input
	ln -sf $< $@

../input/new_construction_spatial_permit_matches.csv: ../../../construction_project_permits/output/new_construction_spatial_permit_matches.csv | ../input
	ln -sf $< $@

../input/preferred_commercial_project_candidates.csv: ../../../construction_commercial_measurements/output/preferred_commercial_project_candidates.csv | ../input
	ln -sf $< $@

../input/preferred_commercial_project_components.csv: ../../../construction_commercial_measurements/output/preferred_commercial_project_components.csv | ../input
	ln -sf $< $@

../input/project_permit_chain_links.csv: ../../../construction_project_permits/output/project_permit_chain_links.csv | ../input
	ln -sf $< $@

../input/residential_improvement_characteristics_full.csv: ../../../../data_raw/construction_review/residential_improvement_characteristics_full.csv | ../input
	ln -sf $< $@

link-inputs: ../input/commercial_address_permit_history.csv ../input/commercial_ground_up_evidence.csv ../input/new_construction_spatial_permit_matches.csv ../input/preferred_commercial_project_candidates.csv ../input/preferred_commercial_project_components.csv ../input/project_permit_chain_links.csv ../input/residential_improvement_characteristics_full.csv

include ../../../shared/code/generic.make
