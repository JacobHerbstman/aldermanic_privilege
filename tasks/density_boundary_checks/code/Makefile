include ../../shared/code/shell_functions.make

all: ../output/density_boundary_robustness.tex \
	../output/density_location_continuity.tex

../%/density_boundary_robustness.tex \
	../%/density_location_continuity.tex: \
	build_density_boundary_checks.R Makefile ../../setup_environment/code/packages.R \
	../input/new_construction_analysis_data.csv \
	../input/alderman_uncertainty_index_through2022.csv \
	../input/density_boundary_characteristics.csv | ../output
	$(R) $<

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../input/alderman_uncertainty_index_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@

../input/density_boundary_characteristics.csv: ../../density_boundary_characteristics/output/density_boundary_characteristics.csv | ../input
	ln -sf $< $@

link-inputs: \
	../input/new_construction_analysis_data.csv \
	../input/alderman_uncertainty_index_through2022.csv \
	../input/density_boundary_characteristics.csv

include ../../shared/code/generic.make
