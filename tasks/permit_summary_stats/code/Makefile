include ../../shared/code/shell_functions.make

MAX_APPLICATION_YM ?= 2022-12

all: ../output/permit_processing_time_high_vs_low_alderman_summary.tex \
	../output/alderman_mean_processing_time_high_vs_low_density.pdf \
	../output/permit_processing_time_volume_correlation.tex

../%/permit_processing_time_high_vs_low_alderman_summary.tex \
	../%/alderman_mean_processing_time_high_vs_low_density.pdf \
	../%/permit_processing_time_volume_correlation.tex: build_permit_summary_stats.R ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_terms.csv Makefile | ../output
	$(R) $< $(MAX_APPLICATION_YM)

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/chicago_alderman_terms.csv: ../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	ln -sf $< $@

link-inputs: ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_terms.csv

include ../../shared/code/generic.make
