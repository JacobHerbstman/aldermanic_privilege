SHELL := bash
include ../../shell_functions.make

MAX_APPLICATION_YM ?= 2022-12

all: \
	../output/permit_processing_time_high_vs_low_summary.csv \
	../output/permit_processing_time_high_vs_low_summary.tex \
	../output/ward_mean_processing_time_high_vs_low.csv \
	../output/ward_mean_processing_time_high_vs_low_density.pdf \
	../output/permit_processing_time_high_vs_low_alderman_summary.csv \
	../output/permit_processing_time_high_vs_low_alderman_summary.tex \
	../output/alderman_mean_processing_time_high_vs_low.csv \
	../output/alderman_mean_processing_time_high_vs_low_density.pdf \
	../output/permit_processing_time_volume_correlation.csv \
	../output/permit_processing_time_volume_correlation.tex

../output/permit_processing_time_high_vs_low_summary.csv: permit_processing_time_high_vs_low_summary.R ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv | ../output
	Rscript permit_processing_time_high_vs_low_summary.R ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../output/permit_processing_time_high_vs_low_summary.csv ../output/permit_processing_time_high_vs_low_summary.tex ../output/ward_mean_processing_time_high_vs_low.csv ../output/ward_mean_processing_time_high_vs_low_density.pdf ../output/permit_processing_time_high_vs_low_alderman_summary.csv ../output/permit_processing_time_high_vs_low_alderman_summary.tex ../output/alderman_mean_processing_time_high_vs_low.csv ../output/alderman_mean_processing_time_high_vs_low_density.pdf ../output/permit_processing_time_volume_correlation.csv ../output/permit_processing_time_volume_correlation.tex $(MAX_APPLICATION_YM)

../output/permit_processing_time_high_vs_low_summary.tex: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/ward_mean_processing_time_high_vs_low.csv: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/ward_mean_processing_time_high_vs_low_density.pdf: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/permit_processing_time_high_vs_low_alderman_summary.csv: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/permit_processing_time_high_vs_low_alderman_summary.tex: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/alderman_mean_processing_time_high_vs_low.csv: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/alderman_mean_processing_time_high_vs_low_density.pdf: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/permit_processing_time_volume_correlation.csv: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/permit_processing_time_volume_correlation.tex: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv

include ../../generic.make
