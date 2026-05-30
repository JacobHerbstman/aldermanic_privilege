MAX_APPLICATION_YM ?= 2022-12

all: \
	../output/permit_processing_time_high_vs_low_alderman_summary.tex \
	../output/alderman_mean_processing_time_high_vs_low_density.pdf \
	../output/permit_processing_time_volume_correlation.tex

../output/permit_processing_time_high_vs_low_alderman_summary.tex: permit_processing_time_high_vs_low_summary.R ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv | ../output
	Rscript $< $(MAX_APPLICATION_YM)

../output/alderman_mean_processing_time_high_vs_low_density.pdf: ../output/permit_processing_time_high_vs_low_alderman_summary.tex
	@test -s "$@"

../output/permit_processing_time_volume_correlation.tex: ../output/permit_processing_time_high_vs_low_alderman_summary.tex
	@test -s "$@"

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/building_permits_clean.gpkg \
	../input/ward_panel.gpkg \
	../input/chicago_alderman_panel.csv

include ../../generic.make
