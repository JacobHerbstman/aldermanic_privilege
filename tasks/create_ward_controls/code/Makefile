all: ../output/ward_controls_2000_2023.csv

../output/ward_controls_2000_2023.csv: create_ward_controls.R ../input/ward_panel.gpkg | ../output
	Rscript $<

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"


link-inputs: ../input/ward_panel.gpkg

include ../../generic.make
