START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/ward_controls_$(START_YEAR)_$(END_YEAR).csv

../output/ward_controls_$(START_YEAR)_$(END_YEAR).csv: create_ward_controls.R ../input/ward_panel.gpkg | ../output
	Rscript $< $(START_YEAR) $(END_YEAR)

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"


link-inputs: ../input/ward_panel.gpkg

include ../../generic.make
