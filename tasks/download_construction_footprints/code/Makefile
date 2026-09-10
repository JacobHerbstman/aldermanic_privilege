include ../../shared/code/shell_functions.make

SNAPSHOT_YEARS ?= 2008 2022

all: $(foreach year,$(SNAPSHOT_YEARS),../output/official_building_footprints_$(year).gpkg)

../output/official_building_footprints_%.gpkg: download_official_footprint_snapshot.R ../../setup_environment/code/packages.R ../input/eligibility_uncorroborated_retained.csv ../input/preferred_project_year_geometry.gpkg | ../output
	$(R) $< $*

../input/eligibility_uncorroborated_retained.csv: ../../new_construction_cleaning/output/eligibility_uncorroborated_retained.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_project_year_geometry.gpkg: ../../new_construction_cleaning/output/preferred_project_year_geometry.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/eligibility_uncorroborated_retained.csv ../input/preferred_project_year_geometry.gpkg

include ../../shared/code/generic.make
