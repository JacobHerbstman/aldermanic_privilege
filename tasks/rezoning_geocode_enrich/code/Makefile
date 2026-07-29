REZONING_DATE_TAG ?= 20101101_20201231

all: ../output/rezoning_geocoded_enriched_$(REZONING_DATE_TAG).csv

../output/rezoning_geocoded_enriched_$(REZONING_DATE_TAG).csv: enrich_geocoded_core.py enrich_geocoded_rezonings.py ../input/rezoning_geocode_with_external_$(REZONING_DATE_TAG).csv ../input/ward_panel.gpkg ../input/zoning_far_lookup_clean.csv | ../output run.sbatch
	$(PYTHON) $< --date-tag $(REZONING_DATE_TAG)

../input/rezoning_geocode_with_external_$(REZONING_DATE_TAG).csv: ../../rezoning_geocode_external_merge/output/rezoning_geocode_with_external_$(REZONING_DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/zoning_far_lookup_clean.csv: ../../zoning_data_cleaning/output/zoning_far_lookup_clean.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rezoning_geocode_with_external_$(REZONING_DATE_TAG).csv ../input/ward_panel.gpkg ../input/zoning_far_lookup_clean.csv

include ../../shell_functions.make
include ../../generic.make
