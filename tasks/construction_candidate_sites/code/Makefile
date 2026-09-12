include ../../shared/code/shell_functions.make

MIN_CONSTRUCTION_YEAR ?= 1999
MAX_DISCOVERY_CONSTRUCTION_YEAR ?= 2026

all: ../output/construction_parcel_boundary_distances.csv

../%/construction_discovery_ward_maps.gpkg \
	../%/construction_discovery_boundaries.gpkg: build_construction_discovery_geography.R \
	../../shared/code/canonical_geometry_helpers.R \
	../input/Chicago_Wards_1998.geojson \
	../input/Wards_2014.geojson \
	../input/Wards_2015.geojson \
	../input/Wards_2024.geojson \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	Makefile | ../output ../report
	$(R) $<

../output/construction_parcel_boundary_distances.csv: build_construction_parcel_distances.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../output/construction_discovery_ward_maps.gpkg \
	../output/construction_discovery_boundaries.gpkg \
	../input/geocoded_residential_data.gpkg \
	../../setup_environment/code/packages.R \
	../../shared/code/canonical_geometry_helpers.R \
	Makefile | ../output ../report
	$(R) $< $(MIN_CONSTRUCTION_YEAR) $(MAX_DISCOVERY_CONSTRUCTION_YEAR)

../input/Chicago_Wards_1998.geojson: ../../../data_raw/Chicago_Wards_1998.geojson | ../input
	ln -sf $< $@

../input/Wards_2014.geojson: ../../../data_raw/Wards_2014.geojson | ../input
	ln -sf $< $@

../input/Wards_2015.geojson: ../../../data_raw/Wards_2015.geojson | ../input
	ln -sf $< $@

../input/Wards_2024.geojson: ../../../data_raw/Wards_2024.geojson | ../input
	ln -sf $< $@

../input/geocoded_residential_data.gpkg: ../../construction_assessor_records/output/geocoded_residential_data.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/Chicago_Wards_1998.geojson \
	../input/Wards_2014.geojson \
	../input/Wards_2015.geojson \
	../input/Wards_2024.geojson \
	../input/geocoded_residential_data.gpkg

include ../../shared/code/generic.make
