include ../../shared/code/shell_functions.make

all: ../report/boundary_feature_measurements.csv.log ../report/density_boundary_characteristics.csv.log

../output/boundary_feature_measurements.csv: build_boundary_features.R Makefile ../../setup_environment/code/packages.R ../input/boundary_segments_1320ft.gpkg ../input/major_streets.geojson ../input/gis_osm_roads_free_1.shp ../input/gis_osm_roads_free_1.dbf ../input/gis_osm_roads_free_1.prj ../input/gis_osm_roads_free_1.shx ../input/gis_osm_roads_free_1.cpg ../input/gis_osm_landuse_a_free_1.shp ../input/gis_osm_landuse_a_free_1.dbf ../input/gis_osm_landuse_a_free_1.prj ../input/gis_osm_landuse_a_free_1.shx ../input/gis_osm_landuse_a_free_1.cpg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg ../input/gis_osm_waterways_free_1.shp ../input/gis_osm_waterways_free_1.dbf ../input/gis_osm_waterways_free_1.prj ../input/gis_osm_waterways_free_1.shx ../input/gis_osm_waterways_free_1.cpg | ../output
	$(R) $<

../output/density_boundary_characteristics.csv: build_density_boundary_characteristics.R Makefile ../../setup_environment/code/packages.R ../input/new_construction_analysis_data.csv ../input/ward_pair_boundaries.gpkg ../input/schools_2015.gpkg ../input/parks.gpkg ../output/boundary_feature_measurements.csv ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	$(R) $<

../report/boundary_feature_measurements.csv.log: ../../shared/code/report.py ../output/boundary_feature_measurements.csv | ../report
	$(PYTHON) $< ../output/boundary_feature_measurements.csv $@ segment_id

../report/density_boundary_characteristics.csv.log: ../../shared/code/report.py ../output/density_boundary_characteristics.csv | ../report
	$(PYTHON) $< ../output/density_boundary_characteristics.csv $@ project_id

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/major_streets.geojson: ../../download_chicago_spatial_data/output/major_streets.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parks.gpkg: ../../prepare_amenities_data/output/parks.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_roads_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.shp | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_roads_free_1.dbf: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.dbf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_roads_free_1.prj: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.prj | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_roads_free_1.shx: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.shx | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_roads_free_1.cpg: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.cpg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.shp | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.dbf: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.dbf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.prj: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.prj | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.shx: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.shx | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.cpg: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.cpg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.dbf: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.dbf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.prj: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.prj | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shx: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shx | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.cpg: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.cpg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_waterways_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.shp | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_waterways_free_1.dbf: ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.dbf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_waterways_free_1.prj: ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.prj | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_waterways_free_1.shx: ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.shx | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_waterways_free_1.cpg: ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.cpg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

include ../../shared/code/generic.make
