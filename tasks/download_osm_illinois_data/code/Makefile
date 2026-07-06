all: \
	../output/gis_osm_roads_free_1.shp \
	../output/gis_osm_roads_free_1.dbf \
	../output/gis_osm_roads_free_1.prj \
	../output/gis_osm_roads_free_1.shx \
	../output/gis_osm_roads_free_1.cpg \
	../output/gis_osm_landuse_a_free_1.shp \
	../output/gis_osm_landuse_a_free_1.dbf \
	../output/gis_osm_landuse_a_free_1.prj \
	../output/gis_osm_landuse_a_free_1.shx \
	../output/gis_osm_landuse_a_free_1.cpg \
	../output/gis_osm_water_a_free_1.shp \
	../output/gis_osm_water_a_free_1.dbf \
	../output/gis_osm_water_a_free_1.prj \
	../output/gis_osm_water_a_free_1.shx \
	../output/gis_osm_water_a_free_1.cpg \
	../output/gis_osm_waterways_free_1.shp \
	../output/gis_osm_waterways_free_1.dbf \
	../output/gis_osm_waterways_free_1.prj \
	../output/gis_osm_waterways_free_1.shx \
	../output/gis_osm_waterways_free_1.cpg

../output/gis_osm_roads_free_1.shp: download_osm_illinois_data.sh | ../output ../temp
	bash $<

../output/gis_osm_roads_free_1.dbf \
../output/gis_osm_roads_free_1.prj \
../output/gis_osm_roads_free_1.shx \
../output/gis_osm_roads_free_1.cpg \
../output/gis_osm_landuse_a_free_1.shp \
../output/gis_osm_landuse_a_free_1.dbf \
../output/gis_osm_landuse_a_free_1.prj \
../output/gis_osm_landuse_a_free_1.shx \
../output/gis_osm_landuse_a_free_1.cpg \
../output/gis_osm_water_a_free_1.shp \
../output/gis_osm_water_a_free_1.dbf \
../output/gis_osm_water_a_free_1.prj \
../output/gis_osm_water_a_free_1.shx \
../output/gis_osm_water_a_free_1.cpg \
../output/gis_osm_waterways_free_1.shp \
../output/gis_osm_waterways_free_1.dbf \
../output/gis_osm_waterways_free_1.prj \
../output/gis_osm_waterways_free_1.shx \
../output/gis_osm_waterways_free_1.cpg: ../output/gis_osm_roads_free_1.shp
	@test -s "$@"

include ../../generic.make
