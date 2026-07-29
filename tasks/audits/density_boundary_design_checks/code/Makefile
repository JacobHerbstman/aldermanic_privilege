all: \
	../output/density_straight_boundary_results.csv \
	../output/density_covariate_continuity.tex \
	../output/density_covariate_continuity_straight.tex

../output/density_straight_boundary_results.csv: \
	build_straight_boundary_check.R \
	../input/new_construction_analysis_data.csv \
	../input/project_centroids.gpkg \
	../input/final_new_construction_audit_ledger.csv \
	../input/ward_pair_boundaries.gpkg \
	../input/segment_classification.csv | ../output
	$(R) $<

../output/density_straight_boundary_classification.csv: ../output/density_straight_boundary_results.csv

../output/density_covariate_continuity.tex: \
	build_covariate_continuity.R \
	../../../_lib/amenity_distance_helpers.R \
	../output/density_straight_boundary_classification.csv \
	../input/new_construction_analysis_data.csv \
	../input/project_centroids.gpkg \
	../input/final_new_construction_audit_ledger.csv \
	../input/block_group_controls.csv \
	../input/block_group_geometry_2019.gpkg \
	../input/schools_2015.gpkg \
	../input/parks.gpkg \
	../input/major_streets.gpkg \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.dbf \
	../input/gis_osm_water_a_free_1.prj \
	../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_water_a_free_1.cpg | ../output
	$(R) $<

../output/density_amenity_continuity.csv: ../output/density_covariate_continuity.tex

../output/density_boundary_characteristics.csv: ../output/density_covariate_continuity.tex

../output/density_acs_continuity.csv: ../output/density_covariate_continuity.tex

../output/density_acs_assignment_diagnostics.csv: ../output/density_covariate_continuity.tex

../output/density_covariate_continuity_straight.tex: ../output/density_covariate_continuity.tex

../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/project_centroids.gpkg: ../../commercial_new_construction_sample_audit/output/preferred_new_construction_project_centroids.gpkg | ../input
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/final_new_construction_audit_ledger.csv: ../../commercial_new_construction_sample_audit/output/final_new_construction_audit_ledger.csv | ../input
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/ward_pair_boundaries.gpkg: ../../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/segment_classification.csv: ../../../border_segment_creation/output/segment_classification.csv | ../input
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/block_group_controls.csv: ../../../create_block_group_controls/output/block_group_controls.csv | ../input
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/block_group_geometry_2019.gpkg: ../../../create_block_group_controls/output/block_group_geometry_2019.gpkg | ../input
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/schools_2015.gpkg: ../../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/parks.gpkg: ../../../prepare_amenities_data/output/parks.gpkg | ../input
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/major_streets.gpkg: ../../../prepare_amenities_data/output/major_streets.gpkg | ../input
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/gis_osm_water_a_free_1.%: ../../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

link-inputs: \
	../input/new_construction_analysis_data.csv \
	../input/project_centroids.gpkg \
	../input/final_new_construction_audit_ledger.csv \
	../input/ward_pair_boundaries.gpkg \
	../input/segment_classification.csv \
	../input/block_group_controls.csv \
	../input/block_group_geometry_2019.gpkg \
	../input/schools_2015.gpkg \
	../input/parks.gpkg \
	../input/major_streets.gpkg \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.dbf \
	../input/gis_osm_water_a_free_1.prj \
	../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_water_a_free_1.cpg

include ../../../shell_functions.make
include ../../../generic.make
