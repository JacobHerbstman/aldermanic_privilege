# Current construction cleaning: execution order

Generated from the current Make targets. Steps at the same level are independent.
The chronological construction outputs feed the density analyses; older unused final-assembly rules are excluded.

## Dependency level 1

- [build_commercial_cross_section.R](../tasks/new_construction_cleaning/code/build_commercial_cross_section.R): `multifamily_data_cleaned.csv`
- [build_construction_discovery_geography.R](../tasks/new_construction_cleaning/code/build_construction_discovery_geography.R): `construction_discovery_boundaries.gpkg`, `construction_discovery_ward_maps.gpkg`
- [build_construction_permit_history.R](../tasks/new_construction_cleaning/code/build_construction_permit_history.R): `building_permits_for_verification.gpkg`
- [build_historical_address_history.R](../tasks/new_construction_cleaning/code/build_historical_address_history.R): `density_parcel_address_selected_history.csv`
- [build_residential_cross_section.R](../tasks/new_construction_cleaning/code/build_residential_cross_section.R): `residential_cross_section.csv`, `residential_discovery_cross_section.csv`

## Dependency level 2

- [build_historical_coordinate_requests.R](../tasks/new_construction_cleaning/code/build_historical_coordinate_requests.R): `density_historical_building_universe.csv`

## Dependency level 3

- [build_historical_coordinates.R](../tasks/new_construction_cleaning/code/build_historical_coordinates.R): `density_historical_coordinate_candidates.csv`
- [build_historical_project_screen.R](../tasks/new_construction_cleaning/code/build_historical_project_screen.R): `density_project_lineage.csv`

## Dependency level 4

- [build_historical_address_screen.R](../tasks/new_construction_cleaning/code/build_historical_address_screen.R): `density_parcel_address_lineage_evidence.csv`

## Dependency level 5

- [select_historical_coordinates.R](../tasks/new_construction_cleaning/code/select_historical_coordinates.R): `density_historical_coordinates.csv`

## Dependency level 6

- [geocode_residential_data.R](../tasks/new_construction_cleaning/code/geocode_residential_data.R): `geocoded_residential_data.gpkg`

## Dependency level 7

- [build_construction_parcel_distances.R](../tasks/new_construction_cleaning/code/build_construction_parcel_distances.R): `construction_parcel_boundary_distances.csv`

## Dependency level 8

- [build_commercial_project_candidates.R](../tasks/new_construction_cleaning/code/build_commercial_project_candidates.R): `commercial_address_family_candidates.csv`, `commercial_entity_component_candidates.csv`, `commercial_entity_version_candidates.csv`, `commercial_family_vintage_summary.csv`, `commercial_production_family_members.csv`, `commercial_project_family_review.csv`
- [build_residential_project_candidates.R](../tasks/new_construction_cleaning/code/build_residential_project_candidates.R): `residential_fractional_base_groups.csv`, `residential_multicard_cards.csv`, `residential_project_candidate_inventory.csv`, `residential_tieback_groups_full.csv`, `residential_tieback_members_full.csv`

## Dependency level 9

- [build_new_construction_permit_evidence.R](../tasks/new_construction_cleaning/code/build_new_construction_permit_evidence.R): `new_construction_exact_permit_matches.csv`, `new_construction_project_components.csv`
- [build_residential_tieback_temporal_evidence.R](../tasks/new_construction_cleaning/code/build_residential_tieback_temporal_evidence.R): `residential_tieback_temporal_lineage_evidence.csv`

## Dependency level 10

- [build_historical_project_geography.R](../tasks/new_construction_cleaning/code/build_historical_project_geography.R): `historical_project_year_geometry.gpkg`
- [build_residential_assessor_projects.R](../tasks/new_construction_cleaning/code/build_residential_assessor_projects.R): `residential_assessor_project_candidates.csv`

## Dependency level 11

- [build_spatial_permit_evidence.R](../tasks/new_construction_cleaning/code/build_spatial_permit_evidence.R): `new_construction_spatial_permit_matches.csv`

## Dependency level 12

- [build_permit_revision_evidence.R](../tasks/new_construction_cleaning/code/build_permit_revision_evidence.R): `project_permit_chain_links.csv`, `project_permit_chain_summary.csv`, `project_permit_chain_unit_mentions.csv`

## Dependency level 13

- [build_preferred_commercial_candidates.R](../tasks/new_construction_cleaning/code/build_preferred_commercial_candidates.R): `commercial_adjudication_queue.csv`, `preferred_commercial_project_candidates.csv`
- [select_residential_assessments.R](../tasks/new_construction_cleaning/code/select_residential_assessments.R): `residential_selected_assessments.csv`

## Dependency level 14

- [apply_residential_building_corrections.R](../tasks/new_construction_cleaning/code/apply_residential_building_corrections.R): `preferred_residential_project_candidates.csv`, `residential_adjudication_queue.csv`

## Dependency level 15

- [build_preferred_geography_requests.R](../tasks/new_construction_cleaning/code/build_preferred_geography_requests.R): `preferred_project_geography_requests.csv`

## Dependency level 16

- [build_preferred_historical_parcels.R](../tasks/new_construction_cleaning/code/build_preferred_historical_parcels.R): `preferred_historical_parcel_coverage.csv`, `preferred_historical_parcels.gpkg`

## Dependency level 17

- [build_preferred_address_geocode_requests.R](../tasks/new_construction_cleaning/code/build_preferred_address_geocode_requests.R): `preferred_address_geocode_requests.csv`

## Dependency level 18

- [read_address_geocodes.R](../tasks/new_construction_cleaning/code/read_address_geocodes.R): `preferred_chicago_address_geocodes.csv`, `preferred_historical_address_geocodes.csv`

## Dependency level 19

- [build_preferred_predecessor_reference_points.R](../tasks/new_construction_cleaning/code/build_preferred_predecessor_reference_points.R): `preferred_predecessor_reference_points.csv`

## Dependency level 20

- [recover_preferred_historical_predecessors.R](../tasks/new_construction_cleaning/code/recover_preferred_historical_predecessors.R): `preferred_historical_predecessor_selected.gpkg`

## Dependency level 21

- [build_preferred_project_geography.R](../tasks/new_construction_cleaning/code/build_preferred_project_geography.R): `preferred_project_year_centroids.gpkg`, `preferred_project_year_geometry.gpkg`, `preferred_project_year_geometry_coverage.csv`

## Dependency level 22

- [build_commercial_city_building_footprints.R](../tasks/new_construction_cleaning/code/build_commercial_city_building_footprints.R): `commercial_city_building_footprints.gpkg`
- [build_preferred_boundary_scope.R](../tasks/new_construction_cleaning/code/build_preferred_boundary_scope.R): `preferred_adjudication_scope.csv`, `preferred_project_boundary_scope.csv`
- [build_residential_successor_condo_requests.R](../tasks/new_construction_cleaning/code/build_residential_successor_condo_requests.R): `residential_successor_condo_requests.csv`
- [build_residential_tieback_episode_resolution.R](../tasks/new_construction_cleaning/code/build_residential_tieback_episode_resolution.R): `residential_tieback_episode_resolution.csv`

## Dependency level 23

- [build_commercial_ground_up_evidence.R](../tasks/new_construction_cleaning/code/build_commercial_ground_up_evidence.R): `commercial_ground_up_evidence.csv`
- [build_residential_class297_resolution.R](../tasks/new_construction_cleaning/code/build_residential_class297_resolution.R): `residential_class297_resolution.csv`

## Dependency level 24

- [build_commercial_address_permit_history.R](../tasks/new_construction_cleaning/code/build_commercial_address_permit_history.R): `commercial_address_permit_history.csv`

## Dependency level 25

- [select_commercial_evidence_rules.R](../tasks/new_construction_cleaning/code/select_commercial_evidence_rules.R): `commercial_post_evidence_resolution.csv`

## Dependency level 26

- [build_preferred_commercial_ledger.R](../tasks/new_construction_cleaning/code/build_preferred_commercial_ledger.R): `preferred_commercial_projects.csv`, `preferred_commercial_source_disposition.csv`

## Dependency level 27

- [build_residential_overlap_resolution.R](../tasks/new_construction_cleaning/code/build_residential_overlap_resolution.R): `residential_overlap_resolution.csv`

## Dependency level 28

- [select_residential_buildings.R](../tasks/new_construction_cleaning/code/select_residential_buildings.R): `residential_selected_buildings.csv`

## Dependency level 29

- [calculate_construction_density.R](../tasks/new_construction_cleaning/code/calculate_construction_density.R): `new_construction_measurements.csv`

## Dependency level 30

- [build_preferred_commercial_final_geography.R](../tasks/new_construction_cleaning/code/build_preferred_commercial_final_geography.R): `preferred_commercial_boundary_scope.csv`, `preferred_commercial_project_centroids.gpkg`, `preferred_commercial_project_component_locations.csv`, `preferred_commercial_project_ledger.csv`
- [build_residential_final_geography.R](../tasks/new_construction_cleaning/code/build_residential_final_geography.R): `preferred_residential_boundary_scope.csv`, `preferred_residential_project_centroids.gpkg`, `preferred_residential_project_components_final.csv`, `preferred_residential_project_ledger.csv`

## Dependency level 31

- [build_preferred_new_construction_ledger.R](../tasks/new_construction_cleaning/code/build_preferred_new_construction_ledger.R): `preferred_new_construction_boundary_scope.csv`, `preferred_new_construction_project_centroids.gpkg`, `preferred_new_construction_project_components.csv`, `preferred_new_construction_project_ledger.csv`

## Dependency level 32

- [build_preferred_construction_zoning.R](../tasks/new_construction_cleaning/code/build_preferred_construction_zoning.R): `preferred_new_construction_zoning.csv`
