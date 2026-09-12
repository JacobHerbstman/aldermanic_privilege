# Construction data: execution order

Generated from the current Makefile prerequisites. Scripts at the same level are independent.

## Dependency level 1

- [tasks/construction_assessor_records/code/prepare_construction_records.R](../tasks/construction_assessor_records/code/prepare_construction_records.R): `multifamily_data_cleaned.csv`, `residential_cross_section.csv`, `residential_discovery_cross_section.csv`
- [tasks/construction_candidate_sites/code/build_construction_discovery_geography.R](../tasks/construction_candidate_sites/code/build_construction_discovery_geography.R): `construction_discovery_boundaries.gpkg`, `construction_discovery_ward_maps.gpkg`

## Dependency level 2

- [tasks/construction_historical_coordinates/code/match_historical_parcel_records.R](../tasks/construction_historical_coordinates/code/match_historical_parcel_records.R): `density_historical_building_universe.csv`, `density_historical_coordinate_candidates.csv`, `density_project_lineage.csv`

## Dependency level 3

- [tasks/construction_historical_coordinates/code/select_historical_coordinates.R](../tasks/construction_historical_coordinates/code/select_historical_coordinates.R): `density_historical_coordinates.csv`, `density_parcel_address_selected_history.csv`

## Dependency level 4

- [tasks/construction_assessor_records/code/geocode_residential_data.R](../tasks/construction_assessor_records/code/geocode_residential_data.R): `geocoded_residential_data.gpkg`

## Dependency level 5

- [tasks/construction_candidate_sites/code/build_construction_parcel_distances.R](../tasks/construction_candidate_sites/code/build_construction_parcel_distances.R): `construction_parcel_boundary_distances.csv`

## Dependency level 6

- [tasks/construction_project_candidates/code/build_commercial_project_candidates.R](../tasks/construction_project_candidates/code/build_commercial_project_candidates.R): `commercial_address_family_candidates.csv`, `commercial_entity_component_candidates.csv`, `commercial_entity_version_candidates.csv`, `commercial_family_vintage_summary.csv`, `commercial_production_family_members.csv`, `commercial_project_family_review.csv`
- [tasks/construction_project_candidates/code/build_residential_project_candidates.R](../tasks/construction_project_candidates/code/build_residential_project_candidates.R): `residential_fractional_base_groups.csv`, `residential_multicard_cards.csv`, `residential_project_candidate_inventory.csv`, `residential_tieback_groups_full.csv`, `residential_tieback_members_full.csv`

## Dependency level 7

- [tasks/construction_project_permits/code/match_construction_permits.R](../tasks/construction_project_permits/code/match_construction_permits.R): `building_permits_for_verification.gpkg`, `new_construction_exact_permit_matches.csv`, `new_construction_project_components.csv`
- [tasks/construction_residential_identity/code/build_residential_tieback_temporal_evidence.R](../tasks/construction_residential_identity/code/build_residential_tieback_temporal_evidence.R): `residential_tieback_temporal_lineage_evidence.csv`

## Dependency level 8

- [tasks/construction_historical_sites/code/build_historical_project_geography.R](../tasks/construction_historical_sites/code/build_historical_project_geography.R): `historical_project_year_geometry.gpkg`
- [tasks/construction_residential_identity/code/build_residential_assessor_projects.R](../tasks/construction_residential_identity/code/build_residential_assessor_projects.R): `residential_assessor_project_candidates.csv`

## Dependency level 9

- [tasks/construction_project_permits/code/match_spatial_permits_and_revisions.R](../tasks/construction_project_permits/code/match_spatial_permits_and_revisions.R): `new_construction_spatial_permit_matches.csv`, `project_permit_chain_links.csv`, `project_permit_chain_summary.csv`, `project_permit_chain_unit_mentions.csv`

## Dependency level 10

- [tasks/construction_commercial_measurements/code/build_preferred_commercial_candidates.R](../tasks/construction_commercial_measurements/code/build_preferred_commercial_candidates.R): `commercial_adjudication_queue.csv`, `preferred_commercial_project_candidates.csv`
- [tasks/construction_residential_measurements/code/select_and_correct_residential_measurements.R](../tasks/construction_residential_measurements/code/select_and_correct_residential_measurements.R): `preferred_residential_project_candidates.csv`, `residential_adjudication_queue.csv`

## Dependency level 11

- [tasks/construction_historical_sites/code/select_project_parcels.R](../tasks/construction_historical_sites/code/select_project_parcels.R): `preferred_historical_parcel_coverage.csv`, `preferred_historical_parcels.gpkg`

## Dependency level 12

- [tasks/construction_project_locations/code/match_project_addresses_and_predecessors.R](../tasks/construction_project_locations/code/match_project_addresses_and_predecessors.R): `preferred_chicago_address_geocodes.csv`, `preferred_historical_predecessor_selected.gpkg`

## Dependency level 13

- [tasks/construction_project_locations/code/locate_construction_projects.R](../tasks/construction_project_locations/code/locate_construction_projects.R): `preferred_adjudication_scope.csv`, `preferred_project_boundary_scope.csv`, `preferred_project_year_centroids.gpkg`, `preferred_project_year_geometry.gpkg`

## Dependency level 14

- [tasks/construction_commercial_permits/code/identify_commercial_new_buildings.R](../tasks/construction_commercial_permits/code/identify_commercial_new_buildings.R): `commercial_ground_up_evidence.csv`

## Dependency level 15

- [tasks/construction_commercial_permits/code/build_commercial_address_permit_history.R](../tasks/construction_commercial_permits/code/build_commercial_address_permit_history.R): `commercial_address_permit_history.csv`

## Dependency level 16

- [tasks/construction_commercial_measurements/code/select_commercial_measurements.R](../tasks/construction_commercial_measurements/code/select_commercial_measurements.R): `preferred_commercial_projects.csv`, `preferred_commercial_source_disposition.csv`

## Dependency level 17

- [tasks/construction_residential_measurements/code/select_residential_buildings.R](../tasks/construction_residential_measurements/code/select_residential_buildings.R): `residential_selected_buildings.csv`

## Dependency level 18

- [tasks/construction_density/code/calculate_construction_density.R](../tasks/construction_density/code/calculate_construction_density.R): `new_construction_measurements.csv`

## Dependency level 19

- [tasks/construction_boundary_distances/code/calculate_project_boundary_distances.R](../tasks/construction_boundary_distances/code/calculate_project_boundary_distances.R): `preferred_commercial_boundary_scope.csv`, `preferred_commercial_project_centroids.gpkg`, `preferred_commercial_project_component_locations.csv`, `preferred_commercial_project_ledger.csv`, `preferred_residential_boundary_scope.csv`, `preferred_residential_project_centroids.gpkg`, `preferred_residential_project_components_final.csv`, `preferred_residential_project_ledger.csv`

## Dependency level 20

- [tasks/construction_boundary_distances/code/assemble_construction_data.R](../tasks/construction_boundary_distances/code/assemble_construction_data.R): `preferred_new_construction_boundary_scope.csv`, `preferred_new_construction_project_centroids.gpkg`, `preferred_new_construction_project_components.csv`, `preferred_new_construction_project_ledger.csv`, `preferred_new_construction_zoning.csv`
