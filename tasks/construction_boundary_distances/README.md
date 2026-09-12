# Construction data and boundary distances

Attach construction-year boundary distances and zoning to the selected buildings. Check that geography does not change the saved construction years, dwelling units, floor areas, or land areas.

Run `make` from `tasks/construction_boundary_distances/code/`. Its Makefile lists each input and its producer.

- [calculate_project_boundary_distances.R](code/calculate_project_boundary_distances.R): `preferred_residential_boundary_scope.csv`, `preferred_residential_project_ledger.csv`, `preferred_residential_project_components_final.csv`, `residential_adjudicated_project_geometry.gpkg`, `preferred_residential_project_centroids.gpkg`, `preferred_commercial_project_ledger.csv`, `preferred_commercial_project_component_locations.csv`, `preferred_commercial_project_centroids.gpkg`, `preferred_commercial_boundary_scope.csv`.
- [assemble_construction_data.R](code/assemble_construction_data.R): `preferred_new_construction_project_ledger.csv`, `preferred_new_construction_project_components.csv`, `preferred_new_construction_boundary_scope.csv`, `preferred_new_construction_project_centroids.gpkg`, `preferred_new_construction_zoning.csv`.

Reviewed decisions come from [new_construction_cleaning](../new_construction_cleaning/README.md).
Data reports are written when the datasets are saved. They are not Make targets.

## Read the construction workflow in this order

1. [Prepare Assessor records](../construction_assessor_records/README.md). Remove empty cards before choosing measurements. [Historical coordinates](../construction_historical_coordinates/README.md) and [candidate sites](../construction_candidate_sites/README.md) locate the source records.
2. [Identify candidate buildings](../construction_project_candidates/README.md) and [residential identities](../construction_residential_identity/README.md). Keep individual townhomes separate. [Permit matches](../construction_project_permits/README.md) and [historical parcels](../construction_historical_sites/README.md) supply evidence about the building represented by a record.
3. Choose one complete assessment in the [residential](../construction_residential_measurements/README.md) and [commercial](../construction_commercial_measurements/README.md) measurement tasks. [Commercial permit evidence](../construction_commercial_permits/README.md) supports the existing new-building checks.
4. Apply the [committed decisions](../new_construction_cleaning/README.md) in those measurement producers. Decisions needed to identify a construction episode are applied in step 2 and carried forward; they are not reapplied here.
5. [Calculate FAR and DUPAC](../construction_density/README.md) from source-reported measurements. Missing or withheld measurements remain missing for the affected measure.
6. [Locate the selected projects](../construction_project_locations/README.md), then attach construction-year ward boundaries and zoning in this task. The final assembly checks the physical measurements against the density task before saving the combined dataset.

A project is a newly constructed residential building or a coherently measured group of buildings. Wholly new replacement buildings qualify; renovations, additions, conversions, and parcel-number changes do not themselves establish new construction. Units, floor area, land area, and year must describe the same object.

Source selection retains the existing 2022, then 2025, then later assessment priority. All cards used for one building must occur together in one assessment. Automatic successor matching retains the approved two-year window and its existing unit, area, and location checks. Historical polygons support identity and location; they do not supply density denominators.

The [script progression](../../task_graph/construction_steps.md) follows the literal file dependencies, including preliminary locations needed for identity matches. The final files feed [analysis preparation](../new_construction_analysis_data/README.md). Run `make` in `paper/` to rebuild the downstream analyses and PDFs.
