# Construction review source snapshots

The paper build reads only `building_permits_full.csv`, `commercial_valuation_data.csv`,
`residential_improvement_characteristics_full.csv`, `parcel_addresses_2025_chicago_20260710.csv`,
`parcel_universe_2025_city_native.csv` and the zoning maps (`zoning_nov2012.zip`, `zoning_sep2014.zip`,
`zoning_jan2016.zip`, `zoning_sep2025.geojson`). The other files support the construction audits. Tasks named
below that are no longer on this branch (such as `download_construction_assessor_history`) remain in the Git
history.

These are preserved source records used to restore the construction-review
pipeline. They are separate from the 2006–2022 sales and permit analysis inputs:
pre-period sales and later permits provide evidence about building histories.
The CSVs are immutable inputs. The recorded source archive is published at
https://github.com/JacobHerbstman/aldermanic_privilege/releases/tag/recorded-sources-2026-09-10.
Run `make -C replication` from the repository root to restore its exact files.

`parcel_sales_city.csv` is the Cook County Assessor Parcel Sales source
(dataset `wvhk-k5uv`), with the field selection and names used by the project's
downloader. The preserved file has 1,369,925 rows, with sale dates from
1971-09-15 through 2026-02-26. It was recovered from the local clean-rerun copy;
the broken-backup copy is byte-identical. Its original acquisition timestamp
has not been independently established. The snapshot retains earlier sales
needed by the recorded project reviews.

`building_permits_full.csv` is the preserved Chicago Building Permits source
(dataset `ydr8-5enu`), recovered from the original download task's
`output/building_permits.csv`. It has 840,301 source records and ISO-formatted
dates. Construction evidence preparation reads this full snapshot; it does
not reuse the narrowed 2006–2022 permit analysis file.

`SHA256SUMS` records the exact recovered bytes. These sources still require
comparison through the full construction pipeline before they can be described
as reproducing the release sample. Matching a source name alone is insufficient.

`density_historical_parcel_records.csv` preserves the Cook County Parcel Universe
(`nj4t-kc8j`) historical PIN-year responses used by the original coordinate
recovery. It was recovered from the local historical-recovery audit output.
`density_historical_address_records.csv` preserves the corresponding historical
Property Locations (`3723-97qp`) responses, recovered from the parcel-address
audit. These are administrative source records with selected/renamed columns,
not accepted-project lists or coordinate decisions. The original acquisition
code is preserved on `research-archive` in the historical-recovery and
project-lineage tasks. Their acquisition timestamps have not been independently
verified. Replication uses these exact bytes; a new API query is a source refresh.

`chicago_building_footprints_2015.zip` is the preserved City of Chicago Building
Footprints shapefile (`syp8-uezg`). The cleaning task reconstructs its project-local
extract from the ZIP and generated project geometries.

`official_building_footprints_2008.gpkg` and
`official_building_footprints_2022.gpkg` are the archived project-local extracts of
Cook County's `buildingFootprint_2008/MapServer/1` and
`buildingFootprint_2022/MapServer/0` services. They are **not citywide raw layers**:
the original downloader queried tiles around the 795 reviewed projects, retained
features intersecting a 100-foot buffer around their search sites, repaired
geometry, and removed empty features. Projects without parcel polygons used
100-foot point buffers before that additional buffer. These fixed source extracts
support the recorded review cohort; changed spatial review scope requires a new
acquisition. The original code is
`research-archive:tasks/audits/new_construction_project_verification/code/download_official_footprint_snapshot.R`.
The acquisition code is preserved in
`tasks/download_construction_footprints/archive/`; its original review cohort is
not the current construction sample.
The recorded extracts are included in the source archive above.

The September 4, 2026 full Assessor acquisition is now pinned here as
`residential_improvement_characteristics_full.csv` (12,320,011 records, assessment
years 1999–2026) and `commercial_valuation_data.csv` (109,409 records). These were
acquired through `tasks/download_construction_assessor_history/` and copied without
alteration. Construction cleaning reads these immutable snapshots; re-running an
acquisition does not replace them automatically.

The commercial source matches the archived source byte-for-byte. The original full
residential source hash (`7f1c6f74b57f8aa9bffd47b43358675bb71d1e7ae28023d7319f7f7b0cd12604`)
was not found in the available local copies. The preserved clean-rerun and
broken-backup sources also produce different cross-sections. The researcher does
not know of an external backup. The current source is therefore a documented new
vintage, not an exact copy of the unlocated original download. Its additional and
changed records require reconciliation before replacing the paper input.

The September 4 historical refresh is separately preserved as
`density_historical_parcel_records_2026-09-04.csv` and
`density_historical_address_records_2026-09-04.csv` (15,455 records each), with
`historical_parcel_requests_2026-09-04.csv` recording every requested PIN. Both
public Cook County endpoints were queried over assessment years 1999–2025 through
`tasks/download_construction_historical_coordinates/`. Returned values agree on
all PIN-year keys shared with the preserved July extracts. The new request scope
is every discovery PIN without current coordinates, including PINs whose response
is empty. The five newly appearing unlocated PINs return no rows from either
endpoint. They remain unresolved, rather than being silently treated as reviewed.
The corrected historical screen reproduces the same 387 accepted coordinate rows
with both source vintages. The original extracts remain available for comparison.

Historical zoning source snapshots are preserved here unchanged. The three ZIP
files were recovered from `tasks/audits/historical_zoning_validation/input/`,
where their file dates are July 20, 2026. Their recorded official Chicago source
IDs are p8va-airx (November 2012), nifi-zqag (September 2014), and xfyf-x4kx
(January 2016). `zoning_sep2025.geojson` is the September 10, 2025 source recovered
from Git blob `3ad08aa86826549033839021edf7c7774fc443b8`, formerly
`data_raw/Boundaries_-_Zoning_Districts_20250910.geojson`. Its old local symlink
was broken after cleanup; Git preserves the original contents. These are source
maps, not manual project zoning assignments. Derived historical project zoning
still needs its producer restored before the new chain is complete.

## Historical parcel source extracts

`preferred_historical_parcel_source.gpkg` and
`preferred_predecessor_parcel_source.gpkg` preserve the administrative features
returned by the July 2026 Cook County historical parcel queries. They were
recovered without changing bytes from the archived construction audit outputs
`preferred_historical_parcels.gpkg` and
`preferred_historical_predecessor_parcels.gpkg`. These files contain source
polygons and source identifiers, not the accepted project-to-parcel matches.
They are scoped administrative extracts, not countywide original downloads or
verbatim HTTP responses. The original acquisition code normalized identifiers
and transformed geometry to EPSG:3435; it rejected invalid geometries.

The accompanying query CSVs contain the distinct annual PIN10s and annual
reference coordinates from the archived query inputs. They include queries
with no returned feature. Cleaning must reject requests outside that coverage,
then recompute exact-PIN matching, point containment, ambiguity, and accepted
geometries from the preserved source polygons. The source service was
https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer;
year-to-layer mapping and request construction are in archived
`download_preferred_historical_parcels.R` and
`recover_preferred_historical_predecessors.R`. These acquisition scripts still
need a standalone source-refresh entry point in the restored archive.

The parcel-universe native CSV preserves the existing 2025 City extract from
`tasks/download_parcel_universe_data/temp/parcel_universe_2025_city_native.csv`.
It was copied without changing bytes on September 6, 2026; the source file had a
July 31 local modification date, which does not independently establish the API
retrieval time. The acquisition task now replays this snapshot for ordinary
builds and keeps a separate Make entry point for an intentional current API
extract. The native-to-legacy column rewrite remains a declared producer.
This snapshot must accompany the data archive; a current API query cannot
guarantee these same bytes.

## Further preserved zoning sources (September 7)

The twelve ordinance PDFs named in `historical_zoning_reviewed_events.csv`, the
`matters_20101101_20260212.csv` export, and `zoning_map_index.geojson` were preserved
from the local `tasks/audits/historical_zoning_validation/input/` sources.
Their exact hashes are in `SHA256SUMS`; they are source records, not the computed
project zoning assignments. Acquisition of these particular frozen inputs still
needs restoration, and they must be included in the replication data bundle.

The map-index source was the City GIS Zoning_update MapServer layer 22 query:
`https://gisapps.chicago.gov/arcgis/rest/services/ExternalApps/Zoning_update/MapServer/22/query`,
with `where=1=1`, `outFields=ZONE_MAP`, `returnGeometry=true`, `outSR=3435`, and
`f=geojson`. The preserved file's local modification date was July 20, 2026;
this is not independently verified as its retrieval date. The ordinance PDFs
were originally supplied by the ELMS PDF-processing task; their matter identifiers
and use are recorded in the zoning decision ledger. The source acquisition chain
must not be described as complete merely because these local bytes survive.

The 2025 Chicago Property Locations extract is also preserved as
`parcel_addresses_2025_chicago_20260710.csv`. Its owning task is
`cook_parcel_addresses_download`; the date reflects the recovered file modification
time, and its exact bytes are fixed by that task's `source_snapshot.sha256`.
