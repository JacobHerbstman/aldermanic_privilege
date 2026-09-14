# Prepare new construction

This task turns the recorded Assessor sources into buildings with density and
construction-year ward-boundary distances. It has two scripts, run in order by
its Makefile.

1. **`select_assessor_buildings.R` reads the Assessor sources.** Remove empty
   cards, identify records describing the same property, and choose one complete
   assessment for each building. Individual townhomes retain separate records.
   The recorded permit history helps distinguish construction episodes.
2. **`build_construction_data.R` applies the correction CSV once.** Replace the
   reviewed identities and measurements, then resolve ordinary duplicate and
   completed-building matches. Calculate FAR and DUPAC from reported areas and
   unit counts. Locate buildings on the recorded parcels and assign their
   construction-year ward and nearest boundary.

The correction section is near the beginning of the second script. Subsequent
calculations use the same building columns for corrected and uncorrected rows.
`source_row_ids` records the ordinary source selection; reviewed replacement
cards are identified by `assessment_rows` in the correction input.
No downstream task reads the correction CSV. The
[committed corrections](../new_construction_cleaning/README.md) document every
reviewed replacement and its evidence.

`assessor_buildings.csv`, `assessor_measurement_records.csv` and
`building_permit_evidence.csv` retain the source information needed by the second
script. The final project ledger, parcel membership, centroids and boundary file
feed `new_construction_analysis_data`, which attaches zoning, aldermen and
regression controls. `density_main_results` reads the citywide project ledger for
the descriptive table and the analysis dataset for regressions.

The source downloads are preserved inputs. The default build does not repeat
web searches or refresh their vintages. Parcel polygons establish identity and
location; they do not supply calculated lot areas for density.

The source importer also retains its documented correction to one mistyped
parcel-reference number in the residential history. That correction is applied
once before linking Assessor cards; it changes no measurement. The building
correction section does not repeat it.
