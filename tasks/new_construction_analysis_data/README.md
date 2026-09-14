# New-construction regression inputs

This task attaches the regressors to the finished building dataset from
`prepare_new_construction`.

1. `attach_construction_regressors.R` retains projects within 1,500 feet of a ward
   boundary and attaches boundary segments, daily alderman terms, the existing
   through-2022 score, and ward-year controls.
2. `build_new_construction_analysis_data.R` assigns construction-year zoning from
   the preserved zoning history and official snapshots, then saves
   `new_construction_analysis_data.csv` for the density tasks. Reviewed zoning
   references arrive as columns on the building records.

The construction-date proxy is June 15 of the recorded completion year. Segment
matching uses the building's ward pair and the existing 1,320-foot segments.
Chicago geometry is in EPSG:3435. Buildings retain their density measures and
multifamily classification from the construction producer.

`density_eligible` identifies the common FAR and DUPAC sample: both measures must
be allowed, positive and finite. A missing alderman score remains missing and
cannot supply a signed boundary distance.

The preserved zoning history defines the zoning inputs for this replication;
reconstruction from the underlying ordinances remains separate work. The
[replication instructions](../../replication/README.md) describe the recorded
source archive. The [review archive](../audits/construction_review_history/README.md)
retains earlier sample comparisons.
