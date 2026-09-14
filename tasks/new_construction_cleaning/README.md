# Recorded construction corrections

`output/recorded_building_changes.csv` contains the reviewed corrections to the
Assessor records. These decisions are committed inputs. This task has no R
scripts: `make` requires the CSV already recorded in Git.

`prepare_new_construction/code/build_construction_data.R` reads this file once,
replaces the specified records and values, then processes all buildings together.
Ordinary Assessor selection, duplicate checks, density and geography remain
programmatic.

Each row identifies a building and one action:

- `update`: replace the populated fields and retain its other measurements.
- `replace`: replace `source_project_ids` with this building. Several rows can
  replace a former combined record with individual buildings.
- `exclude`: remove the specified record from the construction dataset.

Areas are square feet; `dwelling_units` counts homes. A blank replacement field
retains the selected source value. The three `*_unusable` flags explicitly mark
unusable measurements. `allow_far` and `allow_dupac` record the outcome-specific
eligibility decisions. Location and zoning fields identify reviewed sources;
the construction year must agree before those references can be used.

`assessment_rows`, `evidence_ids`, `decision_references` and `decision_reason`
record the evidence. Measurements selected from specific assessment cards are
already resolved in this CSV; a replicator does not repeat the review.

The [original decision tables](../audits/construction_review_history/records/production_decisions_before_consolidation/)
retain their original bytes. A reference such as `construction_modifications.csv:row2`
identifies a row there, counting the header as row 1. The
[review archive](../audits/construction_review_history/README.md) preserves the
research and superseded instructions. This CSV supplies the current building corrections. The source importer separately
records one parcel-reference typo that must be fixed before linking cards.
