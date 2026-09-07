# Density eligibility decisions

`density_denominator_decisions.csv` records Jacob's September 7, 2026 decision
to exclude three reviewed projects for unresolved land allocation and three
Lenox records for unresolved construction timing. The Lenox parent reports
2008, its two successors report 2019, and the original 2007 permits were
reinstated in 2018. These records remain in the construction ledger while their
completion year and correct single representation are investigated. It is a manual source ledger, not a generated dataset.

The key is `project_id`. `exclude_density` disables both `allow_far` and
`allow_dupac` in `build_final_verified_density_input.R`, before the script
calculates the density outcomes. Records, reported units, building area and
reported land area remain intact. The Makefile declares this ledger as a concrete
input. Duplicate IDs, unknown decisions and IDs absent from the retained export
are errors. The ledger is not a list to delete from source files.

These are case-specific decisions based on documented site evidence. A large
parent-to-successor area ratio alone is not an exclusion rule: shared land can
justify that difference, as in the reviewed 13-home Calumet project. Apply the
same measurement standard to comparable cases regardless of location or their
effect on estimates, recording any further judgment explicitly.

The detailed investigation is in the release audit's `denominator_review.md`.
The evidence references in the ledger identify Assessor vintages, City permit
numbers and public planning documents; production does not execute audit code.
This ledger takes effect in the restored exporter. The frozen paper input is not
replaced until the restored production chain and downstream results are verified.

The original three land-allocation decisions were committed in `79d35a3`.
The three Lenox exclusions were approved separately on September 7; they do not
resolve which construction year is correct or authorize double counting the site.
