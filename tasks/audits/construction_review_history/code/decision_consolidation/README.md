# September 12 decision consolidation

These scripts record the one-time migration from the decision tables preserved
in `../../records/production_decisions_before_consolidation/`. They are historical
research code, not production dependencies. Their original paths refer to the
migration checkout and temporary comparison copies.

Only explicitly reviewed card selections and instructions to use source-reported
land were resolved to replacement numbers. Unreviewed Assessor measurements were
not copied into the correction input. `inactive_decisions.csv` records older
instructions that the checkpoint pipeline no longer applied. The checkpoint is
Git commit `bb91ea84`; the new production input is
`tasks/new_construction_cleaning/output/recorded_building_changes.csv`.
