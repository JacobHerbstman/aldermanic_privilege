# Frozen Rezoning Adjudications

This task contains the hand-adjudicated rezoning records used by production tasks.
Its outputs are committed because they cannot be reconstructed mechanically from
the raw sources. The Makefile only verifies that the frozen files are present.

## Outputs

- `output/pd_far_text_review_updates_2011_2020.csv`: reviewed one-sided PD FAR values retained for audit comparisons; the canonical FAR task does not apply them.
- `output/pd_to_pd_far_decisions_20101101_20160831.csv`: ordinance-supported old and new PD FAR pairs for the Khan window.
- `output/pd_to_pd_far_decisions_20160901_20201231.csv`: ordinance-supported old and new PD FAR pairs after the Khan window.
- `output/pd_queue_classifications_20160901_20201231.csv`: manual classifications used to identify post-Khan PD-to-PD matters.
- `output/journal_zoning_code_fills_20101101_20160831.csv`: zoning pairs recovered from 2010 Council journals.
- `output/rezoning_sample_decisions_20101101_20160831.csv`: verified inclusions and false-positive exclusions.
- `output/pd_transition_code_corrections_20101101_20160831.csv`: ordinance-supported PD transition corrections.
- `output/destination_code_corrections_20101101_20160831.csv`: ordinance-supported destination-code corrections.
- `output/destination_code_corrections_20160901_20201231.csv`: post-Khan ordinance-supported destination-code corrections.
- `output/non_scalar_far_decisions_20101101_20201231.csv`: ordinances that cannot be represented by one matter-level FAR pair.

Evidence extraction, unresolved queues, comparisons, and summaries remain in
`tasks/audits/rezoning_pd_ordinance_far_history/` and are not production outputs.
