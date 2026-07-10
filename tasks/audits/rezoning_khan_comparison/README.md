# rezoning_khan_comparison

## Purpose
Compares the finalized rezoning dataset to the sample reported in Khan's Table 12.
This is an audit target, not a production input.

## Benchmark
- Period: November 2010 through August 2016.
- Khan Table 12 reports `N = 2,096`, mean original FAR `2.344`, and mean new FAR `2.393`.
- The audit also reports PD-transition counts because Khan's treatment of planned developments is not directly observable from the paper.

## Input
- `tasks/build_zoning_dataset/output/zoning_matters_<DATE_TAG>.csv`
- `tasks/zoning_data_cleaning/output/zoning_far_lookup_clean.csv`
- `tasks/rezoning_hand_adjudications/output/pd_far_text_review_updates_2011_2020.csv`
- `tasks/elms_pdf_processing/output/pdf_text_20101101_20160831.csv`
- `tasks/audits/rezoning_khan_comparison/input/khan_journal_2010_code_fills.csv`
- Audit-only source decisions in `tasks/audits/rezoning_khan_comparison/input/`

The journal fill file is a small audit-only manual extraction from the November
3, 2010 City Council Journal. It fills title-only ELMS rows where Councilmatic
and ELMS do not expose ordinance text with the old/new zoning district codes
printed in the journal.

The other audit-only input tables record row inclusions and exclusions, parser
corrections, PD subtype evidence, and mixed-PD allowed-use decisions. Each
manual row carries its source evidence and is kept out of the production data.

## Outputs
- `tasks/audits/rezoning_khan_comparison/output/councilmatic_missing_ordinance_text.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_zoning_matters_journal_codes_<DATE_TAG>.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_zoning_matters_far_<DATE_TAG>.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_comparison_summary_<DATE_TAG>.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_comparison_year_counts_<DATE_TAG>.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_comparison_pd_transitions_<DATE_TAG>.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_comparison_sample_<DATE_TAG>.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_family_counts_<DATE_TAG>.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_far_rule_variants_<DATE_TAG>.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_pd_direction_decomposition_<DATE_TAG>.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_reconciled_far_summary.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_reconciled_sample_corrections.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_reconciled_family_summary.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_pd_preexisting_far_audit.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_pd_transition_corrections_audit.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_reconciled_sample.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_allowed_use_family_variants.csv`
- `tasks/audits/rezoning_khan_comparison/output/khan_mixed_pd_allowed_use_review.csv`

## Current Findings
- Current default run: `DATE_TAG=20101101_20160831`.
- The raw ELMS-final comparison has `2,087` rows. The evidence table adds nine
  property-specific applications with independent passage records, restores one
  PD amendment dropped because it has no conventional from/to sentence, removes
  one citywide zoning-code amendment, and removes `O2010-7170`, which is a
  pedestrian-street designation rather than a zoning-district reclassification.
  The corrected sample has `N = 2,095`, one below Khan.
- The journal fills recover 18 November 2010 title-only rows. They improve code
  and FAR coverage but do not change the sample count.
- Eleven apparent PD-to-PD rows have source-backed non-PD underlying zoning.
  A separate ordinance-text audit repairs 14 PD amendments that the parser left
  as `PD -> missing`, plus one `PD -> RT-4.5` row.
- Khan's rounded Bernoulli means and standard deviations imply exactly 86
  Downtown and 67 PD rows. They allow 783-784 Residential, 802-803 Business,
  and 356-357 Commercial rows; only three combinations sum to `2,096`.
- Khan reports new LAPU for `2,029` of `2,096` rows. The 67 missing values equal
  the implied PD count, strong evidence that his five district indicators are an
  exhaustive recoding of destination allowed use and that only 67 custom PDs
  remain unassigned a standard LAPU.
- A mechanical intermediate-base-district rule does not reproduce the published
  shares. Source-backed review of Residential-Business PDs is required.
- Applying only 25 high-confidence mixed-PD decisions gives Residential `780`,
  Business `807`, Commercial `356`, Downtown `85`, and PD `67` on `N = 2,095`.
  Adding four separately marked medium-confidence decisions gives Residential
  `784`, Business `803`, Commercial `356`, Downtown `85`, and PD `67`. This
  matches one rounded-count-compatible Khan combination except for the unresolved
  one-row sample gap, which would have to be Downtown. It is not proof that the
  four medium-confidence decisions or the missing row are correct.
- The earlier project-FAR variant is not a valid Khan reconstruction. It produces
  FAR values above Khan's support because it sometimes selects total FAR after
  bonuses rather than the ordinance's base FAR.
- Applying only Khan-compatible structural rules currently gives mean original
  FAR `1.997` and mean new FAR `2.334`, versus Khan's `2.344` and `2.393`.
  There are 133 missing original values and 114 missing new values. Eighty-five
  of the latter are `PD -> PD` amendments.
- The paper assigns PDs their pre-existing FAR. A defensible completion therefore
  requires a chronological PD history or an ordinance-supported base FAR. The
  local workspace does not currently contain the 1999-2010 parsed journal output
  needed for a full carry-forward. The gap cannot be resolved by changing the
  date window, using bonus-inclusive project FAR, or tuning values to the target.

## Run
```bash
cd tasks/audits/rezoning_khan_comparison/code
make link-inputs
make
```
