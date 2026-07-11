# Rezoning FAR Assignment

Assigns date-appropriate FAR values to the Nov. 2010-2020 rezoning matters. Base
district FARs come from the zoning lookup. Accepted PD-to-PD values come only from
the committed ordinance-supported adjudications.

## Inputs

- `tasks/build_zoning_dataset/output/zoning_matters_20101101_20201231.csv`
- `tasks/zoning_data_cleaning/output/zoning_far_lookup_clean.csv`
- `tasks/rezoning_hand_adjudications/output/destination_code_corrections_20101101_20160831.csv`
- `tasks/rezoning_hand_adjudications/output/destination_code_corrections_20160901_20201231.csv`
- `tasks/rezoning_hand_adjudications/output/non_scalar_far_decisions_20101101_20201231.csv`
- `tasks/rezoning_hand_adjudications/output/pd_to_pd_far_decisions_20101101_20160831.csv`
- `tasks/rezoning_hand_adjudications/output/pd_to_pd_far_decisions_20160901_20201231.csv`

## Output

- `tasks/rezoning_far_pre_geocode/output/zoning_matters_far_20101101_20201231.csv`

One-sided PD transitions remain unresolved. The production task does not apply
Khan's underlying-zone or no-change assumptions. Review queues and summary checks
belong in `tasks/audits/`. `far_transition_status` distinguishes scalar
candidates, matters requiring section review, and reviewed non-scalar ordinances.
