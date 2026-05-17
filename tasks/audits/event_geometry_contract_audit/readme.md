# event_geometry_contract_audit

Purpose: Audits event-study geometry contracts for the permit and sales panels.

The audit checks that saved permit and sales event-study panels use cohort-specific
event geometry consistently: switched rows must use the origin-destination ward
pair, event distances must be present and nonnegative, saved panels must stay
inside the 800m construction window, non-switched controls must retain their
actual treatment destination ward while `event_neighbor_ward` stores the opposite
boundary side, and rows inside 1000ft must have segment assignments from the same
event ward-pair. The sales diagnostics also check that no retained row has a
missing event distance or a pre-redistricting point-origin mismatch.

Produces:
- `output/event_geometry_contract_summary.csv`
- `output/event_geometry_contract_flags.csv`
- `output/event_geometry_contract_memo.md`
