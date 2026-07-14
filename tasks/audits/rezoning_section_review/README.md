# rezoning_section_review

## Purpose
Audits final rezoning matters whose parsed ordinance text names more than one
zoning district on either side of the change. These rows cannot be assigned a
scalar FAR change by selecting the first code in the ordinance.

## Method
The audit resolves every named district against the date-specific FAR lookup.
Multiple district codes are scalar-equivalent only when every code on each side
has the same FAR. Conflicting FARs imply a non-scalar ordinance at the available
matter geography. Ordinance text is reviewed directly for incomplete parses.

## Outputs
- `output/section_review_matters.csv`: one row per final matter with multiple
  parsed district codes, with all codes, FARs, and the audit decision. It
  includes unresolved rows that were already excluded from scalar analysis.
- `output/section_review_summary.csv`: counts by audit decision.

This task is diagnostic. Production consumes only the small frozen decision file
in `tasks/rezoning_hand_adjudications/output/` for incomplete parses.

## Run
```bash
cd tasks/audits/rezoning_section_review/code
make link-inputs
make
```
