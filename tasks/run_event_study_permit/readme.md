# Permit Event Study

This task estimates the 2015 permit event study used in the paper. The main
sample retains blocks whose origin- and destination-ward aldermen remained in
office when the new map took effect. Permits are grouped by application year,
the alderman scores are estimated using data through 2014, the sample is
limited to 500 feet from a ward boundary, and standard errors are clustered by
ward pair.

Pre-redistricting permit volume is allowed to have a different coefficient in
each year. A single post-redistricting adjustment is included for blocks with
no permits before the remap.

Produces:
- `output/permit_event_study_high_discretion_stable_signed_500ft.pdf`
- `output/permit_event_study_high_discretion_stable_separate_500ft.pdf`
- `output/permit_event_study_low_discretion_nosigns_stable_signed_500ft.pdf`
- `output/permit_event_study_high_discretion_all_signed_500ft.pdf`
- `output/permit_event_study_appendix_500ft.tex`
