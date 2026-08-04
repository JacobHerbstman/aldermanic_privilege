# Permit Event Study

This task estimates the 2015 permit event study used in the paper. The main
sample retains blocks whose origin- and destination-ward aldermen remained in
office when the new map took effect. Permits are grouped by application year,
the alderman scores are estimated using data through 2014, the sample is
limited to 500 feet from a ward boundary, and standard errors are clustered by
ward pair.

The sample requires at least one high-discretion permit during the five years
before redistricting. The regressions include block fixed effects and ward-pair
by year fixed effects.

Produces:
- `output/permit_event_study_high_discretion_stable_signed_500ft.pdf`
- `output/permit_event_study_high_discretion_stable_separate_500ft.pdf`
- `output/permit_event_study_low_discretion_nosigns_stable_signed_500ft.pdf`
- `output/permit_event_study_high_discretion_all_signed_500ft.pdf`
- `output/permit_event_study_appendix_500ft.tex`
