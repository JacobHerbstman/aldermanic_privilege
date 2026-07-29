# Permit event study

Runs the 2015 permit event study used in the paper. The main sample retains blocks whose origin- and destination-ward aldermen remained in office when the new map took effect. The regressions use permits grouped by application year, stringency scores estimated through 2014, a 500ft boundary sample, and standard errors clustered by ward pair.

Pre-redistricting permit volume is allowed to have a different coefficient in each year. A single post-redistricting adjustment is included for blocks with no permits before the remap.

Produces:
- `output/permit_event_study_high_discretion_stable_signed_500ft.pdf`
- `output/permit_event_study_high_discretion_stable_separate_500ft.pdf`
- `output/permit_event_study_low_discretion_nosigns_stable_signed_500ft.pdf`
- `output/permit_event_study_high_discretion_all_signed_500ft.pdf`
- `output/permit_event_study_appendix_500ft.tex`

Approx. runtime: ~3-20 minutes.
