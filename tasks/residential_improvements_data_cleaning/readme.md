# residential_improvements_data_cleaning

Purpose: Cleans the dynamically downloaded Cook County Assessor single- and multi-family improvement characteristics file into a residential new-construction cross-section.

For single-card PINs, the construction record is the latest post-1999 report
available by 2022. PINs first reported later use the latest pre-2026 record,
with a later record used only when no earlier report exists. Multicard PINs
retain the earliest reported post-1999 year pending resolution of their
observation unit.

Produces: `output/residential_cross_section.csv`.

Approx. runtime: ~1-10 minutes.
