# commercial_value_data_cleaning

Purpose: Cleans Cook County commercial valuation rows used as the multifamily new-construction source.

The Cook County field `tot_units` is a broad source field; the source metadata says it can include hotel rooms, nursing-home rooms, residential apartments, commercial units, and related units. For density construction, this task treats the apartment-count-by-bedroom-type columns as the preferred residential apartment count when that detail is available, and uses the source `tot_units` only as a fallback when apartment-unit detail is unavailable. The original source field is retained as `source_tot_units`, with disagreement flags for review.

Two rows currently receive manual unit overrides after public/permit review. The 2439/2443 N. Western project is set to 34 residential units because the source bedroom-count detail reports only 30 two-bedroom units while permit/public records describe 34 dwelling units. The 525 S. State / University Center row is set to 524 units because the valuation unit fields appear to count student beds rather than dwelling units. These overrides are retained in the cleaned output with `unit_source == "manual_override"` and documented reason text.

The raw commercial valuation export contains grouped rows where one building can list several component PINs in a single `pins` field. Most grouped rows are legitimate multi-PIN properties and are not edited. The cleaner audits those rows because a small number of grouped records can repeat the same land area once per component PIN, making a dense building look like it sits on an implausibly large lot.

The land correction rule is intentionally narrow. It applies only when the selected multi-PIN row has a lower-land same-building candidate with building square feet within 5 percent, the selected land area is the candidate land area multiplied by the component-PIN count, and the selected FAR/DUPAC are implausibly low. Other multi-PIN rows are left unchanged and listed for review.

Produces:
- `output/multifamily_data_cleaned.csv`: one selected multifamily record per PIN, with corrected `landsf` only for rows that pass the conservative land-audit rule.
- `output/commercial_multi_pin_land_audit.csv`: all selected multi-PIN rows, including same-building land-ratio candidates and correction flags.
- `output/commercial_multi_pin_land_audit_summary.csv`: counts of selected rows, multi-PIN rows, near-multiple candidates, and applied corrections.
- `output/commercial_land_corrections.csv`: the rows whose land square footage was changed.
- `output/commercial_density_review_flags.csv`: selected rows flagged for missing area, missing building square footage, or low-density high-unit patterns that should be reviewed before interpreting density outputs.
- `output/commercial_selected_vs_latest_audit.csv`: rows where the selected record differs from the latest available commercial valuation record for the same PIN, with unit/building/land discrepancy flags.
- `output/commercial_unit_definition_audit.csv`: rows where the residential apartment count relies on the source `tot_units` fallback, disagrees with the apartment-unit sum, or has reported commercial square footage.
- `output/commercial_questionable_apartment_audit.csv`: one-row-per-building review of the selected student-housing and mixed-use apartment rows where the unit definition is most likely to be confusing.

Approx. runtime: ~1-10 minutes.
