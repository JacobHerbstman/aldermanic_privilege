# commercial_value_data_cleaning

Purpose: Cleans Cook County commercial valuation rows used as the multifamily new-construction source.

The raw commercial valuation export contains grouped rows where one building can list several component PINs in a single `pins` field. Most grouped rows are legitimate multi-PIN properties and are not edited. The cleaner audits those rows because a small number of grouped records can repeat the same land area once per component PIN, making a dense building look like it sits on an implausibly large lot.

The land correction rule is intentionally narrow. It applies only when the selected multi-PIN row has a lower-land same-building candidate with building square feet within 5 percent, the selected land area is the candidate land area multiplied by the component-PIN count, and the selected FAR/DUPAC are implausibly low. Other multi-PIN rows are left unchanged and listed for review.

Produces:
- `output/multifamily_data_cleaned.csv`: one selected multifamily record per PIN, with corrected `landsf` only for rows that pass the conservative land-audit rule.
- `output/commercial_multi_pin_land_audit.csv`: all selected multi-PIN rows, including same-building land-ratio candidates and correction flags.
- `output/commercial_multi_pin_land_audit_summary.csv`: counts of selected rows, multi-PIN rows, near-multiple candidates, and applied corrections.
- `output/commercial_land_corrections.csv`: the rows whose land square footage was changed.
- `output/commercial_density_review_flags.csv`: selected rows flagged for missing area, missing building square footage, or low-density high-unit patterns that should be reviewed before interpreting density outputs.
- `output/commercial_selected_vs_latest_audit.csv`: rows where the selected record differs from the latest available commercial valuation record for the same PIN, with unit/building/land discrepancy flags.

Approx. runtime: ~1-10 minutes.
