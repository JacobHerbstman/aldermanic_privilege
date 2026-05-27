# commercial_value_data_cleaning

Purpose: Cleans Cook County commercial valuation rows used as the multifamily new-construction source.

The Cook County field `tot_units` is a broad source field; the source metadata says it can include hotel rooms, nursing-home rooms, residential apartments, commercial units, and related units. For density construction, this task treats the apartment-count-by-bedroom-type columns as the preferred residential apartment count when that detail is available, and uses the source `tot_units` only as a fallback when apartment-unit detail is unavailable. The original source field is retained as `source_tot_units`, with disagreement flags for review.

Two rows currently receive manual unit overrides after public/permit review. The 2439/2443 N. Western project is set to 34 residential units because the source bedroom-count detail reports only 30 two-bedroom units while permit/public records describe 34 dwelling units. The 525 S. State / University Center row is set to 524 units because the valuation unit fields appear to count student beds rather than dwelling units. These overrides are retained in the cleaned output with `unit_source == "manual_override"` and documented reason text.

The raw commercial valuation export contains grouped rows where one building can list several component PINs in a single `pins` field. Most grouped rows are legitimate multi-PIN properties and are not edited. The cleaner checks those rows because a small number of grouped records can repeat the same land area once per component PIN, making a dense building look like it sits on an implausibly large lot.

The land correction rule is intentionally narrow. It applies only when the selected multi-PIN row has a lower-land same-building candidate with building square feet within 5 percent, the selected land area is the candidate land area multiplied by the component-PIN count, and the selected FAR/DUPAC are implausibly low. Other multi-PIN rows are left unchanged.

Produces:
- `output/multifamily_data_cleaned.csv`: one selected multifamily record per PIN, with corrected `landsf` only for rows that pass the conservative land-correction rule.

Approx. runtime: ~1-10 minutes.
