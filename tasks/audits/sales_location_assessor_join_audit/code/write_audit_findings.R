# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_location_assessor_join_audit/code")

source("../../../setup_environment/code/packages.R")

coordinate_summary <- fread("../output/sales_coordinate_match_summary.csv")
assessor_summary <- fread("../output/sales_assessor_match_summary.csv")
assessor_rd <- fread("../output/sales_rd_match_sensitivity.csv")
historical_summary <- fread("../output/historical_parcel_match_summary.csv")
historical_rd <- fread("../output/historical_sales_rd_sensitivity.csv")

analysis_coordinates <- coordinate_summary[sample == "eligible_sales_2006_2022"]
eligible_sales <- analysis_coordinates[, sum(n)]
exact_current <- analysis_coordinates[location_match_status == "exact_full_pin", sum(n)]
no_current_candidate <- analysis_coordinates[
  location_match_status == "no_pin10_coordinate_candidate",
  sum(n)
]
historical_recovered <- historical_summary[
  measure == "exact_sale_year_coordinate_recovered",
  n
]
historical_geometry <- historical_summary[
  measure == "recovered_with_complete_500ft_rd_geometry",
  n
]
current_assessor_missing <- assessor_summary[
  sample == "located_sales_2006_2022" &
    join_rule == "current_pin_format_current_rollends" &
    gap_category == "no_match",
  n
]
normalized_assessor_same_year <- assessor_summary[
  sample == "located_sales_2006_2022" &
    join_rule == "normalized_pin_latest_prior" &
    gap_category == "same_year",
  n
]
current_rd <- historical_rd[specification == "current_2025_parcel_universe"]
corrected_rd <- historical_rd[specification == "historical_exact_sale_year_coordinates"]
stable_rd <- historical_rd[specification == "historical_coordinates_stable_across_years_only"]

writeLines(c(
  "# Sales location and assessor-join audit",
  "",
  "This audit does not modify production data or paper outputs. It holds the existing 2006-2022, 500-foot sales RD specification fixed and changes only location and assessor matching.",
  "",
  "## Findings",
  "",
  sprintf("1. Of %s otherwise eligible sales, %s match an exact full PIN in the 2025 parcel universe and %s do not.", format(eligible_sales, big.mark = ","), format(exact_current, big.mark = ","), format(no_current_candidate, big.mark = ",")),
  "2. The PIN10 fallback recovers none of the unmatched sales. Its arbitrary first-coordinate rule therefore does not affect the current estimate, but it is unsupported dead code and should be removed.",
  sprintf("3. The historical Assessor endpoint recovers an exact sale-year coordinate for all %s unmatched sales. Of these, %s have complete 500-foot boundary, segment, and score geometry.", format(historical_recovered, big.mark = ","), format(historical_geometry, big.mark = ",")),
  sprintf("4. Numeric parsing strips the leading zero from Chicago PINs beginning with zero. This causes %s otherwise located 2006-2022 sales to miss the current assessor join. After restoring 14-digit PIN strings, all %s located sales have a same-year assessor record.", format(current_assessor_missing, big.mark = ","), format(normalized_assessor_same_year, big.mark = ",")),
  "5. The production hedonic_tax_year field is the sale-year join key, not the matched assessor year. It cannot report rolling-match distance. In the current analysis period, the corrected match is always same-year, so distant rolling matches do not presently affect the RD estimate.",
  "",
  "## RD sensitivity",
  "",
  sprintf("- Current 2025-universe sample: %.4f (SE %.4f), N = %s.", current_rd$estimate, current_rd$std_error, format(current_rd$n, big.mark = ",")),
  sprintf("- Exact PIN-by-sale-year historical coordinates: %.4f (SE %.4f), N = %s; %s recovered observations enter the final complete-case model.", corrected_rd$estimate, corrected_rd$std_error, format(corrected_rd$n, big.mark = ","), format(corrected_rd$n_historical_recovered, big.mark = ",")),
  sprintf("- Restricting recovered PINs to coordinates stable across all observed years: %.4f (SE %.4f), N = %s.", stable_rd$estimate, stable_rd$std_error, format(stable_rd$n, big.mark = ",")),
  "",
  "## Recommended production changes",
  "",
  "1. Read every PIN column as text and validate a 14-digit full PIN before any join.",
  "2. Geocode sales by exact full PIN and sale year from the historical parcel-universe endpoint; do not use a current-universe PIN10 fallback.",
  "3. Join assessor characteristics by normalized full PIN, preserve the actual matched assessor year, and make any allowed year gap explicit. Exact-year records cover the present 2006-2022 analysis sample.",
  "4. Rebuild the sales pipeline and paper only after these production changes are approved."
), "../output/audit_findings.md")
