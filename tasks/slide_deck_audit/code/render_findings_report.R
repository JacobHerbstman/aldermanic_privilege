source("../../setup_environment/code/packages.R")

## Render slide audit findings report
## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/slide_deck_audit/code")
# Rscript render_findings_report.R ../output/slides_audit_findings.csv ../output/slide_claim_status.csv ../output/object_verification_index.csv ../output/rerun_reproducibility_log.csv ../output/geometry_geocoding_checks.csv ../output/slides_audit_findings.md

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 6) {
  findings_input <- args[1]
  claim_status_input <- args[2]
  verification_index_input <- args[3]
  rerun_log_input <- args[4]
  geometry_checks_input <- args[5]
  report_output <- args[6]
} else {
  stop(
    "FATAL: Script requires 6 args: <findings_input> <claim_status_input> <verification_index_input> <rerun_log_input> <geometry_checks_input> <report_output>",
    call. = FALSE
  )
}

findings <- read_csv(findings_input, show_col_types = FALSE)
claim_status <- read_csv(claim_status_input, show_col_types = FALSE)
verification_index <- read_csv(verification_index_input, show_col_types = FALSE)
rerun_log <- read_csv(rerun_log_input, show_col_types = FALSE)
geometry_checks <- read_csv(geometry_checks_input, show_col_types = FALSE)

severity_levels <- c("P0", "P1", "P2", "P3")

inspectability_summary <- verification_index %>%
  mutate(
    subsystem = case_when(
      str_detect(section_file, "aldermen_FE|appendix_summary_stats|appendix_alderman_fe") ~ "Strictness and permits",
      str_detect(section_file, "density_analysis|appendix_density") ~ "Density and parcel geography",
      str_detect(section_file, "rental_market|appendix_rents") ~ "Rentals and sales",
      TRUE ~ "Manual dates and context claims"
    ),
    has_producer = producer_path != "",
    has_inspection_path = reviewer_inspection_path != ""
  ) %>%
  group_by(subsystem) %>%
  summarise(
    n_objects = n(),
    producer_coverage = mean(has_producer, na.rm = TRUE),
    inspection_coverage = mean(has_inspection_path, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    claim_status %>%
      mutate(
        subsystem = case_when(
          str_detect(section_file, "aldermen_FE|appendix_summary_stats|appendix_alderman_fe") ~ "Strictness and permits",
          str_detect(section_file, "density_analysis|appendix_density") ~ "Density and parcel geography",
          str_detect(section_file, "rental_market|appendix_rents") ~ "Rentals and sales",
          TRUE ~ "Manual dates and context claims"
        )
      ) %>%
      group_by(subsystem) %>%
      summarise(
        n_claims = n(),
        verified_share = mean(verification_status == "verified", na.rm = TRUE),
        .groups = "drop"
      ),
    by = "subsystem"
  )

summary_lines <- c(
  "# Slide Audit Findings",
  "",
  sprintf("- Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("- Claims audited: %d", nrow(claim_status)),
  sprintf("- Findings: %d", nrow(findings)),
  sprintf("- Task outputs rerun: %d", nrow(rerun_log)),
  sprintf("- Geometry checks: %d", nrow(geometry_checks)),
  ""
)

summary_lines <- c(summary_lines, "## Severity Counts", "")
for (sev in severity_levels) {
  summary_lines <- c(summary_lines, sprintf("- `%s`: %d", sev, sum(findings$severity == sev, na.rm = TRUE)))
}

summary_lines <- c(summary_lines, "", "## Inspectability Summary", "")
for (i in seq_len(nrow(inspectability_summary))) {
  row <- inspectability_summary[i, ]
  summary_lines <- c(
    summary_lines,
    sprintf(
      "- `%s`: %d objects, producer coverage %.0f%%, inspection-path coverage %.0f%%, verified claims %.0f%%",
      row$subsystem,
      row$n_objects,
      100 * row$producer_coverage,
      100 * row$inspection_coverage,
      100 * row$verified_share
    )
  )
}

detail_lines <- c("", "## Findings", "")

if (nrow(findings) == 0) {
  detail_lines <- c(detail_lines, "No findings were recorded.")
} else {
  for (sev in severity_levels) {
    sev_rows <- findings %>% filter(severity == sev)
    if (nrow(sev_rows) == 0) {
      next
    }

    detail_lines <- c(detail_lines, sprintf("### %s", sev), "")

    for (i in seq_len(nrow(sev_rows))) {
      row <- sev_rows[i, ]
      row_lines <- c(
        sprintf("- `%s` [%s / %s] %s", row$claim_id, row$subsystem, row$affects_scope, row$display_object),
        sprintf("  Issue: %s", row$why_matters),
        sprintf("  Slide: %s %s", row$section_file, ifelse(row$frame_label == "", "", paste0("(", row$frame_label, ")"))),
        sprintf("  Evidence: %s", row$evidence_path),
        sprintf("  Review path: %s", row$reviewer_inspection_path),
        sprintf("  Fix target: %s", row$recommended_fix_target)
      )
      if (!is.na(row$observed_value) && row$observed_value != "") {
        row_lines <- c(row_lines, sprintf("  Observed: %s", row$observed_value))
      }
      detail_lines <- c(detail_lines, row_lines, "")
    }
  }
}

writeLines(c(summary_lines, detail_lines), report_output)

message("Saved: ", report_output)
