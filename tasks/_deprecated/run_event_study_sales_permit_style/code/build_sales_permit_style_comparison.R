source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_sales_permit_style/code")
# Rscript build_sales_permit_style_comparison.R
# =======================================================================================

collect_new_spec <- function(stem) {
  metadata <- read_csv(sprintf("../output/event_study_metadata_%s.csv", stem), show_col_types = FALSE)
  pretrend <- read_csv(sprintf("../output/event_study_pretrend_%s.csv", stem), show_col_types = FALSE)

  metadata %>%
    transmute(
      spec_id,
      source = "permit_style_new",
      panel_mode,
      unit_mode,
      hedonics_spec,
      include_hedonics,
      weighting,
      bandwidth,
      geo_fe_level,
      cluster_level,
      n = analysis_n,
      effective_sample = effective_weight_n
    ) %>%
    mutate(
      pretrend_p_value = pretrend$p_value[[1]]
    )
}

collect_reference_spec <- function(panel_mode, metadata_path, pretrend_path) {
  if (!file.exists(metadata_path) || !file.exists(pretrend_path)) {
    return(NULL)
  }

  metadata <- read_csv(metadata_path, show_col_types = FALSE)
  pretrend <- read_csv(pretrend_path, show_col_types = FALSE)

  tibble(
    spec_id = paste0("reference_current_sales_", panel_mode),
    source = "reference_current_segment_triangular",
    panel_mode = panel_mode,
    unit_mode = "transaction",
    hedonics_spec = ifelse(metadata$include_hedonics[[1]], "with_hedonics", "no_hedonics"),
    include_hedonics = metadata$include_hedonics[[1]],
    weighting = metadata$weighting[[1]],
    bandwidth = metadata$bandwidth[[1]],
    geo_fe_level = metadata$geo_fe_level[[1]],
    cluster_level = metadata$cluster_level[[1]],
    n = metadata$analysis_n[[1]],
    effective_sample = metadata$effective_weight_n[[1]],
    pretrend_p_value = pretrend$p_value[[1]]
  )
}

stems <- c(
  "sales_permit_style_block_year_cohort_2012_main",
  "sales_permit_style_block_year_cohort_2015_main",
  "sales_permit_style_block_year_stacked_announcement_main",
  "sales_permit_style_block_year_stacked_implementation_main",
  "sales_permit_style_transaction_cohort_2012_no_hedonics",
  "sales_permit_style_transaction_cohort_2015_no_hedonics",
  "sales_permit_style_transaction_stacked_announcement_no_hedonics",
  "sales_permit_style_transaction_stacked_implementation_no_hedonics",
  "sales_permit_style_transaction_cohort_2012_with_hedonics",
  "sales_permit_style_transaction_cohort_2015_with_hedonics",
  "sales_permit_style_transaction_stacked_announcement_with_hedonics",
  "sales_permit_style_transaction_stacked_implementation_with_hedonics"
)

comparison <- bind_rows(lapply(stems, collect_new_spec))

reference_rows <- bind_rows(
  collect_reference_spec(
    panel_mode = "cohort_2012",
    metadata_path = "../input/event_study_metadata_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv",
    pretrend_path = "../input/event_study_pretrend_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv"
  ),
  collect_reference_spec(
    panel_mode = "cohort_2015",
    metadata_path = "../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv",
    pretrend_path = "../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv"
  ),
  collect_reference_spec(
    panel_mode = "stacked_announcement",
    metadata_path = "../input/event_study_metadata_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv",
    pretrend_path = "../input/event_study_pretrend_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv"
  ),
  collect_reference_spec(
    panel_mode = "stacked_implementation",
    metadata_path = "../input/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv",
    pretrend_path = "../input/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv"
  )
)

comparison <- bind_rows(comparison, reference_rows) %>%
  arrange(panel_mode, unit_mode, include_hedonics, source)

write_csv(comparison, "../output/sales_permit_style_comparison.csv")
