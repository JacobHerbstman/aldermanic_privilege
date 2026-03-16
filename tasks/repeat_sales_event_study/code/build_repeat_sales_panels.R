source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/repeat_sales_event_study/code")
# Rscript build_repeat_sales_panels.R "cohort_2015" "bw1000" "../input/sales_transaction_panel_2012.parquet" "../input/sales_transaction_panel_2015.parquet" "../input/sales_transaction_panel_2022.parquet" "../input/sales_transaction_panel_2023.parquet" "../output/repeat_sales_panel_cohort_2015_bw1000.parquet"
# =======================================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 7) {
  stop(
    "Usage: Rscript build_repeat_sales_panels.R <panel_mode> <approach> <panel_2012> <panel_2015> <panel_2022> <panel_2023> <output_panel>",
    call. = FALSE
  )
}

panel_mode <- args[1]
approach <- args[2]

valid_panel_modes <- c(
  "cohort_2012",
  "cohort_2015",
  "cohort_2022",
  "cohort_2023",
  "stacked_announcement",
  "stacked_implementation"
)
valid_approaches <- c("bw1000", "corridor1320", "citywide_valid")

if (!panel_mode %in% valid_panel_modes) {
  stop(sprintf("panel_mode must be one of: %s", paste(valid_panel_modes, collapse = ", ")), call. = FALSE)
}
if (!approach %in% valid_approaches) {
  stop(sprintf("approach must be one of: %s", paste(valid_approaches, collapse = ", ")), call. = FALSE)
}

input_paths <- c(
  cohort_2012 = args[3],
  cohort_2015 = args[4],
  cohort_2022 = args[5],
  cohort_2023 = args[6]
)
output_panel <- args[7]

event_year_lookup <- c(
  cohort_2012 = 2012L,
  cohort_2015 = 2015L,
  cohort_2022 = 2022L,
  cohort_2023 = 2023L
)

component_lookup <- list(
  cohort_2012 = "cohort_2012",
  cohort_2015 = "cohort_2015",
  cohort_2022 = "cohort_2022",
  cohort_2023 = "cohort_2023",
  stacked_announcement = c("cohort_2012", "cohort_2022"),
  stacked_implementation = c("cohort_2015", "cohort_2023")
)

apply_approach_filter <- function(df, approach_name) {
  if (approach_name == "bw1000") {
    return(df %>% filter(!is.na(segment_id_cohort), segment_id_cohort != "", !is.na(dist_ft), dist_ft <= 1000))
  }
  if (approach_name == "corridor1320") {
    return(df %>% filter(!is.na(segment_id_cohort), segment_id_cohort != ""))
  }
  df
}

collapse_pin_year <- function(df) {
  df %>%
    arrange(pin, sale_year, desc(sale_date), desc(sale_price)) %>%
    distinct(pin, sale_year, .keep_all = TRUE)
}

prepare_repeat_panel <- function(path, cohort_mode, approach_name) {
  event_year <- unname(event_year_lookup[[cohort_mode]])

  message(sprintf("Loading %s for %s", path, cohort_mode))
  df <- read_parquet(path) %>%
    as_tibble() %>%
    mutate(
      pin = as.character(pin),
      block_id = as.character(block_id),
      cohort = as.character(cohort),
      sale_date = as.Date(sale_date),
      sale_year = as.integer(sale_year),
      ward_origin = as.character(ward_origin),
      ward_pair_id = as.character(ward_pair_id),
      ward = as.character(ward),
      segment_id_cohort = na_if(as.character(segment_id_cohort), ""),
      segment_side = na_if(as.character(segment_side), ""),
      cohort_segment = na_if(as.character(cohort_segment), ""),
      cohort_segment_side = na_if(as.character(cohort_segment_side), ""),
      treat = as.integer(treat),
      strictness_change = as.numeric(strictness_change),
      dist_ft = as.numeric(dist_ft),
      sale_price = as.numeric(sale_price)
    ) %>%
    filter(
      !is.na(pin),
      !is.na(sale_date),
      !is.na(sale_year),
      !is.na(sale_price),
      sale_price > 0,
      !is.na(treat),
      !is.na(strictness_change),
      !is.na(ward_origin),
      ward_origin != ""
    ) %>%
    apply_approach_filter(approach_name) %>%
    collapse_pin_year() %>%
    mutate(
      event_year = event_year,
      relative_year = sale_year - event_year,
      relative_year_capped = relative_year,
      panel_mode = cohort_mode,
      approach = approach_name
    )

  eligible_pins <- df %>%
    group_by(pin) %>%
    summarise(
      n_sales = n(),
      has_pre = any(sale_year < first(event_year)),
      has_post = any(sale_year >= first(event_year)),
      .groups = "drop"
    ) %>%
    filter(n_sales >= 2, has_pre, has_post) %>%
    select(pin)

  out <- df %>%
    semi_join(eligible_pins, by = "pin") %>%
    mutate(
      panel_mode = cohort_mode,
      approach = approach_name
    ) %>%
    select(
      pin,
      block_id,
      cohort,
      sale_date,
      sale_year,
      event_year,
      relative_year,
      relative_year_capped,
      sale_price,
      ward,
      ward_pair_id,
      ward_origin,
      segment_id_cohort,
      segment_side,
      cohort_segment,
      cohort_segment_side,
      dist_ft,
      treat,
      strictness_change,
      panel_mode,
      approach
    )

  if (anyDuplicated(paste(out$pin, out$sale_year, sep = "__")) > 0) {
    stop(sprintf("Duplicate pin x sale_year rows remain in %s %s", cohort_mode, approach_name), call. = FALSE)
  }

  message(sprintf(
    "%s %s: %s repeat-sale rows for %s PINs",
    cohort_mode,
    approach_name,
    format(nrow(out), big.mark = ","),
    format(n_distinct(out$pin), big.mark = ",")
  ))

  out
}

build_output_panel <- function(panel_mode_name, approach_name) {
  components <- component_lookup[[panel_mode_name]]

  if (length(components) == 1) {
    return(prepare_repeat_panel(input_paths[[components]], components, approach_name))
  }

  out <- bind_rows(lapply(components, function(component_mode) {
    prepare_repeat_panel(input_paths[[component_mode]], component_mode, approach_name)
  })) %>%
    mutate(
      panel_mode = panel_mode_name,
      cohort_pin_id = paste(cohort, pin, sep = "_"),
      cohort_segment = if_else(!is.na(segment_id_cohort), paste(cohort, segment_id_cohort, sep = "_"), NA_character_),
      cohort_ward_origin = if_else(!is.na(ward_origin), paste(cohort, ward_origin, sep = "_"), NA_character_)
    ) %>%
    select(
      cohort_pin_id,
      cohort_segment,
      cohort_ward_origin,
      everything()
    )

  if (anyDuplicated(paste(out$cohort_pin_id, out$sale_year, sep = "__")) > 0) {
    stop(sprintf("Duplicate cohort_pin_id x sale_year rows remain in %s %s", panel_mode_name, approach_name), call. = FALSE)
  }

  message(sprintf(
    "%s %s: %s repeat-sale rows for %s cohort-specific PINs",
    panel_mode_name,
    approach_name,
    format(nrow(out), big.mark = ","),
    format(n_distinct(out$cohort_pin_id), big.mark = ",")
  ))

  out
}

output_df <- build_output_panel(panel_mode, approach)
write_parquet(output_df, output_panel)
