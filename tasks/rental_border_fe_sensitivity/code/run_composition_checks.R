source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_fe_sensitivity/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 500
# window <- "pre_2021"
# sample_filter <- "all"
# output_tex <- "../output/composition_checks_pre_2021_all_bw500.tex"
# output_csv <- "../output/composition_checks_pre_2021_all_bw500.csv"
# Rscript run_composition_checks.R "../input/rent_with_ward_distances.parquet" 500 "pre_2021" "all" "../output/composition_checks_pre_2021_all_bw500.tex" "../output/composition_checks_pre_2021_all_bw500.csv"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 6) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  output_tex <- cli_args[5]
  output_csv <- cli_args[6]
} else {
  if (!exists("input") || !exists("bw_ft") || !exists("window") || !exists("sample_filter") || !exists("output_tex") || !exists("output_csv")) {
    stop("FATAL: Script requires 6 args: <input> <bw_ft> <window> <sample_filter> <output_tex> <output_csv>", call. = FALSE)
  }
}

if (!window %in% c("full", "pre_covid", "pre_2021", "pre_2023", "drop_mid")) {
  stop("--window must be one of: full, pre_covid, pre_2021, pre_2023, drop_mid", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily_only")) {
  stop("--sample_filter must be one of: all, multifamily_only", call. = FALSE)
}

apply_window <- function(df, window_name) {
  if (window_name == "full") return(df)
  if (window_name == "pre_covid") return(df %>% filter(year <= 2019))
  if (window_name == "pre_2021") return(df %>% filter(year <= 2020))
  if (window_name == "pre_2023") return(df %>% filter(year <= 2022))
  if (window_name == "drop_mid") return(df %>% filter(year <= 2020 | year >= 2024))
  df
}

dat <- read_parquet(input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id)
  ) %>%
  filter(
    !is.na(file_date),
    !is.na(ward_pair),
    !is.na(signed_dist),
    abs(signed_dist) <= bw_ft,
    !is.na(strictness_own)
  ) %>%
  apply_window(window)

if (sample_filter == "multifamily_only") {
  dat <- dat %>% filter(building_type_clean == "multi_family")
}

sd_strict <- sd(dat$strictness_own, na.rm = TRUE)
if (!is.finite(sd_strict) || sd_strict <= 0) {
  stop("strictness_own has invalid SD in filtered sample.", call. = FALSE)
}

dat <- dat %>%
  mutate(
    strictness_std = strictness_own / sd_strict,
    log_sqft = if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(!is.na(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(!is.na(baths) & baths > 0, log(baths), NA_real_),
    is_multifamily = as.numeric(building_type_clean == "multi_family")
  )

specs <- tibble::tribble(
  ~outcome, ~label,
  "log_sqft", "Log Sqft",
  "log_beds", "Log Beds",
  "log_baths", "Log Baths",
  "is_multifamily", "Multifamily Indicator"
)

mean_y_level <- function(x) {
  y_name <- as.character(x$fml[[2]])
  sprintf("%.3f", mean(x$custom_data[[y_name]], na.rm = TRUE))
}
fitstat_register("myo", mean_y_level, alias = "Dep. Var. Mean")

n_ward_pairs <- function(x) {
  length(unique(stats::na.omit(x$custom_data$ward_pair)))
}
fitstat_register("nwp", n_ward_pairs, alias = "Ward Pairs")

models <- list()
rows <- list()

for (i in seq_len(nrow(specs))) {
  yv <- specs$outcome[i]
  df_i <- dat %>% filter(!is.na(.data[[yv]]))
  if (nrow(df_i) == 0) next

  m <- feols(
    as.formula(paste0(yv, " ~ strictness_std | ward_pair^year_month")),
    data = df_i,
    cluster = ~ward_pair
  )
  m$custom_data <- df_i
  models[[specs$label[i]]] <- m

  rows[[length(rows) + 1]] <- tibble(
    outcome = specs$label[i],
    estimate = coef(m)[["strictness_std"]],
    std_error = se(m)[["strictness_std"]],
    p_value = pvalue(m)[["strictness_std"]],
    n_obs = m$nobs,
    dep_var_mean = mean(df_i[[yv]], na.rm = TRUE),
    ward_pairs = length(unique(df_i$ward_pair)),
    bw_ft = bw_ft,
    window = window,
    sample_filter = sample_filter
  )
}

if (length(models) == 0) {
  stop("No composition models estimated.", call. = FALSE)
}

write_csv(bind_rows(rows), output_csv)

setFixest_dict(c(
  strictness_std = "Strictness Score"
))

etable(
  models,
  keep = "Strictness Score",
  fitstat = ~ n + myo + nwp,
  style.tex = style.tex("aer",
    model.format = "",
    fixef.title = "",
    fixef.suffix = "",
    yesNo = c("$\\checkmark$", "")
  ),
  depvar = FALSE,
  drop.section = "fixef",
  digits = 3,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  extralines = list(
    "_Ward-Pair $\\times$ Year-Month FE" = rep("$\\checkmark$", length(models)),
    "_Window" = rep(window, length(models)),
    "_Sample" = rep(sample_filter, length(models))
  ),
  file = output_tex,
  replace = TRUE
)

message(sprintf("Saved: %s", output_tex))
message(sprintf("Saved: %s", output_csv))