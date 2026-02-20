source("../../setup_environment/code/packages.R")
library(optparse)

parser <- OptionParser()
parser <- add_option(parser, c("-b", "--bw_ft"), type = "integer", default = 250)
parser <- add_option(parser, c("-u", "--units_cap"), type = "integer", default = 100)
parser <- add_option(parser, c("-f", "--fe_spec"), type = "character", default = "pair_x_year")
parser <- add_option(parser, c("-o", "--output"), type = "character", default = "")

args <- parse_args(parser, positional_arguments = TRUE)
opts <- args$options
yvars <- args$args

if (!is.finite(opts$bw_ft) || opts$bw_ft <= 0) {
  stop("bw_ft must be positive.")
}
if (length(yvars) == 0) {
  yvars <- c("log(density_far)", "log(density_dupac)", "log(unitscount)")
}

base_name <- function(x) gsub("^log\\(|\\)$", "", x)
is_log_spec <- function(x) str_detect(x, "^log\\(.+\\)$")

pretty_label <- function(x) {
  labels <- c(
    density_far = "FAR",
    density_dupac = "DUPAC",
    unitscount = "Units"
  )
  b <- base_name(x)
  core <- ifelse(b %in% names(labels), labels[[b]], b)
  if (is_log_spec(x)) paste0("ln(", core, ")") else core
}

mean_y_level <- function(model) {
  dat <- model$custom_data
  lhs <- deparse(model$fml[[2]])
  level_var <- if (is_log_spec(lhs)) base_name(lhs) else lhs
  sprintf("%.2f", mean(dat[[level_var]], na.rm = TRUE))
}
fitstat_register("myo", mean_y_level, alias = "Dep. Var. Mean")

n_ward_pairs <- function(model) {
  dat <- model$custom_data
  length(unique(dat$ward_pair))
}
fitstat_register("nwp", n_ward_pairs, alias = "Ward Pairs")

fe_formulas <- list(
  zone_x_pair_year = "zone_code^ward_pair + construction_year",
  zone_pair_x_year = "zone_code + ward_pair^construction_year",
  triple = "zone_code^ward_pair^construction_year",
  pair_year_only = "ward_pair + construction_year",
  pair_x_year = "ward_pair^construction_year"
)

fe_labels <- list(
  zone_x_pair_year = list(
    "Zoning Code $\\times$ Ward-Pair FE" = "zone_code\\^ward_pair",
    "Year FE" = "construction_year"
  ),
  zone_pair_x_year = list(
    "Zoning Code FE" = "zone_code",
    "Ward-Pair $\\times$ Year FE" = "ward_pair\\^construction_year"
  ),
  triple = list(
    "Zoning $\\times$ Ward-Pair $\\times$ Year FE" = "zone_code\\^ward_pair\\^construction_year"
  ),
  pair_year_only = list(
    "Ward-Pair FE" = "ward_pair",
    "Year FE" = "construction_year"
  ),
  pair_x_year = list(
    "Ward-Pair $\\times$ Year FE" = "ward_pair\\^construction_year"
  )
)

if (!opts$fe_spec %in% names(fe_formulas)) {
  stop("Invalid fe_spec: ", opts$fe_spec)
}

output_file <- opts$output
if (output_file == "") {
  output_file <- paste0("../output/fe_table_bw", opts$bw_ft, "_", opts$fe_spec, ".tex")
}

parcels <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  mutate(strictness_own = strictness_own / sd(strictness_own, na.rm = TRUE)) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    unitscount > 1,
    construction_year >= 1999
  )

if (is.finite(opts$units_cap) && opts$units_cap > 0) {
  parcels <- parcels %>% filter(unitscount <= opts$units_cap)
}

models <- list()
model_meta <- tibble()

for (yv in yvars) {
  b <- base_name(yv)
  if (!b %in% names(parcels)) {
    next
  }

  run_data <- parcels %>% filter(dist_to_boundary <= opts$bw_ft)
  if (is_log_spec(yv)) {
    run_data <- run_data %>% filter(.data[[b]] > 0)
  }
  if (nrow(run_data) == 0) {
    next
  }

  fml <- as.formula(
    paste0(
      yv,
      " ~ strictness_own + share_white_own + share_black_own + median_hh_income_own + ",
      "share_bach_plus_own + homeownership_rate_own + avg_rent_own | ",
      fe_formulas[[opts$fe_spec]]
    )
  )

  model <- feols(fml, data = run_data, cluster = ~ward_pair)
  model$custom_data <- run_data

  models[[length(models) + 1]] <- model
  names(models)[length(models)] <- pretty_label(yv)

  coef_table <- coeftable(model)
  has_coef <- "strictness_own" %in% rownames(coef_table)

  model_meta <- bind_rows(
    model_meta,
    tibble(
      output_file = basename(output_file),
      bw_ft = opts$bw_ft,
      units_cap = opts$units_cap,
      fe_spec = opts$fe_spec,
      yvar = yv,
      n_obs = nobs(model),
      n_ward_pairs = length(unique(run_data$ward_pair)),
      coef_strictness = if (has_coef) coef_table["strictness_own", "Estimate"] else NA_real_,
      se_strictness = if (has_coef) coef_table["strictness_own", "Std. Error"] else NA_real_,
      p_strictness = if (has_coef) coef_table["strictness_own", "Pr(>|t|)"] else NA_real_,
      coef_negative = if (has_coef) as.numeric(coef_table["strictness_own", "Estimate"] < 0) else NA_real_,
      run_timestamp = as.character(Sys.time())
    )
  )
}

if (length(models) == 0) {
  stop("No models estimated.")
}

rename_dict <- c(
  strictness_own = "Uncertainty Index",
  zone_code = "Zoning Code FE",
  construction_year = "Year FE",
  ward_pair = "Ward-Pair FE",
  "zone_code^ward_pair" = "Zoning Code $\\times$ Ward-Pair FE",
  "ward_pair^construction_year" = "Ward-Pair $\\times$ Year FE",
  "zone_code^ward_pair^construction_year" = "Zoning $\\times$ Ward-Pair $\\times$ Year FE"
)

etable(
  models,
  keep = "Uncertainty Index",
  fitstat = ~ n + myo + nwp,
  style.tex = style.tex(
    "aer",
    model.format = "",
    fixef.title = "",
    fixef.suffix = "",
    yesNo = c("$\\checkmark$", "")
  ),
  depvar = FALSE,
  digits = 2,
  headers = names(models),
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  dict = rename_dict,
  fixef.group = fe_labels[[opts$fe_spec]],
  float = FALSE,
  tex = TRUE,
  file = output_file,
  replace = TRUE
)

qc_path <- "../output/fe_run_qc.csv"
existing_qc <- if (file.exists(qc_path)) {
  read_csv(qc_path, show_col_types = FALSE)
} else {
  tibble()
}

output_file_base <- basename(output_file)
yvars_in_run <- unique(model_meta$yvar)

if (nrow(existing_qc) > 0 && "run_timestamp" %in% names(existing_qc)) {
  existing_qc <- existing_qc %>% mutate(run_timestamp = as.character(run_timestamp))
}

if (nrow(existing_qc) > 0) {
  existing_qc <- existing_qc %>%
    filter(!(output_file == output_file_base & yvar %in% yvars_in_run))
}

updated_qc <- bind_rows(existing_qc, model_meta) %>%
  arrange(output_file, yvar)
write_csv(updated_qc, qc_path)

negative_count <- sum(updated_qc$coef_negative == 1, na.rm = TRUE)
coef_count <- sum(!is.na(updated_qc$coef_negative))

txt_lines <- c(
  "border pair fe qc summary",
  paste0("rows_in_qc: ", nrow(updated_qc)),
  paste0("strictness_negative_count: ", negative_count),
  paste0("strictness_coef_count: ", coef_count),
  paste0("strictness_negative_share: ", round(negative_count / max(coef_count, 1), 4)),
  paste0("latest_output_file: ", basename(output_file)),
  paste0("latest_bw_ft: ", opts$bw_ft),
  paste0("latest_units_cap: ", opts$units_cap),
  paste0("latest_fe_spec: ", opts$fe_spec),
  "latest_rows:",
  paste0(
    "  ",
    model_meta$yvar,
    " | n=", model_meta$n_obs,
    " | coef=", round(model_meta$coef_strictness, 4),
    " | p=", round(model_meta$p_strictness, 4)
  )
)
writeLines(txt_lines, "../output/fe_run_qc.txt")

message("Wrote ", output_file)
message("Wrote ../output/fe_run_qc.csv")
message("Wrote ../output/fe_run_qc.txt")
