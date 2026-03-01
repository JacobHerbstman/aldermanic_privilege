source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_border_pair_fe/code")
# input <- "../input/sales_with_hedonics.parquet"
# bw_ft <- 1000
# fe_time <- "year_quarter"
# output_tex <- "../output/fe_table_sales_bw1000.tex"
# output_csv <- "../output/fe_table_sales_bw1000.csv"
# output_year_diag <- "../output/year_diagnostics_sales_bw1000.csv"
# Rscript sales_border_pair_fe.R "../input/sales_with_hedonics.parquet" 1000 "year_quarter" "../output/fe_table_sales_bw1000.tex" "../output/fe_table_sales_bw1000.csv" "../output/year_diagnostics_sales_bw1000.csv"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 8) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  fe_time <- cli_args[3]
  output_tex <- cli_args[4]
  output_csv <- cli_args[5]
  output_year_diag <- cli_args[6]
  fe_geo <- tolower(cli_args[7])
  cluster_level <- tolower(cli_args[8])
} else if (length(cli_args) >= 6) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  fe_time <- cli_args[3]
  output_tex <- cli_args[4]
  output_csv <- cli_args[5]
  output_year_diag <- cli_args[6]
  fe_geo <- tolower(Sys.getenv("FE_GEO", "segment"))
  cluster_level <- tolower(Sys.getenv("CLUSTER_LEVEL", "segment"))
} else {
  if (!exists("input") || !exists("bw_ft") || !exists("fe_time") ||
      !exists("output_tex") || !exists("output_csv") || !exists("output_year_diag") ||
      !exists("fe_geo") || !exists("cluster_level")) {
    stop(
      "FATAL: Script requires args: <input> <bw_ft> <fe_time> <output_tex> <output_csv> <output_year_diag> [<fe_geo> <cluster_level>]",
      call. = FALSE
    )
  }
}

stopifnot(
  is.finite(bw_ft), bw_ft > 0,
  fe_time %in% c("year", "year_quarter", "year_month")
)
if (!fe_geo %in% c("segment", "ward_pair")) {
  stop("--fe_geo must be one of: segment, ward_pair", call. = FALSE)
}
if (!cluster_level %in% c("segment", "ward_pair")) {
  stop("--cluster_level must be one of: segment, ward_pair", call. = FALSE)
}

fe_time_label <- c(year = "Year", year_quarter = "Year-Quarter", year_month = "Year-Month")

message(sprintf("=== Sales Border Pair FE | bw=%d | fe=%s | geo=%s | cluster=%s ===", bw_ft, fe_time, fe_geo, cluster_level))

# ── Load and filter ──
sales <- read_parquet(input) %>%
  as_tibble() %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    year_factor = as.character(year)
  ) %>%
  filter(
    !is.na(sale_price), sale_price > 0,
    !is.na(ward_pair), !is.na(signed_dist),
    abs(signed_dist) <= bw_ft,
    !is.na(strictness_own)
  )

need_segment <- fe_geo == "segment" || cluster_level == "segment"
if (need_segment) {
  sales <- sales %>% filter(!is.na(segment_id), segment_id != "")
}

# Year diagnostics
year_diag <- sales %>%
  group_by(year) %>%
  summarise(
    n = n(),
    median_price = median(sale_price, na.rm = TRUE),
    mean_price = mean(sale_price, na.rm = TRUE),
    coverage_sqft = mean(!is.na(log_sqft)),
    coverage_bedrooms = mean(!is.na(log_bedrooms)),
    coverage_baths = mean(!is.na(log_baths)),
    .groups = "drop"
  )
write_csv(year_diag, output_year_diag)

# Standardize strictness
strictness_sd <- sd(sales$strictness_own, na.rm = TRUE)
stopifnot(is.finite(strictness_sd), strictness_sd > 0)
sales <- sales %>% mutate(strictness_std = strictness_own / strictness_sd)

# Hedonic sample
sales_hed <- sales %>%
  filter(!is.na(log_sqft), !is.na(log_land_sqft), !is.na(log_building_age),
         !is.na(log_bedrooms), !is.na(log_baths), !is.na(has_garage))

message(sprintf("Full: %s obs, %d pairs | Hedonic: %s obs, %d pairs",
                format(nrow(sales), big.mark = ","), n_distinct(sales$ward_pair),
                format(nrow(sales_hed), big.mark = ","), n_distinct(sales_hed$ward_pair)))

# ── Custom fit stats ──
fitstat_register("myo", function(x) sprintf("%.0f", mean(x$custom_data$sale_price, na.rm = TRUE)),
                 alias = "Dep. Var. Mean")
fitstat_register("nwp", function(x) length(unique(stats::na.omit(x$custom_data$ward_pair))),
                 alias = "Ward Pairs")

# ── Regressions ──
fe_var <- switch(fe_time, year = "year_factor", year_quarter = "year_quarter", year_month = "year_month")
fe_term <- ifelse(fe_geo == "segment", paste0("segment_id^", fe_var), paste0("ward_pair^", fe_var))
cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair

m1 <- feols(as.formula(paste0("log(sale_price) ~ strictness_std | ", fe_term)),
            data = sales, cluster = cluster_formula)
m1$custom_data <- sales

hedonic_rhs <- "strictness_std + log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage"
m2 <- feols(as.formula(paste0("log(sale_price) ~ ", hedonic_rhs, " | ", fe_term)),
            data = sales_hed, cluster = cluster_formula)
m2$custom_data <- sales_hed

# ── Output table ──
setFixest_dict(c(
  strictness_std = "Uncertainty Index", ward_pair = "Ward Pair",
  year_factor = "Year", year_quarter = "Year-Quarter", year_month = "Year-Month"
))

fe_label <- ifelse(
  fe_geo == "segment",
  paste0("Segment $\\times$ ", fe_time_label[[fe_time]], " FE"),
  paste0("Ward-Pair $\\times$ ", fe_time_label[[fe_time]], " FE")
)

etable(
  list(m1, m2),
  keep = "Uncertainty Index",
  fitstat = ~ n + myo + nwp,
  style.tex = style.tex("aer", model.format = "", fixef.title = "", fixef.suffix = "",
                         yesNo = c("$\\checkmark$", "")),
  depvar = FALSE, drop.section = "fixef", digits = 3,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  extralines = c(
    list("_Hedonic Controls" = c("", "$\\checkmark$")),
    setNames(list(c("$\\checkmark$", "$\\checkmark$")), paste0("_", fe_label)),
    list("_Cluster Level" = c(
      ifelse(cluster_level == "segment", "Segment", "Ward Pair"),
      ifelse(cluster_level == "segment", "Segment", "Ward Pair")
    ))
  ),
  file = output_tex, replace = TRUE
)

# ── Coefficient CSV ──
write_csv(tibble(
  specification = c("no_hedonics", "with_hedonics"),
  estimate = c(coef(m1)[["strictness_std"]], coef(m2)[["strictness_std"]]),
  std_error = c(se(m1)[["strictness_std"]], se(m2)[["strictness_std"]]),
  p_value = c(pvalue(m1)[["strictness_std"]], pvalue(m2)[["strictness_std"]]),
  n_obs = c(m1$nobs, m2$nobs),
  dep_var_mean = c(mean(sales$sale_price, na.rm = TRUE), mean(sales_hed$sale_price, na.rm = TRUE)),
  ward_pairs = c(n_distinct(sales$ward_pair), n_distinct(sales_hed$ward_pair)),
  bandwidth_ft = bw_ft, fe_time = fe_time, fe_geo = fe_geo, cluster_level = cluster_level
), output_csv)

message(sprintf("Saved: %s", output_tex))
