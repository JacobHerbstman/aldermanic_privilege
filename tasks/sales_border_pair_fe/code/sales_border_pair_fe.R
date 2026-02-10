source("../../setup_environment/code/packages.R")
library(optparse)

option_list <- list(
  make_option("--input", type = "character", default = "../input/sales_with_hedonics.parquet"),
  make_option("--bw_ft", type = "integer", default = 1000),
  make_option("--fe_time", type = "character", default = "year_quarter"),
  make_option("--output_tex", type = "character", default = "../output/fe_table_sales_bw1000.tex"),
  make_option("--output_csv", type = "character", default = "../output/fe_table_sales_bw1000.csv"),
  make_option("--output_year_diag", type = "character", default = "../output/year_diagnostics_sales_bw1000.csv")
)
opt <- parse_args(OptionParser(option_list = option_list))

stopifnot(
  is.finite(opt$bw_ft), opt$bw_ft > 0,
  opt$fe_time %in% c("year", "year_quarter", "year_month")
)

fe_time_label <- c(year = "Year", year_quarter = "Year-Quarter", year_month = "Year-Month")

message(sprintf("=== Sales Border Pair FE | bw=%d | fe=%s ===", opt$bw_ft, opt$fe_time))

# ── Load and filter ──
sales <- read_parquet(opt$input) %>%
  as_tibble() %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    year_factor = as.character(year)
  ) %>%
  filter(
    !is.na(sale_price), sale_price > 0,
    !is.na(ward_pair), !is.na(signed_dist),
    abs(signed_dist) <= opt$bw_ft,
    !is.na(strictness_own)
  )

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
write_csv(year_diag, opt$output_year_diag)

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
fe_var <- switch(opt$fe_time, year = "year_factor", year_quarter = "year_quarter", year_month = "year_month")

m1 <- feols(as.formula(paste0("log(sale_price) ~ strictness_std | ward_pair^", fe_var)),
            data = sales, cluster = ~ward_pair)
m1$custom_data <- sales

hedonic_rhs <- "strictness_std + log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage"
m2 <- feols(as.formula(paste0("log(sale_price) ~ ", hedonic_rhs, " | ward_pair^", fe_var)),
            data = sales_hed, cluster = ~ward_pair)
m2$custom_data <- sales_hed

# ── Output table ──
setFixest_dict(c(
  strictness_std = "Uncertainty Index", ward_pair = "Ward Pair",
  year_factor = "Year", year_quarter = "Year-Quarter", year_month = "Year-Month"
))

fe_label <- paste0("Ward-Pair $\\times$ ", fe_time_label[[opt$fe_time]], " FE")

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
    setNames(list(c("$\\checkmark$", "$\\checkmark$")), paste0("_", fe_label))
  ),
  file = opt$output_tex, replace = TRUE
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
  bandwidth_ft = opt$bw_ft, fe_time = opt$fe_time
), opt$output_csv)

message(sprintf("Saved: %s", opt$output_tex))
