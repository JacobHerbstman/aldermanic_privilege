source("../../setup_environment/code/packages.R")
library(optparse)

option_list <- list(
  make_option("--input", type = "character", default = "../input/rent_with_ward_distances.parquet"),
  make_option("--bw_ft", type = "integer", default = 1000),
  make_option("--window", type = "character", default = "pre_2021"),
  make_option("--output_tex", type = "character"),
  make_option("--output_csv", type = "character")
)
opt <- parse_args(OptionParser(option_list = option_list))

apply_window <- function(df, w) {
  if (w == "full") return(df)
  if (w == "pre_2021") return(df %>% filter(year <= 2020))
  if (w == "pre_covid") return(df %>% filter(year <= 2019))
  df
}

window_label <- c(full = "All years", pre_covid = "Pre-COVID (2014-2019)", pre_2021 = "Through 2020")

message(sprintf("=== Characteristic FE Table | bw=%d | window=%s ===", opt$bw_ft, opt$window))

# ── Load and filter ──
dat <- read_parquet(opt$input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    is_multifamily = as.integer(building_type_clean == "multi_family"),
    right = as.integer(signed_dist >= 0)
  ) %>%
  filter(
    !is.na(file_date), !is.na(ward_pair), !is.na(signed_dist), !is.na(strictness_own),
    abs(signed_dist) <= opt$bw_ft
  ) %>%
  apply_window(opt$window)

# ── Outcomes to estimate ──
outcomes <- list(
  list(name = "sqft",       var = "sqft",           label = "Sqft",            log = FALSE),
  list(name = "log_sqft",   var = "sqft",           label = "Log(Sqft)",       log = TRUE),
  list(name = "beds",       var = "beds",           label = "Beds",            log = FALSE),
  list(name = "baths",      var = "baths",          label = "Baths",           log = FALSE),
  list(name = "multifamily",var = "is_multifamily", label = "Multi-Family",    log = FALSE),
  list(name = "laundry",    var = "laundry",        label = "Laundry",         log = FALSE),
  list(name = "gym",        var = "gym",            label = "Gym",             log = FALSE)
)

results <- list()
for (oc in outcomes) {
  d <- dat
  if (oc$log) {
    d <- d %>% mutate(Y = if_else(!is.na(.data[[oc$var]]) & .data[[oc$var]] > 0,
                                   log(.data[[oc$var]]), NA_real_))
  } else {
    d <- d %>% mutate(Y = as.numeric(.data[[oc$var]]))
  }
  d <- d %>% filter(!is.na(Y))
  if (nrow(d) < 10 || n_distinct(d$ward_pair) < 2) {
    message(sprintf("  Skipping %s: too few obs (%d)", oc$name, nrow(d)))
    next
  }
  m <- feols(Y ~ right | ward_pair^year_month, data = d, cluster = ~ward_pair)
  ct <- coeftable(m)
  results[[length(results) + 1]] <- tibble(
    outcome = oc$name,
    label = oc$label,
    estimate = ct["right", "Estimate"],
    std_error = ct["right", "Std. Error"],
    p_value = ct["right", "Pr(>|t|)"],
    n_obs = nobs(m),
    dep_var_mean = mean(d$Y, na.rm = TRUE),
    ward_pairs = n_distinct(d$ward_pair[d$ward_pair %in% names(which(table(d$ward_pair) > 0))]),
    bandwidth_ft = opt$bw_ft,
    window = opt$window
  )
  message(sprintf("  %s: b=%.4f (SE %.4f, p=%.3f), N=%s",
                  oc$label, ct["right", "Estimate"], ct["right", "Std. Error"],
                  ct["right", "Pr(>|t|)"], format(nobs(m), big.mark = ",")))
}

coef_tbl <- bind_rows(results)
write_csv(coef_tbl, opt$output_csv)

# ── LaTeX table ──
stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

ncol <- nrow(coef_tbl)
header <- paste0("\\begingroup\n\\centering\n\\begin{tabular}{l", paste(rep("c", ncol), collapse = ""), "}\n   \\toprule\n")
col_headers <- paste0("   ", paste(sprintf("& %s", coef_tbl$label), collapse = " "), "\\\\  \n")
midrule <- "   \\midrule \n"

coef_row <- paste0("   Stricter Side ",
  paste(sapply(seq_len(ncol), function(i) {
    sprintf("& %.4f%s", coef_tbl$estimate[i], stars(coef_tbl$p_value[i]))
  }), collapse = " "), "\\\\   \n")

se_row <- paste0("   ",
  paste(sapply(seq_len(ncol), function(i) {
    sprintf("& (%.4f)", coef_tbl$std_error[i])
  }), collapse = " "), "\\\\   \n")

blank <- "    \\\\\n"
obs_row <- paste0("   Observations ",
  paste(sapply(coef_tbl$n_obs, function(n) sprintf("& %s", format(n, big.mark = ","))), collapse = " "), "\\\\  \n")
mean_row <- paste0("   Dep. Var. Mean ",
  paste(sapply(coef_tbl$dep_var_mean, function(m) sprintf("& %.2f", m)), collapse = " "), "\\\\  \n")
fe_row <- paste0("   Ward-Pair $\\times$ Year-Month FE ",
  paste(rep("& $\\checkmark$", ncol), collapse = " "), "\\\\   \n")
window_row <- paste0("   Window ",
  paste(rep(sprintf("& %s", window_label[[opt$window]]), ncol), collapse = " "), "\\\\  \n")

footer <- "   \\bottomrule\n\\end{tabular}\n\\par\\endgroup\n"

tex <- paste0(header, col_headers, midrule, coef_row, se_row, blank,
              obs_row, mean_row, fe_row, window_row, footer)
writeLines(tex, opt$output_tex)

message(sprintf("Saved: %s", opt$output_tex))
message(sprintf("Saved: %s", opt$output_csv))
