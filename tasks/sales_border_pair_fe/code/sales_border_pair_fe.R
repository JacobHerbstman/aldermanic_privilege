source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_border_pair_fe/code")
# bw_ft <- 500
# fe_time <- "year_quarter"
# fe_geo <- "segment"
# cluster_level <- "ward_pair"
# table_mode <- "amenity"
# estimand <- "strictness_score"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bw_ft, fe_time, fe_geo, cluster_level, table_mode, estimand)
}

if (length(cli_args) != 6) {
  stop(
    "FATAL: Script requires args: <bw_ft> <fe_time> <fe_geo> <cluster_level> <table_mode> <estimand>",
    call. = FALSE
  )
}

bw_ft <- suppressWarnings(as.integer(cli_args[1]))
fe_time <- cli_args[2]
fe_geo <- tolower(cli_args[3])
cluster_level <- tolower(cli_args[4])
table_mode <- tolower(cli_args[5])
estimand <- tolower(cli_args[6])

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
if (!table_mode %in% c("baseline", "amenity")) {
  stop("--table_mode must be one of: baseline, amenity", call. = FALSE)
}
if (!estimand %in% c("strictness_score", "right")) {
  stop("--estimand must be one of: strictness_score, right", call. = FALSE)
}

estimand_prefix <- ifelse(estimand == "right", "_right", "")
output_tex <- sprintf(
  "../output/fe_table_sales%s_bw%d_%s_%s_clust_%s.tex",
  estimand_prefix,
  bw_ft,
  fe_time,
  table_mode,
  cluster_level
)

fe_time_label <- c(year = "Year", year_quarter = "Year-Quarter", year_month = "Year-Month")

message(sprintf("=== Sales Border Pair FE | bw=%d | fe=%s | geo=%s | cluster=%s | mode=%s | estimand=%s ===", bw_ft, fe_time, fe_geo, cluster_level, table_mode, estimand))

sales <- read_parquet("../output/sales_with_hedonics_amenities.parquet") %>%
  as_tibble()

if (!"signed_dist" %in% names(sales) && "signed_dist_m" %in% names(sales)) {
  sales <- sales %>% mutate(signed_dist = signed_dist_m / 0.3048)
}
if (!"signed_dist" %in% names(sales)) {
  stop("Sales input must include signed_dist in feet or signed_dist_m in meters.", call. = FALSE)
}

sales <- sales %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    year_factor = as.character(year),
    signed_dist = as.numeric(signed_dist),
    right = as.integer(signed_dist >= 0)
  ) %>%
  filter(
    !is.na(sale_price), sale_price > 0,
    year >= 2006,
    year <= 2022,
    !is.na(ward_pair), is.finite(signed_dist),
    abs(signed_dist) <= bw_ft,
    !is.na(strictness_own),
    !is.na(strictness_neighbor)
  )

need_segment <- fe_geo == "segment" || cluster_level == "segment"
if (need_segment) {
  sales <- sales %>% filter(!is.na(segment_id), segment_id != "")
}

strictness_sd <- sd(sales$strictness_own, na.rm = TRUE)
stopifnot(is.finite(strictness_sd), strictness_sd > 0)
sales <- sales %>% mutate(strictness_std = strictness_own / strictness_sd)
treatment_var <- ifelse(estimand == "right", "right", "strictness_std")
treatment_label <- ifelse(estimand == "right", "Stricter Side", "Stringency Index")

sales_hed <- sales %>%
  filter(!is.na(log_sqft), !is.na(log_land_sqft), !is.na(log_building_age),
         !is.na(log_bedrooms), !is.na(log_baths), !is.na(has_garage))

if (table_mode == "amenity") {
  amenity_cols <- c(
    "nearest_school_dist_ft",
    "nearest_park_dist_ft",
    "nearest_major_road_dist_ft",
    "lake_michigan_dist_ft"
  )
  missing_amenity_cols <- setdiff(amenity_cols, names(sales_hed))
  if (length(missing_amenity_cols) > 0) {
    stop(sprintf("Amenity mode requires columns: %s", paste(missing_amenity_cols, collapse = ", ")), call. = FALSE)
  }
  sales_amenity <- sales_hed %>%
    filter(
      !is.na(nearest_school_dist_ft),
      !is.na(nearest_park_dist_ft),
      !is.na(nearest_major_road_dist_ft),
      !is.na(lake_michigan_dist_ft)
    )
} else {
  sales_amenity <- NULL
}

message(sprintf("Full: %s obs, %d pairs | Hedonic: %s obs, %d pairs",
                format(nrow(sales), big.mark = ","), n_distinct(sales$ward_pair),
                format(nrow(sales_hed), big.mark = ","), n_distinct(sales_hed$ward_pair)))
if (table_mode == "amenity") {
  message(sprintf("Amenity complete-case: %s obs, %d pairs",
                  format(nrow(sales_amenity), big.mark = ","), n_distinct(sales_amenity$ward_pair)))
}

fitstat_register("myo", function(x) sprintf("%.0f", mean(x$custom_data$sale_price, na.rm = TRUE)),
                 alias = "Dep. Var. Mean")
fitstat_register("nseg", function(x) length(unique(stats::na.omit(x$custom_data$segment_id))),
                 alias = "Segments")
fitstat_register("nwp", function(x) length(unique(stats::na.omit(x$custom_data$ward_pair))),
                 alias = "Ward Pairs")

fe_var <- switch(fe_time, year = "year_factor", year_quarter = "year_quarter", year_month = "year_month")
fe_term <- ifelse(fe_geo == "segment", paste0("segment_id^", fe_var), paste0("ward_pair^", fe_var))
cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair

m1 <- feols(as.formula(paste0("log(sale_price) ~ ", treatment_var, " | ", fe_term)),
            data = sales, cluster = cluster_formula)
m1$custom_data <- sales

hedonic_rhs <- paste(
  treatment_var,
  "log_sqft",
  "log_land_sqft",
  "log_building_age",
  "log_bedrooms",
  "log_baths",
  "has_garage",
  sep = " + "
)
m2 <- feols(as.formula(paste0("log(sale_price) ~ ", hedonic_rhs, " | ", fe_term)),
            data = sales_hed, cluster = cluster_formula)
m2$custom_data <- sales_hed

if (table_mode == "amenity") {
  amenity_rhs <- paste0(
    hedonic_rhs,
    " + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + lake_michigan_dist_ft"
  )
  m3 <- feols(as.formula(paste0("log(sale_price) ~ ", amenity_rhs, " | ", fe_term)),
              data = sales_amenity, cluster = cluster_formula)
  m3$custom_data <- sales_amenity
}

setFixest_dict(c(
  strictness_std = "Stringency Index", right = "Stricter Side", ward_pair = "Ward Pair",
  year_factor = "Year", year_quarter = "Year-Quarter", year_month = "Year-Month"
))

fe_label <- ifelse(
  fe_geo == "segment",
  paste0("Segment $\\times$ ", fe_time_label[[fe_time]], " FE"),
  paste0("Ward-Pair $\\times$ ", fe_time_label[[fe_time]], " FE")
)

etable(
  if (table_mode == "amenity") list(m1, m2, m3) else list(m1, m2),
  keep = treatment_label,
  fitstat = ~ n + myo + nseg + nwp,
  style.tex = style.tex("aer", model.format = "", fixef.title = "", fixef.suffix = "",
                         yesNo = c("$\\checkmark$", "")),
  depvar = FALSE, drop.section = "fixef", digits = 3,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  extralines = {
    n_cols <- if (table_mode == "amenity") 3 else 2
    cluster_entries <- rep(ifelse(cluster_level == "segment", "Segment", "Ward Pair"), n_cols)
    fe_entries <- rep("$\\checkmark$", n_cols)
    hedonic_entries <- if (table_mode == "amenity") c("", "$\\checkmark$", "$\\checkmark$") else c("", "$\\checkmark$")
    out <- c(
      list("_Estimand" = rep(treatment_label, n_cols)),
      list("_Hedonic Controls" = hedonic_entries)
    )
    if (table_mode == "amenity") {
      out <- c(out, list("_Amenity Controls" = c("", "", "$\\checkmark$")))
    }
    out <- c(
      out,
      setNames(list(fe_entries), paste0("_", fe_label)),
      list("_Cluster Level" = cluster_entries)
    )
    out
  },
  file = output_tex, replace = TRUE
)

message(sprintf("Saved: %s", output_tex))
