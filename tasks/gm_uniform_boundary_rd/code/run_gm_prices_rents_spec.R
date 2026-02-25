library(arrow)
library(data.table)
library(fixest)
library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 6L) {
  stop(
    paste(
      "Usage: Rscript run_gm_prices_rents_spec.R",
      "<dataset:{sales|rent}> <transform:{level|log}>",
      "<sample_tag:{all|pruned}> <bandwidth_ft:{500|1000}>",
      "<fe_structure:{additive|interacted}> <controls_tag:{no_hedonic|hedonic}>"
    ),
    call. = FALSE
  )
}

dataset <- tolower(trimws(args[[1]]))
transform <- tolower(trimws(args[[2]]))
sample_tag <- tolower(trimws(args[[3]]))
bandwidth_ft <- suppressWarnings(as.integer(args[[4]]))
fe_structure <- tolower(trimws(args[[5]]))
controls_tag <- tolower(trimws(args[[6]]))

if (!dataset %in% c("sales", "rent")) {
  stop("dataset must be one of: sales, rent", call. = FALSE)
}
if (!transform %in% c("level", "log")) {
  stop("transform must be one of: level, log", call. = FALSE)
}
if (!sample_tag %in% c("all", "pruned")) {
  stop("sample_tag must be one of: all, pruned", call. = FALSE)
}
if (!is.finite(bandwidth_ft) || !bandwidth_ft %in% c(500L, 1000L)) {
  stop("bandwidth_ft must be one of: 500, 1000", call. = FALSE)
}
if (!fe_structure %in% c("additive", "interacted")) {
  stop("fe_structure must be one of: additive, interacted", call. = FALSE)
}
if (!controls_tag %in% c("no_hedonic", "hedonic")) {
  stop("controls_tag must be one of: no_hedonic, hedonic", call. = FALSE)
}

sales_path <- "../input/sales_with_hedonics.parquet"
rent_path <- "../input/rent_with_ward_distances_full.parquet"
flags_path <- "../input/confounded_pair_era_flags.csv"
out_dir <- "../output/bayer_kulka_prices_rents"
spec_inference_dir <- file.path(out_dir, "spec_inference")
spec_plot_dir <- file.path(out_dir, "spec_plot")
plots_dir <- file.path(out_dir, "plots")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(spec_inference_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(spec_plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(flags_path))
if (dataset == "sales") {
  stopifnot(file.exists(sales_path))
} else {
  stopifnot(file.exists(rent_path))
}

kernel <- "uniform"
bin_width_m <- 10L
bandwidth_m <- as.numeric(bandwidth_ft) * 0.3048

d_2003 <- as.Date("2003-05-01")
d_2015 <- as.Date("2015-05-18")
d_2023 <- as.Date("2023-05-15")

sanitize_token <- function(x) {
  x <- trimws(as.character(x))
  if (!nzchar(x)) {
    return("")
  }
  gsub("[^A-Za-z0-9_-]", "_", x)
}

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  out <- rep(NA_character_, length(x))
  ok <- grepl("^[0-9]+-[0-9]+$", x)
  if (!any(ok)) {
    return(out)
  }
  parts <- strsplit(x[ok], "-", fixed = TRUE)
  out[ok] <- vapply(parts, function(v) {
    a <- suppressWarnings(as.integer(v[1]))
    b <- suppressWarnings(as.integer(v[2]))
    if (!is.finite(a) || !is.finite(b)) {
      return(NA_character_)
    }
    paste(min(a, b), max(a, b), sep = "-")
  }, character(1))
  out
}

era_from_date <- function(date_vec) {
  d <- as.Date(date_vec)
  fifelse(
    d < d_2003, "1998_2002",
    fifelse(d < d_2015, "2003_2014", fifelse(d < d_2023, "2015_2023", "post_2023"))
  )
}

extract_side_term <- function(model) {
  out <- list(
    estimate = NA_real_,
    std_error = NA_real_,
    p_value = NA_real_,
    t_value = NA_real_,
    n_obs_model = NA_integer_
  )
  if (is.null(model)) {
    return(out)
  }
  ct <- tryCatch(coeftable(model), error = function(e) NULL)
  out$n_obs_model <- as.integer(model$nobs)
  if (is.null(ct) || !("side" %in% rownames(ct))) {
    return(out)
  }
  p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)[1]
  t_col <- grep("t value", colnames(ct), value = TRUE)[1]
  out$estimate <- as.numeric(ct["side", "Estimate"])
  out$std_error <- as.numeric(ct["side", "Std. Error"])
  out$p_value <- if (!is.na(p_col)) as.numeric(ct["side", p_col]) else NA_real_
  out$t_value <- if (!is.na(t_col)) as.numeric(ct["side", t_col]) else NA_real_
  out
}

safe_log <- function(x) {
  x <- as.numeric(x)
  fifelse(is.finite(x) & x > 0, log(x), NA_real_)
}

plot_y_label <- function(outcome_name, transform_name) {
  if (outcome_name == "home_price") {
    return(if (transform_name == "log") "Log(Home price)" else "Home price")
  }
  if (transform_name == "log") {
    "Log(Rent)"
  } else {
    "Rent"
  }
}

build_bins_plot <- function(bin_dt, y_col, title_text, y_label, bw_m, out_path) {
  if (!(y_col %in% names(bin_dt))) {
    stop(sprintf("Missing column %s in bin_dt.", y_col), call. = FALSE)
  }
  d_plot <- copy(bin_dt)
  d_plot <- d_plot[is.finite(get(y_col))]
  if (nrow(d_plot) == 0) {
    p <- ggplot() +
      geom_blank() +
      labs(
        title = title_text,
        x = "Distance to border (meters)",
        y = y_label
      ) +
      annotate("text", x = 0, y = 0, label = "No non-missing bins to display.") +
      theme_minimal(base_size = 11)
    ggsave(out_path, p, width = 8.8, height = 4.8, dpi = 300)
    return(invisible(NULL))
  }

  p <- ggplot(d_plot, aes(x = bin_mid_m, y = get(y_col))) +
    geom_point(size = 1.8, alpha = 0.95, color = "#1f4e79") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray35", linewidth = 0.6) +
    coord_cartesian(xlim = c(-bw_m, bw_m)) +
    labs(
      title = title_text,
      x = "Distance to border (meters)",
      y = y_label
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "gray85", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = 11)
    )
  ggsave(out_path, p, width = 8.8, height = 4.8, dpi = 300)
}

load_confound_flags <- function(path) {
  flags <- as.data.table(readr::read_csv(
    path,
    show_col_types = FALSE,
    col_select = c("ward_pair_id_dash", "era", "drop_confound")
  ))
  flags[, ward_pair := normalize_pair_dash(ward_pair_id_dash)]
  flags[, era := as.character(era)]
  flags[, drop_confound := as.logical(drop_confound)]
  unique(flags[!is.na(ward_pair) & !is.na(era), .(ward_pair, era, drop_confound)])
}

prepare_sales <- function() {
  dt <- as.data.table(read_parquet(
    sales_path,
    col_select = c(
      "ward_pair_id", "sale_date", "year", "signed_dist", "sale_price",
      "log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths",
      "has_garage"
    )
  ))
  n_rows_raw <- nrow(dt)

  dt[, ward_pair := normalize_pair_dash(ward_pair_id)]
  dt[, trans_date := as.Date(sale_date)]
  dt[, year := as.integer(year)]
  dt[is.na(year), year := as.integer(format(trans_date, "%Y"))]
  dt[, year_month := format(trans_date, "%Y-%m")]
  dt[, dist_m := as.numeric(signed_dist) * 0.3048]
  dt[, side := as.integer(dist_m >= 0)]
  dt[, outcome_raw := as.numeric(sale_price)]
  dt[, outcome := "home_price"]
  dt[, era := era_from_date(trans_date)]

  dt[, has_garage := as.integer(as.logical(has_garage))]
  dt[, log_sqft := as.numeric(log_sqft)]
  dt[, log_land_sqft := as.numeric(log_land_sqft)]
  dt[, log_building_age := as.numeric(log_building_age)]
  dt[, log_bedrooms := as.numeric(log_bedrooms)]
  dt[, log_baths := as.numeric(log_baths)]

  dt[, hedonic_complete := (
    is.finite(log_sqft) &
      is.finite(log_land_sqft) &
      is.finite(log_building_age) &
      is.finite(log_bedrooms) &
      is.finite(log_baths) &
      !is.na(has_garage)
  )]

  list(
    dt = dt,
    n_rows_raw = as.integer(n_rows_raw),
    controls_rhs = "log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage",
    fe_formula_additive = "ward_pair + year",
    fe_formula_interacted = "ward_pair^year"
  )
}

prepare_rent <- function() {
  dt <- as.data.table(read_parquet(
    rent_path,
    col_select = c(
      "ward_pair_id", "file_date", "signed_dist", "rent_price",
      "sqft", "beds", "baths", "building_type_clean"
    )
  ))
  n_rows_raw <- nrow(dt)

  dt[, ward_pair := normalize_pair_dash(ward_pair_id)]
  dt[, trans_date := as.Date(file_date)]
  dt[, year := as.integer(format(trans_date, "%Y"))]
  dt[, year_month := format(trans_date, "%Y-%m")]
  dt[, dist_m := as.numeric(signed_dist) * 0.3048]
  dt[, side := as.integer(dist_m >= 0)]
  dt[, outcome_raw := as.numeric(rent_price)]
  dt[, outcome := "rent_price"]
  dt[, era := era_from_date(trans_date)]

  dt[, log_sqft := safe_log(sqft)]
  dt[, log_beds := safe_log(beds)]
  dt[, log_baths := safe_log(baths)]
  dt[, building_type_factor := fifelse(
    is.na(building_type_clean) | trimws(as.character(building_type_clean)) == "",
    "missing",
    as.character(building_type_clean)
  )]

  dt[, hedonic_complete := (
    is.finite(log_sqft) &
      is.finite(log_beds) &
      is.finite(log_baths) &
      !is.na(building_type_factor) &
      trimws(building_type_factor) != ""
  )]

  list(
    dt = dt,
    n_rows_raw = as.integer(n_rows_raw),
    controls_rhs = "log_sqft + log_beds + log_baths + building_type_factor",
    fe_formula_additive = "ward_pair + year_month",
    fe_formula_interacted = "ward_pair^year_month"
  )
}

flags <- load_confound_flags(flags_path)

prep <- if (dataset == "sales") prepare_sales() else prepare_rent()
dt <- prep$dt
n_rows_raw <- prep$n_rows_raw

pair_parse_missing <- sum(is.na(dt$ward_pair))
era_missing <- sum(is.na(dt$era))

dt <- dt[
  !is.na(ward_pair) &
    !is.na(era) &
    !is.na(dist_m) &
    !is.na(side) &
    !is.na(trans_date) &
    !is.na(year)
]

dt <- merge(
  dt,
  flags,
  by = c("ward_pair", "era"),
  all.x = TRUE,
  sort = FALSE
)
dt[, flag_matched := !is.na(drop_confound)]
dt[is.na(drop_confound), drop_confound := FALSE]

n_rows_base <- nrow(dt)
join_match_rate <- if (n_rows_base > 0) mean(dt$flag_matched) else NA_real_
n_unmapped_rows <- dt[flag_matched == FALSE, .N]
n_unmapped_pair_era <- uniqueN(dt[flag_matched == FALSE, .(ward_pair, era)])
drop_confound_share <- if (n_rows_base > 0) mean(dt$drop_confound) else NA_real_
unmatched_out <- NA_character_

d <- copy(dt)
if (sample_tag == "pruned") {
  d <- d[drop_confound == FALSE]
}
d <- d[abs(dist_m) <= bandwidth_m]

if (transform == "log") {
  d <- d[is.finite(outcome_raw) & outcome_raw > 0]
  d[, outcome_val := log(outcome_raw)]
} else {
  d <- d[is.finite(outcome_raw)]
  d[, outcome_val := outcome_raw]
}

# Lock the sample for no_hedonic and hedonic comparisons.
d <- d[hedonic_complete == TRUE]
d[, row_uid := as.integer(.I)]
d[, w_uniform := 1]

fe_formula <- if (fe_structure == "additive") prep$fe_formula_additive else prep$fe_formula_interacted
controls_rhs <- prep$controls_rhs

rhs <- "side + dist_m + side:dist_m"
if (controls_tag == "hedonic") {
  rhs <- paste(rhs, controls_rhs, sep = " + ")
}
fml <- as.formula(sprintf("outcome_val ~ %s | %s", rhs, fe_formula))

n_obs_input <- nrow(d)
n_pairs_input <- uniqueN(d$ward_pair)
n_left <- d[side == 0L, .N]
n_right <- d[side == 1L, .N]

model <- NULL
if (n_obs_input >= 25L && n_pairs_input >= 2L && n_left >= 2L && n_right >= 2L) {
  model <- tryCatch(
    feols(
      fml,
      data = d,
      cluster = ~ward_pair,
      weights = ~w_uniform
    ),
    error = function(e) NULL
  )
}
est <- extract_side_term(model)
n_obs_model <- if (is.finite(est$n_obs_model)) as.integer(est$n_obs_model) else as.integer(n_obs_input)

resid_fml <- if (controls_tag == "hedonic") {
  as.formula(sprintf("outcome_val ~ %s | %s", controls_rhs, fe_formula))
} else {
  as.formula(sprintf("outcome_val ~ 1 | %s", fe_formula))
}

d_plot <- copy(d)
resid_model <- tryCatch(
  feols(resid_fml, data = d_plot),
  error = function(e) NULL
)

if (!is.null(resid_model)) {
  removed <- resid_model$obs_selection$obsRemoved
  if (is.null(removed)) {
    keep_idx <- seq_len(nrow(d_plot))
  } else {
    keep_idx <- setdiff(seq_len(nrow(d_plot)), abs(as.integer(removed)))
  }
  d_plot <- d_plot[keep_idx]
  if (nrow(d_plot) == nobs(resid_model)) {
    d_plot[, y_plot := as.numeric(resid(resid_model)) + mean(outcome_val, na.rm = TRUE)]
  } else {
    d_plot <- copy(d)
    d_plot[, y_plot := outcome_val]
  }
} else {
  d_plot <- copy(d)
  d_plot[, y_plot := outcome_val]
}

d_plot[, bin_id := floor(dist_m / bin_width_m)]
bin_dt <- d_plot[, .(
  n_bin = .N,
  y_bin_allbins = mean(y_plot, na.rm = TRUE)
), by = .(bin_id)]
bin_dt[, bin_mid_m := (bin_id + 0.5) * bin_width_m]
bin_dt[, y_bin_display_n5 := fifelse(n_bin >= 5L, y_bin_allbins, NA_real_)]

spec_outcome <- unique(d_plot$outcome)
if (length(spec_outcome) == 0L) {
  spec_outcome <- unique(dt$outcome)
}
if (length(spec_outcome) == 0L) {
  spec_outcome <- if (dataset == "sales") "home_price" else "rent_price"
}
spec_outcome <- as.character(spec_outcome[[1]])

spec_id <- sprintf(
  "dataset_%s__outcome_%s__transform_%s__sample_%s__bw_%dft__fe_%s__ctrl_%s",
  sanitize_token(dataset),
  sanitize_token(spec_outcome),
  sanitize_token(transform),
  sanitize_token(sample_tag),
  as.integer(bandwidth_ft),
  sanitize_token(fe_structure),
  sanitize_token(controls_tag)
)

setorder(bin_dt, bin_id)
plot_out <- copy(bin_dt)
plot_out[, `:=`(
  spec_id = spec_id,
  dataset = dataset,
  outcome = spec_outcome,
  transform = transform,
  sample_tag = sample_tag,
  bandwidth_ft = as.integer(bandwidth_ft),
  bandwidth_m = as.numeric(bandwidth_m),
  fe_structure = fe_structure,
  controls_tag = controls_tag,
  bin_width_m = as.integer(bin_width_m)
)]
setcolorder(plot_out, c(
  "spec_id", "dataset", "outcome", "transform", "sample_tag",
  "bandwidth_ft", "bandwidth_m", "fe_structure", "controls_tag",
  "bin_width_m", "bin_id", "bin_mid_m", "n_bin", "y_bin_allbins", "y_bin_display_n5"
))

bins_csv <- file.path(spec_plot_dir, sprintf("spec_%s.csv", spec_id))
fwrite(plot_out, bins_csv)

first_left <- plot_out[bin_id == -1L]
first_right <- plot_out[bin_id == 0L]

first_bin_left_n <- if (nrow(first_left) > 0L) as.integer(first_left$n_bin[[1]]) else 0L
first_bin_right_n <- if (nrow(first_right) > 0L) as.integer(first_right$n_bin[[1]]) else 0L
first_bin_left_mean <- if (nrow(first_left) > 0L) as.numeric(first_left$y_bin_allbins[[1]]) else NA_real_
first_bin_right_mean <- if (nrow(first_right) > 0L) as.numeric(first_right$y_bin_allbins[[1]]) else NA_real_
first_bin_gap <- if (is.finite(first_bin_right_mean) && is.finite(first_bin_left_mean)) {
  first_bin_right_mean - first_bin_left_mean
} else {
  NA_real_
}

obs_lock_signature <- if (n_obs_input == 0L) {
  "0|0|0"
} else {
  paste(
    as.integer(n_obs_input),
    format(sum(as.numeric(d$row_uid), na.rm = TRUE), scientific = FALSE, digits = 16),
    format(sum((as.numeric(d$row_uid) %% 1000003)^2, na.rm = TRUE), scientific = FALSE, digits = 16),
    sep = "|"
  )
}

p_val_label <- if (is.finite(est$p_value)) sprintf("%.4f", est$p_value) else "NA"
title_stub <- sprintf(
  "%s | %s | %s FE | %s | p=%s",
  if (spec_outcome == "home_price") "Home price" else "Rent",
  transform,
  fe_structure,
  paste0("bw=", as.integer(bandwidth_ft), "ft"),
  p_val_label
)

plot_allbins_pdf <- file.path(plots_dir, sprintf("gm_prices_rents_%s_allbins.pdf", spec_id))
plot_display_n5_pdf <- file.path(plots_dir, sprintf("gm_prices_rents_%s_display_n5.pdf", spec_id))

build_bins_plot(
  plot_out,
  "y_bin_allbins",
  paste0(title_stub, " | all bins"),
  plot_y_label(spec_outcome, transform),
  bandwidth_m,
  plot_allbins_pdf
)

build_bins_plot(
  plot_out,
  "y_bin_display_n5",
  paste0(title_stub, " | display n>=5"),
  plot_y_label(spec_outcome, transform),
  bandwidth_m,
  plot_display_n5_pdf
)

inference_out <- data.table(
  spec_id = spec_id,
  dataset = dataset,
  outcome = spec_outcome,
  transform = transform,
  sample_tag = sample_tag,
  bandwidth_ft = as.integer(bandwidth_ft),
  bandwidth_m = as.numeric(bandwidth_m),
  fe_structure = fe_structure,
  controls_tag = controls_tag,
  kernel = kernel,
  estimate = est$estimate,
  std_error = est$std_error,
  t_value = est$t_value,
  p_value = est$p_value,
  n_obs = as.integer(n_obs_model),
  n_pairs = as.integer(n_pairs_input),
  n_left = as.integer(n_left),
  n_right = as.integer(n_right),
  bin_width_m = as.integer(bin_width_m),
  first_bin_left_n = as.integer(first_bin_left_n),
  first_bin_right_n = as.integer(first_bin_right_n),
  first_bin_left_mean = as.numeric(first_bin_left_mean),
  first_bin_right_mean = as.numeric(first_bin_right_mean),
  first_bin_gap = as.numeric(first_bin_gap),
  bins_csv = bins_csv,
  plot_allbins_pdf = plot_allbins_pdf,
  plot_display_n5_pdf = plot_display_n5_pdf,
  obs_lock_signature = obs_lock_signature,
  n_rows_raw = as.integer(n_rows_raw),
  n_rows_base = as.integer(n_rows_base),
  join_match_rate = as.numeric(join_match_rate),
  n_unmapped_rows = as.integer(n_unmapped_rows),
  n_unmapped_pair_era = as.integer(n_unmapped_pair_era),
  drop_confound_share = as.numeric(drop_confound_share),
  pair_parse_missing = as.integer(pair_parse_missing),
  era_missing = as.integer(era_missing),
  unmatched_pair_era_csv = unmatched_out
)

inference_csv <- file.path(spec_inference_dir, sprintf("spec_%s.csv", spec_id))
fwrite(inference_out, inference_csv)

message(sprintf("Saved spec outputs for %s", spec_id))
message(sprintf("  - %s", inference_csv))
message(sprintf("  - %s", bins_csv))
message(sprintf("  - %s", plot_allbins_pdf))
message(sprintf("  - %s", plot_display_n5_pdf))
