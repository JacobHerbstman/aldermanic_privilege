source("../../setup_environment/code/packages.R")

library(arrow)
library(data.table)
library(fixest)
library(ggplot2)

sales_path <- "../input/sales_with_hedonics.parquet"
flags_path <- "../input/confounded_pair_era_flags.csv"
out_dir <- "../output"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}
stopifnot(file.exists(sales_path), file.exists(flags_path))

bw_ft <- 500
outcomes <- c("density_far", "density_dupac", "units_count")
outcome_labels <- c(
  density_far = "FAR",
  density_dupac = "DUPAC",
  units_count = "Units"
)

d_2003 <- as.Date("2003-05-01")
d_2015 <- as.Date("2015-05-18")
d_2023 <- as.Date("2023-05-15")

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  x <- x[grepl("^[0-9]+-[0-9]+$", x)]
  if (length(x) == 0) return(character())
  parts <- strsplit(x, "-", fixed = TRUE)
  vapply(parts, function(v) {
    a <- suppressWarnings(as.integer(v[1]))
    b <- suppressWarnings(as.integer(v[2]))
    if (!is.finite(a) || !is.finite(b)) return(NA_character_)
    paste(min(a, b), max(a, b), sep = "-")
  }, character(1))
}

extract_side_term <- function(model) {
  out <- list(
    estimate = NA_real_,
    std_error = NA_real_,
    p_value = NA_real_,
    t_value = NA_real_,
    n_obs = NA_integer_
  )
  if (is.null(model)) return(out)
  ct <- tryCatch(coeftable(model), error = function(e) NULL)
  out$n_obs <- as.integer(model$nobs)
  if (is.null(ct) || !("side" %in% rownames(ct))) return(out)
  p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)[1]
  t_col <- grep("t value", colnames(ct), value = TRUE)[1]
  out$estimate <- as.numeric(ct["side", "Estimate"])
  out$std_error <- as.numeric(ct["side", "Std. Error"])
  out$p_value <- if (!is.na(p_col)) as.numeric(ct["side", p_col]) else NA_real_
  out$t_value <- if (!is.na(t_col)) as.numeric(ct["side", t_col]) else NA_real_
  out
}

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p <= 0.01) return("***")
  if (p <= 0.05) return("**")
  if (p <= 0.10) return("*")
  ""
}

fit_plot <- function(d, model, sample_tag, transform_tag, outcome, out_pdf, out_bins) {
  d_plot <- copy(d)
  b_side <- c(estimate = NA_real_, se = NA_real_, p = NA_real_)
  b_x <- c(estimate = NA_real_, se = NA_real_, p = NA_real_)
  b_int <- c(estimate = NA_real_, se = NA_real_, p = NA_real_)

  if (!is.null(model)) {
    ct <- tryCatch(coeftable(model), error = function(e) NULL)
    get_coef <- function(ctab, names_vec) {
      if (is.null(ctab)) return(c(estimate = NA_real_, se = NA_real_, p = NA_real_))
      idx <- which(rownames(ctab) %in% names_vec)
      if (length(idx) == 0) return(c(estimate = NA_real_, se = NA_real_, p = NA_real_))
      p_col <- grep("^Pr\\(", colnames(ctab), value = TRUE)[1]
      c(
        estimate = as.numeric(ctab[idx[1], "Estimate"]),
        se = as.numeric(ctab[idx[1], "Std. Error"]),
        p = if (!is.na(p_col)) as.numeric(ctab[idx[1], p_col]) else NA_real_
      )
    }
    b_side <- get_coef(ct, c("side"))
    b_x <- get_coef(ct, c("signed_dist"))
    b_int <- get_coef(ct, c("side:signed_dist", "signed_dist:side"))

    d_plot[, xb := b_side["estimate"] * side +
      b_x["estimate"] * signed_dist +
      b_int["estimate"] * (side * signed_dist)]
    d_plot[, outcome_plot := as.numeric(resid(model)) + xb]
  } else {
    d_plot[, outcome_plot := outcome_val]
  }

  bin_w <- 25
  d_plot[, bin_id := floor(signed_dist / bin_w)]
  d_plot <- d_plot[bin_id >= floor(-bw_ft / bin_w) & bin_id <= floor(bw_ft / bin_w)]

  bins <- d_plot[, .(
    n = .N,
    mean_y = mean(outcome_plot, na.rm = TRUE),
    se_y = sd(outcome_plot, na.rm = TRUE) / sqrt(.N)
  ), by = .(side, bin_id)]
  bins[, x_center := (bin_id + 0.5) * bin_w]
  bins[, sample_tag := sample_tag]
  bins[, transform := transform_tag]
  bins[, outcome := outcome]
  setorder(bins, side, bin_id)
  fwrite(bins, out_bins)

  line_df <- data.table()
  if (!is.null(model)) {
    x_left <- seq(-bw_ft, 0, length.out = 200)
    x_right <- seq(0, bw_ft, length.out = 200)
    pred_left <- data.table(side = 0L, signed_dist = x_left)
    pred_right <- data.table(side = 1L, signed_dist = x_right)
    pred_left[, fit := b_x["estimate"] * signed_dist]
    pred_right[, fit := b_side["estimate"] + (b_x["estimate"] + b_int["estimate"]) * signed_dist]
    line_df <- rbindlist(list(pred_left, pred_right), fill = TRUE)
  }

  coef_side <- extract_side_term(model)
  jump_txt <- if (transform_tag == "log") {
    ifelse(
      is.finite(coef_side$estimate),
      sprintf("Jump = %.4f%s (SE %.4f; p=%.3f) ~ %.1f%%", coef_side$estimate, stars(coef_side$p_value), coef_side$std_error, coef_side$p_value, 100 * (exp(coef_side$estimate) - 1)),
      "Jump = NA"
    )
  } else {
    ifelse(
      is.finite(coef_side$estimate),
      sprintf("Jump = %.4f%s (SE %.4f; p=%.3f)", coef_side$estimate, stars(coef_side$p_value), coef_side$std_error, coef_side$p_value),
      "Jump = NA"
    )
  }

  p <- ggplot() +
    geom_point(
      data = bins,
      aes(x = x_center, y = mean_y, color = factor(side)),
      size = 1.6, alpha = 0.9
    ) +
    {
      if (nrow(line_df) > 0) {
        geom_line(
          data = line_df,
          aes(x = signed_dist, y = fit, color = factor(side)),
          linewidth = 1.0
        )
      }
    } +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
    scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    labs(
      title = sprintf("GM boundary RD bridge: %s (%s, %s)", outcome_labels[[outcome]], sample_tag, transform_tag),
      subtitle = sprintf("%s | bw = %d ft | FE: boundary_area_id + year", jump_txt, bw_ft),
      x = "Signed distance to border (feet)",
      y = ifelse(transform_tag == "log", sprintf("log(%s)", outcome_labels[[outcome]]), outcome_labels[[outcome]])
    ) +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  ggsave(out_pdf, p, width = 8.2, height = 5.6, dpi = 300)
}

message("Loading sales microdata...")
sales <- as.data.table(read_parquet(
  sales_path,
  col_select = c(
    "ward_pair_id", "sale_date", "year", "signed_dist",
    "sale_price", "land_sqft", "building_sqft", "num_apartments"
  )
))

sales[, ward_pair_id := normalize_pair_dash(ward_pair_id)]
sales[, sale_date := as.Date(sale_date)]
sales[, year := as.integer(year)]
sales[is.na(year), year := as.integer(format(sale_date, "%Y"))]
sales[, era := fifelse(
  sale_date < d_2003, "1998_2002",
  fifelse(sale_date < d_2015, "2003_2014",
    fifelse(sale_date < d_2023, "2015_2023", "post_2023")
  )
)]
sales <- sales[
  !is.na(ward_pair_id) &
    !is.na(sale_date) &
    !is.na(year) &
    !is.na(signed_dist)
]

sales[, density_far := as.numeric(building_sqft) / as.numeric(land_sqft)]
sales[, units_count := as.numeric(num_apartments)]
sales[, density_dupac := fifelse(
  is.finite(as.numeric(land_sqft)) & as.numeric(land_sqft) > 0 & is.finite(as.numeric(num_apartments)),
  43560 * as.numeric(num_apartments) / as.numeric(land_sqft),
  NA_real_
)]
sales[, side := as.integer(signed_dist >= 0)]
sales[, boundary_area_id := paste(ward_pair_id, era, sep = "__")]

flags <- fread(flags_path)
flags <- unique(flags[, .(
  ward_pair_id_dash = normalize_pair_dash(ward_pair_id_dash),
  era = as.character(era),
  drop_confound = as.logical(drop_confound)
)])

sales <- merge(
  sales,
  flags,
  by.x = c("ward_pair_id", "era"),
  by.y = c("ward_pair_id_dash", "era"),
  all.x = TRUE,
  sort = FALSE
)
sales[is.na(drop_confound), drop_confound := FALSE]

results <- list()
k <- 1L

for (sample_tag in c("all", "pruned")) {
  d_base <- sales[
    abs(signed_dist) <= bw_ft &
      !is.na(side) &
      !is.na(boundary_area_id) &
      !is.na(year)
  ]
  if (sample_tag == "pruned") {
    d_base <- d_base[drop_confound == FALSE]
  }

  for (outcome in outcomes) {
    for (transform_tag in c("log", "level")) {
      d <- copy(d_base)
      if (transform_tag == "log") {
        d <- d[is.finite(get(outcome)) & get(outcome) > 0]
        d[, outcome_val := log(get(outcome))]
      } else {
        d <- d[is.finite(get(outcome)) & get(outcome) >= 0]
        d[, outcome_val := get(outcome)]
      }

      n_obs <- nrow(d)
      n_boundaries <- uniqueN(d$boundary_area_id)
      model <- NULL

      if (n_obs >= 25 && uniqueN(d$side) >= 2 && n_boundaries >= 2) {
        model <- tryCatch(
          feols(
            outcome_val ~ side + signed_dist + side:signed_dist | boundary_area_id + year,
            data = d,
            cluster = ~boundary_area_id
          ),
          error = function(e) NULL
        )
      }

      est <- extract_side_term(model)
      pdf_path <- file.path(out_dir, sprintf(
        "gm_uniform_bridge_rd_bw500ft_%s_%s_%s.pdf",
        transform_tag, outcome, sample_tag
      ))
      bins_path <- file.path(out_dir, sprintf(
        "gm_uniform_bridge_rd_bw500ft_%s_%s_%s_bins.csv",
        transform_tag, outcome, sample_tag
      ))
      fit_plot(d, model, sample_tag, transform_tag, outcome, pdf_path, bins_path)

      results[[k]] <- data.table(
        sample_tag = sample_tag,
        transform = transform_tag,
        outcome = outcome,
        bandwidth_ft = bw_ft,
        estimate = est$estimate,
        std_error = est$std_error,
        p_value = est$p_value,
        t_value = est$t_value,
        n_obs = as.integer(ifelse(is.finite(est$n_obs), est$n_obs, n_obs)),
        n_boundaries = as.integer(n_boundaries),
        plot_pdf = pdf_path,
        bins_csv = bins_path
      )
      k <- k + 1L
    }
  }
}

out_detail <- file.path(out_dir, "gm_uniform_bridge_rd_bw500ft_detail.csv")
res_dt <- rbindlist(results, fill = TRUE)
setorder(res_dt, sample_tag, transform, outcome)
fwrite(res_dt, out_detail)

message("Saved:")
message(sprintf("  - %s", out_detail))
for (pp in res_dt$plot_pdf) message(sprintf("  - %s", pp))
