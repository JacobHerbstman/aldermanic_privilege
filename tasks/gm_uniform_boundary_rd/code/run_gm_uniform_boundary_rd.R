source("../../setup_environment/code/packages.R")

library(arrow)
library(data.table)
library(ggplot2)

sales_path <- "../input/sales_with_hedonics.parquet"
flags_path <- "../input/confounded_pair_era_flags.csv"
out_dir <- "../output"
include_units_dupac <- tolower(Sys.getenv("INCLUDE_UNITS_DUPAC", "FALSE")) %in% c("true", "t", "1", "yes")
run_tag <- trimws(Sys.getenv("RUN_TAG", ""))
if (nchar(run_tag) > 0 && substr(run_tag, 1, 1) != "_") {
  run_tag <- paste0("_", run_tag)
}

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

stopifnot(file.exists(sales_path), file.exists(flags_path))

bandwidths <- data.table(
  bandwidth_m = c(1000L, 500L, 425L, 300L, 175L, 50L)
)
bandwidths[, bandwidth_ft := as.integer(round(bandwidth_m * 3.28084))]

outcomes <- c("lot_size_ft2", "density_far", "sale_price_usd")
outcome_labels <- c(
  lot_size_ft2 = "Lot size (ft^2)",
  density_far = "Density FAR",
  sale_price_usd = "Sale price (USD)",
  density_dupac = "Density DUPAC",
  units_count = "Units"
)
if (include_units_dupac) {
  outcomes <- c(outcomes, "density_dupac", "units_count")
}

d_2003 <- as.Date("2003-05-01")
d_2015 <- as.Date("2015-05-18")
d_2023 <- as.Date("2023-05-15")

fmt_num <- function(x, digits = 3) {
  ifelse(is.finite(x), format(round(x, digits), nsmall = digits, trim = TRUE), "NA")
}

star_code <- function(p) {
  p_num <- suppressWarnings(as.numeric(p))
  out <- rep("", length(p_num))
  out[is.finite(p_num) & p_num < 0.10] <- "*"
  out[is.finite(p_num) & p_num < 0.05] <- "**"
  out[is.finite(p_num) & p_num < 0.01] <- "***"
  out
}

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
    if (!is.finite(a) || !is.finite(b)) {
      return(NA_character_)
    }
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
  if (is.null(model)) {
    return(out)
  }
  ct <- tryCatch(coeftable(model), error = function(e) NULL)
  if (is.null(ct)) {
    out$n_obs <- as.integer(model$nobs)
    return(out)
  }
  if (!"side" %in% rownames(ct)) {
    out$n_obs <- as.integer(model$nobs)
    return(out)
  }
  p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)[1]
  t_col <- grep("t value", colnames(ct), value = TRUE)[1]
  out$estimate <- as.numeric(ct["side", "Estimate"])
  out$std_error <- as.numeric(ct["side", "Std. Error"])
  out$p_value <- if (!is.na(p_col)) as.numeric(ct["side", p_col]) else NA_real_
  out$t_value <- if (!is.na(t_col)) as.numeric(ct["side", t_col]) else NA_real_
  out$n_obs <- as.integer(model$nobs)
  out
}

write_latex_table <- function(detail_dt, path_tex, caption) {
  bw_order <- as.character(bandwidths$bandwidth_m)
  dt <- copy(detail_dt)
  dt[, bw_label := as.character(bandwidth_m)]
  dt[, cell := ifelse(
    is.finite(estimate),
    sprintf("%s%s (%s)", fmt_num(estimate), star_code(p_value), fmt_num(std_error)),
    "NA"
  )]

  est_wide <- dcast(dt, outcome ~ bw_label, value.var = "cell")
  n_wide <- dcast(dt, outcome ~ bw_label, value.var = "n_obs")
  b_wide <- dcast(dt, outcome ~ bw_label, value.var = "n_boundaries")

  setnames(est_wide, old = intersect(names(est_wide), bw_order), new = paste0("bw_", bw_order, "m"))
  setnames(n_wide, old = intersect(names(n_wide), bw_order), new = paste0("n_", bw_order, "m"))
  setnames(b_wide, old = intersect(names(b_wide), bw_order), new = paste0("b_", bw_order, "m"))

  setkey(est_wide, outcome)
  setkey(n_wide, outcome)
  setkey(b_wide, outcome)
  tbl <- n_wide[est_wide][b_wide]
  setcolorder(tbl, c(
    "outcome",
    paste0("bw_", bw_order, "m"),
    paste0("n_", bw_order, "m"),
    paste0("b_", bw_order, "m")
  ))

  lines <- c(
    "\\begin{table}[H]",
    "\\centering",
    sprintf("\\caption{%s}", caption),
    "\\begin{tabular}{lcccccc}",
    "\\hline",
    "Outcome & 1000m & 500m & 425m & 300m & 175m & 50m \\\\",
    "\\hline"
  )

  for (i in seq_len(nrow(tbl))) {
    rr <- tbl[i]
    out_lab <- outcome_labels[[rr$outcome]]
    est_cells <- unlist(rr[, paste0("bw_", bw_order, "m"), with = FALSE], use.names = FALSE)
    n_cells <- unlist(rr[, paste0("n_", bw_order, "m"), with = FALSE], use.names = FALSE)
    b_cells <- unlist(rr[, paste0("b_", bw_order, "m"), with = FALSE], use.names = FALSE)
    lines <- c(
      lines,
      sprintf("%s & %s \\\\", out_lab, paste(est_cells, collapse = " & ")),
      sprintf("N (%s) & %s \\\\", out_lab, paste(format(as.integer(n_cells), big.mark = ","), collapse = " & ")),
      sprintf("Boundary areas (%s) & %s \\\\", out_lab, paste(format(as.integer(b_cells), big.mark = ","), collapse = " & ")),
      "\\hline"
    )
  }

  lines <- c(
    lines,
    "\\end{tabular}",
    "\\end{table}"
  )
  writeLines(lines, path_tex)
}

run_grid <- function(base_dt, sample_tag) {
  grid <- CJ(outcome = outcomes, bandwidth_m = bandwidths$bandwidth_m)
  grid <- merge(grid, bandwidths, by = "bandwidth_m", all.x = TRUE, sort = FALSE)
  out <- vector("list", nrow(grid))

  for (i in seq_len(nrow(grid))) {
    rr <- grid[i]
    outcome <- rr$outcome
    bw_m <- rr$bandwidth_m
    bw_ft <- rr$bandwidth_ft

    d <- base_dt[
      abs(signed_dist) <= bw_ft &
        is.finite(get(outcome)) &
        get(outcome) > 0 &
        !is.na(side) &
        !is.na(boundary_area_id) &
        !is.na(year)
    ]

    if (sample_tag == "pruned") {
      d <- d[drop_confound == FALSE]
    }

    n_obs <- nrow(d)
    n_boundaries <- uniqueN(d$boundary_area_id)

    model <- NULL
    if (n_obs >= 25 && uniqueN(d$side) >= 2 && n_boundaries >= 2) {
      fml <- as.formula(sprintf(
        "%s ~ side + signed_dist + side:signed_dist | boundary_area_id + year",
        outcome
      ))
      model <- tryCatch(
        feols(fml, data = d, cluster = ~boundary_area_id),
        error = function(e) NULL
      )
    }

    est <- extract_side_term(model)
    out[[i]] <- data.table(
      sample_tag = sample_tag,
      outcome = outcome,
      bandwidth_m = as.integer(bw_m),
      bandwidth_ft = as.integer(bw_ft),
      estimate = est$estimate,
      std_error = est$std_error,
      p_value = est$p_value,
      t_value = est$t_value,
      n_obs = as.integer(ifelse(is.finite(est$n_obs), est$n_obs, n_obs)),
      n_boundaries = as.integer(n_boundaries)
    )
  }

  res <- rbindlist(out, fill = TRUE)
  setorder(res, outcome, -bandwidth_m)
  res
}

make_bins_and_plot <- function(base_dt, sample_tag, out_bins_csv, out_pdf) {
  bw_1000_ft <- bandwidths[bandwidth_m == 1000L, bandwidth_ft][1]
  d <- base_dt[
    abs(signed_dist) <= bw_1000_ft &
      !is.na(signed_dist_m) &
      !is.na(side)
  ]
  if (sample_tag == "pruned") {
    d <- d[drop_confound == FALSE]
  }

  long <- melt(
    d,
    measure.vars = outcomes,
    variable.name = "outcome",
    value.name = "outcome_value"
  )
  long <- long[is.finite(outcome_value) & outcome_value > 0]
  if (nrow(long) == 0) {
    stop(sprintf("No observations available for plotting (%s sample).", sample_tag), call. = FALSE)
  }

  bin_width_m <- 25
  max_m <- 1000
  long[, bin_id := floor(signed_dist_m / bin_width_m)]
  long <- long[bin_id >= -max_m / bin_width_m & bin_id < max_m / bin_width_m]

  bins <- long[, .(
    n = .N,
    mean_y = mean(outcome_value, na.rm = TRUE),
    se_y = sd(outcome_value, na.rm = TRUE) / sqrt(.N)
  ), by = .(outcome, side, bin_id)]
  bins[, bin_center_m := (bin_id + 0.5) * bin_width_m]
  bins[, sample_tag := sample_tag]

  fit_lines <- list()
  idx <- 1L
  for (outcome_i in outcomes) {
    for (side_i in c(0L, 1L)) {
      d_fit <- long[outcome == outcome_i & side == side_i]
      if (nrow(d_fit) < 10) next
      fit <- tryCatch(
        lm(outcome_value ~ signed_dist_m, data = d_fit),
        error = function(e) NULL
      )
      if (is.null(fit)) next
      x_grid <- if (side_i == 0L) seq(-1000, 0, length.out = 250) else seq(0, 1000, length.out = 250)
      y_hat <- as.numeric(predict(fit, newdata = data.frame(signed_dist_m = x_grid)))
      fit_lines[[idx]] <- data.table(
        outcome = outcome_i,
        side = side_i,
        signed_dist_m = x_grid,
        fit = y_hat
      )
      idx <- idx + 1L
    }
  }
  fit_dt <- if (length(fit_lines) > 0) rbindlist(fit_lines, fill = TRUE) else data.table()

  bins[, outcome_label := outcome_labels[outcome]]
  if (nrow(fit_dt) > 0) {
    fit_dt[, outcome_label := outcome_labels[outcome]]
  }

  bins_out <- bins[, .(
    sample_tag,
    outcome,
    outcome_label,
    side,
    bin_id,
    bin_center_m,
    n,
    mean_y,
    se_y
  )]
  setorder(bins_out, outcome, side, bin_id)
  fwrite(bins_out, out_bins_csv)

  p <- ggplot() +
    geom_point(
      data = bins,
      aes(x = bin_center_m, y = mean_y, color = factor(side)),
      size = 1.8,
      alpha = 0.9
    ) +
    {
      if (nrow(fit_dt) > 0) {
        geom_line(
          data = fit_dt,
          aes(x = signed_dist_m, y = fit, color = factor(side)),
          linewidth = 0.9
        )
      }
    } +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray25", linewidth = 0.7) +
    facet_wrap(~outcome_label, ncol = 1, scales = "free_y") +
    scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    coord_cartesian(xlim = c(-1000, 1000)) +
    labs(
      title = sprintf("Distance gradients in boundary outcomes (%s sample)", sample_tag),
      subtitle = "Binned means with side-specific local linear fits",
      x = "Distance to border (meters)",
      y = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )

  ggsave(out_pdf, p, width = 8, height = 10, dpi = 300)
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

sales[, lot_size_ft2 := as.numeric(land_sqft)]
sales[, density_far := as.numeric(building_sqft) / as.numeric(land_sqft)]
sales[, sale_price_usd := as.numeric(sale_price)]
sales[, units_count := as.numeric(num_apartments)]
sales[, density_dupac := fifelse(
  is.finite(as.numeric(land_sqft)) & as.numeric(land_sqft) > 0 &
    is.finite(as.numeric(num_apartments)),
  43560 * as.numeric(num_apartments) / as.numeric(land_sqft),
  NA_real_
)]
sales[, side := as.integer(signed_dist >= 0)]
sales[, signed_dist_m := as.numeric(signed_dist) / 3.28084]
sales[, boundary_area_id := paste(ward_pair_id, era, sep = "__")]

flags <- fread(flags_path)
needed_flag_cols <- c("ward_pair_id_dash", "era", "drop_confound")
missing_flag_cols <- setdiff(needed_flag_cols, names(flags))
if (length(missing_flag_cols) > 0) {
  stop(
    sprintf("Confound flags file missing required columns: %s", paste(missing_flag_cols, collapse = ", ")),
    call. = FALSE
  )
}
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

detail_all <- run_grid(sales, "all")
detail_pruned <- run_grid(sales, "pruned")

tag_file <- function(filename) {
  full <- file.path(out_dir, filename)
  if (nchar(run_tag) == 0) {
    return(full)
  }
  sub("(\\.[^.]+)$", paste0(run_tag, "\\1"), full)
}

path_detail_all <- tag_file("gm_uniform_boundary_rd_detail_all.csv")
path_detail_pruned <- tag_file("gm_uniform_boundary_rd_detail_pruned.csv")
fwrite(detail_all, path_detail_all)
fwrite(detail_pruned, path_detail_pruned)

build_table_csv <- function(detail_dt) {
  bw_order <- as.character(bandwidths$bandwidth_m)
  dt <- copy(detail_dt)
  dt[, bw_label := as.character(bandwidth_m)]
  dt[, est_se := ifelse(
    is.finite(estimate),
    sprintf("%s%s (%s)", fmt_num(estimate), star_code(p_value), fmt_num(std_error)),
    "NA"
  )]

  est_wide <- dcast(dt, outcome ~ bw_label, value.var = "est_se")
  n_wide <- dcast(dt, outcome ~ bw_label, value.var = "n_obs")
  b_wide <- dcast(dt, outcome ~ bw_label, value.var = "n_boundaries")

  setnames(est_wide, old = intersect(names(est_wide), bw_order), new = paste0("bw_", bw_order, "m"))
  setnames(n_wide, old = intersect(names(n_wide), bw_order), new = paste0("n_obs_", bw_order, "m"))
  setnames(b_wide, old = intersect(names(b_wide), bw_order), new = paste0("n_boundaries_", bw_order, "m"))

  setkey(est_wide, outcome)
  setkey(n_wide, outcome)
  setkey(b_wide, outcome)
  out <- n_wide[est_wide][b_wide]
  out[, outcome_label := outcome_labels[outcome]]
  setcolorder(out, c(
    "outcome",
    "outcome_label",
    paste0("bw_", bw_order, "m"),
    paste0("n_obs_", bw_order, "m"),
    paste0("n_boundaries_", bw_order, "m")
  ))
  out
}

table_all <- build_table_csv(detail_all)
table_pruned <- build_table_csv(detail_pruned)

path_table_all_csv <- file.path(out_dir, "gm_uniform_boundary_rd_table_all.csv")
path_table_pruned_csv <- file.path(out_dir, "gm_uniform_boundary_rd_table_pruned.csv")
path_table_all_csv <- tag_file("gm_uniform_boundary_rd_table_all.csv")
path_table_pruned_csv <- tag_file("gm_uniform_boundary_rd_table_pruned.csv")
fwrite(table_all, path_table_all_csv)
fwrite(table_pruned, path_table_pruned_csv)

path_table_all_tex <- tag_file("gm_uniform_boundary_rd_table_all.tex")
path_table_pruned_tex <- tag_file("gm_uniform_boundary_rd_table_pruned.tex")
write_latex_table(detail_all, path_table_all_tex, "GM-style uniform-kernel boundary RD (all sample)")
write_latex_table(detail_pruned, path_table_pruned_tex, "GM-style uniform-kernel boundary RD (pruned sample)")

path_plot_all <- tag_file("gm_uniform_distance_gradients_bw1000m_all.pdf")
path_plot_pruned <- tag_file("gm_uniform_distance_gradients_bw1000m_pruned.pdf")
path_bins_all <- tag_file("gm_uniform_distance_gradients_bw1000m_all_bins.csv")
path_bins_pruned <- tag_file("gm_uniform_distance_gradients_bw1000m_pruned_bins.csv")

make_bins_and_plot(sales, "all", path_bins_all, path_plot_all)
make_bins_and_plot(sales, "pruned", path_bins_pruned, path_plot_pruned)

message("Saved:")
message(sprintf("  - include_units_dupac = %s", include_units_dupac))
message(sprintf("  - run_tag = '%s'", run_tag))
message(sprintf("  - %s", path_detail_all))
message(sprintf("  - %s", path_detail_pruned))
message(sprintf("  - %s", path_table_all_csv))
message(sprintf("  - %s", path_table_pruned_csv))
message(sprintf("  - %s", path_table_all_tex))
message(sprintf("  - %s", path_table_pruned_tex))
message(sprintf("  - %s", path_plot_all))
message(sprintf("  - %s", path_plot_pruned))
message(sprintf("  - %s", path_bins_all))
message(sprintf("  - %s", path_bins_pruned))
