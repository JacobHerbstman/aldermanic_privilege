source("../../setup_environment/code/packages.R")

library(data.table)
library(readr)
library(fixest)
library(ggplot2)

parcels_path <- "../input/parcels_with_ward_distances.csv"
flags_path <- "../input/confounded_pair_era_flags.csv"
out_dir <- "../output"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}
stopifnot(file.exists(parcels_path), file.exists(flags_path))

args <- commandArgs(trailingOnly = TRUE)
bw_ft <- if (length(args) >= 1) as.numeric(args[1]) else 500
if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("Bandwidth in feet must be positive.", call. = FALSE)
}

outcomes <- c("density_far", "density_dupac", "unitscount")
outcome_labels <- c(
  density_far = "FAR",
  density_dupac = "DUPAC",
  unitscount = "Units"
)

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

fit_plot <- function(d, model, sample_tag, transform_tag, outcome, out_pdf, out_bins, bw_ft) {
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
    b_x <- get_coef(ct, c("signed_distance"))
    b_int <- get_coef(ct, c("side:signed_distance", "signed_distance:side"))

    d_plot[, xb := b_side["estimate"] * side +
      b_x["estimate"] * signed_distance +
      b_int["estimate"] * (side * signed_distance)]
    d_plot[, outcome_plot := as.numeric(resid(model)) + xb]
  } else {
    d_plot[, outcome_plot := outcome_val]
  }

  bin_w <- 25
  d_plot[, bin_id := floor(signed_distance / bin_w)]
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
    pred_left <- data.table(side = 0L, signed_distance = x_left)
    pred_right <- data.table(side = 1L, signed_distance = x_right)
    pred_left[, fit := b_x["estimate"] * signed_distance]
    pred_right[, fit := b_side["estimate"] + (b_x["estimate"] + b_int["estimate"]) * signed_distance]
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
          aes(x = signed_distance, y = fit, color = factor(side)),
          linewidth = 1.0
        )
      }
    } +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
    scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    labs(
      title = sprintf("GM-style RD on construction data: %s (%s, %s)", outcome_labels[[outcome]], sample_tag, transform_tag),
      subtitle = sprintf("%s | bw = %dft | FE: ward_pair + construction_year", jump_txt, as.integer(bw_ft)),
      x = "Signed distance to boundary (feet)",
      y = ifelse(transform_tag == "log", sprintf("log(%s)", outcome_labels[[outcome]]), outcome_labels[[outcome]])
    ) +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  ggsave(out_pdf, p, width = 8.2, height = 5.6, dpi = 300)
}

message("Loading construction/density data...")
dat <- as.data.table(read_csv(parcels_path, show_col_types = FALSE))
need_cols <- c("ward_pair", "construction_year", "dist_to_boundary", "signed_distance", "arealotsf", "areabuilding", "unitscount", "density_far", "density_dupac")
missing_cols <- setdiff(need_cols, names(dat))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
}

dat[, ward_pair_id := normalize_pair_dash(ward_pair)]
dat[, construction_year := as.integer(construction_year)]
dat[, era := fifelse(
  construction_year < 2003, "1998_2002",
  fifelse(construction_year < 2015, "2003_2014",
    fifelse(construction_year < 2023, "2015_2023", "post_2023")
  )
)]

dat <- dat[
  arealotsf > 1 &
    areabuilding > 1 &
    construction_year >= 2006 &
    dist_to_boundary <= bw_ft &
    !is.na(ward_pair_id) &
    !is.na(construction_year) &
    !is.na(signed_distance) &
    unitscount > 0
]
dat[, side := as.integer(signed_distance > 0)]

flags <- fread(flags_path)
flags <- unique(flags[, .(
  ward_pair_id_dash = normalize_pair_dash(ward_pair_id_dash),
  era = as.character(era),
  drop_confound = as.logical(drop_confound)
)])

dat <- merge(
  dat,
  flags,
  by.x = c("ward_pair_id", "era"),
  by.y = c("ward_pair_id_dash", "era"),
  all.x = TRUE,
  sort = FALSE
)
dat[is.na(drop_confound), drop_confound := FALSE]

results <- list()
k <- 1L

for (sample_tag in c("all", "pruned")) {
  d_base <- copy(dat)
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
      n_pairs <- uniqueN(d$ward_pair_id)
      model <- NULL

      if (n_obs >= 25 && uniqueN(d$side) >= 2 && n_pairs >= 2) {
        model <- tryCatch(
          feols(
            outcome_val ~ side + signed_distance + side:signed_distance | ward_pair_id + construction_year,
            data = d,
            cluster = ~ward_pair_id
          ),
          error = function(e) NULL
        )
      }

      est <- extract_side_term(model)
      pdf_path <- file.path(out_dir, sprintf(
        "gm_uniform_construction_rd_bw%dft_%s_%s_%s.pdf",
        as.integer(bw_ft), transform_tag, outcome, sample_tag
      ))
      bins_path <- file.path(out_dir, sprintf(
        "gm_uniform_construction_rd_bw%dft_%s_%s_%s_bins.csv",
        as.integer(bw_ft), transform_tag, outcome, sample_tag
      ))
      fit_plot(d, model, sample_tag, transform_tag, outcome, pdf_path, bins_path, bw_ft)

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
        n_pairs = as.integer(n_pairs),
        plot_pdf = pdf_path,
        bins_csv = bins_path
      )
      k <- k + 1L
    }
  }
}

out_detail <- file.path(out_dir, sprintf("gm_uniform_construction_rd_bw%dft_detail.csv", as.integer(bw_ft)))
res_dt <- rbindlist(results, fill = TRUE)
setorder(res_dt, sample_tag, transform, outcome)
fwrite(res_dt, out_detail)

message("Saved:")
message(sprintf("  - %s", out_detail))
for (pp in res_dt$plot_pdf) message(sprintf("  - %s", pp))
