source("../../setup_environment/code/packages.R")

library(arrow)
library(data.table)
library(sf)

sf_use_s2(FALSE)

sales_path <- "../input/sales_with_hedonics.parquet"
rent_path <- "../input/rent_with_ward_distances_full.parquet"
segment_gpkg <- "../input/boundary_segments_1320ft.gpkg"
flags_path <- "../input/confounded_pair_era_flags.csv"
out_dir <- "../output"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

stopifnot(
  file.exists(sales_path),
  file.exists(rent_path),
  file.exists(segment_gpkg),
  file.exists(flags_path)
)

out_cov_sales <- file.path(out_dir, "segment_assignment_coverage_sales.csv")
out_cov_rent <- file.path(out_dir, "segment_assignment_coverage_rental.csv")

out_sales_all <- file.path(out_dir, "segment_fe_results_sales_all.csv")
out_sales_pruned <- file.path(out_dir, "segment_fe_results_sales_pruned.csv")
out_rent_all <- file.path(out_dir, "segment_fe_results_rental_all.csv")
out_rent_pruned <- file.path(out_dir, "segment_fe_results_rental_pruned.csv")

out_sales_all_tex <- file.path(out_dir, "segment_fe_table_sales_all.tex")
out_sales_pruned_tex <- file.path(out_dir, "segment_fe_table_sales_pruned.tex")
out_rent_all_tex <- file.path(out_dir, "segment_fe_table_rental_all.tex")
out_rent_pruned_tex <- file.path(out_dir, "segment_fe_table_rental_pruned.tex")

out_summary <- file.path(out_dir, "segment_fe_comparison_summary.md")

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
    if (!is.finite(a) || !is.finite(b)) {
      return(NA_character_)
    }
    paste(min(a, b), max(a, b), sep = "-")
  }, character(1))
}

star_code <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

fmt_num <- function(x, digits = 4) {
  ifelse(is.finite(x), format(round(x, digits), nsmall = digits, trim = TRUE), "NA")
}

load_segment_layer <- function(gpkg, layer_name, era_label) {
  seg <- st_read(gpkg, layer = layer_name, quiet = TRUE)
  if (!all(c("segment_id", "ward_pair_id") %in% names(seg))) {
    stop(sprintf("Layer %s missing required columns.", layer_name), call. = FALSE)
  }
  seg <- seg[, c("segment_id", "ward_pair_id"), drop = FALSE]
  seg <- st_make_valid(seg)
  seg$era <- era_label
  seg$ward_pair_id_dash <- normalize_pair_dash(seg$ward_pair_id)
  seg
}

segments_by_era <- list(
  "2003_2014" = load_segment_layer(segment_gpkg, "2003_2014_bw1000", "2003_2014"),
  "2015_2023" = load_segment_layer(segment_gpkg, "2015_2023_bw1000", "2015_2023"),
  "post_2023" = load_segment_layer(segment_gpkg, "post_2023_bw1000", "post_2023")
)

assign_segments <- function(dt, segments_map, dataset_label, chunk_n = 80000L) {
  dt <- copy(dt)
  dt[, row_id := .I]

  assign_parts <- list()
  coverage_parts <- list()
  assign_idx <- 1L
  cov_idx <- 1L

  eras <- sort(unique(dt$era))
  for (era_i in eras) {
    d_era <- dt[era == era_i]
    seg_era <- segments_map[[era_i]]

    if (nrow(d_era) == 0) {
      next
    }

    if (is.null(seg_era) || nrow(seg_era) == 0) {
      coverage_parts[[cov_idx]] <- data.table(
        dataset = dataset_label,
        era = era_i,
        total_obs = nrow(d_era),
        matched_obs = 0L,
        coverage_rate = 0,
        total_pairs = uniqueN(d_era$ward_pair_id),
        matched_pairs = 0L
      )
      cov_idx <- cov_idx + 1L
      next
    }

    valid_pairs <- intersect(unique(d_era$ward_pair_id), unique(seg_era$ward_pair_id_dash))
    era_assign_parts <- list()

    for (pair_j in seq_along(valid_pairs)) {
      pair_id <- valid_pairs[pair_j]
      seg_pair <- seg_era[seg_era$ward_pair_id_dash == pair_id, c("segment_id", "ward_pair_id_dash")]
      idx_pair <- which(d_era$ward_pair_id == pair_id)
      if (length(idx_pair) == 0 || nrow(seg_pair) == 0) {
        next
      }

      starts <- seq(1L, length(idx_pair), by = chunk_n)
      for (s in starts) {
        e <- min(s + chunk_n - 1L, length(idx_pair))
        chunk_idx <- idx_pair[s:e]
        chunk <- d_era[chunk_idx, .(row_id, longitude, latitude)]

        pts <- st_as_sf(chunk, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
        pts <- suppressWarnings(st_transform(pts, st_crs(seg_pair)))
        hits <- st_within(pts, seg_pair)
        hit_idx <- vapply(hits, function(v) if (length(v) == 0) NA_integer_ else v[1], integer(1))

        seg_id <- rep(NA_character_, length(hit_idx))
        ok <- !is.na(hit_idx)
        if (any(ok)) {
          seg_id[ok] <- as.character(seg_pair$segment_id[hit_idx[ok]])
        }
        era_assign_parts[[length(era_assign_parts) + 1L]] <- data.table(
          row_id = chunk$row_id,
          segment_id = seg_id
        )
      }

      if (pair_j %% 25L == 0L) {
        message(sprintf("[%s] era=%s processed pairs: %d / %d", dataset_label, era_i, pair_j, length(valid_pairs)))
      }
    }

    era_assign <- if (length(era_assign_parts) > 0) rbindlist(era_assign_parts, fill = TRUE) else data.table(row_id = integer(), segment_id = character())
    matched_ids <- era_assign[!is.na(segment_id), unique(row_id)]

    coverage_parts[[cov_idx]] <- data.table(
      dataset = dataset_label,
      era = era_i,
      total_obs = nrow(d_era),
      matched_obs = length(matched_ids),
      coverage_rate = length(matched_ids) / max(nrow(d_era), 1L),
      total_pairs = uniqueN(d_era$ward_pair_id),
      matched_pairs = uniqueN(d_era[row_id %in% matched_ids, ward_pair_id])
    )
    cov_idx <- cov_idx + 1L
    assign_parts[[assign_idx]] <- era_assign
    assign_idx <- assign_idx + 1L
  }

  assignments <- if (length(assign_parts) > 0) rbindlist(assign_parts, fill = TRUE) else data.table(row_id = integer(), segment_id = character())
  assignments <- unique(assignments, by = "row_id")
  dt <- merge(dt, assignments, by = "row_id", all.x = TRUE, sort = FALSE)
  dt[, row_id := NULL]

  coverage <- if (length(coverage_parts) > 0) rbindlist(coverage_parts, fill = TRUE) else data.table(
    dataset = dataset_label, era = character(), total_obs = integer(), matched_obs = integer(),
    coverage_rate = numeric(), total_pairs = integer(), matched_pairs = integer()
  )
  setorder(coverage, era)

  list(data = dt, coverage = coverage)
}

extract_coef <- function(model, term) {
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
  out$n_obs <- as.integer(model$nobs)
  if (is.null(ct) || !term %in% rownames(ct)) {
    return(out)
  }
  p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)[1]
  t_col <- grep("t value", colnames(ct), value = TRUE)[1]
  out$estimate <- as.numeric(ct[term, "Estimate"])
  out$std_error <- as.numeric(ct[term, "Std. Error"])
  out$p_value <- if (!is.na(p_col)) as.numeric(ct[term, p_col]) else NA_real_
  out$t_value <- if (!is.na(t_col)) as.numeric(ct[term, t_col]) else NA_real_
  out
}

result_row <- function(dataset, sample_tag, specification, coef_info, n_obs_input, n_segments, n_pairs) {
  data.table(
    dataset = dataset,
    sample_tag = sample_tag,
    specification = specification,
    estimate = coef_info$estimate,
    std_error = coef_info$std_error,
    p_value = coef_info$p_value,
    t_value = coef_info$t_value,
    n_obs = as.integer(ifelse(is.finite(coef_info$n_obs), coef_info$n_obs, n_obs_input)),
    n_segments = as.integer(n_segments),
    n_pairs = as.integer(n_pairs)
  )
}

run_sales_models <- function(dt, sample_tag) {
  if (nrow(dt) == 0) {
    return(data.table(
      dataset = "sales",
      sample_tag = sample_tag,
      specification = c("baseline", "hedonic"),
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      t_value = NA_real_,
      n_obs = 0L,
      n_segments = 0L,
      n_pairs = 0L
    ))
  }

  d <- copy(dt)
  sd_strict <- sd(d$strictness_own, na.rm = TRUE)
  if (!is.finite(sd_strict) || sd_strict <= 0) {
    return(data.table(
      dataset = "sales",
      sample_tag = sample_tag,
      specification = c("baseline", "hedonic"),
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      t_value = NA_real_,
      n_obs = c(nrow(d), nrow(d)),
      n_segments = c(uniqueN(d$segment_id), uniqueN(d$segment_id)),
      n_pairs = c(uniqueN(d$ward_pair_id), uniqueN(d$ward_pair_id))
    ))
  }
  d[, strictness_std := strictness_own / sd_strict]

  base_data <- d[
    is.finite(sale_price) & sale_price > 0 &
      !is.na(segment_id) & !is.na(year) & !is.na(ward_pair_id)
  ]
  model_base <- NULL
  if (nrow(base_data) >= 25 && uniqueN(base_data$segment_id) >= 2) {
    model_base <- tryCatch(
      feols(log(sale_price) ~ strictness_std | segment_id + year, data = base_data, cluster = ~ward_pair_id),
      error = function(e) NULL
    )
  }

  hed_data <- d[
    is.finite(sale_price) & sale_price > 0 &
      !is.na(log_sqft) & !is.na(log_land_sqft) & !is.na(log_building_age) &
      !is.na(log_bedrooms) & !is.na(log_baths) & !is.na(has_garage) &
      !is.na(segment_id) & !is.na(year) & !is.na(ward_pair_id)
  ]
  model_hed <- NULL
  if (nrow(hed_data) >= 25 && uniqueN(hed_data$segment_id) >= 2) {
    model_hed <- tryCatch(
      feols(
        log(sale_price) ~ strictness_std + log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage |
          segment_id + year,
        data = hed_data,
        cluster = ~ward_pair_id
      ),
      error = function(e) NULL
    )
  }

  out <- rbindlist(list(
    result_row(
      "sales", sample_tag, "baseline",
      extract_coef(model_base, "strictness_std"),
      nrow(base_data), uniqueN(base_data$segment_id), uniqueN(base_data$ward_pair_id)
    ),
    result_row(
      "sales", sample_tag, "hedonic",
      extract_coef(model_hed, "strictness_std"),
      nrow(hed_data), uniqueN(hed_data$segment_id), uniqueN(hed_data$ward_pair_id)
    )
  ))
  out
}

run_rental_models <- function(dt, sample_tag) {
  if (nrow(dt) == 0) {
    return(data.table(
      dataset = "rental",
      sample_tag = sample_tag,
      specification = c("baseline", "hedonic"),
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      t_value = NA_real_,
      n_obs = 0L,
      n_segments = 0L,
      n_pairs = 0L
    ))
  }

  d <- copy(dt)
  sd_strict <- sd(d$strictness_own, na.rm = TRUE)
  if (!is.finite(sd_strict) || sd_strict <= 0) {
    return(data.table(
      dataset = "rental",
      sample_tag = sample_tag,
      specification = c("baseline", "hedonic"),
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      t_value = NA_real_,
      n_obs = c(nrow(d), nrow(d)),
      n_segments = c(uniqueN(d$segment_id), uniqueN(d$segment_id)),
      n_pairs = c(uniqueN(d$ward_pair_id), uniqueN(d$ward_pair_id))
    ))
  }
  d[, strictness_std := strictness_own / sd_strict]

  base_data <- d[
    is.finite(rent_price) & rent_price > 0 &
      !is.na(segment_id) & !is.na(year_month) & !is.na(ward_pair_id)
  ]
  model_base <- NULL
  if (nrow(base_data) >= 25 && uniqueN(base_data$segment_id) >= 2) {
    model_base <- tryCatch(
      feols(log(rent_price) ~ strictness_std | segment_id + year_month, data = base_data, cluster = ~ward_pair_id),
      error = function(e) NULL
    )
  }

  hed_data <- d[
    is.finite(rent_price) & rent_price > 0 &
      !is.na(log_sqft) & !is.na(log_beds) & !is.na(log_baths) &
      !is.na(segment_id) & !is.na(year_month) & !is.na(ward_pair_id)
  ]
  hed_data[, building_type_factor := factor(fifelse(
    is.na(building_type_clean) | building_type_clean == "",
    "missing",
    as.character(building_type_clean)
  ))]
  include_btype <- uniqueN(hed_data$building_type_factor) >= 2

  model_hed <- NULL
  if (nrow(hed_data) >= 25 && uniqueN(hed_data$segment_id) >= 2) {
    hed_rhs <- if (include_btype) {
      "strictness_std + log_sqft + log_beds + log_baths + building_type_factor"
    } else {
      "strictness_std + log_sqft + log_beds + log_baths"
    }
    hed_fml <- as.formula(paste0("log(rent_price) ~ ", hed_rhs, " | segment_id + year_month"))
    model_hed <- tryCatch(
      feols(hed_fml, data = hed_data, cluster = ~ward_pair_id),
      error = function(e) NULL
    )
  }

  out <- rbindlist(list(
    result_row(
      "rental", sample_tag, "baseline",
      extract_coef(model_base, "strictness_std"),
      nrow(base_data), uniqueN(base_data$segment_id), uniqueN(base_data$ward_pair_id)
    ),
    result_row(
      "rental", sample_tag, "hedonic",
      extract_coef(model_hed, "strictness_std"),
      nrow(hed_data), uniqueN(hed_data$segment_id), uniqueN(hed_data$ward_pair_id)
    )
  ))
  out
}

write_result_tex <- function(res, path_tex, title) {
  lines <- c(
    "\\begin{table}[H]",
    "\\centering",
    sprintf("\\caption{%s}", title),
    "\\begin{tabular}{lrrrrrr}",
    "\\hline",
    "Specification & Estimate & Std. Error & p-value & N & Segments & Pairs \\\\",
    "\\hline"
  )

  for (i in seq_len(nrow(res))) {
    rr <- res[i]
    est_str <- ifelse(is.finite(rr$estimate), sprintf("%s%s", fmt_num(rr$estimate), star_code(rr$p_value)), "NA")
    se_str <- fmt_num(rr$std_error)
    p_str <- fmt_num(rr$p_value, digits = 3)
    lines <- c(
      lines,
      sprintf(
        "%s & %s & %s & %s & %s & %s & %s \\\\",
        rr$specification,
        est_str,
        se_str,
        p_str,
        format(as.integer(rr$n_obs), big.mark = ","),
        format(as.integer(rr$n_segments), big.mark = ","),
        format(as.integer(rr$n_pairs), big.mark = ",")
      )
    )
  }

  lines <- c(
    lines,
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )
  writeLines(lines, path_tex)
}

message("Loading confound flags...")
flags <- fread(flags_path)
if (!all(c("ward_pair_id_dash", "era", "drop_confound") %in% names(flags))) {
  stop("confounded_pair_era_flags.csv missing required columns.", call. = FALSE)
}
flags <- unique(flags[, .(
  ward_pair_id_dash = normalize_pair_dash(ward_pair_id_dash),
  era = as.character(era),
  drop_confound = as.logical(drop_confound)
)])

message("Loading sales microdata...")
sales <- as.data.table(read_parquet(
  sales_path,
  col_select = c(
    "sale_date", "year", "sale_price", "ward_pair_id", "signed_dist",
    "strictness_own", "log_sqft", "log_land_sqft", "log_building_age",
    "log_bedrooms", "log_baths", "has_garage", "longitude", "latitude"
  )
))
sales[, sale_date := as.Date(sale_date)]
sales[, year := as.integer(year)]
sales[is.na(year), year := as.integer(format(sale_date, "%Y"))]
sales[, ward_pair_id := normalize_pair_dash(ward_pair_id)]
sales <- sales[
  !is.na(sale_date) &
    !is.na(year) &
    !is.na(ward_pair_id) &
    !is.na(longitude) &
    !is.na(latitude) &
    is.finite(signed_dist) &
    abs(signed_dist) <= 1000 &
    is.finite(sale_price) & sale_price > 0 &
    is.finite(strictness_own)
]
sales[, era := fifelse(
  sale_date < d_2003, "1998_2002",
  fifelse(sale_date < d_2015, "2003_2014",
    fifelse(sale_date < d_2023, "2015_2023", "post_2023")
  )
)]

message("Assigning sales observations to segment polygons...")
sales_assign <- assign_segments(sales, segments_by_era, "sales", chunk_n = 50000L)
sales_cov <- sales_assign$coverage
sales_dt <- sales_assign$data
fwrite(sales_cov, out_cov_sales)

message("Loading rental microdata...")
rental <- as.data.table(read_parquet(
  rent_path,
  col_select = c(
    "file_date", "rent_price", "ward_pair_id", "signed_dist", "strictness_own",
    "sqft", "beds", "baths", "building_type_clean", "longitude", "latitude"
  )
))
rental[, file_date := as.Date(file_date)]
rental[, year := as.integer(format(file_date, "%Y"))]
rental[, year_month := format(file_date, "%Y-%m")]
rental[, ward_pair_id := normalize_pair_dash(ward_pair_id)]
rental <- rental[
  !is.na(file_date) &
    !is.na(year) &
    year <= 2022 &
    !is.na(ward_pair_id) &
    !is.na(longitude) &
    !is.na(latitude) &
    is.finite(signed_dist) &
    abs(signed_dist) <= 1000 &
    is.finite(rent_price) & rent_price > 0 &
    is.finite(strictness_own)
]
rental[, log_sqft := fifelse(is.finite(sqft) & sqft > 0, log(sqft), NA_real_)]
rental[, log_beds := fifelse(is.finite(beds) & beds > 0, log(beds), NA_real_)]
rental[, log_baths := fifelse(is.finite(baths) & baths > 0, log(baths), NA_real_)]
rental[, era := fifelse(
  file_date < d_2015, "2003_2014",
  fifelse(file_date < d_2023, "2015_2023", "post_2023")
)]

message("Assigning rental observations to segment polygons...")
rental_assign <- assign_segments(rental, segments_by_era, "rental", chunk_n = 80000L)
rental_cov <- rental_assign$coverage
rental_dt <- rental_assign$data
fwrite(rental_cov, out_cov_rent)

sales_dt <- merge(
  sales_dt,
  flags,
  by.x = c("ward_pair_id", "era"),
  by.y = c("ward_pair_id_dash", "era"),
  all.x = TRUE,
  sort = FALSE
)
sales_dt[is.na(drop_confound), drop_confound := FALSE]

rental_dt <- merge(
  rental_dt,
  flags,
  by.x = c("ward_pair_id", "era"),
  by.y = c("ward_pair_id_dash", "era"),
  all.x = TRUE,
  sort = FALSE
)
rental_dt[is.na(drop_confound), drop_confound := FALSE]

sales_all_dt <- sales_dt[!is.na(segment_id)]
sales_pruned_dt <- sales_all_dt[drop_confound == FALSE]
rental_all_dt <- rental_dt[!is.na(segment_id)]
rental_pruned_dt <- rental_all_dt[drop_confound == FALSE]

message("Running sales segment FE models...")
sales_all_res <- run_sales_models(sales_all_dt, "all")
sales_pruned_res <- run_sales_models(sales_pruned_dt, "pruned")
setorder(sales_all_res, specification)
setorder(sales_pruned_res, specification)
fwrite(sales_all_res, out_sales_all)
fwrite(sales_pruned_res, out_sales_pruned)

message("Running rental segment FE models...")
rental_all_res <- run_rental_models(rental_all_dt, "all")
rental_pruned_res <- run_rental_models(rental_pruned_dt, "pruned")
setorder(rental_all_res, specification)
setorder(rental_pruned_res, specification)
fwrite(rental_all_res, out_rent_all)
fwrite(rental_pruned_res, out_rent_pruned)

write_result_tex(sales_all_res, out_sales_all_tex, "Segment FE exploration: sales (all sample)")
write_result_tex(sales_pruned_res, out_sales_pruned_tex, "Segment FE exploration: sales (pruned sample)")
write_result_tex(rental_all_res, out_rent_all_tex, "Segment FE exploration: rental (all sample)")
write_result_tex(rental_pruned_res, out_rent_pruned_tex, "Segment FE exploration: rental (pruned sample)")

merge_compare <- function(all_dt, pruned_dt) {
  a <- copy(all_dt)
  p <- copy(pruned_dt)
  setnames(a, c("estimate", "std_error", "p_value", "n_obs", "n_segments", "n_pairs"), c("estimate_all", "std_error_all", "p_value_all", "n_obs_all", "n_segments_all", "n_pairs_all"))
  setnames(p, c("estimate", "std_error", "p_value", "n_obs", "n_segments", "n_pairs"), c("estimate_pruned", "std_error_pruned", "p_value_pruned", "n_obs_pruned", "n_segments_pruned", "n_pairs_pruned"))
  out <- merge(
    a[, .(dataset, specification, estimate_all, std_error_all, p_value_all, n_obs_all, n_segments_all, n_pairs_all)],
    p[, .(dataset, specification, estimate_pruned, std_error_pruned, p_value_pruned, n_obs_pruned, n_segments_pruned, n_pairs_pruned)],
    by = c("dataset", "specification"),
    all = TRUE
  )
  out
}

comp_sales <- merge_compare(sales_all_res, sales_pruned_res)
comp_rent <- merge_compare(rental_all_res, rental_pruned_res)
comparison <- rbindlist(list(comp_sales, comp_rent), fill = TRUE)
setorder(comparison, dataset, specification)

cov_all <- rbindlist(list(sales_cov, rental_cov), fill = TRUE)
low_cov <- cov_all[coverage_rate < 0.80]

lines <- c(
  "# Segment FE Comparison Summary",
  "",
  sprintf("- generated: %s", as.character(Sys.time())),
  "",
  "## Segment Assignment Coverage",
  "",
  "| Dataset | Era | Total Obs | Matched Obs | Coverage | Total Pairs | Matched Pairs |",
  "|---|---|---:|---:|---:|---:|---:|"
)

for (i in seq_len(nrow(cov_all))) {
  rr <- cov_all[i]
  lines <- c(
    lines,
    sprintf(
      "| %s | %s | %s | %s | %.1f%% | %s | %s |",
      rr$dataset,
      rr$era,
      format(as.integer(rr$total_obs), big.mark = ","),
      format(as.integer(rr$matched_obs), big.mark = ","),
      100 * rr$coverage_rate,
      format(as.integer(rr$total_pairs), big.mark = ","),
      format(as.integer(rr$matched_pairs), big.mark = ",")
    )
  )
}

if (nrow(low_cov) > 0) {
  lines <- c(
    lines,
    "",
    "## Coverage Warning",
    "- One or more dataset-era cells are below 80% segment assignment coverage."
  )
} else {
  lines <- c(
    lines,
    "",
    "## Coverage Warning",
    "- No dataset-era cell is below 80% segment assignment coverage."
  )
}

lines <- c(
  lines,
  "",
  "## FE Coefficient Comparison (All vs Pruned)",
  "",
  "| Dataset | Specification | All: Estimate (SE) | Pruned: Estimate (SE) | All N | Pruned N |",
  "|---|---|---:|---:|---:|---:|"
)

for (i in seq_len(nrow(comparison))) {
  rr <- comparison[i]
  left <- ifelse(
    is.finite(rr$estimate_all),
    sprintf("%s%s (%s)", fmt_num(rr$estimate_all), star_code(rr$p_value_all), fmt_num(rr$std_error_all)),
    "NA"
  )
  right <- ifelse(
    is.finite(rr$estimate_pruned),
    sprintf("%s%s (%s)", fmt_num(rr$estimate_pruned), star_code(rr$p_value_pruned), fmt_num(rr$std_error_pruned)),
    "NA"
  )
  lines <- c(
    lines,
    sprintf(
      "| %s | %s | %s | %s | %s | %s |",
      rr$dataset,
      rr$specification,
      left,
      right,
      format(as.integer(rr$n_obs_all), big.mark = ","),
      format(as.integer(rr$n_obs_pruned), big.mark = ",")
    )
  )
}

writeLines(lines, out_summary)

message("Saved:")
message(sprintf("  - %s", out_cov_sales))
message(sprintf("  - %s", out_cov_rent))
message(sprintf("  - %s", out_sales_all))
message(sprintf("  - %s", out_sales_pruned))
message(sprintf("  - %s", out_rent_all))
message(sprintf("  - %s", out_rent_pruned))
message(sprintf("  - %s", out_sales_all_tex))
message(sprintf("  - %s", out_sales_pruned_tex))
message(sprintf("  - %s", out_rent_all_tex))
message(sprintf("  - %s", out_rent_pruned_tex))
message(sprintf("  - %s", out_summary))
