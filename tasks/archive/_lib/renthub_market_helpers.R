month_floor <- function(x) {
  as.Date(format(as.Date(x), "%Y-%m-01"))
}

month_end <- function(x) {
  x <- month_floor(x)
  as.Date(format(x + 32, "%Y-%m-01")) - 1
}

duck_escape <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

load_monthly_cpi_deflator <- function(cpi_input, start_month, end_month, base_year) {
  raw <- suppressWarnings(read_csv(cpi_input, show_col_types = FALSE, progress = FALSE))
  raw <- raw[, 1:2]
  setDT(raw)
  setnames(raw, names(raw), c("date", "cpi_value"))
  raw[, month_start := as.Date(date)]
  raw[, cpi_value := as.numeric(cpi_value)]
  raw <- raw[!is.na(month_start), .(month_start, cpi_value)]

  month_grid <- data.table(month_start = seq.Date(
    from = month_floor(start_month),
    to = month_floor(end_month),
    by = "month"
  ))
  cpi_dt <- merge(month_grid, raw, by = "month_start", all.x = TRUE, sort = TRUE)

  if (cpi_dt[!is.finite(cpi_value), .N] > 0) {
    idx_known <- which(is.finite(cpi_dt$cpi_value))
    if (length(idx_known) < 2) {
      stop("CPI input does not have enough non-missing observations to interpolate.", call. = FALSE)
    }
    interpolated <- approx(
      x = idx_known,
      y = cpi_dt$cpi_value[idx_known],
      xout = seq_len(nrow(cpi_dt)),
      method = "linear",
      rule = 2
    )$y
    cpi_dt[, cpi_value := fifelse(is.finite(cpi_value), cpi_value, interpolated)]
  }

  base_cpi <- cpi_dt[format(month_start, "%Y") == as.character(base_year), mean(cpi_value, na.rm = TRUE)]
  if (!is.finite(base_cpi) || base_cpi <= 0) {
    stop(sprintf("Unable to compute CPI base for %d.", base_year), call. = FALSE)
  }

  cpi_dt[, deflator_to_2024 := base_cpi / cpi_value]
  cpi_dt[]
}

load_trim_spec <- function(trim_summary_input) {
  if (is.na(trim_summary_input) || !nzchar(trim_summary_input) || !file.exists(trim_summary_input)) {
    return("strict_p1")
  }

  trim_summary <- suppressWarnings(fread(trim_summary_input))
  if (!"trim_spec" %in% names(trim_summary)) {
    return("strict_p1")
  }

  if ("is_chosen" %in% names(trim_summary)) {
    chosen <- trim_summary[is_chosen %in% c(TRUE, 1), trim_spec][1]
    if (length(chosen) == 1 && !is.na(chosen)) {
      return(chosen)
    }
  }

  if ("chosen_trim_spec" %in% names(trim_summary)) {
    chosen <- trim_summary$chosen_trim_spec[1]
    if (length(chosen) == 1 && !is.na(chosen)) {
      return(chosen)
    }
  }

  "strict_p1"
}

three_month_average <- function(x) {
  zoo::rollapplyr(x, width = 3, FUN = mean, fill = NA_real_, partial = FALSE)
}

compute_yoy_pct <- function(x) {
  100 * (x / data.table::shift(x, 12) - 1)
}

compute_log_growth_pct <- function(x) {
  100 * (log(x) - log(data.table::shift(x, 12)))
}

normalize_to_base_or_first <- function(dt, value_col, month_col, group_col, base_month) {
  out <- copy(dt)
  out[, `:=`(
    value_for_index = get(value_col),
    month_for_index = as.Date(get(month_col))
  )]
  out[
    ,
    normalization_month := {
      base_hits <- month_for_index[is.finite(value_for_index) & month_for_index == as.Date(base_month)]
      if (length(base_hits) > 0) {
        base_hits[1]
      } else {
        min(month_for_index[is.finite(value_for_index)], na.rm = TRUE)
      }
    },
    by = group_col
  ]
  out[
    ,
    normalization_value := value_for_index[month_for_index == normalization_month][1],
    by = group_col
  ]
  out[, index_value := 100 * value_for_index / normalization_value]
  out[, c("value_for_index", "month_for_index", "normalization_value") := NULL]
  out[]
}

two_way_repeat_index <- function(dt,
                                 value_col,
                                 key_col,
                                 month_col,
                                 base_month,
                                 min_repeats = 2L) {
  work_dt <- copy(dt)[is.finite(get(value_col))]
  work_dt[, month_value := as.Date(get(month_col))]
  work_dt[, key_value := as.character(get(key_col))]
  work_dt[, repeat_value := as.numeric(get(value_col))]
  work_dt <- work_dt[!is.na(month_value) & !is.na(key_value)]

  key_support <- work_dt[, .(n_months = uniqueN(month_value)), by = key_value]
  keep_keys <- key_support[n_months >= min_repeats, key_value]
  work_dt <- work_dt[key_value %in% keep_keys]
  if (nrow(work_dt) == 0) {
    stop("Repeat-index sample is empty after restricting to repeated keys.", call. = FALSE)
  }

  month_levels <- sort(unique(work_dt$month_value))
  base_month <- as.Date(base_month)
  normalization_month <- if (base_month %in% month_levels) base_month else min(month_levels)
  work_dt[, month_label := as.character(month_value)]
  ref_label <- as.character(normalization_month)

  repeat_fit <- fixest::feols(
    repeat_value ~ i(month_label, ref = ref_label) | key_value,
    data = work_dt,
    lean = TRUE,
    warn = FALSE
  )
  month_terms <- stats::coef(repeat_fit)

  month_dt <- data.table(
    month_value = month_levels,
    month_label = as.character(month_levels),
    month_effect = 0,
    normalization_month = normalization_month
  )
  month_dt[month_label != ref_label, month_effect := month_terms[paste0("month_label::", month_label)]]
  month_dt[, index_value := 100 * exp(month_effect)]
  month_dt[, month_label := NULL]
  month_dt[]
}

support_rule_classification <- function(current_n, prior_n) {
  support_n <- pmin(current_n, prior_n)
  fifelse(
    !is.finite(support_n) | support_n < 30,
    "suppressed",
    fifelse(support_n >= 75, "raw", "shrunk")
  )
}
