source("../../setup_environment/code/packages.R")
library(fixest)

standardize_uncertainty <- function(x) {
  x_sd <- sd(x, na.rm = TRUE)
  if (!is.finite(x_sd) || x_sd == 0) {
    return(rep(0, length(x)))
  }
  (x - mean(x, na.rm = TRUE)) / x_sd
}

default_uncertainty_config <- function() {
  list(
    permit_type_fe = TRUE,
    review_type_fe = TRUE,
    include_porch = TRUE,
    ca_fe = FALSE,
    two_stage = TRUE,
    stage2_weight = "N_PERMITS",
    volume_ctrl = "LAG1",
    volume_stage = "BOTH"
  )
}

variant_construction_rule <- function(variant_id) {
  dplyr::case_when(
    variant_id == "baseline" ~ "Baseline residualized score dropping share_bach_plus",
    variant_id == "raw_rank_days" ~ "Rank aldermen on raw mean processing_time",
    variant_id == "days_unlogged" ~ "Baseline score with unlogged processing_time in stage 1",
    variant_id == "reduced_ses" ~ "Baseline score dropping share_bach_plus and median_hh_income_10k",
    variant_id == "drop_bach" ~ "Baseline score dropping share_bach_plus",
    variant_id == "drop_bach_pop" ~ "Baseline score dropping share_bach_plus and pop_total_10k",
    TRUE ~ variant_id
  )
}

variant_display_label <- function(variant_id) {
  dplyr::case_when(
    variant_id == "baseline" ~ "Baseline",
    variant_id == "raw_rank_days" ~ "Raw mean-days rank",
    variant_id == "days_unlogged" ~ "Unlogged days",
    variant_id == "reduced_ses" ~ "Reduced SES controls",
    variant_id == "drop_bach" ~ "Drop bachelors",
    variant_id == "drop_bach_pop" ~ "Drop bachelors + pop",
    TRUE ~ variant_id
  )
}

build_uncertainty_output_suffix <- function(config, max_permit_year = NA_integer_) {
  output_suffix <- paste0(
    "ptfe", ifelse(config$permit_type_fe, "TRUE", "FALSE"),
    "_rtfe", ifelse(config$review_type_fe, "TRUE", "FALSE"),
    "_porch", ifelse(config$include_porch, "TRUE", "FALSE"),
    "_cafe", ifelse(config$ca_fe, "TRUE", "FALSE"),
    ifelse(config$two_stage, "_2stage", "")
  )

  if (config$two_stage && config$stage2_weight != "N_PERMITS") {
    output_suffix <- paste0(output_suffix, "_w", config$stage2_weight)
  }
  if (config$volume_ctrl != "NONE") {
    output_suffix <- paste0(
      output_suffix,
      "_vol", config$volume_ctrl, "_", config$volume_stage
    )
  }

  if (is.finite(max_permit_year)) {
    output_suffix <- paste0(output_suffix, "_through", as.integer(max_permit_year))
  }

  output_suffix
}

load_uncertainty_permits <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(month = as.yearmon(month))
}

prepare_uncertainty_sample <- function(
    permits,
    include_porch,
    volume_ctrl,
    volume_stage) {
  volume_ctrl <- toupper(volume_ctrl)
  volume_stage <- toupper(volume_stage)

  if (!volume_ctrl %in% c("NONE", "CURRENT", "LAG1")) {
    stop("volume_ctrl must be one of: NONE, CURRENT, LAG1", call. = FALSE)
  }
  if (!volume_stage %in% c("STAGE1", "STAGE2", "BOTH")) {
    stop("volume_stage must be one of: STAGE1, STAGE2, BOTH", call. = FALSE)
  }

  permits_prepared <- permits

  if (!include_porch) {
    permits_prepared <- permits_prepared %>% filter(!is_porch)
  }

  keep_aldermen <- permits_prepared %>%
    group_by(alderman) %>%
    summarise(n_months = n_distinct(month), .groups = "drop") %>%
    filter(n_months > 3) %>%
    pull(alderman)

  permits_prepared <- permits_prepared %>%
    filter(alderman %in% keep_aldermen) %>%
    mutate(
      ward = as.character(ward),
      month_date = as.Date(month)
    )

  wm_counts <- permits_prepared %>%
    count(ward, month_date, name = "n_permits_wm")

  all_months <- seq(min(wm_counts$month_date), max(wm_counts$month_date), by = "month")
  wm_grid <- expand_grid(
    ward = sort(unique(wm_counts$ward)),
    month_date = all_months
  ) %>%
    left_join(wm_counts, by = c("ward", "month_date")) %>%
    mutate(n_permits_wm = replace_na(n_permits_wm, 0L)) %>%
    group_by(ward) %>%
    arrange(month_date, .by_group = TRUE) %>%
    mutate(n_permits_wm_l1 = lag(n_permits_wm, 1)) %>%
    ungroup() %>%
    mutate(month = as.yearmon(month_date)) %>%
    select(ward, month, n_permits_wm, n_permits_wm_l1)

  permits_prepared <- permits_prepared %>%
    left_join(wm_grid, by = c("ward", "month")) %>%
    mutate(
      median_hh_income_10k = median_hh_income / 10000,
      pop_total_10k = pop_total / 10000
    )

  place_covariates <- c("dist_cbd_km", "dist_lake_km", "n_rail_stations_800m")
  if (!all(place_covariates %in% names(permits_prepared))) {
    place_covariates <- c("dist_cbd_km", "lakefront_share_1km", "n_rail_stations_800m")
  }

  volume_var <- dplyr::case_when(
    volume_ctrl == "CURRENT" ~ "n_permits_wm",
    volume_ctrl == "LAG1" ~ "n_permits_wm_l1",
    TRUE ~ ""
  )

  list(
    permits = permits_prepared,
    place_covariates = place_covariates,
    volume_var = volume_var,
    include_volume_stage1 = volume_ctrl != "NONE" && volume_stage %in% c("STAGE1", "BOTH"),
    include_volume_stage2 = volume_ctrl != "NONE" && volume_stage %in% c("STAGE2", "BOTH")
  )
}

get_stage1_covariates <- function(
    place_covariates,
    include_volume_stage1,
    volume_var,
    drop_covariates = character()) {
  covariates <- c(
    "median_hh_income_10k",
    "share_black",
    "share_hisp",
    "share_white",
    "homeownership_rate",
    "share_bach_plus",
    "pop_total_10k",
    place_covariates
  )

  if (include_volume_stage1) {
    covariates <- c(covariates, volume_var)
  }

  setdiff(covariates, drop_covariates)
}

get_stage1_fe_terms <- function(config) {
  fe_terms <- c("month")

  if (config$permit_type_fe) {
    fe_terms <- c(fe_terms, "permit_type_clean")
  }
  if (config$review_type_fe) {
    fe_terms <- c(fe_terms, "review_type_clean")
  }
  if (config$ca_fe) {
    fe_terms <- c(fe_terms, "ca_id")
  }

  fe_terms
}

extract_stage1_terms <- function(model, variant_id) {
  coef_tbl <- as.data.frame(coeftable(model))
  p_col <- grep("^Pr\\(", colnames(coef_tbl), value = TRUE)[1]

  tibble(
    variant_id = variant_id,
    term = rownames(coef_tbl),
    estimate = as.numeric(coef_tbl[, "Estimate"]),
    std_error = as.numeric(coef_tbl[, "Std. Error"]),
    p_value = as.numeric(coef_tbl[, p_col])
  )
}

fit_stage1_model <- function(permits, stage1_outcome, covariates, fe_terms, variant_id) {
  missing_cols <- setdiff(unique(c(stage1_outcome, covariates, fe_terms)), names(permits))
  if (length(missing_cols) > 0) {
    stop(
      paste("Missing columns for stage 1:", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  rhs_terms <- if (length(covariates) > 0) {
    paste(covariates, collapse = " + ")
  } else {
    "1"
  }
  reg_formula <- as.formula(paste0(
    stage1_outcome, " ~ ", rhs_terms, " | ", paste(fe_terms, collapse = " + ")
  ))

  permits_for_reg <- permits
  for (col in unique(c(stage1_outcome, covariates))) {
    permits_for_reg <- permits_for_reg %>%
      filter(!is.na(.data[[col]]) & is.finite(.data[[col]]))
  }
  for (col in fe_terms) {
    permits_for_reg <- permits_for_reg %>%
      filter(!is.na(.data[[col]]))
  }

  model <- feols(reg_formula, data = permits_for_reg, warn = FALSE)
  permits_for_reg <- permits_for_reg %>%
    mutate(
      stage1_fitted = as.numeric(predict(model, newdata = permits_for_reg)),
      resid = .data[[stage1_outcome]] - stage1_fitted
    )

  list(
    model = model,
    permits_for_reg = permits_for_reg,
    stage1_terms = extract_stage1_terms(model, variant_id),
    stage1_nobs = model$nobs,
    stage1_r2 = tryCatch(as.numeric(r2(model, type = "ar2")), error = function(e) NA_real_),
    stage1_outcome = stage1_outcome,
    covariates = covariates,
    fe_terms = fe_terms
  )
}

resolve_reference_alderman <- function(alderman_vec) {
  if ("Andre Vasquez" %in% alderman_vec) {
    return("Andre Vasquez")
  }
  names(sort(table(alderman_vec), decreasing = TRUE))[1]
}

build_two_stage_index <- function(
    permits_for_reg,
    include_volume_stage2,
    volume_var,
    stage2_weight) {
  ward_month_resid <- permits_for_reg %>%
    filter(!is.na(resid)) %>%
    group_by(ward, month, alderman) %>%
    summarise(
      mean_resid_wm = mean(resid, na.rm = TRUE),
      sd_resid_wm = sd(resid, na.rm = TRUE),
      var_resid_wm = var(resid, na.rm = TRUE),
      n_permits_wm = n(),
      n_permits_wm_l1 = first(n_permits_wm_l1),
      .groups = "drop"
    )

  ref_alderman <- resolve_reference_alderman(ward_month_resid$alderman)
  stage2_formula <- as.formula(paste0(
    "mean_resid_wm ~ i(alderman, ref = ref_alderman)",
    if (include_volume_stage2) paste0(" + ", volume_var) else ""
  ))

  stage2_data <- ward_month_resid
  weights_formula <- NULL
  if (stage2_weight == "N_PERMITS") {
    weights_formula <- ~n_permits_wm
  } else if (stage2_weight == "SQRT_N_PERMITS") {
    stage2_data <- stage2_data %>%
      mutate(stage2_weight = sqrt(n_permits_wm))
    weights_formula <- ~stage2_weight
  }

  stage2_model <- if (is.null(weights_formula)) {
    feols(stage2_formula, data = stage2_data, se = "hetero", warn = FALSE)
  } else {
    feols(
      stage2_formula,
      data = stage2_data,
      weights = weights_formula,
      se = "hetero",
      warn = FALSE
    )
  }

  coefs <- enframe(coef(stage2_model), name = "term", value = "alderman_fe")
  ses <- enframe(se(stage2_model), name = "term", value = "alderman_se")

  alderman_effects <- coefs %>%
    filter(str_detect(term, "^alderman::")) %>%
    mutate(alderman = str_remove(term, "^alderman::")) %>%
    left_join(
      ses %>%
        filter(str_detect(term, "^alderman::")) %>%
        mutate(alderman = str_remove(term, "^alderman::")) %>%
        select(alderman, alderman_se),
      by = "alderman"
    ) %>%
    select(alderman, alderman_fe, alderman_se)

  alderman_effects <- bind_rows(
    alderman_effects,
    tibble(alderman = ref_alderman, alderman_fe = 0, alderman_se = 0)
  )

  tau2 <- max(
    0,
    var(alderman_effects$alderman_fe, na.rm = TRUE) -
      mean(alderman_effects$alderman_se^2, na.rm = TRUE)
  )

  alderman_effects <- alderman_effects %>%
    mutate(
      shrinkage_B = tau2 / (tau2 + alderman_se^2),
      alderman_fe_shrunk = alderman_fe * shrinkage_B
    )

  permit_level_stats <- permits_for_reg %>%
    filter(!is.na(resid)) %>%
    group_by(alderman) %>%
    summarise(
      n_permits = n(),
      sd_resid = sd(resid, na.rm = TRUE),
      var_resid = var(resid, na.rm = TRUE),
      .groups = "drop"
    )

  alderman_index <- alderman_effects %>%
    transmute(
      alderman,
      mean_resid = alderman_fe_shrunk,
      alderman_fe_raw = alderman_fe,
      alderman_se,
      shrinkage_B
    ) %>%
    left_join(permit_level_stats, by = "alderman") %>%
    mutate(uncertainty_index = standardize_uncertainty(mean_resid)) %>%
    select(
      alderman,
      n_permits,
      mean_resid,
      alderman_fe_raw,
      alderman_se,
      shrinkage_B,
      uncertainty_index
    )

  list(
    alderman_index = alderman_index,
    stage2_model = stage2_model,
    stage2_nobs = stage2_model$nobs,
    stage2_r2 = tryCatch(as.numeric(r2(stage2_model, type = "ar2")), error = function(e) NA_real_)
  )
}

build_simple_index <- function(permits_for_reg) {
  alderman_index <- permits_for_reg %>%
    filter(!is.na(resid)) %>%
    group_by(alderman) %>%
    summarise(
      n_permits = n(),
      mean_resid = mean(resid, na.rm = TRUE),
      sd_resid = sd(resid, na.rm = TRUE),
      var_resid = var(resid, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(uncertainty_index = standardize_uncertainty(mean_resid)) %>%
    select(alderman, n_permits, mean_resid, uncertainty_index)

  list(
    alderman_index = alderman_index,
    stage2_model = NULL,
    stage2_nobs = NA_real_,
    stage2_r2 = NA_real_
  )
}

build_residualized_uncertainty_index <- function(
    permits,
    config,
    variant_id,
    stage1_outcome = "log_processing_time",
    drop_covariates = character(),
    construction_rule = variant_construction_rule(variant_id)) {
  prepared <- prepare_uncertainty_sample(
    permits,
    include_porch = config$include_porch,
    volume_ctrl = config$volume_ctrl,
    volume_stage = config$volume_stage
  )

  covariates <- get_stage1_covariates(
    prepared$place_covariates,
    prepared$include_volume_stage1,
    prepared$volume_var,
    drop_covariates
  )
  fe_terms <- get_stage1_fe_terms(config)

  stage1_result <- fit_stage1_model(
    permits = prepared$permits,
    stage1_outcome = stage1_outcome,
    covariates = covariates,
    fe_terms = fe_terms,
    variant_id = variant_id
  )

  index_result <- if (config$two_stage) {
    build_two_stage_index(
      permits_for_reg = stage1_result$permits_for_reg,
      include_volume_stage2 = prepared$include_volume_stage2,
      volume_var = prepared$volume_var,
      stage2_weight = config$stage2_weight
    )
  } else {
    build_simple_index(stage1_result$permits_for_reg)
  }

  metadata <- tibble(
    variant_id = variant_id,
    variant_label = variant_display_label(variant_id),
    construction_rule = construction_rule,
    n_aldermen = nrow(index_result$alderman_index),
    n_permits_used = sum(!is.na(stage1_result$permits_for_reg$resid)),
    stage1_outcome = stage1_outcome,
    stage1_nobs = stage1_result$stage1_nobs,
    stage1_r2 = stage1_result$stage1_r2,
    stage2_nobs = index_result$stage2_nobs,
    stage2_r2 = index_result$stage2_r2
  )

  list(
    alderman_index = index_result$alderman_index,
    metadata = metadata,
    stage1_terms = stage1_result$stage1_terms,
    stage1_model = stage1_result$model,
    stage2_model = index_result$stage2_model,
    covariates = covariates,
    fe_terms = fe_terms,
    stage1_outcome = stage1_outcome
  )
}

build_raw_rank_uncertainty_index <- function(
    permits,
    config = default_uncertainty_config(),
    variant_id = "raw_rank_days",
    construction_rule = variant_construction_rule(variant_id)) {
  prepared <- prepare_uncertainty_sample(
    permits,
    include_porch = config$include_porch,
    volume_ctrl = config$volume_ctrl,
    volume_stage = config$volume_stage
  )

  analysis_sample <- prepared$permits %>%
    filter(!is.na(alderman), is.finite(processing_time), processing_time > 0)

  alderman_index <- analysis_sample %>%
    group_by(alderman) %>%
    summarise(
      n_permits = n(),
      mean_processing_time_raw = mean(processing_time, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      raw_rank = rank(mean_processing_time_raw, ties.method = "average"),
      uncertainty_index = standardize_uncertainty(raw_rank)
    ) %>%
    select(alderman, n_permits, mean_processing_time_raw, raw_rank, uncertainty_index)

  list(
    alderman_index = alderman_index,
    metadata = tibble(
      variant_id = variant_id,
      variant_label = variant_display_label(variant_id),
      construction_rule = construction_rule,
      n_aldermen = nrow(alderman_index),
      n_permits_used = nrow(analysis_sample),
      stage1_outcome = NA_character_,
      stage1_nobs = NA_real_,
      stage1_r2 = NA_real_,
      stage2_nobs = NA_real_,
      stage2_r2 = NA_real_
    ),
    stage1_terms = tibble(
      variant_id = character(),
      term = character(),
      estimate = numeric(),
      std_error = numeric(),
      p_value = numeric()
    ),
    stage1_model = NULL,
    stage2_model = NULL,
    covariates = character(),
    fe_terms = character(),
    stage1_outcome = NA_character_
  )
}

write_stage1_regression_table <- function(model, output_path, stage1_outcome) {
  outcome_label <- if (stage1_outcome == "processing_time") {
    "Processing Time (days)"
  } else {
    "Log Processing Time"
  }
  etable(
    model,
    digits = 3,
    se.below = TRUE,
    depvar = FALSE,
    headers = c(outcome_label),
    dict = c(
      median_hh_income_10k = "Median HH Income ($10k)",
      share_black = "Share Black",
      share_hisp = "Share Hispanic",
      share_white = "Share White",
      homeownership_rate = "Homeownership Rate",
      share_bach_plus = "Share Bachelor's+",
      pop_total_10k = "Population (10k)",
      dist_cbd_km = "Dist. to CBD (km)",
      dist_lake_km = "Dist. to Lake (km)",
      lakefront_share_1km = "Lakefront Share",
      n_rail_stations_800m = "CTA Stations (800m)",
      n_permits_wm = "Current Permits",
      n_permits_wm_l1 = "Lag Permits"
    ),
    fitstat = ~ n + r2,
    file = output_path,
    replace = TRUE,
    style.tex = style.tex(
      main = "aer",
      model.format = "",
      fixef.title = "",
      fixef.suffix = "",
      yesNo = c("$\\checkmark$", "")
    )
  )

  table_tex <- readLines(output_path)
  obs_idx <- grep("^\\s*Observations\\s*&", table_tex)
  n_line <- if (length(obs_idx) == 1) {
    sub("^\\s*Observations", "   N", table_tex[obs_idx])
  } else {
    sprintf("   N & %s\\\\  ", format(nobs(model), big.mark = ","))
  }
  drop_idx <- c(
    obs_idx,
    grep("^\\s*R\\$\\^2\\$\\s*&", table_tex)
  )
  if (length(drop_idx) > 0) {
    table_tex <- table_tex[-drop_idx]
  }
  table_tex <- sub(
    "^\\s*month\\s*&",
    "   Year $\\\\times$ Month FE    &",
    table_tex
  )
  table_tex <- sub(
    "^\\s*permit\\\\_type\\\\_clean\\s*&",
    "   Permit Type FE            &",
    table_tex
  )
  table_tex <- sub(
    "^\\s*review\\\\_type\\\\_clean\\s*&",
    "   Review Type FE            &",
    table_tex
  )
  table_tex <- sub(
    "^\\s*ca\\\\_id\\s*&",
    "   Community Area FE         &",
    table_tex
  )
  bottom_idx <- grep("^\\s*\\\\bottomrule", table_tex)
  if (length(bottom_idx) == 1) {
    table_tex <- append(table_tex, n_line, after = bottom_idx - 1L)
  }
  writeLines(table_tex, output_path)
}

write_stage2_regression_table <- function(model, output_path) {
  etable(
    model,
    digits = 3,
    se.below = TRUE,
    depvar = FALSE,
    headers = c("Mean Residual"),
    fitstat = ~ n + r2,
    file = output_path,
    replace = TRUE,
    style.tex = style.tex(
      main = "aer",
      model.format = "",
      fixef.title = "",
      fixef.suffix = "",
      yesNo = c("$\\checkmark$", "")
    )
  )
}

write_uncertainty_plot <- function(score_df, output_path) {
  plot_data <- score_df %>%
    filter(!is.na(uncertainty_index)) %>%
    arrange(uncertainty_index) %>%
    mutate(alderman = factor(alderman, levels = alderman))

  p <- ggplot(plot_data, aes(x = uncertainty_index, y = alderman, fill = uncertainty_index)) +
    geom_col() +
    scale_fill_distiller(
      palette = "RdYlBu",
      direction = -1,
      name = "Stringency Index"
    ) +
    labs(
      title = "Alderman Stringency Index",
      x = "Stringency Index (standardized mean residual)",
      y = NULL
    ) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold")
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7)

  ggsave(
    output_path,
    plot = p,
    width = 11.5,
    height = 8,
    device = "pdf",
    bg = "white"
  )
}
