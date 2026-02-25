source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/diagnose_fe_rd_leverage/code")
# input_file <- "../input/parcels_with_ward_distances.csv"
# output_model_summary <- "../output/fe_rd_model_summary.csv"
# output_pair_loo <- "../output/fe_rd_alderman_pair_loo.csv"
# output_project_influence <- "../output/fe_rd_project_influence.csv"
# output_pair_gap <- "../output/fe_rd_pair_gap_summary.csv"
# output_top_pairs_far_pdf <- "../output/fe_rd_top_pairs_density_far.pdf"
# output_top_pairs_dupac_pdf <- "../output/fe_rd_top_pairs_density_dupac.pdf"
# output_top_projects_far_pdf <- "../output/fe_rd_top_projects_density_far.pdf"
# output_top_projects_dupac_pdf <- "../output/fe_rd_top_projects_density_dupac.pdf"
# bw_ft <- 500
# frameworks <- c("border_fe", "spatial_rd_fe")
# samples <- c("all", "multifamily")
# specs <- c("pair_x_year", "pair_year", "zone_pair_year_additive")
# outcomes <- c("density_far", "density_dupac")
# source("diagnose_fe_rd_leverage.R")
# =======================================================================================

input_file <- "../input/parcels_with_ward_distances.csv"
output_model_summary <- "../output/fe_rd_model_summary.csv"
output_pair_loo <- "../output/fe_rd_alderman_pair_loo.csv"
output_project_influence <- "../output/fe_rd_project_influence.csv"
output_pair_gap <- "../output/fe_rd_pair_gap_summary.csv"
output_top_pairs_far_pdf <- "../output/fe_rd_top_pairs_density_far.pdf"
output_top_pairs_dupac_pdf <- "../output/fe_rd_top_pairs_density_dupac.pdf"
output_top_projects_far_pdf <- "../output/fe_rd_top_projects_density_far.pdf"
output_top_projects_dupac_pdf <- "../output/fe_rd_top_projects_density_dupac.pdf"

bw_ft <- 500
frameworks <- c("border_fe", "spatial_rd_fe")
samples <- c("all", "multifamily")
specs <- c("pair_x_year", "pair_year", "zone_pair_year_additive")
outcomes <- c("density_far", "density_dupac")

fe_formula_by_spec <- list(
  pair_x_year = "ward_pair^construction_year",
  pair_year = "ward_pair + construction_year",
  zone_pair_year_additive = "zone_code + ward_pair + construction_year"
)

controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  stats::reorder(paste(x, within, sep = sep), by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  scale_x_discrete(labels = function(x) gsub(paste0(sep, ".*$"), "", x), ...)
}

extract_coef_info <- function(model, coef_name) {
  ct <- coeftable(model)
  ct_cols <- colnames(ct)

  scalar_or_na <- function(x) {
    if (length(x) == 0) {
      return(NA_real_)
    }
    as.numeric(x[[1]])
  }

  if (!(coef_name %in% rownames(ct))) {
    return(tibble(
      beta = NA_real_,
      se = NA_real_,
      t_value = NA_real_,
      p_value = NA_real_,
      coef_present = FALSE
    ))
  }

  est_col <- if ("Estimate" %in% ct_cols) "Estimate" else ct_cols[1]
  se_col <- if ("Std. Error" %in% ct_cols) "Std. Error" else ct_cols[2]
  t_col <- if ("t value" %in% ct_cols) "t value" else if ("z value" %in% ct_cols) "z value" else ct_cols[3]
  p_col <- if ("Pr(>|t|)" %in% ct_cols) "Pr(>|t|)" else if ("Pr(>|z|)" %in% ct_cols) "Pr(>|z|)" else ct_cols[4]

  tibble(
    beta = scalar_or_na(ct[coef_name, est_col]),
    se = scalar_or_na(ct[coef_name, se_col]),
    t_value = scalar_or_na(ct[coef_name, t_col]),
    p_value = scalar_or_na(ct[coef_name, p_col]),
    coef_present = TRUE
  )
}

get_kept_index <- function(model, n_rows) {
  removed <- model$obs_selection$obsRemoved
  if (is.null(removed)) {
    return(seq_len(n_rows))
  }

  removed <- abs(as.integer(removed))
  removed <- removed[is.finite(removed) & removed >= 1 & removed <= n_rows]
  setdiff(seq_len(n_rows), removed)
}

get_model_sample <- function(model, data) {
  data[get_kept_index(model, nrow(data)), , drop = FALSE]
}

fit_clustered <- function(formula_obj, data) {
  tryCatch(
    feols(formula_obj, data = data, cluster = ~ward_pair),
    error = function(e) structure(list(message = conditionMessage(e)), class = "fit_error")
  )
}

fit_unclustered <- function(formula_obj, data) {
  tryCatch(
    feols(formula_obj, data = data),
    error = function(e) structure(list(message = conditionMessage(e)), class = "fit_error")
  )
}

is_fit_error <- function(x) inherits(x, "fit_error")

safe_divide <- function(x, y) {
  ifelse(is.finite(x) & is.finite(y) & y != 0, x / y, NA_real_)
}

pair_from_names <- function(a, b) {
  ifelse(
    is.na(a) | is.na(b),
    NA_character_,
    paste(pmin(a, b), pmax(a, b), sep = " / ")
  )
}

build_analysis_data <- function(data, framework, sample_filter, spec, outcome, bw_ft) {
  df <- data %>%
    filter(
      arealotsf > 1,
      areabuilding > 1,
      construction_year >= 2006,
      dist_to_boundary <= bw_ft
    )

  if (sample_filter == "all") {
    df <- df %>% filter(unitscount > 0)
  } else if (sample_filter == "multifamily") {
    df <- df %>% filter(unitscount > 1)
  }

  if (framework == "spatial_rd_fe") {
    df <- df %>%
      filter(!is.na(ward_pair), !is.na(construction_year))
  }

  if (framework == "spatial_rd_fe" && spec == "zone_pair_year_additive") {
    df <- df %>% filter(!is.na(zone_code))
  }

  df %>%
    filter(
      is.finite(.data[[outcome]]),
      .data[[outcome]] > 0
    )
}

build_model_components <- function(framework, spec, outcome, controls) {
  fe_formula <- fe_formula_by_spec[[spec]]

  if (framework == "border_fe") {
    target <- "strictness_own"
    rhs_terms <- c(target, controls)
    coef_name <- target
    rhs_without_target <- controls
  } else {
    target <- "side"
    rhs_terms <- c("side", "signed_distance", "side:signed_distance", controls)
    coef_name <- target
    rhs_without_target <- c("signed_distance", "side:signed_distance", controls)
  }

  formula_text <- paste0(
    "log(", outcome, ") ~ ",
    paste(rhs_terms, collapse = " + "),
    " | ", fe_formula
  )

  list(
    formula_text = formula_text,
    formula_obj = as.formula(formula_text),
    fe_formula = fe_formula,
    rhs_terms = rhs_terms,
    rhs_without_target = rhs_without_target,
    target = target,
    coef_name = coef_name
  )
}

compute_overlap_metrics <- function(df_cell, top_k) {
  fe_top <- df_cell %>%
    filter(framework == "border_fe") %>%
    arrange(desc(abs_delta_beta), alderman_pair) %>%
    slice_head(n = top_k) %>%
    pull(alderman_pair)

  rd_top <- df_cell %>%
    filter(framework == "spatial_rd_fe") %>%
    arrange(desc(abs_delta_beta), alderman_pair) %>%
    slice_head(n = top_k) %>%
    pull(alderman_pair)

  overlap_n <- length(intersect(fe_top, rd_top))
  tibble(
    overlap_n = overlap_n,
    overlap_share = safe_divide(overlap_n, top_k)
  )
}

message("Loading merged parcel data...")
raw <- read_csv(
  input_file,
  show_col_types = FALSE,
  col_types = cols(pin = col_character())
)

strictness_sd <- sd(raw$strictness_own, na.rm = TRUE)
if (!is.finite(strictness_sd) || strictness_sd == 0) {
  stop("strictness_own has invalid standard deviation; cannot normalize.", call. = FALSE)
}

base <- raw %>%
  mutate(
    strictness_own = strictness_own / strictness_sd,
    side = as.integer(signed_distance > 0),
    alderman_pair = pair_from_names(alderman_own, alderman_neighbor),
    strictness_gap_abs = abs(strictness_own - strictness_neighbor)
  )

model_rows <- list()
model_objects <- list()
model_counter <- 0L

for (framework in frameworks) {
  for (sample_filter in samples) {
    for (spec in specs) {
      for (outcome in outcomes) {
        model_counter <- model_counter + 1L
        model_id <- sprintf("%s__%s__%s__%s", framework, sample_filter, spec, outcome)

        message(sprintf(
          "Fitting model %d: framework=%s sample=%s spec=%s outcome=%s",
          model_counter, framework, sample_filter, spec, outcome
        ))

        df <- build_analysis_data(base, framework, sample_filter, spec, outcome, bw_ft)

        if (nrow(df) == 0) {
          model_rows[[length(model_rows) + 1L]] <- tibble(
            model_id = model_id,
            framework = framework,
            sample = sample_filter,
            spec = spec,
            outcome = outcome,
            beta = NA_real_,
            se = NA_real_,
            t_value = NA_real_,
            p_value = NA_real_,
            coef_present = FALSE,
            nobs_model = 0L,
            n_ward_pairs = 0L,
            n_alderman_pairs = 0L,
            fit_error = "No observations after filters"
          )
          next
        }

        parts <- build_model_components(framework, spec, outcome, controls)
        fit <- fit_clustered(parts$formula_obj, df)

        if (is_fit_error(fit)) {
          model_rows[[length(model_rows) + 1L]] <- tibble(
            model_id = model_id,
            framework = framework,
            sample = sample_filter,
            spec = spec,
            outcome = outcome,
            beta = NA_real_,
            se = NA_real_,
            t_value = NA_real_,
            p_value = NA_real_,
            coef_present = FALSE,
            nobs_model = 0L,
            n_ward_pairs = 0L,
            n_alderman_pairs = 0L,
            fit_error = fit$message
          )
          next
        }

        coef_info <- extract_coef_info(fit, parts$coef_name)
        est_sample <- get_model_sample(fit, df)

        model_rows[[length(model_rows) + 1L]] <- tibble(
          model_id = model_id,
          framework = framework,
          sample = sample_filter,
          spec = spec,
          outcome = outcome,
          beta = coef_info$beta,
          se = coef_info$se,
          t_value = coef_info$t_value,
          p_value = coef_info$p_value,
          coef_present = coef_info$coef_present,
          nobs_model = as.integer(nobs(fit)),
          n_ward_pairs = as.integer(n_distinct(est_sample$ward_pair)),
          n_alderman_pairs = as.integer(n_distinct(est_sample$alderman_pair, na.rm = TRUE)),
          fit_error = NA_character_
        )

        model_objects[[model_id]] <- list(
          model_id = model_id,
          framework = framework,
          sample = sample_filter,
          spec = spec,
          outcome = outcome,
          formula_obj = parts$formula_obj,
          formula_text = parts$formula_text,
          fe_formula = parts$fe_formula,
          rhs_terms = parts$rhs_terms,
          rhs_without_target = parts$rhs_without_target,
          target = parts$target,
          coef_name = parts$coef_name,
          fit = fit,
          est_sample = est_sample,
          full_beta = coef_info$beta,
          full_se = coef_info$se,
          full_t = coef_info$t_value,
          full_p = coef_info$p_value
        )
      }
    }
  }
}

model_summary <- bind_rows(model_rows)

if (length(model_objects) == 0) {
  write_csv(model_summary, output_model_summary)
  stop("No models estimated successfully; cannot run leverage diagnostics.", call. = FALSE)
}

message("Running exact alderman-pair leave-one-out diagnostics...")
loo_rows <- list()

for (model_id in names(model_objects)) {
  obj <- model_objects[[model_id]]
  dat <- obj$est_sample %>% filter(!is.na(alderman_pair))

  if (nrow(dat) == 0) {
    next
  }

  groups <- sort(unique(dat$alderman_pair))
  message(sprintf("  %s: %d alderman pairs", model_id, length(groups)))

  for (group_name in groups) {
    group_data <- dat %>% filter(alderman_pair == group_name)
    drop_data <- dat %>% filter(alderman_pair != group_name)

    group_meta <- group_data %>%
      summarise(
        n_obs_group = n(),
        mean_unitscount = mean(unitscount, na.rm = TRUE),
        mean_density_far = mean(density_far, na.rm = TRUE),
        mean_density_dupac = mean(density_dupac, na.rm = TRUE),
        mean_strictness_gap_abs = mean(strictness_gap_abs, na.rm = TRUE)
      )

    if (nrow(drop_data) == 0) {
      loo_rows[[length(loo_rows) + 1L]] <- tibble(
        model_id = model_id,
        framework = obj$framework,
        sample = obj$sample,
        spec = obj$spec,
        outcome = obj$outcome,
        alderman_pair = group_name,
        n_obs_group = group_meta$n_obs_group,
        mean_unitscount = group_meta$mean_unitscount,
        mean_density_far = group_meta$mean_density_far,
        mean_density_dupac = group_meta$mean_density_dupac,
        mean_strictness_gap_abs = group_meta$mean_strictness_gap_abs,
        beta_full = obj$full_beta,
        se_full = obj$full_se,
        t_full = obj$full_t,
        p_full = obj$full_p,
        beta_drop = NA_real_,
        se_drop = NA_real_,
        t_drop = NA_real_,
        p_drop = NA_real_,
        nobs_drop = 0L,
        delta_beta = NA_real_,
        abs_delta_beta = NA_real_,
        delta_t = NA_real_,
        delta_p = NA_real_,
        sign_flip = NA,
        sig05_flip = NA,
        estimable_drop = FALSE,
        drop_fit_error = "All observations removed"
      )
      next
    }

    drop_fit <- fit_clustered(obj$formula_obj, drop_data)

    if (is_fit_error(drop_fit)) {
      loo_rows[[length(loo_rows) + 1L]] <- tibble(
        model_id = model_id,
        framework = obj$framework,
        sample = obj$sample,
        spec = obj$spec,
        outcome = obj$outcome,
        alderman_pair = group_name,
        n_obs_group = group_meta$n_obs_group,
        mean_unitscount = group_meta$mean_unitscount,
        mean_density_far = group_meta$mean_density_far,
        mean_density_dupac = group_meta$mean_density_dupac,
        mean_strictness_gap_abs = group_meta$mean_strictness_gap_abs,
        beta_full = obj$full_beta,
        se_full = obj$full_se,
        t_full = obj$full_t,
        p_full = obj$full_p,
        beta_drop = NA_real_,
        se_drop = NA_real_,
        t_drop = NA_real_,
        p_drop = NA_real_,
        nobs_drop = NA_integer_,
        delta_beta = NA_real_,
        abs_delta_beta = NA_real_,
        delta_t = NA_real_,
        delta_p = NA_real_,
        sign_flip = NA,
        sig05_flip = NA,
        estimable_drop = FALSE,
        drop_fit_error = drop_fit$message
      )
      next
    }

    drop_coef <- extract_coef_info(drop_fit, obj$coef_name)

    delta_beta <- obj$full_beta - drop_coef$beta
    delta_t <- obj$full_t - drop_coef$t_value
    delta_p <- drop_coef$p_value - obj$full_p

    sign_flip <- ifelse(
      is.finite(obj$full_beta) & is.finite(drop_coef$beta) & obj$full_beta != 0 & drop_coef$beta != 0,
      sign(obj$full_beta) != sign(drop_coef$beta),
      NA
    )

    sig05_flip <- ifelse(
      is.finite(obj$full_p) & is.finite(drop_coef$p_value),
      (obj$full_p < 0.05) != (drop_coef$p_value < 0.05),
      NA
    )

    loo_rows[[length(loo_rows) + 1L]] <- tibble(
      model_id = model_id,
      framework = obj$framework,
      sample = obj$sample,
      spec = obj$spec,
      outcome = obj$outcome,
      alderman_pair = group_name,
      n_obs_group = group_meta$n_obs_group,
      mean_unitscount = group_meta$mean_unitscount,
      mean_density_far = group_meta$mean_density_far,
      mean_density_dupac = group_meta$mean_density_dupac,
      mean_strictness_gap_abs = group_meta$mean_strictness_gap_abs,
      beta_full = obj$full_beta,
      se_full = obj$full_se,
      t_full = obj$full_t,
      p_full = obj$full_p,
      beta_drop = drop_coef$beta,
      se_drop = drop_coef$se,
      t_drop = drop_coef$t_value,
      p_drop = drop_coef$p_value,
      nobs_drop = as.integer(nobs(drop_fit)),
      delta_beta = delta_beta,
      abs_delta_beta = abs(delta_beta),
      delta_t = delta_t,
      delta_p = delta_p,
      sign_flip = sign_flip,
      sig05_flip = sig05_flip,
      estimable_drop = is.finite(drop_coef$beta),
      drop_fit_error = NA_character_
    )
  }
}

pair_loo <- bind_rows(loo_rows)

message("Running project-level FWL influence diagnostics...")
project_rows <- list()
fwl_rows <- list()

for (model_id in names(model_objects)) {
  obj <- model_objects[[model_id]]
  dat <- obj$est_sample %>% mutate(row_id = row_number())
  beta_full_scalar <- if (length(obj$full_beta) == 1L) as.numeric(obj$full_beta) else NA_real_
  coef_name_scalar <- if (length(obj$coef_name) == 1L) as.character(obj$coef_name) else NA_character_

  if (nrow(dat) == 0) {
    next
  }

  rhs_other <- paste(obj$rhs_without_target, collapse = " + ")

  x_formula <- as.formula(
    paste0(obj$target, " ~ ", rhs_other, " | ", obj$fe_formula)
  )
  y_formula <- as.formula(
    paste0("log(", obj$outcome, ") ~ ", rhs_other, " | ", obj$fe_formula)
  )

  x_fit <- fit_unclustered(x_formula, dat)
  y_fit <- fit_unclustered(y_formula, dat)

  if (is_fit_error(x_fit) || is_fit_error(y_fit)) {
    fwl_rows[[length(fwl_rows) + 1L]] <- tibble(
      model_id = model_id,
      beta_fwl_reconstructed = NA_real_,
      beta_reconstruction_error = NA_real_,
      n_project_rows = 0L,
      fwl_error = paste(
        if (is_fit_error(x_fit)) x_fit$message else NA_character_,
        if (is_fit_error(y_fit)) y_fit$message else NA_character_,
        sep = " | "
      )
    )
    next
  }

  x_idx <- get_kept_index(x_fit, nrow(dat))
  y_idx <- get_kept_index(y_fit, nrow(dat))

  x_tbl <- tibble(
    row_id = dat$row_id[x_idx],
    x_tilde = as.numeric(resid(x_fit))
  )

  y_tbl <- tibble(
    row_id = dat$row_id[y_idx],
    y_tilde = as.numeric(resid(y_fit))
  )

  influence_df <- dat %>%
    select(
      row_id,
      pin,
      ward_pair,
      alderman_pair,
      construction_year,
      signed_distance,
      unitscount,
      density_far,
      density_dupac,
      strictness_gap_abs
    ) %>%
    inner_join(x_tbl, by = "row_id") %>%
    inner_join(y_tbl, by = "row_id")

  if (nrow(influence_df) == 0) {
    fwl_rows[[length(fwl_rows) + 1L]] <- tibble(
      model_id = model_id,
      beta_fwl_reconstructed = NA_real_,
      beta_reconstruction_error = NA_real_,
      n_project_rows = 0L,
      fwl_error = "No overlapping residualized rows"
    )
    next
  }

  s_xy <- sum(influence_df$x_tilde * influence_df$y_tilde, na.rm = TRUE)
  s_xx <- sum(influence_df$x_tilde^2, na.rm = TRUE)
  beta_fwl <- safe_divide(s_xy, s_xx)
  beta_error <- beta_full_scalar - beta_fwl

  influence_df <- influence_df %>%
    mutate(
      framework = obj$framework,
      sample = obj$sample,
      spec = obj$spec,
      outcome = obj$outcome,
      model_id = model_id,
      coefficient_name = coef_name_scalar,
      beta_full = beta_full_scalar,
      beta_fwl_reconstructed = beta_fwl,
      beta_reconstruction_error = beta_error,
      contribution_term = x_tilde * y_tilde,
      leverage_term = x_tilde^2,
      s_xy = s_xy,
      s_xx = s_xx,
      s_xy_minus_i = s_xy - contribution_term,
      s_xx_minus_i = s_xx - leverage_term,
      beta_drop_project = ifelse(
        is.finite(s_xx_minus_i) & s_xx_minus_i > 1e-12,
        s_xy_minus_i / s_xx_minus_i,
        NA_real_
      ),
      delta_beta_project = beta_full - beta_drop_project,
      abs_delta_beta_project = abs(delta_beta_project)
    )

  project_rows[[length(project_rows) + 1L]] <- influence_df

  fwl_rows[[length(fwl_rows) + 1L]] <- tibble(
    model_id = model_id,
    beta_fwl_reconstructed = beta_fwl,
    beta_reconstruction_error = beta_error,
    n_project_rows = nrow(influence_df),
    fwl_error = NA_character_
  )
}

project_influence <- bind_rows(project_rows)
fwl_summary <- bind_rows(fwl_rows)

model_summary <- model_summary %>%
  left_join(fwl_summary, by = "model_id")

loo_scored <- pair_loo %>%
  filter(estimable_drop) %>%
  group_by(framework, sample, spec, outcome) %>%
  mutate(
    rank_abs_delta = row_number(desc(abs_delta_beta)),
    leverage_z = ifelse(
      is.finite(sd(abs_delta_beta, na.rm = TRUE)) & sd(abs_delta_beta, na.rm = TRUE) > 0,
      (abs_delta_beta - mean(abs_delta_beta, na.rm = TRUE)) / sd(abs_delta_beta, na.rm = TRUE),
      0
    )
  ) %>%
  ungroup()

fe_pairs <- loo_scored %>%
  filter(framework == "border_fe") %>%
  select(sample, spec, outcome, alderman_pair, abs_delta_beta_fe = abs_delta_beta, leverage_z_fe = leverage_z, rank_fe = rank_abs_delta)

rd_pairs <- loo_scored %>%
  filter(framework == "spatial_rd_fe") %>%
  select(sample, spec, outcome, alderman_pair, abs_delta_beta_rd = abs_delta_beta, leverage_z_rd = leverage_z, rank_rd = rank_abs_delta)

pair_gap <- full_join(
  fe_pairs,
  rd_pairs,
  by = c("sample", "spec", "outcome", "alderman_pair")
) %>%
  mutate(
    rd_minus_fe_z = leverage_z_rd - leverage_z_fe
  )

overlap_stats <- loo_scored %>%
  group_by(sample, spec, outcome) %>%
  group_modify(~{
    top10 <- compute_overlap_metrics(.x, 10) %>%
      rename(overlap_top10_n = overlap_n, overlap_top10_share = overlap_share)
    top20 <- compute_overlap_metrics(.x, 20) %>%
      rename(overlap_top20_n = overlap_n, overlap_top20_share = overlap_share)
    bind_cols(top10, top20)
  }) %>%
  ungroup()

pair_gap <- pair_gap %>%
  left_join(overlap_stats, by = c("sample", "spec", "outcome"))

build_facet_info <- function(model_summary, outcome_name) {
  wide <- model_summary %>%
    filter(outcome == outcome_name, framework %in% c("border_fe", "spatial_rd_fe")) %>%
    select(sample, spec, framework, beta, p_value) %>%
    mutate(framework = ifelse(framework == "border_fe", "fe", "rd")) %>%
    pivot_wider(
      names_from = framework,
      values_from = c(beta, p_value)
    )

  for (nm in c("beta_fe", "p_value_fe", "beta_rd", "p_value_rd")) {
    if (!nm %in% names(wide)) {
      wide[[nm]] <- NA_real_
    }
  }

  wide %>%
    mutate(
      facet_label = sprintf(
        "%s | %s\nFE beta=%.3f (p=%.3f); RD jump=%.3f (p=%.3f)",
        sample,
        spec,
        beta_fe,
        p_value_fe,
        beta_rd,
        p_value_rd
      )
    ) %>%
    select(sample, spec, facet_label)
}

plot_top_pairs <- function(outcome_name, output_pdf) {
  facet_info <- build_facet_info(model_summary, outcome_name)

  plot_df <- loo_scored %>%
    filter(outcome == outcome_name, framework %in% c("border_fe", "spatial_rd_fe")) %>%
    group_by(sample, spec, framework) %>%
    arrange(desc(abs_delta_beta), alderman_pair) %>%
    slice_head(n = 15) %>%
    ungroup() %>%
    mutate(
      framework_label = ifelse(framework == "border_fe", "Border FE", "Spatial RD FE"),
      item_label = paste0(alderman_pair, " [", framework_label, "]")
    ) %>%
    left_join(facet_info, by = c("sample", "spec")) %>%
    mutate(facet_id = facet_label)

  p <- ggplot(
    plot_df,
    aes(
      x = reorder_within(item_label, abs_delta_beta, facet_id),
      y = abs_delta_beta,
      fill = framework_label
    )
  ) +
    geom_col(position = "identity", alpha = 0.85) +
    coord_flip() +
    facet_wrap(~facet_id, nrow = 2, scales = "free_y") +
    scale_x_reordered() +
    scale_fill_manual(values = c("Border FE" = "#1f77b4", "Spatial RD FE" = "#d62728")) +
    labs(
      title = paste0("Top 15 Alderman-Pair Leverage by Framework: ", outcome_name),
      x = "Alderman pair",
      y = "|Delta beta| from exact leave-one-pair-out",
      fill = "Framework"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(size = 8),
      axis.text.y = element_text(size = 6),
      legend.position = "bottom"
    )

  ggsave(output_pdf, p, width = 17, height = 10)
}

plot_top_projects <- function(outcome_name, output_pdf) {
  facet_info <- build_facet_info(model_summary, outcome_name)

  plot_df <- project_influence %>%
    filter(outcome == outcome_name, framework %in% c("border_fe", "spatial_rd_fe")) %>%
    group_by(sample, spec, framework) %>%
    arrange(desc(abs_delta_beta_project), pin) %>%
    slice_head(n = 15) %>%
    ungroup() %>%
    mutate(
      framework_label = ifelse(framework == "border_fe", "Border FE", "Spatial RD FE"),
      item_label = paste0(pin, " [", framework_label, "]")
    ) %>%
    left_join(facet_info, by = c("sample", "spec")) %>%
    mutate(facet_id = facet_label)

  p <- ggplot(
    plot_df,
    aes(
      x = reorder_within(item_label, abs_delta_beta_project, facet_id),
      y = abs_delta_beta_project,
      fill = framework_label
    )
  ) +
    geom_col(position = "identity", alpha = 0.85) +
    coord_flip() +
    facet_wrap(~facet_id, nrow = 2, scales = "free_y") +
    scale_x_reordered() +
    scale_fill_manual(values = c("Border FE" = "#1f77b4", "Spatial RD FE" = "#d62728")) +
    labs(
      title = paste0("Top 15 Project Influence by Framework: ", outcome_name),
      x = "Project (PIN)",
      y = "|Delta beta| from one-project deletion (FWL closed form)",
      fill = "Framework"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(size = 8),
      axis.text.y = element_text(size = 6),
      legend.position = "bottom"
    )

  ggsave(output_pdf, p, width = 17, height = 10)
}

message("Writing CSV outputs...")
write_csv(model_summary, output_model_summary)
write_csv(pair_loo, output_pair_loo)
write_csv(project_influence, output_project_influence)
write_csv(pair_gap, output_pair_gap)

message("Writing PDF outputs...")
plot_top_pairs("density_far", output_top_pairs_far_pdf)
plot_top_pairs("density_dupac", output_top_pairs_dupac_pdf)
plot_top_projects("density_far", output_top_projects_far_pdf)
plot_top_projects("density_dupac", output_top_projects_dupac_pdf)

message("Diagnostics complete.")
message(sprintf("- %s", output_model_summary))
message(sprintf("- %s", output_pair_loo))
message(sprintf("- %s", output_project_influence))
message(sprintf("- %s", output_pair_gap))
message(sprintf("- %s", output_top_pairs_far_pdf))
message(sprintf("- %s", output_top_pairs_dupac_pdf))
message(sprintf("- %s", output_top_projects_far_pdf))
message(sprintf("- %s", output_top_projects_dupac_pdf))
