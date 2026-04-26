source("../../setup_environment/code/packages.R")

options(dplyr.summarise.inform = FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_block_land_value_panel_models/code")
# in_panel <- "../input/block_land_value_panel.parquet"
# in_aldermen <- "../input/chicago_alderman_panel.csv"
# out_pair_summary <- "../output/building_positive_ward_pair_summary_event2015_longpre_1000ft.csv"
# out_leave_one_out <- "../output/building_positive_ward_pair_leave_one_out_event2015_longpre_1000ft.csv"
# out_top_pairs_pdf <- "../output/building_positive_ward_pair_leave_one_out_event2015_longpre_1000ft.pdf"
# out_transition_leave_one_out <- "../output/building_positive_alder_transition_leave_one_out_event2015_longpre_1000ft.csv"
# out_top_transitions_pdf <- "../output/building_positive_alder_transition_leave_one_out_event2015_longpre_1000ft.pdf"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    in_panel,
    in_aldermen,
    out_pair_summary,
    out_leave_one_out,
    out_top_pairs_pdf,
    out_transition_leave_one_out,
    out_top_transitions_pdf
  )
}

if (length(cli_args) != 7) {
  stop(
    paste(
      "FATAL: Script requires 7 args:",
      "<block_panel_parquet> <alderman_panel_csv> <out_pair_summary_csv>",
      "<out_leave_one_out_csv> <out_top_pairs_pdf>",
      "<out_transition_leave_one_out_csv> <out_top_transitions_pdf>"
    ),
    call. = FALSE
  )
}

in_panel <- cli_args[1]
in_aldermen <- cli_args[2]
out_pair_summary <- cli_args[3]
out_leave_one_out <- cli_args[4]
out_top_pairs_pdf <- cli_args[5]
out_transition_leave_one_out <- cli_args[6]
out_top_transitions_pdf <- cli_args[7]

stopifnot(file.exists(in_panel), file.exists(in_aldermen))

event_year <- 2015L
analysis_years <- c(2002L, 2003L, 2004L, 2006L, 2007L, 2008L, 2009L, 2010L, 2011L, 2012L, 2014L, 2016L, 2017L, 2018L)

extract_sign_terms <- function(model) {
  coef_table <- fixest::coeftable(model)
  conf_int <- stats::confint(model)
  p_col <- grep("^Pr\\(", colnames(coef_table), value = TRUE)[1]

  tibble::tibble(
    term = rownames(coef_table),
    estimate = as.numeric(coef_table[, "Estimate"]),
    std_error = as.numeric(coef_table[, "Std. Error"]),
    p_value = if (!is.na(p_col)) as.numeric(coef_table[, p_col]) else NA_real_,
    conf_low = as.numeric(conf_int[, 1]),
    conf_high = as.numeric(conf_int[, 2])
  ) %>%
    dplyr::mutate(
      event_time = as.integer(stringr::str_match(term, "relative_year::(-?[0-9]+):")[, 2]),
      treatment_group = dplyr::case_when(
        stringr::str_detect(term, ":to_lenient$") ~ "to_lenient",
        stringr::str_detect(term, ":to_stricter$") ~ "to_stricter",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(event_time), !is.na(treatment_group))
}

fit_sign_model <- function(df) {
  fixest::feols(
    building_positive_share ~
      i(relative_year, to_lenient, ref = -1) +
      i(relative_year, to_stricter, ref = -1) |
      block_border_side_id + segment_id^tax_year,
    data = df,
    cluster = ~segment_id,
    warn = FALSE
  )
}

summarize_post_terms <- function(coef_df) {
  coef_df %>%
    dplyr::filter(event_time %in% 1L:3L) %>%
    dplyr::group_by(treatment_group) %>%
    dplyr::summarise(avg_post_pp = 100 * mean(estimate), .groups = "drop") %>%
    dplyr::right_join(
      tibble::tibble(treatment_group = c("to_lenient", "to_stricter")),
      by = "treatment_group"
    ) %>%
    tidyr::pivot_wider(names_from = treatment_group, values_from = avg_post_pp, names_prefix = "avg_post_pp_")
}

message("\n=== Ward-Pair Influence Diagnostics ===")
message("Spec: building_positive_share, 1000ft, admin_95, long pre-period, event year 2015")

aldermen_2015 <- readr::read_csv(in_aldermen, show_col_types = FALSE) %>%
  dplyr::filter(month == "Jun 2015") %>%
  dplyr::transmute(
    ward = as.integer(ward),
    alder_2015 = alderman
  )

analysis_df <- arrow::read_parquet(in_panel) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    block_origin_side_id = as.character(block_origin_side_id),
    block_border_side_id = as.character(block_border_side_id),
    ward_pair_id = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    ward_origin = as.integer(ward_origin),
    ward_dest = as.integer(ward_dest),
    tax_year = as.integer(tax_year),
    comparison_sample = as.logical(comparison_sample),
    treatment_sign = factor(treatment_sign, levels = c("no_change", "to_lenient", "to_stricter")),
    to_lenient = as.integer(treatment_sign == "to_lenient"),
    to_stricter = as.integer(treatment_sign == "to_stricter"),
    relative_year = tax_year - event_year,
    transition_id = paste(ward_origin, ward_dest, sep = "_to_")
  ) %>%
  dplyr::filter(
    sample_scope %in% c("history_vacant_core", "developable_core"),
    bandwidth == "1000ft",
    tax_year %in% analysis_years,
    comparison_sample %in% TRUE,
    coverage_share >= 0.95,
    admin_share >= 0.95,
    !is.na(building_positive_share),
    !is.na(strictness_change),
    !is.na(treatment_sign),
    !is.na(block_border_side_id),
    block_border_side_id != "",
    !is.na(segment_id),
    segment_id != "",
    relative_year != 0L
  ) %>%
  dplyr::left_join(
    aldermen_2015 %>%
      dplyr::rename(ward_origin = ward, origin_alder_2015 = alder_2015),
    by = "ward_origin"
  ) %>%
  dplyr::left_join(
    aldermen_2015 %>%
      dplyr::rename(ward_dest = ward, dest_alder_2015 = alder_2015),
    by = "ward_dest"
  )

pair_summary <- analysis_df %>%
  dplyr::group_by(
    sample_scope, ward_pair_id, ward_origin, ward_dest,
    origin_alder_2015, dest_alder_2015, treatment_sign
  ) %>%
  dplyr::summarise(
    n_obs = dplyr::n(),
    n_block_sides = dplyr::n_distinct(block_border_side_id),
    n_segments = dplyr::n_distinct(segment_id),
    n_pin10 = sum(n_pin10, na.rm = TRUE),
    ref_n_block_sides = dplyr::n_distinct(block_border_side_id[relative_year == -1L]),
    post_n_block_sides = dplyr::n_distinct(block_border_side_id[relative_year %in% 1L:3L]),
    ref_mean = mean(building_positive_share[relative_year == -1L], na.rm = TRUE),
    post_mean = mean(building_positive_share[relative_year %in% 1L:3L], na.rm = TRUE),
    t1_mean = mean(building_positive_share[relative_year == 1L], na.rm = TRUE),
    t2_mean = mean(building_positive_share[relative_year == 2L], na.rm = TRUE),
    t3_mean = mean(building_positive_share[relative_year == 3L], na.rm = TRUE),
    mean_strictness_change = mean(strictness_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    ref_mean_pp = 100 * ref_mean,
    post_mean_pp = 100 * post_mean,
    post_minus_ref_pp = 100 * (post_mean - ref_mean),
    t1_minus_ref_pp = 100 * (t1_mean - ref_mean),
    t2_minus_ref_pp = 100 * (t2_mean - ref_mean),
    t3_minus_ref_pp = 100 * (t3_mean - ref_mean)
  ) %>%
  dplyr::arrange(sample_scope, treatment_sign, post_minus_ref_pp)

pair_labels <- dplyr::bind_rows(
  analysis_df %>% dplyr::distinct(sample_scope, ward_pair_id, ward = ward_origin),
  analysis_df %>% dplyr::distinct(sample_scope, ward_pair_id, ward = ward_dest)
) %>%
  dplyr::distinct() %>%
  dplyr::left_join(aldermen_2015, by = "ward") %>%
  dplyr::arrange(sample_scope, ward_pair_id, ward) %>%
  dplyr::group_by(sample_scope, ward_pair_id) %>%
  dplyr::summarise(
    pair_wards = paste(ward, collapse = "-"),
    pair_alders_2015 = paste(sprintf("%s: %s", ward, alder_2015), collapse = " | "),
    .groups = "drop"
  )

pair_support <- analysis_df %>%
  dplyr::group_by(sample_scope, ward_pair_id) %>%
  dplyr::summarise(
    n_obs = dplyr::n(),
    n_block_sides = dplyr::n_distinct(block_border_side_id),
    n_segments = dplyr::n_distinct(segment_id),
    n_to_lenient_block_sides = dplyr::n_distinct(block_border_side_id[treatment_sign == "to_lenient"]),
    n_to_stricter_block_sides = dplyr::n_distinct(block_border_side_id[treatment_sign == "to_stricter"]),
    n_no_change_block_sides = dplyr::n_distinct(block_border_side_id[treatment_sign == "no_change"]),
    .groups = "drop"
  )

transition_support <- analysis_df %>%
  dplyr::group_by(sample_scope, transition_id, ward_origin, ward_dest, origin_alder_2015, dest_alder_2015) %>%
  dplyr::summarise(
    n_obs = dplyr::n(),
    n_block_sides = dplyr::n_distinct(block_border_side_id),
    n_segments = dplyr::n_distinct(segment_id),
    n_to_lenient_block_sides = dplyr::n_distinct(block_border_side_id[treatment_sign == "to_lenient"]),
    n_to_stricter_block_sides = dplyr::n_distinct(block_border_side_id[treatment_sign == "to_stricter"]),
    n_no_change_block_sides = dplyr::n_distinct(block_border_side_id[treatment_sign == "no_change"]),
    .groups = "drop"
  )

leave_one_out <- purrr::map_dfr(c("history_vacant_core", "developable_core"), function(sample_i) {
  sample_df <- analysis_df %>% dplyr::filter(sample_scope == sample_i)
  full_model <- fit_sign_model(sample_df)
  full_avg <- summarize_post_terms(extract_sign_terms(full_model))

  purrr::map_dfr(sort(unique(sample_df$ward_pair_id)), function(pair_i) {
    loo_df <- sample_df %>% dplyr::filter(ward_pair_id != pair_i)
    loo_row <- tryCatch(
      summarize_post_terms(extract_sign_terms(fit_sign_model(loo_df))),
      error = function(e) tibble::tibble(
        avg_post_pp_to_lenient = NA_real_,
        avg_post_pp_to_stricter = NA_real_
      )
    )

    tibble::tibble(sample_scope = sample_i, ward_pair_id = pair_i) %>%
      dplyr::bind_cols(
        full_avg %>%
          dplyr::rename(
            full_avg_post_pp_to_lenient = avg_post_pp_to_lenient,
            full_avg_post_pp_to_stricter = avg_post_pp_to_stricter
          ),
        loo_row %>%
          dplyr::rename(
            without_pair_avg_post_pp_to_lenient = avg_post_pp_to_lenient,
            without_pair_avg_post_pp_to_stricter = avg_post_pp_to_stricter
          )
      )
  })
}) %>%
  dplyr::left_join(pair_support, by = c("sample_scope", "ward_pair_id")) %>%
  dplyr::left_join(pair_labels, by = c("sample_scope", "ward_pair_id")) %>%
  dplyr::mutate(
    drop_minus_full_to_lenient_pp = without_pair_avg_post_pp_to_lenient - full_avg_post_pp_to_lenient,
    drop_minus_full_to_stricter_pp = without_pair_avg_post_pp_to_stricter - full_avg_post_pp_to_stricter
  ) %>%
  dplyr::arrange(sample_scope, dplyr::desc(drop_minus_full_to_lenient_pp))

transition_leave_one_out <- purrr::map_dfr(c("history_vacant_core", "developable_core"), function(sample_i) {
  sample_df <- analysis_df %>% dplyr::filter(sample_scope == sample_i)
  full_model <- fit_sign_model(sample_df)
  full_avg <- summarize_post_terms(extract_sign_terms(full_model))

  transition_ids <- transition_support %>%
    dplyr::filter(
      sample_scope == sample_i,
      n_to_lenient_block_sides > 0
    ) %>%
    dplyr::pull(transition_id)

  purrr::map_dfr(sort(transition_ids), function(transition_i) {
    loo_df <- sample_df %>% dplyr::filter(transition_id != transition_i)
    loo_row <- tryCatch(
      summarize_post_terms(extract_sign_terms(fit_sign_model(loo_df))),
      error = function(e) tibble::tibble(
        avg_post_pp_to_lenient = NA_real_,
        avg_post_pp_to_stricter = NA_real_
      )
    )

    tibble::tibble(sample_scope = sample_i, transition_id = transition_i) %>%
      dplyr::bind_cols(
        full_avg %>%
          dplyr::rename(
            full_avg_post_pp_to_lenient = avg_post_pp_to_lenient,
            full_avg_post_pp_to_stricter = avg_post_pp_to_stricter
          ),
        loo_row %>%
          dplyr::rename(
            without_transition_avg_post_pp_to_lenient = avg_post_pp_to_lenient,
            without_transition_avg_post_pp_to_stricter = avg_post_pp_to_stricter
          )
      )
  })
}) %>%
  dplyr::left_join(transition_support, by = c("sample_scope", "transition_id")) %>%
  dplyr::mutate(
    transition_label = sprintf(
      "%s (%s) -> %s (%s)",
      ward_origin,
      origin_alder_2015,
      ward_dest,
      dest_alder_2015
    ),
    drop_minus_full_to_lenient_pp = without_transition_avg_post_pp_to_lenient - full_avg_post_pp_to_lenient,
    drop_minus_full_to_stricter_pp = without_transition_avg_post_pp_to_stricter - full_avg_post_pp_to_stricter
  ) %>%
  dplyr::arrange(sample_scope, dplyr::desc(drop_minus_full_to_lenient_pp))

plot_df <- leave_one_out %>%
  dplyr::filter(!is.na(drop_minus_full_to_lenient_pp)) %>%
  dplyr::group_by(sample_scope) %>%
  dplyr::slice_max(order_by = drop_minus_full_to_lenient_pp, n = 12, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    sample_label = dplyr::case_when(
      sample_scope == "history_vacant_core" ~ "History vacant core",
      sample_scope == "developable_core" ~ "Developable core",
      TRUE ~ sample_scope
    ),
    pair_label = stringr::str_trunc(paste0(pair_wards, " | ", pair_alders_2015), width = 72),
    pair_label = stats::reorder(pair_label, drop_minus_full_to_lenient_pp)
  )

p <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = pair_label, y = drop_minus_full_to_lenient_pp)
) +
  ggplot2::geom_hline(yintercept = 0, color = "grey60", linewidth = 0.35) +
  ggplot2::geom_col(fill = "#7f3b2e", width = 0.72) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~sample_label, scales = "free_y") +
  ggplot2::labs(
    title = "Ward pairs pulling the to-lenient building-positive estimate downward",
    subtitle = "Positive bars mean dropping the pair makes the average t=1..3 estimate less negative",
    x = NULL,
    y = "Drop-pair estimate minus full estimate (percentage points)"
  ) +
  ggplot2::theme_minimal(base_size = 10.5) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold"),
    plot.title = ggplot2::element_text(face = "bold")
  )

transition_plot_df <- transition_leave_one_out %>%
  dplyr::filter(!is.na(drop_minus_full_to_lenient_pp)) %>%
  dplyr::group_by(sample_scope) %>%
  dplyr::slice_max(order_by = drop_minus_full_to_lenient_pp, n = 12, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    sample_label = dplyr::case_when(
      sample_scope == "history_vacant_core" ~ "History vacant core",
      sample_scope == "developable_core" ~ "Developable core",
      TRUE ~ sample_scope
    ),
    transition_label = stringr::str_trunc(transition_label, width = 80),
    transition_label = stats::reorder(transition_label, drop_minus_full_to_lenient_pp)
  )

transition_plot <- ggplot2::ggplot(
  transition_plot_df,
  ggplot2::aes(x = transition_label, y = drop_minus_full_to_lenient_pp)
) +
  ggplot2::geom_hline(yintercept = 0, color = "grey60", linewidth = 0.35) +
  ggplot2::geom_col(fill = "#2d6b73", width = 0.72) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~sample_label, scales = "free_y") +
  ggplot2::labs(
    title = "Alder transitions pulling the to-lenient building-positive estimate downward",
    subtitle = "Positive bars mean dropping transition makes average t=1..3 estimate less negative",
    x = NULL,
    y = "Drop-transition estimate minus full estimate (percentage points)"
  ) +
  ggplot2::theme_minimal(base_size = 10.5) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold"),
    plot.title = ggplot2::element_text(face = "bold")
  )

readr::write_csv(pair_summary, out_pair_summary)
readr::write_csv(leave_one_out, out_leave_one_out)
readr::write_csv(transition_leave_one_out, out_transition_leave_one_out)
ggplot2::ggsave(out_top_pairs_pdf, p, width = 10, height = 7, device = "pdf")
ggplot2::ggsave(out_top_transitions_pdf, transition_plot, width = 11, height = 7, device = "pdf")

message(sprintf("Saved %s", out_pair_summary))
message(sprintf("Saved %s", out_leave_one_out))
message(sprintf("Saved %s", out_top_pairs_pdf))
message(sprintf("Saved %s", out_transition_leave_one_out))
message(sprintf("Saved %s", out_top_transitions_pdf))
