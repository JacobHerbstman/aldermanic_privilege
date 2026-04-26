source("../../setup_environment/code/packages.R")

options(dplyr.summarise.inform = FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_block_land_value_panel_models/code")
# in_panel <- "../input/block_land_value_panel.parquet"
# in_aldermen <- "../input/chicago_alderman_panel.csv"
# out_year_means <- "../output/hopkins_origin_land_value_year_means_event2015_longpre_1000ft.csv"
# out_period_means <- "../output/hopkins_origin_land_value_period_means_event2015_longpre_1000ft.csv"
# out_plot <- "../output/hopkins_origin_land_value_year_means_event2015_longpre_1000ft.pdf"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    in_panel,
    in_aldermen,
    out_year_means,
    out_period_means,
    out_plot
  )
}

if (length(cli_args) != 5) {
  stop(
    paste(
      "FATAL: Script requires 5 args:",
      "<block_panel_parquet> <alderman_panel_csv> <out_year_means_csv>",
      "<out_period_means_csv> <out_plot_pdf>"
    ),
    call. = FALSE
  )
}

in_panel <- cli_args[1]
in_aldermen <- cli_args[2]
out_year_means <- cli_args[3]
out_period_means <- cli_args[4]
out_plot <- cli_args[5]

stopifnot(file.exists(in_panel), file.exists(in_aldermen))

event_year <- 2015L
analysis_years <- c(2002L, 2003L, 2004L, 2006L, 2007L, 2008L, 2009L, 2010L, 2011L, 2012L, 2014L, 2016L, 2017L, 2018L)

message("\n=== Hopkins-Origin Land Value Means ===")
message("Sample: origin ward 2 / Brian Hopkins, 1000ft, admin_95, long pre-period years")

aldermen_2015 <- readr::read_csv(in_aldermen, show_col_types = FALSE) %>%
  dplyr::filter(month == "Jun 2015") %>%
  dplyr::transmute(
    ward = as.integer(ward),
    alder_2015 = alderman
  )

analysis_df <- arrow::read_parquet(in_panel) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    block_border_side_id = as.character(block_border_side_id),
    segment_id = as.character(segment_id),
    ward_origin = as.integer(ward_origin),
    ward_dest = as.integer(ward_dest),
    tax_year = as.integer(tax_year),
    comparison_sample = as.logical(comparison_sample),
    treatment_sign = factor(treatment_sign, levels = c("no_change", "to_lenient", "to_stricter")),
    relative_year = tax_year - event_year
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
  ) %>%
  dplyr::filter(
    sample_scope %in% c("history_vacant_core", "developable_core"),
    bandwidth == "1000ft",
    tax_year %in% analysis_years,
    comparison_sample %in% TRUE,
    coverage_share >= 0.95,
    admin_share >= 0.95,
    ward_origin == 2L,
    origin_alder_2015 == "Brian Hopkins",
    ward_dest != ward_origin,
    !is.na(land_psf_block),
    !is.na(log_land_psf_block),
    !is.na(block_border_side_id),
    block_border_side_id != "",
    !is.na(segment_id),
    segment_id != ""
  ) %>%
  dplyr::mutate(
    sample_label = dplyr::case_when(
      sample_scope == "developable_core" ~ "Developable core",
      sample_scope == "history_vacant_core" ~ "History vacant core",
      TRUE ~ sample_scope
    ),
    transition_label = sprintf(
      "2 Brian Hopkins -> %s %s",
      ward_dest,
      dest_alder_2015
    ),
    plot_label = sprintf("to %s %s", ward_dest, dest_alder_2015),
    period = dplyr::case_when(
      tax_year %in% c(2002L, 2003L, 2004L, 2006L, 2007L, 2008L, 2009L) ~ "long_pre_2002_2009",
      tax_year %in% c(2010L, 2011L, 2012L, 2014L) ~ "pre_2010_2014",
      tax_year %in% c(2016L, 2017L, 2018L) ~ "post_2016_2018",
      TRUE ~ NA_character_
    )
  )

summarise_land_values <- function(df) {
  df %>%
    dplyr::summarise(
      n_block_years = dplyr::n(),
      n_block_sides = dplyr::n_distinct(block_border_side_id),
      n_segments = dplyr::n_distinct(segment_id),
      n_pin10 = sum(n_pin10, na.rm = TRUE),
      total_lot_sqft = sum(lot_sqft_block, na.rm = TRUE),
      total_land_value = sum(land_sum_block, na.rm = TRUE),
      aggregate_land_psf = total_land_value / total_lot_sqft,
      mean_land_psf = mean(land_psf_block, na.rm = TRUE),
      median_land_psf = stats::median(land_psf_block, na.rm = TRUE),
      mean_log_land_psf = mean(log_land_psf_block, na.rm = TRUE),
      mean_building_positive_share = mean(building_positive_share, na.rm = TRUE),
      .groups = "drop"
    )
}

year_means <- analysis_df %>%
  dplyr::group_by(
    sample_scope, sample_label, transition_label, ward_origin, ward_dest,
    origin_alder_2015, dest_alder_2015, treatment_sign, tax_year, relative_year
  ) %>%
  summarise_land_values() %>%
  dplyr::arrange(sample_scope, ward_dest, tax_year)

period_means <- analysis_df %>%
  dplyr::filter(!is.na(period)) %>%
  dplyr::group_by(
    sample_scope, sample_label, transition_label, ward_origin, ward_dest,
    origin_alder_2015, dest_alder_2015, treatment_sign, period
  ) %>%
  summarise_land_values() %>%
  dplyr::arrange(sample_scope, ward_dest, period)

plot_df <- year_means %>%
  dplyr::mutate(
    sample_label = factor(sample_label, levels = c("History vacant core", "Developable core")),
    plot_label = factor(sprintf("to %s %s", ward_dest, dest_alder_2015))
  )

p <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = tax_year, y = aggregate_land_psf, color = plot_label, group = plot_label)
) +
  ggplot2::geom_vline(xintercept = 2014.5, color = "grey65", linetype = "dashed", linewidth = 0.35) +
  ggplot2::geom_line(linewidth = 0.6) +
  ggplot2::geom_point(size = 1.6) +
  ggplot2::facet_wrap(~sample_label, ncol = 1, scales = "free_y") +
  ggplot2::labs(
    title = "Hopkins-origin transitions: assessed land value per sqft",
    subtitle = "Aggregate land value per sqft = sum land assessed value / sum lot sqft",
    x = "Tax year",
    y = "Aggregate assessed land value per sqft",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 10.5) +
  ggplot2::guides(color = ggplot2::guide_legend(ncol = 3)) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold"),
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 8)
  )

readr::write_csv(year_means, out_year_means)
readr::write_csv(period_means, out_period_means)
ggplot2::ggsave(out_plot, p, width = 10.5, height = 6.5, device = "pdf")

message(sprintf("Saved %s", out_year_means))
message(sprintf("Saved %s", out_period_means))
message(sprintf("Saved %s", out_plot))
