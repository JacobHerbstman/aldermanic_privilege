# Render the optional listed-rent RD attenuation coefficient plot.

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_rd_rent_attenuation/code")
# bandwidth_ft <- 500
# output_format <- "pdf"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft, output_format)
}
if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <bandwidth_ft> <output_format>", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
output_format <- cli_args[2]
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
if (!output_format %in% c("pdf", "png")) {
  stop("output_format must be pdf or png.", call. = FALSE)
}

bandwidth_label <- as.character(as.integer(round(bandwidth_ft)))

attenuation <- read_csv(
  sprintf("../output/rental_rd_rent_attenuation_bw%s.csv", bandwidth_label),
  show_col_types = FALSE
) %>%
  mutate(
    ci_low = estimate - 1.96 * std_error,
    ci_high = estimate + 1.96 * std_error,
    spec_label = factor(spec_label, levels = c("No controls", "Hedonics", "Hedonics + amenities")),
    sample_label = factor(
      sample_label,
      levels = c("All", "Clean location", "No modal pair change", "No modal ward change", "No questionable address")
    )
  )

attenuation_plot <- ggplot(
  attenuation,
  aes(x = spec_label, y = estimate, ymin = ci_low, ymax = ci_high, color = sample_label)
) +
  geom_hline(yintercept = 0, color = "gray55", linetype = "dotted") +
  geom_pointrange(position = position_dodge(width = 0.55), linewidth = 0.45) +
  labs(
    title = "Listed-Rent Stringency Estimates With Hedonic And Amenity Controls",
    subtitle = sprintf("Common complete-case sample within %.0fft; segment-by-month FE", bandwidth_ft),
    x = NULL,
    y = "Coefficient on stringency index",
    color = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(
  sprintf("../output/rental_rd_rent_attenuation_bw%s.%s", bandwidth_label, output_format),
  attenuation_plot,
  width = 9,
  height = 5.5,
  dpi = ifelse(output_format == "png", 220, 300),
  bg = "white"
)

message(sprintf("Saved listed-rent RD attenuation %s plot.", output_format))
