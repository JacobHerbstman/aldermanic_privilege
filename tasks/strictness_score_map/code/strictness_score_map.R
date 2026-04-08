# Map Chicago ward polygons filled by the alderman stringency index for a given month


source("../../setup_environment/code/packages.R")


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/strictness_score_map/code")
# date_str <- "2022-01"
# scores_file <- "../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# score_col <- "uncertainty_index"
# outfile <- "../output/uncertainty_score_map_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022_2022-01.pdf"
# legend_title <- "Regulatory Stringency Index"
# plot_title <- "Regulatory Stringency Index by Ward (2022-01)"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(date_str, scores_file, score_col, outfile, legend_title, plot_title)
}

if (length(args) < 6) {
  stop("FATAL: Script requires 6 args: <date_str> <scores_file> <score_col> <outfile> <legend_title> <plot_title>", call. = FALSE)
}

date_str <- args[1]
scores_file <- args[2]
score_col <- args[3]
outfile <- args[4]
legend_title <- args[5]
plot_title <- args[6]

month_dt <- as.Date(paste0(date_str, "-01"))
use_year <- as.integer(format(month_dt, "%Y"))

# Shapes for the chosen year
wards <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  filter(year == use_year)

# Scores (alderman-level, time-invariant)
scores_raw <- read_csv(scores_file,
                   show_col_types = FALSE) %>%
  mutate(alderman = str_squish(str_to_lower(alderman)))

if (!score_col %in% names(scores_raw)) {
  stop(paste0("Score column not found: ", score_col))
}

scores <- scores_raw %>%
  transmute(alderman,
            score = suppressWarnings(as.numeric(.data[[score_col]])))

# Alderman → Ward mapping for the chosen year
panel <- read_csv("../input/chicago_alderman_panel.csv",
                  show_col_types = FALSE) %>%
  filter(month == as.yearmon(month_dt)) %>%
  transmute(ward,
            alderman = str_squish(str_to_lower(alderman)))

# Join: scores → panel (get ward) → polygons
ward_scores <- panel %>%
  left_join(scores, by = "alderman") %>%
  distinct(ward, .keep_all = TRUE)

ward_map <- wards %>%
  left_join(ward_scores, by = "ward")

p <- ggplot(ward_map) +
  geom_sf(aes(fill = score), color = "grey20", linewidth = 0.2) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1, name = legend_title, na.value = "grey90") +
  labs(
    title   = plot_title
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

p

ggsave(outfile, plot = p, width = 8, height = 10, dpi = 300)
cat("Map saved to", outfile, "\n")
