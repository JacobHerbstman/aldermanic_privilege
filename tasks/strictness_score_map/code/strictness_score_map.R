# Map Chicago ward polygons filled by alderman strictness score for a given month

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")


args <- commandArgs(trailingOnly = TRUE)
date_str <- if (length(args) >= 1) args[1] else "2025-01"
scores_file <- if (length(args) >= 2) args[2] else "../input/alderman_restrictiveness_scores_month_FEs.csv"
score_col <- if (length(args) >= 3) args[3] else "strictness_index"
outfile <- if (length(args) >= 4) args[4] else file.path("../output", paste0("strictness_score_map_", date_str, ".pdf"))
legend_title <- if (length(args) >= 5) args[5] else "Strictness index"
plot_title <- if (length(args) >= 6) args[6] else paste0("Alderman Strictness Index by Ward (", as.yearmon(date_str), ")")

month_dt <- as.Date(paste0(date_str, "-01"))
use_year <- as.integer(format(month_dt, "%Y"))

# date_str <-  "2025-01"
# month_dt <- as.Date(paste0(date_str, "-01"))
# #extract year from date_str directly
# use_year <- as.integer(format(month_dt, "%Y"))

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
