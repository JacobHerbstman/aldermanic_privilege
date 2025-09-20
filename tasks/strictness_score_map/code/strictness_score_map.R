# Map Chicago ward polygons filled by alderman strictness score for a given month

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")


args <- commandArgs(trailingOnly = TRUE)
date_str <- if (length(args) >= 1) args[1] else "2014-05"
month_dt <- as.Date(paste0(date_str, "-01"))
use_year <- if (month_dt < as.Date("2015-05-01")) 2014 else 2015


# Shapes for the chosen year
wards <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  filter(year == use_year)

# Scores (alderman-level, time-invariant)
scores <- read_csv("../input/alderman_restrictiveness_scores_month_FEs.csv",
                   show_col_types = FALSE) %>%
  transmute(alderman = str_squish(str_to_lower(alderman)),
            score    = strictness_index)

# Alderman → Ward mapping for the chosen year
panel <- read_csv("../input/chicago_alderman_panel.csv",
                  show_col_types = FALSE) %>%
  filter(year_month_date == month_dt) %>%
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
  scale_fill_viridis_c(option = "turbo", name = "Strictness index", na.value = "grey90") +
  labs(
    title   = paste0("Alderman Strictness Index by Ward (", date_str, ")")
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

p

outfile <- file.path("../output", paste0("strictness_score_map_", date_str, ".pdf"))
ggsave(outfile, plot = p, width = 8, height = 10, dpi = 300)
cat("Map saved to", outfile, "\n")
