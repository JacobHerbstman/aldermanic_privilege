# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/strictness_score_map/code")
# date_str <- "2022-01"
# uncertainty_spec <- "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022"

source("../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(date_str, uncertainty_spec)
}

if (length(args) != 2) {
  stop("FATAL: Script requires args: <date_str> <uncertainty_spec>", call. = FALSE)
}

date_str <- args[1]
uncertainty_spec <- args[2]
legend_title <- "Regulatory Stringency Index"
plot_title <- sprintf("Regulatory Stringency Index by Ward (%s)", date_str)
outfile <- sprintf("../output/uncertainty_score_map_%s_%s.pdf", uncertainty_spec, date_str)

month_dt <- as.Date(paste0(date_str, "-01"))
use_year <- as.integer(format(month_dt, "%Y"))

# Shapes for the chosen year
wards <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  filter(year == use_year)
if (anyDuplicated(st_drop_geometry(wards)$ward) > 0) {
  stop("Ward panel must be unique by ward for the selected map year.", call. = FALSE)
}

# Scores (alderman-level, time-invariant)
scores_raw <- read_csv(sprintf("../input/alderman_uncertainty_index_%s.csv", uncertainty_spec),
                   show_col_types = FALSE) %>%
  mutate(alderman = str_squish(str_to_lower(alderman)))

if (!"uncertainty_index" %in% names(scores_raw)) {
  stop("Score column not found: uncertainty_index", call. = FALSE)
}

scores <- scores_raw %>%
  transmute(alderman,
            score = suppressWarnings(as.numeric(uncertainty_index)))
if (anyDuplicated(scores$alderman) > 0) {
  stop("Scores must be unique by alderman before joining to wards.", call. = FALSE)
}

# Alderman → Ward mapping for the chosen year
panel <- read_csv("../input/chicago_alderman_panel.csv",
                  show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month)) %>%
  filter(month == as.yearmon(month_dt)) %>%
  transmute(ward,
            alderman = str_squish(str_to_lower(alderman)))
if (anyDuplicated(panel$ward) > 0) {
  stop("Alderman panel must be unique by ward for the selected map month.", call. = FALSE)
}

# Join: scores → panel (get ward) → polygons
ward_scores <- panel %>%
  left_join(scores, by = "alderman", relationship = "many-to-one")
if (any(is.na(ward_scores$score))) {
  missing_scores <- ward_scores %>%
    filter(is.na(score)) %>%
    arrange(ward)
  stop(
    paste0(
      "Missing uncertainty scores for map aldermen: ",
      paste(unique(missing_scores$alderman), collapse = ", ")
    ),
    call. = FALSE
  )
}
if (anyDuplicated(ward_scores$ward) > 0) {
  stop("Ward scores must be unique by ward before joining to ward geometries.", call. = FALSE)
}

ward_map <- wards %>%
  left_join(ward_scores, by = "ward", relationship = "many-to-one")
if (any(is.na(ward_map$score))) {
  missing_wards <- ward_map %>%
    st_drop_geometry() %>%
    filter(is.na(score)) %>%
    arrange(ward)
  stop(
    paste0(
      "Ward map has geometries without matched scores: ",
      paste(missing_wards$ward, collapse = ", ")
    ),
    call. = FALSE
  )
}

p <- ggplot(ward_map) +
  geom_sf(aes(fill = score), color = "grey20", linewidth = 0.2) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1, name = legend_title, na.value = "grey90") +
  labs(
    title   = plot_title
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

ggsave(outfile, plot = p, width = 8, height = 10, dpi = 300)
cat("Map saved to", outfile, "\n")
