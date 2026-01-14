# Maps Chicago building permits with ward boundaries by period.
# Uniform point color; points are restricted to Chicago via spatial join.
# Usage:
#   Rscript permit_map.R --period=2003-2014 --out=../output/permit_map_2003_2014.pdf
#   Rscript permit_map.R --period=2015-2023 --out=../output/permit_map_2015_2023.pdf

## run this line when editing in RStudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")
suppressMessages({ library(sf); library(dplyr); library(ggplot2); library(lubridate) })

# ----------------------------
# Parse CLI args
# ----------------------------
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^", flag, "="), "", hit[1])
}
period  <- get_arg("--period", "2015-2023")   # "2003-2014" or "2015-2023"
outpath <- get_arg("--out",    "../output/permit_map.pdf")

message("Period: ", period)
message("Output: ", outpath)

# ----------------------------
# Load data
# ----------------------------
permits    <- st_read("../input/building_permits_clean.gpkg", quiet = TRUE)
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)

# Keep high-discretion only
permits <- permits %>% filter(permit_type == "PERMIT - NEW CONSTRUCTION")

# ----------------------------
# Period + ward vintage
# ----------------------------
permits <- permits %>%
  mutate(issue_date = as.Date(issue_date),
         .year      = year(issue_date))

if (period == "2003-2014") {
  yr_lo <- 2003L; yr_hi <- 2014L; ward_vintage <- 2014L
  title_txt <- "Chicago Building Permits (High-Discretion), 2003-2014"
  subtitle_txt <- "Points restricted to ward polygons; Wards = 2014 boundaries"
} else if (period == "2015-2023") {
  yr_lo <- 2015L; yr_hi <- 2023L; ward_vintage <- 2015L
  title_txt <- "Chicago Building Permits (High-Discretion), 2015-2023"
  subtitle_txt <- "Points restricted to ward polygons; Wards = 2015 boundaries"
} else stop("Unknown --period. Use 2003-2014 or 2015-2023.")

permits_sub <- permits %>% filter(!is.na(.year), .year >= yr_lo, .year <= yr_hi)
wards_sub   <- ward_panel %>% filter(year == ward_vintage)

# Align CRS for join/plot
if (!identical(st_crs(permits_sub), st_crs(wards_sub))) {
  wards_sub <- st_transform(wards_sub, st_crs(permits_sub))
}

# ----------------------------
# Spatially merge: keep ONLY permits that intersect a ward
# (drops any out-of-bounds points that were causing the "zoomed-out" bbox)
# ----------------------------
permits_chi <- st_join(permits_sub, wards_sub["ward"], left = FALSE, join = st_intersects)

# If a point touches two polygons on a boundary, deduplicate by id
if ("id" %in% names(permits_chi)) {
  permits_chi <- permits_chi %>% distinct(id, .keep_all = TRUE)
}

# Compute bbox from wards to set the plot extent
bb <- st_bbox(wards_sub)

# ----------------------------
# Plot: uniform point color
# ----------------------------
# Draw points first, then ward boundaries on top with a white halo + dark stroke
p <- ggplot() + 
  geom_sf(data = wards_sub, fill = "white", color = "grey30", linewidth = 0.3) + 
  geom_sf(data = permits_chi, color = "#2E86AB", size = 0.25, alpha = 0.6, show.legend = FALSE) + 
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"]), expand = FALSE) + 
  labs( title = title_txt, subtitle = subtitle_txt, caption = paste0("n permits: ", scales::comma(nrow(permits_chi))) ) + 
  theme_void() +
  theme( legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"),
         plot.subtitle = element_text(hjust = 0.5) )


p

ggsave(outpath, plot = p, width = 8, height = 10, dpi = 300)
message("Saved: ", outpath)
