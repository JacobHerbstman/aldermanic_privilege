#this code assigns wards + distance to nearest ward border for all parcels in the assessor panel 

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")

# ---- Load ----
land_values    <- sfarrow::st_read_parquet("../input/land_values_geo.parquet", show_col_types = FALSE) 
ward_panel <- st_read("../input/ward_panel.gpkg")
alderman_panel <- read_csv("../input/alderman_panel.csv") 

# -------------------------------------------------------------------
# 1) Choose one geometry per pin (most recent)
# -------------------------------------------------------------------
pins_latest <- land_values %>%
  group_by(pin10) %>%
  slice_max(tax_year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(pin10)  # keep only id + geometry

# -------------------------------------------------------------------
# 2) Assign ward for EACH ward-map year (1998 / 2014 / 2015 in your gpkg)
#    This does NOT iterate over the full panel; just unique pins.
# -------------------------------------------------------------------
pin_year_ward <- st_join(
  pins_latest,
  ward_panel %>% select(year, ward),
  left  = TRUE,
  join  = st_within
) %>%
  st_drop_geometry() %>%
  # In case any pin falls just outside due to slivers, snap to nearest ward that year
  bind_rows({
    missing <- anti_join(
      expand.grid(pin10 = pins_latest$pin10, year = unique(ward_panel$year)) %>% as_tibble(),
      ., by = c("pin10","year")
    )
    if (nrow(missing) == 0L) return(tibble())
    # nearest ward per year
    map_dfr(split(missing, missing$year), function(df_y) {
      y <- unique(df_y$year)
      pts <- pins_latest %>% semi_join(df_y, by = "pin10")
      polys <- ward_panel %>% filter(year == y) %>% select(year, ward)
      nn <- st_nearest_feature(pts, polys)
      tibble(pin10 = pts$pin10, year = y, ward = polys$ward[nn])
    })
  }) %>%
  distinct(pin10, year, ward)

# -------------------------------------------------------------------
# 3) Build ward internal border lines and ward pairs per year (simple loop)
# -------------------------------------------------------------------
years <- sort(unique(ward_panel$year))

build_borders_one_year <- function(polys) {
  polys <- polys %>% select(ward) %>% st_transform(3435)
  # heal geometry without requiring lwgeom
  polys$geometry <- st_buffer(st_make_valid(polys), 0)
  
  nb <- st_touches(polys)
  pairs <- tibble(
    i = rep(seq_len(nrow(polys)), lengths(nb)),
    j = unlist(nb)
  ) %>% filter(i < j)
  
  if (nrow(pairs) == 0L) {
    return(st_sf(ward_a = integer(), ward_b = integer(),
                 ward_pair = character(),
                 geometry = st_sfc(crs = 3435)))
  }
  
  segs <- purrr::pmap_dfr(pairs, function(i, j) {
    g <- suppressWarnings(st_intersection(st_geometry(polys[i, ]), st_geometry(polys[j, ])))
    if (length(g) == 0L) return(NULL)
    g <- st_collection_extract(g, "LINESTRING")
    if (length(g) == 0L) return(NULL)
    st_sf(ward_a = polys$ward[i], ward_b = polys$ward[j], geometry = g)
  })
  
  segs %>%
    mutate(ward_pair = paste0(pmin(ward_a, ward_b), "-", pmax(ward_a, ward_b))) %>%
    st_cast("MULTILINESTRING")
}

ward_borders <- purrr::map_dfr(
  years,
  ~ build_borders_one_year(ward_panel %>% filter(year == .x)) %>% mutate(year = .x)
)

# De-duplicate while keeping sf+CRS
ward_borders <- ward_borders %>%
  group_by(year, ward_pair) %>%
  summarise(do_union = TRUE, .groups = "drop")

# Safety: ensure CRS is set (should already be 3435)
if (is.na(st_crs(ward_borders))) st_crs(ward_borders) <- 3435


## -------------------------------------------------------------------
# 4) Nearest border + distance (ft) for each (pin, ward-map year)
## -------------------------------------------------------------------
pin_year_border <- purrr::map_dfr(
  years,
  function(y) {
    df_y <- pin_year_ward %>% filter(year == y)
    if (nrow(df_y) == 0L) {
      return(tibble(pin10 = character(), year = integer(),
                    ward_pair = character(), dist_to_boundary_ft = double()))
    }
    
    borders_y <- ward_borders %>% filter(year == y)
    
    # points for these pins, in 3435
    pts <- pins_latest %>% semi_join(df_y, by = "pin10") %>% st_transform(3435)
    
    if (nrow(borders_y) == 0L) {
      return(df_y %>% mutate(ward_pair = NA_character_, dist_to_boundary_ft = NA_real_))
    }
    
    idx <- st_nearest_feature(pts, borders_y)
    d   <- st_distance(pts, borders_y[idx, ], by_element = TRUE)
    
    out <- tibble(
      pin10 = pts$pin10,
      ward_pair = borders_y$ward_pair[idx],
      dist_to_boundary_ft = as.numeric(units::set_units(d, "ft"))
    )
    
    # preserve df_y’s row order
    message(sprintf("→ processed year %s: %s pins, %s borders", y, nrow(df_y), nrow(borders_y)))
    df_y %>% left_join(out, by = "pin10")
  }
)


# -------------------------------------------------------------------
# 5) Bring it together: pin × ward-map-year summary table
# -------------------------------------------------------------------
pin_ward_map <- pin_year_ward %>%
  left_join(pin_year_border, by = c("pin10","year","ward")) %>%
  arrange(pin10, year)

# At this point you have, per pin and per ward-map year:
#   pin10 | year (map version) | ward | ward_pair | dist_to_boundary_ft

# -------------------------------------------------------------------
# 6) Merge back to your land_values panel (choose a mapping rule)
# -------------------------------------------------------------------
map_year_for <- function(tax_year) {
  case_when(
    tax_year <= 2003 ~ 1998L,
    tax_year <= 2014 ~ 2014L,
    TRUE             ~ 2015L
  )
}

land_values_aug <- land_values %>%
  mutate(ward_map_year = map_year_for(tax_year)) %>%
  left_join(
    pin_ward_map %>%
      rename(ward_map_year = year),
    by = c("pin10", "ward_map_year")
  ) 

land_values_aug <- land_values_aug %>% 
  distinct(pin10, tax_year, .keep_all = TRUE)

# -------------------------------------------------------------------
# 7) Merge in alderman 
# -------------------------------------------------------------------
# land_values_aug <- st_read_parquet("../output/land_values_aug.parquet")


alderman_year <- alderman_panel %>%
  mutate(
    month = as.yearmon(month),
    tax_year = as.integer(format(as.Date(month), "%Y")),
    ward     = as.integer(ward)
  ) %>%
  group_by(ward, tax_year, alderman) %>%
  summarise(
    months_held = n(),
    last_month  = max(month), 
    .groups = "drop"
  ) %>%
  arrange(ward, tax_year, desc(months_held), desc(last_month)) %>%
  group_by(ward, tax_year) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(ward, tax_year, alderman)


land_values_aug <- land_values_aug %>%
  mutate(ward = as.integer(ward)) %>%
  left_join(alderman_year, by = c("ward", "tax_year"))

# -------------------------------------------------------------------
# 8) Save outputs
# -------------------------------------------------------------------
st_write(ward_borders, "../output/ward_borders.gpkg", delete_dsn = TRUE)
sfarrow::st_write_parquet(land_values_aug, "../output/land_values_aug.parquet")


# arrow::write_feather(st_drop_geometry(pin_ward_map), "../output/pin_ward_map.feather")


