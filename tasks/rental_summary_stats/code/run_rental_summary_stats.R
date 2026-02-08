source("../../setup_environment/code/packages.R")
library(data.table)
library(arrow)
library(sf)
library(patchwork)

sf_use_s2(FALSE)

AREA_LEVELS <- c("Ward 42 (River North)", "Bordering Wards", "All Other Wards")
AREA_COLORS <- c(
  "Ward 42 (River North)" = "#d7301f",
  "Bordering Wards" = "#fc8d59",
  "All Other Wards" = "#f0f0f0"
)

classify_area <- function(ward_vec, neighbors) {
  out <- rep("All Other Wards", length(ward_vec))
  out[ward_vec %in% neighbors] <- "Bordering Wards"
  out[ward_vec == 42] <- "Ward 42 (River North)"
  out[is.na(ward_vec)] <- "Outside / Unmatched"
  out
}

clean_building_type <- function(x) {
  raw <- as.character(x)
  x <- tolower(trimws(raw))
  missing_like <- is.na(raw) | x == "" | x %in% c("na", "n/a", "unknown", "unk", "null")

  out <- rep("other", length(x))
  out[missing_like] <- "missing"
  out[!missing_like & grepl("condo|condominium|\\bcon\\b", x, perl = TRUE)] <- "condo"
  out[!missing_like & grepl("multi|apartment|\\bapt\\b|duplex|triplex|fourplex", x, perl = TRUE)] <- "multi_family"
  out[!missing_like & grepl("single|house|detached|\\bsfr\\b", x, perl = TRUE)] <- "single_family"
  out[!missing_like & grepl("town|\\bth\\b", x, perl = TRUE)] <- "townhouse"
  out
}

is_missing_like_type <- function(x) {
  raw <- as.character(x)
  txt <- tolower(trimws(raw))
  is.na(raw) | txt == "" | txt %in% c("na", "n/a", "unknown", "unk", "null")
}

normalize_clean_type <- function(clean_type, raw_type = NULL) {
  out <- tolower(trimws(as.character(clean_type)))
  out[is.na(out) | out == ""] <- "missing"
  out[!out %in% c("single_family", "multi_family", "condo", "townhouse", "other", "missing")] <- "other"

  if (!is.null(raw_type)) {
    out[out == "other" & is_missing_like_type(raw_type)] <- "missing"
  }

  out
}

make_beds_bin <- function(x) {
  out <- cut(
    x,
    breaks = c(-Inf, 1, 2, 3, Inf),
    labels = c("0-1", "2", "3", "4+"),
    right = TRUE
  )
  out <- as.character(out)
  out[is.na(out)] <- "missing"
  out
}

make_sqft_bin <- function(x) {
  out <- cut(
    x,
    breaks = c(-Inf, 600, 900, 1200, Inf),
    labels = c("<600", "600-899", "900-1199", "1200+"),
    right = FALSE
  )
  out <- as.character(out)
  out[is.na(out)] <- "missing"
  out
}

get_neighbors <- function(wards_sf, target_ward = 42L) {
  idx <- which(wards_sf$ward == target_ward)
  if (length(idx) != 1L) {
    stop(sprintf("Expected one polygon for ward %s, found %s", target_ward, length(idx)))
  }
  touches <- st_touches(wards_sf[idx, ], wards_sf, sparse = TRUE)[[1]]
  sort(unique(as.integer(wards_sf$ward[touches])))
}

summarize_panel <- function(base_dt, ward_vec, dataset_name, map_version, neighbors) {
  analysis <- data.table(
    year = as.integer(base_dt$year),
    rent_price = as.numeric(base_dt$rent_price),
    building_type_clean = as.character(base_dt$building_type_clean),
    beds_bin = as.character(base_dt$beds_bin),
    sqft_bin = as.character(base_dt$sqft_bin),
    ward = as.integer(ward_vec)
  )

  analysis <- analysis[!is.na(year) & !is.na(rent_price) & rent_price > 0]
  analysis[, log_rent := log(rent_price)]
  analysis[, area_group := classify_area(ward, neighbors)]
  analysis <- analysis[area_group != "Outside / Unmatched"]

  yearly <- analysis[, {
    qs <- quantile(rent_price, probs = c(0.1, 0.25, 0.75, 0.9), na.rm = TRUE, type = 7)
    .(
      n_listings = .N,
      mean_rent = mean(rent_price, na.rm = TRUE),
      median_rent = median(rent_price, na.rm = TRUE),
      p10_rent = as.numeric(qs[1]),
      p25_rent = as.numeric(qs[2]),
      p75_rent = as.numeric(qs[3]),
      p90_rent = as.numeric(qs[4]),
      mean_log_rent = mean(log_rent, na.rm = TRUE),
      median_log_rent = median(log_rent, na.rm = TRUE)
    )
  }, by = .(year, area_group)]

  yearly_totals <- yearly[, .(n_total = sum(n_listings)), by = year]
  yearly <- merge(yearly, yearly_totals, by = "year", all.x = TRUE)
  yearly[, share_of_dataset_year := n_listings / n_total]
  yearly[, c("dataset", "map_version") := .(dataset_name, map_version)]

  building <- analysis[, .(n = .N), by = .(year, area_group, building_type_clean)]
  building[, share_in_area_year := n / sum(n), by = .(year, area_group)]
  building[, c("dataset", "map_version") := .(dataset_name, map_version)]

  beds <- analysis[, .(n = .N), by = .(year, area_group, bin = beds_bin)]
  beds[, bin_type := "beds_bin"]
  beds[, share_in_area_year := n / sum(n), by = .(year, area_group)]

  sqft <- analysis[, .(n = .N), by = .(year, area_group, bin = sqft_bin)]
  sqft[, bin_type := "sqft_bin"]
  sqft[, share_in_area_year := n / sum(n), by = .(year, area_group)]

  size <- rbindlist(list(beds, sqft), use.names = TRUE)
  size[, c("dataset", "map_version") := .(dataset_name, map_version)]

  overall <- analysis[, .(
    n_listings = .N,
    mean_rent = mean(rent_price, na.rm = TRUE),
    median_rent = median(rent_price, na.rm = TRUE),
    mean_log_rent = mean(log_rent, na.rm = TRUE),
    median_log_rent = median(log_rent, na.rm = TRUE)
  ), by = area_group]

  overall[, share_of_dataset := n_listings / sum(n_listings)]
  overall[, c("dataset", "map_version") := .(dataset_name, map_version)]

  baseline <- analysis[, .(
    n_all = .N,
    median_rent_all = median(rent_price, na.rm = TRUE),
    mean_rent_all = mean(rent_price, na.rm = TRUE),
    mean_log_rent_all = mean(log_rent, na.rm = TRUE)
  ), by = year]

  exclusions <- c("Ward 42 (River North)", "Bordering Wards")
  leave_out <- rbindlist(lapply(exclusions, function(ex) {
    dt <- analysis[area_group != ex, .(
      n_excluding = .N,
      median_rent_excluding = median(rent_price, na.rm = TRUE),
      mean_rent_excluding = mean(rent_price, na.rm = TRUE),
      mean_log_rent_excluding = mean(log_rent, na.rm = TRUE)
    ), by = year]
    dt[, excluded_area := ex]
    dt
  }))

  leave_out <- merge(leave_out, baseline, by = "year", all.x = TRUE)
  leave_out[, delta_median_pct_vs_all := 100 * (median_rent_excluding / median_rent_all - 1)]
  leave_out[, delta_mean_pct_vs_all := 100 * (mean_rent_excluding / mean_rent_all - 1)]
  leave_out[, delta_mean_log_pts_vs_all := 100 * (mean_log_rent_excluding - mean_log_rent_all)]
  leave_out[, c("dataset", "map_version") := .(dataset_name, map_version)]

  list(
    yearly = yearly,
    building = building,
    size = size,
    overall = overall,
    leave_out = leave_out
  )
}

message("Loading ward boundaries...")
wards <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
wards_2015 <- wards %>% filter(year == 2015) %>% select(ward)
wards_2024 <- wards %>% filter(year == 2024) %>% select(ward)

neighbors_2015 <- get_neighbors(wards_2015, 42L)
neighbors_2024 <- get_neighbors(wards_2024, 42L)

neighbor_def <- data.table(
  map_version = c("2015", "2024"),
  target_ward = 42L,
  bordering_wards = c(
    paste(neighbors_2015, collapse = ";"),
    paste(neighbors_2024, collapse = ";")
  ),
  n_bordering_wards = c(length(neighbors_2015), length(neighbors_2024))
)

fwrite(neighbor_def, "../output/ward42_neighbors_definition.csv")

message("Creating ward-neighbor map figure...")
map_2015 <- wards_2015 %>% mutate(area_group = classify_area(as.integer(ward), neighbors_2015))
map_2024 <- wards_2024 %>% mutate(area_group = classify_area(as.integer(ward), neighbors_2024))

label_2015 <- st_point_on_surface(map_2015 %>% filter(area_group != "All Other Wards"))
label_2024 <- st_point_on_surface(map_2024 %>% filter(area_group != "All Other Wards"))

p_map_2015 <- ggplot(map_2015) +
  geom_sf(aes(fill = area_group), color = "white", linewidth = 0.15) +
  geom_sf_text(data = label_2015, aes(label = ward), size = 2.5) +
  scale_fill_manual(values = AREA_COLORS, drop = FALSE) +
  theme_void() +
  labs(title = "Ward 42 and Bordering Wards (2015 Map)", fill = NULL)

p_map_2024 <- ggplot(map_2024) +
  geom_sf(aes(fill = area_group), color = "white", linewidth = 0.15) +
  geom_sf_text(data = label_2024, aes(label = ward), size = 2.5) +
  scale_fill_manual(values = AREA_COLORS, drop = FALSE) +
  theme_void() +
  labs(title = "Ward 42 and Bordering Wards (2024 Map)", fill = NULL)

p_maps <- p_map_2015 + p_map_2024 + plot_layout(ncol = 2, guides = "collect")
ggsave("../output/fig_ward42_neighbor_maps.pdf", p_maps, width = 12, height = 7, bg = "white")

message("Loading full-market rent panel...")
full <- as.data.table(read_parquet(
  "../input/chicago_rent_panel.parquet",
  col_select = c("id", "file_date", "rent_price", "building_type", "beds", "sqft", "longitude", "latitude")
))

full[, file_date := as.Date(file_date)]
full[, year := year(file_date)]
full[, building_type_clean := clean_building_type(building_type)]
full[, beds_bin := make_beds_bin(beds)]
full[, sqft_bin := make_sqft_bin(sqft)]

message("Assigning wards to full-market listings for both map years...")
full[, `:=`(lon6 = round(longitude, 6), lat6 = round(latitude, 6))]
coord_lookup <- unique(full[!is.na(lon6) & !is.na(lat6), .(lon6, lat6)])

coord_sf <- st_as_sf(coord_lookup, coords = c("lon6", "lat6"), crs = 4326, remove = FALSE)

coord_2015 <- st_join(
  st_transform(coord_sf, st_crs(wards_2015)),
  wards_2015[, "ward"],
  join = st_within,
  left = TRUE
)

coord_2024 <- st_join(
  st_transform(coord_sf, st_crs(wards_2024)),
  wards_2024[, "ward"],
  join = st_within,
  left = TRUE
)

ward_lookup <- data.table(
  lon6 = coord_lookup$lon6,
  lat6 = coord_lookup$lat6,
  ward_2015 = as.integer(coord_2015$ward),
  ward_2024 = as.integer(coord_2024$ward)
)

full <- merge(full, ward_lookup, by = c("lon6", "lat6"), all.x = TRUE, sort = FALSE)
raw_type_lookup <- unique(full[, .(id, building_type_raw = building_type)])
full <- full[!is.na(year) & year >= 2010]
full[, c("lon6", "lat6") := NULL]

message("Loading regression-sample panel...")
sample <- as.data.table(read_parquet(
  "../input/rental_listing_panel.parquet",
  col_select = c("id", "year", "rent_price", "building_type_clean", "beds", "sqft", "ward_pair_side")
))

sample <- merge(sample, raw_type_lookup, by = "id", all.x = TRUE, sort = FALSE)
sample[, building_type_clean := normalize_clean_type(building_type_clean, building_type_raw)]
sample[, beds_bin := make_beds_bin(beds)]
sample[, sqft_bin := make_sqft_bin(sqft)]
sample[, ward_side := as.integer(sub(".*_", "", as.character(ward_pair_side)))]
sample[, building_type_raw := NULL]
sample <- sample[!is.na(year) & year >= 2010]

message("Computing yearly summaries for full market and regression sample...")
res_full_2015 <- summarize_panel(full, full$ward_2015, "full_market", "2015", neighbors_2015)
res_full_2024 <- summarize_panel(full, full$ward_2024, "full_market", "2024", neighbors_2024)
res_sample_2015 <- summarize_panel(sample, sample$ward_side, "regression_sample", "2015", neighbors_2015)
res_sample_2024 <- summarize_panel(sample, sample$ward_side, "regression_sample", "2024", neighbors_2024)

all_yearly <- rbindlist(list(
  res_full_2015$yearly,
  res_full_2024$yearly,
  res_sample_2015$yearly,
  res_sample_2024$yearly
), use.names = TRUE)

all_building <- rbindlist(list(
  res_full_2015$building,
  res_full_2024$building,
  res_sample_2015$building,
  res_sample_2024$building
), use.names = TRUE)

all_size <- rbindlist(list(
  res_full_2015$size,
  res_full_2024$size,
  res_sample_2015$size,
  res_sample_2024$size
), use.names = TRUE)

all_overall <- rbindlist(list(
  res_full_2015$overall,
  res_full_2024$overall,
  res_sample_2015$overall,
  res_sample_2024$overall
), use.names = TRUE)

all_leave_out <- rbindlist(list(
  res_full_2015$leave_out,
  res_full_2024$leave_out,
  res_sample_2015$leave_out,
  res_sample_2024$leave_out
), use.names = TRUE)

all_yearly[, area_group := factor(area_group, levels = AREA_LEVELS)]
all_building[, area_group := factor(area_group, levels = AREA_LEVELS)]
all_size[, area_group := factor(area_group, levels = AREA_LEVELS)]
all_overall[, area_group := factor(area_group, levels = AREA_LEVELS)]

fwrite(all_yearly, "../output/yearly_rent_trends.csv")
fwrite(all_building, "../output/yearly_building_type_shares.csv")
fwrite(all_size, "../output/yearly_size_bin_shares.csv")
fwrite(all_overall, "../output/overall_area_stats.csv")
fwrite(all_leave_out, "../output/yearly_leave_out_comparison.csv")

building_diag <- all_building[
  building_type_clean %in% c("other", "missing"),
  .(n = sum(n)),
  by = .(dataset, map_version, year, area_group, building_type_clean)
]
building_diag <- dcast(
  building_diag,
  dataset + map_version + year + area_group ~ building_type_clean,
  value.var = "n",
  fill = 0
)
if (!"other" %in% names(building_diag)) {
  building_diag[, other := 0L]
}
if (!"missing" %in% names(building_diag)) {
  building_diag[, missing := 0L]
}
building_diag <- merge(
  building_diag,
  all_yearly[, .(dataset, map_version, year, area_group, n_listings)],
  by = c("dataset", "map_version", "year", "area_group"),
  all.x = TRUE
)
building_diag[, share_other := fifelse(n_listings > 0, other / n_listings, NA_real_)]
building_diag[, share_missing := fifelse(n_listings > 0, missing / n_listings, NA_real_)]
fwrite(building_diag, "../output/yearly_building_type_other_missing_diagnostics.csv")

plot_yearly <- copy(all_yearly)
plot_yearly[, dataset := factor(dataset, levels = c("regression_sample", "full_market"), labels = c("Regression Sample", "Full Market"))]
plot_yearly[, map_version := factor(map_version, levels = c("2015", "2024"), labels = c("2015 Wards", "2024 Wards"))]

p_median <- ggplot(plot_yearly, aes(x = year, y = median_rent, color = area_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2) +
  facet_grid(dataset ~ map_version) +
  scale_color_manual(values = AREA_COLORS) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Yearly Median Rents: Ward 42, Bordering Wards, and the Rest",
    x = "Year",
    y = "Median Rent",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("../output/fig_yearly_median_rent_trends.pdf", p_median, width = 12, height = 7, bg = "white")

p_dist <- ggplot(plot_yearly, aes(x = year, group = area_group)) +
  geom_ribbon(aes(ymin = p10_rent, ymax = p90_rent, fill = area_group), alpha = 0.18) +
  geom_line(aes(y = median_rent, color = area_group), linewidth = 0.95) +
  facet_grid(dataset ~ map_version) +
  scale_color_manual(values = AREA_COLORS) +
  scale_fill_manual(values = AREA_COLORS) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Rent Distribution Over Time (p10-p90 band, median line)",
    x = "Year",
    y = "Rent",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("../output/fig_yearly_rent_distribution_ribbon.pdf", p_dist, width = 12, height = 7, bg = "white")

p_share <- ggplot(plot_yearly, aes(x = year, y = 100 * share_of_dataset_year, color = area_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.1) +
  facet_grid(dataset ~ map_version) +
  scale_color_manual(values = AREA_COLORS) +
  labs(
    title = "Share of Listings by Area Group Over Time",
    x = "Year",
    y = "Share of Dataset-Year (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("../output/fig_yearly_area_share.pdf", p_share, width = 12, height = 7, bg = "white")

plot_building <- copy(all_building)
plot_building[, dataset := factor(dataset, levels = c("regression_sample", "full_market"), labels = c("Regression Sample", "Full Market"))]
plot_building[, map_version := factor(map_version, levels = c("2015", "2024"), labels = c("2015 Wards", "2024 Wards"))]
plot_building[, building_type_clean := factor(
  building_type_clean,
  levels = c("multi_family", "condo", "single_family", "townhouse", "other", "missing")
)]

p_building <- ggplot(plot_building, aes(x = year, y = 100 * share_in_area_year, color = building_type_clean)) +
  geom_line(linewidth = 0.8) +
  facet_grid(dataset + map_version ~ area_group) +
  labs(
    title = "Building-Type Composition Over Time",
    x = "Year",
    y = "Share in Area-Year (%)",
    color = "Building Type"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

ggsave("../output/fig_building_type_shares.pdf", p_building, width = 13, height = 10, bg = "white")

plot_size <- copy(all_size)
plot_size <- plot_size[bin != "missing"]
plot_size[, dataset := factor(dataset, levels = c("regression_sample", "full_market"), labels = c("Regression Sample", "Full Market"))]
plot_size[, map_version := factor(map_version, levels = c("2015", "2024"), labels = c("2015 Wards", "2024 Wards"))]

p_beds <- ggplot(plot_size[bin_type == "beds_bin"], aes(x = year, y = 100 * share_in_area_year, color = bin)) +
  geom_line(linewidth = 0.8) +
  facet_grid(dataset + map_version ~ area_group) +
  labs(
    title = "Beds Bin Composition Over Time",
    x = "Year",
    y = "Share in Area-Year (%)",
    color = "Beds Bin"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

p_sqft <- ggplot(plot_size[bin_type == "sqft_bin"], aes(x = year, y = 100 * share_in_area_year, color = bin)) +
  geom_line(linewidth = 0.8) +
  facet_grid(dataset + map_version ~ area_group) +
  labs(
    title = "Sqft Bin Composition Over Time",
    x = "Year",
    y = "Share in Area-Year (%)",
    color = "Sqft Bin"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

p_size <- p_beds / p_sqft
ggsave("../output/fig_size_bin_shares.pdf", p_size, width = 13, height = 16, bg = "white")

message("Done. Outputs written to ../output/")
