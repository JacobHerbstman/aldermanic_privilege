# setwd("~/Desktop/aldermanic_privilege/tasks/ward49_development_summary/code")
# Rscript ward49_development_summary.R --permits ../input/building_permits_clean.gpkg --out_plot ../output/ward_permits_by_year.pdf --out_table ../output/ward_summary_table.txt

source("../../setup_environment/code/packages.R")
library(optparse)
library(sf)

opts <- parse_args(OptionParser(option_list = list(
  make_option("--permits",   type = "character"),
  make_option("--out_plot",  type = "character"),
  make_option("--out_table", type = "character")
)))

ANALYSIS_START <- 2008
ANALYSIS_END   <- 2024

# Ward configs: political facts about each transition
wards_cfg <- tribble(
  ~ward, ~before,                    ~after,               ~transition_year,
      1, "Proco Joe Moreno",         "Daniel La Spata",                2019,
     11, "P. Daley Thompson",        "Nicole Lee",                     2022,
     25, "Daniel Solis",             "Byron Sigcho-Lopez",             2019,
     49, "Joe Moore",                "Maria Hadden",                   2019
) |>
  mutate(
    ward_label = sprintf("Ward %d\n%s -> %s (%d)", ward, before, after, transition_year)
  )

all_permits <- st_drop_geometry(st_read(opts$permits, quiet = TRUE)) |>
  filter(!is.na(ward)) |>
  mutate(year = as.integer(format(application_start_date, "%Y")),
         ward = as.integer(ward)) |>
  filter(year >= ANALYSIS_START, year <= ANALYSIS_END, ward %in% 1:50)

# City-average new construction per year across all 50 wards
city_avg <- all_permits |>
  group_by(ward, year) |>
  summarise(n_new = sum(permit_type == "PERMIT - NEW CONSTRUCTION", na.rm = TRUE),
            .groups = "drop") |>
  group_by(year) |>
  summarise(city_new_avg = mean(n_new), .groups = "drop")

# Focus ward data
permits <- all_permits |>
  filter(ward %in% wards_cfg$ward) |>
  left_join(wards_cfg, by = "ward")

by_year <- permits |>
  group_by(ward, ward_label, transition_year, year) |>
  summarise(
    n_new_construction = sum(permit_type == "PERMIT - NEW CONSTRUCTION", na.rm = TRUE),
    n_renovation       = sum(permit_type == "PERMIT - RENOVATION/ALTERATION", na.rm = TRUE),
    n_total            = n(),
    .groups = "drop"
  ) |>
  left_join(city_avg, by = "year")

# Expected trend line: ward pre-mean * (city_t / city_pre_mean)
# Answers: what would this ward look like if it just tracked the city trend?
ward_pre_means <- by_year |>
  left_join(select(wards_cfg, ward, transition_year), by = c("ward", "transition_year")) |>
  filter(year < transition_year) |>
  group_by(ward) |>
  summarise(
    ward_pre_mean = mean(n_new_construction),
    city_pre_mean = mean(city_new_avg),
    .groups = "drop"
  )

by_year <- by_year |>
  left_join(ward_pre_means, by = "ward") |>
  mutate(expected_new = ward_pre_mean * (city_new_avg / city_pre_mean))

# --- Plot ---
vlines <- wards_cfg |>
  select(ward_label, transition_year) |>
  mutate(xintercept = transition_year + 0.5)

p <- ggplot(by_year, aes(x = year)) +
  geom_line(aes(y = n_new_construction, color = "New Construction (actual)"), linewidth = 0.9) +
  geom_point(aes(y = n_new_construction, color = "New Construction (actual)"), size = 1.8) +
  geom_line(aes(y = expected_new, color = "Expected (city trend)"),
            linetype = "dotted", linewidth = 0.9) +
  geom_vline(data = vlines, aes(xintercept = xintercept),
             linetype = "dashed", color = "gray40", inherit.aes = FALSE) +
  facet_wrap(~ward_label, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(ANALYSIS_START, ANALYSIS_END, by = 2)) +
  scale_color_manual(values = c(
    "New Construction (actual)" = "#1f77b4",
    "Expected (city trend)"     = "gray60"
  )) +
  labs(
    title    = "New Construction Permits: Actual vs. City Trend",
    subtitle = "Dotted = expected count if ward tracked citywide trend. Dashed = transition.",
    x = NULL, y = "New Construction Permits/yr", color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", strip.text = element_text(size = 9))

ggsave(opts$out_plot, p, width = 7, height = 10)

# --- Summary table ---
summary_tbl <- by_year |>
  left_join(select(wards_cfg, ward, before, after), by = "ward") |>
  mutate(era = case_when(
    year < transition_year ~ "before",
    year > transition_year ~ "after",
    TRUE ~ "transition"
  )) |>
  filter(era != "transition") |>
  group_by(ward, era, before, after, transition_year) |>
  summarise(
    n_years          = n(),
    new_construction = mean(n_new_construction),
    city_new_avg     = mean(city_new_avg),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = era,
              values_from = c(n_years, new_construction, city_new_avg)) |>
  mutate(
    d_raw  = new_construction_after - new_construction_before,
    d_city = city_new_avg_after    - city_new_avg_before,
    d_adj  = d_raw - d_city,
    pct_adj = d_adj / pmax(new_construction_before, 0.5) * 100
  ) |>
  arrange(ward)

hdr <- sprintf("%-4s  %-24s -> %-22s  %5s  %5s  %+7s  %+7s  %+7s  %+7s",
  "Ward", "Before", "After", "Bef", "Aft", "D_Raw", "D_City", "D_Adj", "Pct_Adj")

print_table <- function(con) {
  cat(con, "\nNew Construction Permits: Ward Transitions with City Detrending\n")
  cat(con, strrep("=", nchar(hdr)), "\n")
  cat(con, hdr, "\n")
  cat(con, strrep("-", nchar(hdr)), "\n")
  for (i in seq_len(nrow(summary_tbl))) {
    r <- summary_tbl[i, ]
    flag <- if (r$n_years_after <= 2) " *" else ""
    cat(con, sprintf(
      "%-4d  %-24s -> %-22s  %5.1f  %5.1f  %+7.1f  %+7.1f  %+7.1f  %+6.0f%%%s\n",
      r$ward, substr(r$before, 1, 24), substr(r$after, 1, 22),
      r$new_construction_before, r$new_construction_after,
      r$d_raw, r$d_city, r$d_adj, r$pct_adj, flag))
  }
  cat(con, strrep("-", nchar(hdr)), "\n")
  cat(con, "Bef/Aft = avg new const/yr (full era). D_City = citywide avg change over same years.\n")
  cat(con, "D_Adj = DiD (ward change minus city trend). Pct_Adj = D_Adj / Bef.\n")
  cat(con, "* = 2 or fewer years of post-transition data.\n")
  cat(con, "Data: Chicago building permits 2008-2024, application_start_date.\n")
}

print_table("")
sink(opts$out_table); print_table(""); sink()
