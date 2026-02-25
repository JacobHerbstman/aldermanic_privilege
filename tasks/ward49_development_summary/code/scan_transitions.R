# setwd("~/Desktop/aldermanic_privilege/tasks/ward49_development_summary/code")
# Rscript scan_transitions.R --permits ../input/building_permits_clean.gpkg --panel ../input/chicago_alderman_panel.csv --out ../output/transition_scan.txt

source("../../setup_environment/code/packages.R")
library(optparse)
library(sf)
library(lubridate)

opts <- parse_args(OptionParser(option_list = list(
  make_option("--permits", type = "character"),
  make_option("--panel",   type = "character"),
  make_option("--out",     type = "character")
)))

WINDOW      <- 3   # years on each side (excluding transition year)
MIN_YRS     <- 2   # minimum years required on each side
DATA_START  <- 2008
DATA_END    <- 2024

# --- Load alderman panel, identify transitions ---
panel <- read_csv(opts$panel, col_names = c("month_yr", "ward", "alderman"),
                  show_col_types = FALSE) |>
  filter(month_yr != "month", !is.na(alderman), alderman != "NA",
         ward %in% as.character(1:50)) |>
  mutate(date = parse_date(paste("01", month_yr), "%d %b %Y"),
         ward = as.integer(ward),
         year = year(date)) |>
  arrange(ward, date)

transitions <- panel |>
  group_by(ward) |>
  mutate(prev = lag(alderman)) |>
  filter(!is.na(prev), alderman != prev) |>
  distinct(ward, year, .keep_all = TRUE) |>
  transmute(ward, transition_year = year, outgoing = prev, incoming = alderman) |>
  ungroup()

# --- Permit counts by ward x year (all 50 wards for city avg) ---
permits_wy <- st_drop_geometry(st_read(opts$permits, quiet = TRUE)) |>
  filter(!is.na(ward)) |>
  mutate(year = as.integer(format(application_start_date, "%Y")),
         ward = as.integer(ward)) |>
  filter(year >= DATA_START, year <= DATA_END, ward %in% 1:50) |>
  group_by(ward, year) |>
  summarise(
    n_new_const = sum(permit_type == "PERMIT - NEW CONSTRUCTION", na.rm = TRUE),
    n_renov     = sum(permit_type == "PERMIT - RENOVATION/ALTERATION", na.rm = TRUE),
    n_total     = n(),
    .groups = "drop"
  )

# City average per year (mean across all 50 wards)
city_avg <- permits_wy |>
  group_by(year) |>
  summarise(city_new_const = mean(n_new_const), .groups = "drop")

# --- Compute before/after for each transition ---
ward_windows <- transitions |>
  filter(transition_year > DATA_START, transition_year < DATA_END) |>
  left_join(permits_wy, by = "ward", relationship = "many-to-many") |>
  mutate(rel = year - transition_year) |>
  filter(rel != 0, abs(rel) <= WINDOW) |>
  mutate(era = if_else(rel < 0, "before", "after")) |>
  group_by(ward, transition_year, outgoing, incoming, era) |>
  summarise(
    n_yrs     = n(),
    new_const = mean(n_new_const),
    total     = mean(n_total),
    .groups   = "drop"
  )

# City averages for the same windows as each transition
# Expand transitions to one row per window-year, then join city_avg
city_windows <- transitions |>
  filter(transition_year > DATA_START, transition_year < DATA_END) |>
  rowwise() |>
  mutate(year = list(seq(transition_year - WINDOW, transition_year + WINDOW))) |>
  unnest(year) |>
  filter(year != transition_year) |>
  left_join(city_avg, by = "year") |>
  mutate(era = if_else(year < transition_year, "before", "after")) |>
  group_by(ward, transition_year, era) |>
  summarise(city_new_const = mean(city_new_const, na.rm = TRUE), .groups = "drop")

results <- ward_windows |>
  left_join(city_windows, by = c("ward", "transition_year", "era")) |>
  pivot_wider(names_from = era,
              values_from = c(n_yrs, new_const, total, city_new_const)) |>
  filter(!is.na(n_yrs_before), !is.na(n_yrs_after),
         n_yrs_before >= MIN_YRS, n_yrs_after >= MIN_YRS) |>
  mutate(
    d_raw   = new_const_after - new_const_before,
    d_city  = city_new_const_after - city_new_const_before,
    d_adj   = d_raw - d_city,                                      # DiD: ward change minus city trend
    pct_raw = d_raw / pmax(new_const_before, 0.5) * 100,
    pct_adj = d_adj / pmax(new_const_before, 0.5) * 100,
    pct_tot = (total_after - total_before) / total_before * 100
  ) |>
  arrange(d_adj)

# --- Print ranked table ---
hdr <- sprintf("%-4s  %-6s  %-24s  %-22s  %5s  %5s  %5s  %6s  %6s  %6s",
  "Ward", "Yr", "Outgoing", "Incoming",
  "Bef", "Aft", "City", "D_Raw", "D_Adj", "Pct_Adj")
sep <- strrep("-", nchar(hdr))

print_table <- function(con) {
  cat(con, "Aldermanic Transition Scan: New Construction, city-detrended (", WINDOW, "-yr window)\n")
  cat(con, strrep("=", nchar(hdr)), "\n")
  cat(con, hdr, "\n")
  cat(con, sep, "\n")
  for (i in seq_len(nrow(results))) {
    r <- results[i, ]
    flag <- if (r$n_yrs_before < WINDOW | r$n_yrs_after < WINDOW) "*" else " "
    cat(con, sprintf(
      "%-4d  %-6d  %-24s  %-22s  %5.1f  %5.1f  %5.1f  %+6.1f  %+6.1f  %+5.0f%%%s\n",
      r$ward, r$transition_year,
      substr(r$outgoing, 1, 24), substr(r$incoming, 1, 22),
      r$new_const_before, r$new_const_after, r$city_new_const_before,
      r$d_raw, r$d_adj, r$pct_adj, flag))
  }
  cat(con, sep, "\n")
  cat(con, "Bef/Aft = ward avg new const/yr; City = city avg in before window\n")
  cat(con, "D_Raw = raw ward change; D_Adj = DiD (ward change minus city trend change); Pct_Adj = D_Adj / Bef\n")
  cat(con, "* = fewer than", WINDOW, "years on one side\n")
  cat(con, "Data: Chicago building permits 2008-2024, application_start_date.\n")
}

print_table("")
sink(opts$out); print_table(""); sink()
cat("\nOutput written to", opts$out, "\n")
