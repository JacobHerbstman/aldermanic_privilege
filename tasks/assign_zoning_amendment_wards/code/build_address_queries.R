# setwd("tasks/assign_zoning_amendment_wards/code")
# The address to geocode from each zoning map amendment's title (tasks/clean_zoning_map_amendments): the first one
# after "at", with an address range reduced to its first number ("3939-3935 W Devon Ave" becomes 3939 W Devon Ave).
# Titles naming only an intersection or no address have no query. One row per amendment, with its query and the query's
# number among the distinct queries in alphabetical order.

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

amendments <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE)
addresses <- amendments |>
  transmute(matter_id,
    title_address = str_squish(str_match(title, regex("\\bat\\s+(.+?)\\s*(?:-\\s*App|,|;|\\s+and\\s+|$)",
      ignore_case = TRUE))[, 2]),
    address_query = str_replace(str_to_upper(title_address), "^([0-9]+)\\s*-\\s*[0-9]+\\b", "\\1")) |>
  mutate(address_query = if_else(str_detect(address_query, "^[0-9]+\\s+[NSEW]\\b"), address_query, NA_character_))

queries <- addresses |>
  filter(!is.na(address_query)) |>
  distinct(address_query) |>
  arrange(address_query) |>
  mutate(address_id = row_number())
SaveData(left_join(addresses, queries, by = "address_query", relationship = "many-to-one"), "matter_id",
  "../output/zoning_amendment_address_queries.csv")
