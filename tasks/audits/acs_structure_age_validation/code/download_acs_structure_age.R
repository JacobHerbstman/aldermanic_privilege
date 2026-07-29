# American Community Survey table B25127, Tenure by Year Structure Built by
# Units in Structure, for the City of Chicago.
#
# The 2015-2019 five-year vintage is used because it reports "Built 2010 or
# later" as its own category. Later vintages collapse 2000-2019 into one bin,
# which cannot be compared to a 2010s construction window.
#
# The universe is OCCUPIED housing units, so these counts are a lower bound on
# units built: a newly built unit that is vacant at survey time is excluded.
#
# setwd("tasks/audits/acs_structure_age_validation/code")
# acs_year <- "2019"

source("../../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop("Usage: download_acs_structure_age.R <acs_year>")
}
acs_year <- as.integer(args[1])

if (Sys.getenv("CENSUS_API_KEY") == "") {
  stop("CENSUS_API_KEY not found in the environment.", call. = FALSE)
}
census_api_key(Sys.getenv("CENSUS_API_KEY"))

# Built 2010 or later, by units in structure, for owner- and renter-occupied
# units. Variable numbers are specific to the 2015-2019 vintage of B25127.
acs_vars <- c(
  owner_units1 = "B25127_004",
  owner_units2to4 = "B25127_005",
  owner_units5to19 = "B25127_006",
  owner_units20to49 = "B25127_007",
  owner_units50plus = "B25127_008",
  owner_mobile = "B25127_009",
  renter_units1 = "B25127_047",
  renter_units2to4 = "B25127_048",
  renter_units5to19 = "B25127_049",
  renter_units20to49 = "B25127_050",
  renter_units50plus = "B25127_051",
  renter_mobile = "B25127_052"
)

raw <- get_acs(
  geography = "place",
  variables = acs_vars,
  state = "IL",
  year = acs_year,
  survey = "acs5",
  output = "tidy"
)

chicago <- raw |>
  dplyr::filter(GEOID == "1714000")

if (nrow(chicago) != length(acs_vars)) {
  stop(
    sprintf(
      "Expected %d Chicago rows from B25127; received %d.",
      length(acs_vars), nrow(chicago)
    ),
    call. = FALSE
  )
}
if (!all(grepl("^Chicago city", chicago$NAME))) {
  stop("GEOID 1714000 did not resolve to Chicago city.", call. = FALSE)
}

# Confirm the vintage really reports "Built 2010 or later" as its own category,
# so a later collapsed-bin vintage cannot be substituted silently.
label_check <- tidycensus::load_variables(acs_year, "acs5", cache = TRUE) |>
  dplyr::filter(name == "B25127_005") |>
  dplyr::pull(label)
if (!any(grepl("2010 or later", label_check))) {
  stop(
    sprintf("B25127_005 in the %d vintage is not a 'Built 2010 or later' cell.", acs_year),
    call. = FALSE
  )
}

output <- chicago |>
  dplyr::transmute(
    acs_year = acs_year,
    geoid = GEOID,
    place_name = NAME,
    measure = variable,
    tenure = dplyr::if_else(grepl("^owner", variable), "owner", "renter"),
    structure_size = sub("^(owner|renter)_", "", variable),
    occupied_units = estimate,
    margin_of_error = moe
  ) |>
  dplyr::arrange(tenure, structure_size)

readr::write_csv(output, "../output/acs_b25127_chicago_built_2010_or_later.csv")

message(sprintf(
  "\nChicago occupied units built 2010 or later (%d ACS 5-year): %d\n",
  acs_year, sum(output$occupied_units)
))
print(as.data.frame(output |> dplyr::select(tenure, structure_size, occupied_units, margin_of_error)),
      row.names = FALSE)
