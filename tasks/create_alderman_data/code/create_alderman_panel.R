# This code handmakes a panel of aldermen and uses the "majority of the month" rule to assign alderman to wards monthly from 2003-01 to 2025-06

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# Aldermanic data
alderman_data <- tribble(
  ~ward, ~alderman, ~start_date, ~end_date,
  1, "Jesse Granato",       "2003-01-01", "2003-05-04",
  1, "Manuel Flores",       "2003-05-05", "2010-01-04",
  1, "Proco Joe Moreno",    "2010-03-26", "2019-05-19",
  1, "Daniel La Spata",     "2019-05-20", "2025-06-24",
  2, "Madeline Haithcock", "2003-01-01", "2007-05-20",
  2, "Robert Fioretti", "2007-05-21", "2015-05-17",
  2, "Brian Hopkins", "2015-05-18", "2025-06-24",
  3, "Dorothy Tillman",     "2003-01-01", "2007-05-20",
  3, "Pat Dowell",          "2007-05-21", "2025-06-24",
  4, "Toni Preckwinkle",    "2003-01-01", "2010-12-05",
  4, "Shirley Newsome",     "2011-01-13", "2011-05-15",
  4, "William D. Burns",    "2011-05-16", "2016-02-01",
  4, "Sophia King",         "2016-04-13", "2023-05-14",
  4, "Lamont Robinson",     "2023-05-15", "2025-06-24",
  5, "Leslie Hairston", "2003-01-01", "2023-05-14",
  5, "Desmon Yancy", "2023-05-15", "2025-06-24",
  6, "Fredrenna Lyle", "2003-01-01", "2011-05-15",
  6, "Roderick Sawyer", "2011-05-16", "2023-05-14",
  6, "William Hall", "2023-05-15", "2025-06-24",
  7, "William Beavers", "2003-01-01", "2006-12-03",
  7, "Darcel Beavers", "2006-12-04", "2007-05-20",
  7, "Sandi Jackson", "2007-05-21", "2013-01-11",
  7, "Natashia Holmes", "2013-02-13", "2015-05-17",
  7, "Gregory Mitchell", "2015-05-18", "2025-06-24",
  8, "Todd Stroger", "2003-01-01", "2006-12-03",
  8, "Michelle A. Harris", "2006-12-04", "2025-06-24",
  9, "Anthony Beale", "2003-01-01", "2025-06-24",
  10, "John Pope", "2003-01-01", "2015-05-17",
  10, "Susan Sadlowski Garza", "2015-05-18", "2023-05-14",
  10, "Peter Chico", "2023-05-15", "2025-06-24",
  11, "James Balcer", "2003-01-01", "2015-05-17",
  11, "Patrick Daley Thompson", "2015-05-18", "2022-02-28",
  11, "Nicole Lee", "2022-03-28", "2025-06-24",
  12, "Ray Frias",          "2003-01-01", "2003-05-04",
  12, "George Cardenas",    "2003-05-05", "2022-11-30",
  12, "Anabel Abarca",      "2022-12-14", "2023-05-14",
  12, "Julia Ramirez",      "2023-05-15", "2025-06-24",
  13, "Frank Olivo", "2003-01-01", "2010-12-31",
  13, "Marty Quinn", "2011-01-01", "2025-06-24",
  14, "Ed Burke", "2003-01-01", "2023-05-14",
  14, "Jeylu Gutierrez", "2023-05-15", "2025-06-24",
  15, "Ted Thomas", "2003-01-01", "2006-12-31",
  15, "Toni Foulkes", "2007-01-01", "2015-05-17",
  15, "Raymond Lopez", "2015-05-18", "2025-06-24",
  16, "Shirley Coleman", "2003-01-01", "2007-05-20",
  16, "JoAnn Thompson", "2007-05-21", "2015-02-09",
  16, "Toni Foulkes", "2015-05-18", "2019-05-19",
  16, "Stephanie Coleman", "2019-05-20", "2025-06-24",
  17, "Latasha Thomas",     "2003-01-01", "2015-05-17",
  17, "David Moore",        "2015-05-18", "2025-06-24",
  18, "Thomas Murphy",      "2003-01-01", "2006-12-12",
  18, "Lona Lane",          "2006-12-13", "2015-05-17",
  18, "Derrick Curtis",     "2015-05-18", "2025-06-24",
  19, "Virginia Rugai", "2003-01-01", "2011-05-15",
  19, "Matthew O'Shea", "2011-05-16", "2025-06-24",
  20, "Arenda Troutman", "2003-01-01", "2007-02-28",
  20, "Willie Cochran", "2007-03-01", "2019-03-23",
  20, "Jeanette Taylor", "2019-05-20", "2025-06-24",
  21, "Howard Brookins", "2003-01-01", "2023-05-14",
  21, "Ronnie Mosley", "2023-05-15", "2025-06-24",
  22, "Ricardo Munoz", "2003-01-01", "2019-05-19",
  22, "Michael Rodriguez", "2019-05-20", "2025-06-24",
  23, "Michael Zalewski", "2003-01-01", "2018-05-31",
  23, "Silvana Tabares", "2018-06-27", "2025-06-24",
  24, "Michael Chandler",        "2003-01-01", "2007-05-20",
  24, "Sharon Denise Dixon",     "2007-05-21", "2011-05-15",
  24, "Michael Chandler",        "2011-05-16", "2015-05-17",
  24, "Michael Scott Jr.",       "2015-05-18", "2022-06-15",  
  25, "Daniel Solis", "2003-01-01", "2019-05-19",
  25, "Byron Sigcho-Lopez", "2019-05-20", "2025-06-24",
  26, "Billy Ocasio", "2003-01-01", "2009-07-26",
  26, "Roberto Maldonado", "2009-08-01", "2023-05-14",
  26, "Jessie Fuentes", "2023-05-15", "2025-06-24",
  27, "Walter Burnett, Jr.", "2003-01-01", "2025-06-24",
  28, "Ed Smith", "2003-01-01", "2010-12-31",
  28, "Jason Ervin", "2011-01-01", "2025-06-24",
  29, "Isaac Carothers", "2003-01-01", "2010-02-28",
  29, "Deborah Graham", "2010-03-01", "2015-05-17",
  29, "Chris Taliaferro", "2015-05-18", "2025-06-24",
  30, "Ariel Reboyras", "2003-01-01", "2023-05-14",
  30, "Ruth Cruz", "2023-05-15", "2025-06-24",
  31, "Ray Suarez", "2003-01-01", "2015-05-17",
  31, "Milagros Santiago", "2015-05-18", "2019-05-19",
  31, "Felix Cardona, Jr.", "2019-05-20", "2025-06-24",
  32, "Theodore Matlak",    "2003-01-01", "2007-05-20",
  32, "Scott Waguespack",   "2007-05-21", "2025-06-24",
  33, "Dick Mell", "2003-01-01", "2013-07-24",
  33, "Deb Mell", "2013-07-24", "2019-05-19",
  33, "Rossana Rodriguez-Sanchez", "2019-05-20", "2025-06-24",
  34, "Carrie Austin", "2003-01-01", "2023-05-14",
  34, "Bill Conway", "2023-05-15", "2025-06-24",
  35, "Vilma Colom",        "2003-01-01", "2003-05-04",
  35, "Rey Colon",          "2003-05-05", "2015-05-17",
  35, "Carlos Ramirez-Rosa","2015-05-18", "2025-06-24",
  36, "William Banks", "2003-01-01", "2009-10-31",
  36, "John Rice", "2009-11-01", "2011-05-15",
  36, "Nicholas Sposato", "2011-05-16", "2015-05-17",
  36, "Gilbert Villegas", "2015-05-18", "2025-06-24",
  37, "Emma Mitts", "2003-01-01", "2025-06-24",
  38, "Thomas Allen", "2003-01-01", "2011-05-15",
  38, "Timothy Cullerton", "2011-05-16", "2015-05-17",
  38, "Nicholas Sposato", "2015-05-18", "2025-06-24",
  39, "Margaret Laurino", "2003-01-01", "2019-05-19",
  39, "Samantha Nugent", "2019-05-20", "2025-06-24",
  40, "Patrick O'Connor", "2003-01-01", "2019-05-19",
  40, "Andre Vasquez", "2019-05-20", "2025-06-24",
  41, "Brian Doherty", "2003-01-01", "2011-05-15",
  41, "Mary O'Connor", "2011-05-16", "2015-05-17",
  41, "Anthony Napolitano", "2015-05-18", "2025-06-24",
  42, "Burton Natarus", "2003-01-01", "2007-05-20",
  42, "Brendan Reilly", "2007-05-21", "2025-06-24",
  43, "Vi Daley", "2003-01-01", "2011-05-15",
  43, "Michele Smith", "2011-05-16", "2022-08-12",
  43, "Timmy Knudsen", "2022-09-21", "2025-06-24",
  44, "Tom Tunney", "2003-01-01", "2023-05-14",
  44, "Bennett Lawson", "2023-05-15", "2025-06-24",
  45, "Patrick Levar", "2003-01-01", "2011-05-15",
  45, "John Arena", "2011-05-16", "2019-05-19",
  45, "Jim Gardiner", "2019-05-20", "2025-06-24",
  46, "Helen Shiller", "2003-01-01", "2011-05-15",
  46, "James Cappleman", "2011-05-16", "2023-05-14",
  46, "Angela Clay", "2023-05-15", "2025-06-24",
  47, "Eugene Schulter", "2003-01-01", "2011-05-15",
  47, "Ameya Pawar", "2011-05-16", "2019-05-19",
  47, "Matt Martin", "2019-05-20", "2025-06-24",
  48, "Mary Ann Smith", "2003-01-01", "2011-05-15",
  48, "Harry Osterman", "2011-05-16", "2023-05-14",
  48, "Leni Manaa-Hoppenworth", "2023-05-15", "2025-06-24",
  49, "Joe Moore", "2003-01-01", "2019-05-19",
  49, "Maria Hadden", "2019-05-20", "2025-06-24",
  50, "Bernard Stone", "2003-01-01", "2011-05-15",
  50, "Debra Silverstein", "2011-05-16", "2025-06-24"
) %>%
  mutate(across(c(start_date, end_date), as.Date))

# Finance Committee Chair data
finance_chair_data <- tribble(
  ~alderman, ~start_date, ~end_date,
  "Ed Burke", "2003-01-01", "2019-05-19",
  "Scott Waguespack", "2019-05-20", "2023-05-14",
  "Pat Dowell", "2023-05-15", "2025-06-24"
) %>%
  mutate(across(c(start_date, end_date), as.Date), finance_chair = 1)

# Zoning Committee Chair data
zoning_chair_data <- tribble(
  ~alderman, ~start_date, ~end_date,
  "William J.P. Banks", "2003-01-01", "2011-05-15",
  "Daniel Solis", "2011-05-16", "2019-05-19",
  "James Cappleman", "2019-05-20", "2023-05-14",
  "Carlos Ramirez-Rosa", "2023-05-15", "2025-06-24"
) %>%
  mutate(across(c(start_date, end_date), as.Date), zoning_chair = 1)

# Budget and Government Operations Committee Chair data
budget_chair_data <- tribble(
  ~alderman, ~start_date, ~end_date,
  "Carrie Austin", "2003-01-01", "2019-05-19",
  "Pat Dowell", "2019-05-20", "2023-05-14",
  "Jason Ervin", "2023-05-15", "2025-06-24"
) %>%
  mutate(across(c(start_date, end_date), as.Date), budget_chair = 1)


# Create a month-year panel using zoo::as.yearmon
panel_grid <- expand_grid(
  year_month = as.yearmon(seq(as.Date("2003-01-01"), as.Date("2025-06-01"), by = "months")),
  ward = 1:50
)

# --- New helpers: strict majority-of-month logic (month-length aware) ---
gets_month_start <- function(date) {
  # TRUE iff the incoming alderperson holds strictly more than half of the days in 'date''s month
  if (is.na(date)) return(FALSE)
  n <- lubridate::days_in_month(date)
  remaining <- n - lubridate::day(date) + 1L
  remaining > n/2
}

gets_month_end <- function(date) {
  # TRUE iff the outgoing alderperson held strictly more than half of the days in 'date''s month
  if (is.na(date)) return(FALSE)
  n <- lubridate::days_in_month(date)
  elapsed <- lubridate::day(date)
  elapsed > n/2
}

# --- Replace your entire get_alderman() with this version ---
get_alderman <- function(current_year_month, current_ward) {
  ward_data <- alderman_data %>% dplyr::filter(ward == current_ward)
  
  # As before, we’ll compare in yearmon space (12ths of a year)
  for (i in seq_len(nrow(ward_data))) {
    term <- ward_data[i, ]
    
    start_month <- if (gets_month_start(term$start_date)) {
      zoo::as.yearmon(term$start_date)
    } else {
      zoo::as.yearmon(term$start_date) + 1/12
    }
    
    end_month <- if (gets_month_end(term$end_date)) {
      zoo::as.yearmon(term$end_date)
    } else {
      zoo::as.yearmon(term$end_date) - 1/12
    }
    
    # small numeric guard; compare on rounded yearmon
    if (round(current_year_month, 4) >= round(start_month, 4) &&
        round(current_year_month, 4) <= round(end_month, 4)) {
      return(term$alderman)
    }
  }
  return(NA_character_)  # if no one holds the strict majority that month
}

# Apply the function to the panel grid
final_panel <- panel_grid %>%
  rowwise() %>%
  mutate(alderman = get_alderman(year_month, ward)) %>%
  ungroup()

# Add committee chair indicators
final_panel <- final_panel %>%
  mutate(year_month_date = as.Date(year_month)) # Helper column for joining

# Join Finance Chair info
final_panel <- final_panel %>%
  left_join(finance_chair_data, by = "alderman") %>%
  mutate(finance_chair = if_else(year_month_date >= start_date & year_month_date <= end_date & !is.na(finance_chair), 1, 0)) %>%
  select(-start_date, -end_date)

# Join Zoning Chair info
final_panel <- final_panel %>%
  left_join(zoning_chair_data, by = "alderman") %>%
  mutate(zoning_chair = if_else(year_month_date >= start_date & year_month_date <= end_date & !is.na(zoning_chair), 1, 0)) %>%
  select(-start_date, -end_date)

# Join Budget Chair info
final_panel <- final_panel %>%
  left_join(budget_chair_data, by = "alderman") %>%
  mutate(budget_chair = if_else(year_month_date >= start_date & year_month_date <= end_date & !is.na(budget_chair), 1, 0)) %>%
  select(-start_date, -end_date, year_month_date) %>%
  rename(month = year_month)


# Write to CSV
write_csv(final_panel, "../output/chicago_alderman_panel.csv")

print("CSV file created successfully: chicago_alderman_panel.csv")