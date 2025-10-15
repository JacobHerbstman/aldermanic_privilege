# This code handmakes a panel of aldermen and uses the "majority of the month" rule to assign alderman to wards monthly from 2003-01 to 2025-06

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# Aldermanic data
alderman_data <- tribble(
  ~ward, ~alderman, ~start_date, ~end_date,
  
  # 1st Ward
  1, "Jesse Granato",       "1998-01-01", "2003-05-04",
  1, "Manuel Flores",       "2003-05-05", "2010-01-04",
  1, "Proco Joe Moreno",    "2010-03-26", "2019-05-19",
  1, "Daniel La Spata",     "2019-05-20", "2025-06-24",
  
  # 2nd Ward
  2, "Madeline Haithcock",  "1998-01-01", "2007-05-20",
  2, "Robert Fioretti",     "2007-05-21", "2015-05-17",
  2, "Brian Hopkins",       "2015-05-18", "2025-06-24",
  
  # 3rd Ward
  3, "Dorothy Tillman",     "1998-01-01", "2007-05-20",
  3, "Pat Dowell",          "2007-05-21", "2025-06-24",
  
  # 4th Ward
  4, "Toni Preckwinkle",    "1998-01-01", "2010-12-05",
  4, "Shirley Newsome",     "2011-01-13", "2011-05-15",
  4, "William D. Burns",    "2011-05-16", "2016-02-01",
  4, "Sophia King",         "2016-04-13", "2023-05-14",
  4, "Lamont Robinson",     "2023-05-15", "2025-06-24",
  
  # 5th Ward
  5, "Barbara Holt",        "1998-01-01", "1999-05-02",
  5, "Leslie Hairston",     "1999-05-03", "2023-05-14",
  5, "Desmon Yancy",        "2023-05-15", "2025-06-24",
  
  # 6th Ward
  6, "John O. Steele",      "1998-01-01", "1998-02-04",
  6, "Fredrenna Lyle",      "1998-02-05", "2011-05-15",
  6, "Roderick Sawyer",     "2011-05-16", "2023-05-14",
  6, "William Hall",        "2023-05-15", "2025-06-24",
  
  # 7th Ward
  7, "William Beavers",     "1998-01-01", "2006-12-03",
  7, "Darcel Beavers",      "2006-12-04", "2007-05-20",
  7, "Sandi Jackson",       "2007-05-21", "2013-01-11",
  7, "Natashia Holmes",     "2013-02-13", "2015-05-17",
  7, "Gregory Mitchell",    "2015-05-18", "2025-06-24",
  
  # 8th Ward
  8, "Lorraine Dixon",      "1998-01-01", "2001-06-29",
  8, "Todd Stroger",        "2001-09-05", "2006-12-03",
  8, "Michelle A. Harris",  "2006-12-04", "2025-06-24",
  
  # 9th Ward
  9, "Robert Shaw",         "1998-01-01", "1998-12-31",  # resigned Dec 1998
  9, "Melvin Powell Sr.",   "1999-01-01", "1999-05-02",  # appointed; placeholder day
  9, "Anthony Beale",       "1999-05-03", "2025-06-24",
  
  # 10th Ward
  10, "John J. Buchanan",   "1998-01-01", "1999-05-02",
  10, "John Pope",          "1999-05-03", "2015-05-17",
  10, "Susan Sadlowski Garza","2015-05-18","2023-05-14",
  10, "Peter Chico",        "2023-05-15", "2025-06-24",
  
  # 11th Ward
  11, "James Balcer",       "1998-01-01", "2015-05-17",
  11, "Patrick Daley Thompson","2015-05-18","2022-02-28",
  11, "Nicole Lee",         "2022-03-28", "2025-06-24",
  
  # 12th Ward
  12, "Ray Frias",          "1998-01-01", "2003-05-04",
  12, "George Cardenas",    "2003-05-05", "2022-11-30",
  12, "Anabel Abarca",      "2022-12-14", "2023-05-14",
  12, "Julia Ramirez",      "2023-05-15", "2025-06-24",
  
  # 13th Ward
  13, "Frank Olivo",        "1998-01-01", "2010-12-31",
  13, "Marty Quinn",        "2011-01-01", "2025-06-24",
  
  # 14th Ward
  14, "Ed Burke",           "1998-01-01", "2023-05-14",
  14, "Jeylu Gutierrez",    "2023-05-15", "2025-06-24",
  
  # 15th Ward
  15, "Virgil E. Jones",    "1998-01-01", "1999-05-02",
  15, "Ted Thomas",         "1999-05-03", "2006-12-31",
  15, "Toni Foulkes",       "2007-01-01", "2015-05-17",
  15, "Raymond Lopez",      "2015-05-18", "2025-06-24",
  
  # 16th Ward
  16, "Shirley Coleman",    "1998-01-01", "2007-05-20",
  16, "JoAnn Thompson",     "2007-05-21", "2015-02-09",
  16, "Toni Foulkes",       "2015-05-18", "2019-05-19",
  16, "Stephanie Coleman",  "2019-05-20", "2025-06-24",
  
  # 17th Ward (per your instruction)
  17, "Terry Peterson",     "1998-01-01", "2000-07-31",
  17, "Latasha Thomas",     "2000-08-01", "2015-05-17",
  17, "David Moore",        "2015-05-18", "2025-06-24",
  
  # 18th Ward
  18, "Thomas Murphy",      "1998-01-01", "2006-12-12",
  18, "Lona Lane",          "2006-12-13", "2015-05-17",
  18, "Derrick Curtis",     "2015-05-18", "2025-06-24",
  
  # 19th Ward
  19, "Virginia Rugai",     "1998-01-01", "2011-05-15",
  19, "Matthew O'Shea",     "2011-05-16", "2025-06-24",
  
  # 20th Ward
  20, "Arenda Troutman",    "1998-01-01", "2007-02-28",
  20, "Willie Cochran",     "2007-03-01", "2019-03-23",
  20, "Jeanette Taylor",    "2019-05-20", "2025-06-24",
  
  # 21st Ward
  21, "Leonard DeVille",    "1998-01-01", "2003-05-04",
  21, "Howard Brookins",    "2003-05-05", "2023-05-14",
  21, "Ronnie Mosley",      "2023-05-15", "2025-06-24",
  
  # 22nd Ward
  22, "Ricardo Munoz",      "1998-01-01", "2019-05-19",
  22, "Michael Rodriguez",  "2019-05-20", "2025-06-24",
  
  # 23rd Ward
  23, "Michael Zalewski",   "1998-01-01", "2018-05-31",
  23, "Silvana Tabares",    "2018-06-27", "2025-06-24",
  
  # 24th Ward
  24, "Michael Chandler",   "1998-01-01", "2007-05-20",
  24, "Sharon Denise Dixon","2007-05-21", "2011-05-15",
  24, "Michael Chandler",   "2011-05-16", "2015-05-17",
  24, "Michael Scott Jr.",  "2015-05-18", "2022-06-15",
  
  # 25th Ward
  25, "Daniel Solis",       "1998-01-01", "2019-05-19",
  25, "Byron Sigcho-Lopez", "2019-05-20", "2025-06-24",
  
  # 26th Ward
  26, "Billy Ocasio",       "1998-01-01", "2009-07-26",
  26, "Roberto Maldonado",  "2009-08-01", "2023-05-14",
  26, "Jessie Fuentes",     "2023-05-15", "2025-06-24",
  
  # 27th Ward
  27, "Walter Burnett, Jr.", "1998-01-01", "2025-06-24",
  
  # 28th Ward
  28, "Ed Smith",           "1998-01-01", "2010-12-31",
  28, "Jason Ervin",        "2011-01-01", "2025-06-24",
  
  # 29th Ward
  29, "Sam Burrell",        "1998-01-01", "1999-05-02",
  29, "Isaac Carothers",    "1999-05-03", "2010-02-28",
  29, "Deborah Graham",     "2010-03-01", "2015-05-17",
  29, "Chris Taliaferro",   "2015-05-18", "2025-06-24",
  
  # 30th Ward
  30, "Michael Wojcik",     "1998-01-01", "2003-05-04",
  30, "Ariel Reboyras",     "2003-05-05", "2023-05-14",
  30, "Ruth Cruz",          "2023-05-15", "2025-06-24",
  
  # 31st Ward
  31, "Ray Suarez",         "1998-01-01", "2015-05-17",
  31, "Milagros Santiago",  "2015-05-18", "2019-05-19",
  31, "Felix Cardona, Jr.", "2019-05-20", "2025-06-24",
  
  # 32nd Ward
  32, "Terry Gabinski",     "1998-01-01", "1998-05-20",
  32, "Theodore Matlak",    "1998-05-21", "2007-05-20",
  32, "Scott Waguespack",   "2007-05-21", "2025-06-24",
  
  # 33rd Ward
  33, "Dick Mell",          "1998-01-01", "2013-07-24",
  33, "Deb Mell",           "2013-07-24", "2019-05-19",
  33, "Rossana Rodriguez-Sanchez", "2019-05-20", "2025-06-24",
  
  # 34th Ward
  34, "Carrie Austin",      "1998-01-01", "2023-05-14",
  34, "Bill Conway",        "2023-05-15", "2025-06-24",
  
  # 35th Ward
  35, "Vilma Colom",        "1998-01-01", "2003-05-04",
  35, "Rey Colon",          "2003-05-05", "2015-05-17",
  35, "Carlos Ramirez-Rosa","2015-05-18", "2025-06-24",
  
  # 36th Ward
  36, "William Banks",      "1998-01-01", "2009-10-31",
  36, "John Rice",          "2009-11-01", "2011-05-15",
  36, "Nicholas Sposato",   "2011-05-16", "2015-05-17",
  36, "Gilbert Villegas",   "2015-05-18", "2025-06-24",
  
  # 37th Ward
  37, "Percy Z. Giles",     "1998-01-01", "2000-01-06",
  37, "Emma Mitts",         "2000-01-07", "2025-06-24",
  
  # 38th Ward
  38, "Thomas Allen",       "1998-01-01", "2011-05-15",
  38, "Timothy Cullerton",  "2011-05-16", "2015-05-17",
  38, "Nicholas Sposato",   "2015-05-18", "2025-06-24",
  
  # 39th Ward
  39, "Margaret Laurino",   "1998-01-01", "2019-05-19",
  39, "Samantha Nugent",    "2019-05-20", "2025-06-24",
  
  # 40th Ward
  40, "Patrick O'Connor",   "1998-01-01", "2019-05-19",
  40, "Andre Vasquez",      "2019-05-20", "2025-06-24",
  
  # 41st Ward
  41, "Brian Doherty",      "1998-01-01", "2011-05-15",
  41, "Mary O'Connor",      "2011-05-16", "2015-05-17",
  41, "Anthony Napolitano", "2015-05-18", "2025-06-24",
  
  # 42nd Ward
  42, "Burton Natarus",     "1998-01-01", "2007-05-20",
  42, "Brendan Reilly",     "2007-05-21", "2025-06-24",
  
  # 43rd Ward
  43, "Charles Bernardini", "1998-01-01", "1999-05-02",
  43, "Vi Daley",           "1999-05-03", "2011-05-15",
  43, "Michele Smith",      "2011-05-16", "2022-08-12",
  43, "Timmy Knudsen",      "2022-09-21", "2025-06-24",
  
  # 44th Ward
  44, "Bernie Hansen",      "1998-01-01", "2003-01-15",
  44, "Tom Tunney",         "2003-01-16", "2023-05-14",
  44, "Bennett Lawson",     "2023-05-15", "2025-06-24",
  
  # 45th Ward
  45, "Patrick Levar",      "1998-01-01", "2011-05-15",
  45, "John Arena",         "2011-05-16", "2019-05-19",
  45, "Jim Gardiner",       "2019-05-20", "2025-06-24",
  
  # 46th Ward
  46, "Helen Shiller",      "1998-01-01", "2011-05-15",
  46, "James Cappleman",    "2011-05-16", "2023-05-14",
  46, "Angela Clay",        "2023-05-15", "2025-06-24",
  
  # 47th Ward
  47, "Eugene Schulter",    "1998-01-01", "2011-05-15",
  47, "Ameya Pawar",        "2011-05-16", "2019-05-19",
  47, "Matt Martin",        "2019-05-20", "2025-06-24",
  
  # 48th Ward
  48, "Mary Ann Smith",     "1998-01-01", "2011-05-15",
  48, "Harry Osterman",     "2011-05-16", "2023-05-14",
  48, "Leni Manaa-Hoppenworth", "2023-05-15", "2025-06-24",
  
  # 49th Ward
  49, "Joe Moore",          "1998-01-01", "2019-05-19",
  49, "Maria Hadden",       "2019-05-20", "2025-06-24",
  
  # 50th Ward
  50, "Bernard Stone",      "1998-01-01", "2011-05-15",
  50, "Debra Silverstein",  "2011-05-16", "2025-06-24"
) %>%
  mutate(across(c(start_date, end_date), as.Date))

# # Finance Committee Chair data
# finance_chair_data <- tribble(
#   ~alderman, ~start_date, ~end_date,
#   "Ed Burke", "2003-01-01", "2019-05-19",
#   "Scott Waguespack", "2019-05-20", "2023-05-14",
#   "Pat Dowell", "2023-05-15", "2025-06-24"
# ) %>%
#   mutate(across(c(start_date, end_date), as.Date), finance_chair = 1)
# 
# # Zoning Committee Chair data
# zoning_chair_data <- tribble(
#   ~alderman, ~start_date, ~end_date,
#   "William J.P. Banks", "2003-01-01", "2011-05-15",
#   "Daniel Solis", "2011-05-16", "2019-05-19",
#   "James Cappleman", "2019-05-20", "2023-05-14",
#   "Carlos Ramirez-Rosa", "2023-05-15", "2025-06-24"
# ) %>%
#   mutate(across(c(start_date, end_date), as.Date), zoning_chair = 1)
# 
# # Budget and Government Operations Committee Chair data
# budget_chair_data <- tribble(
#   ~alderman, ~start_date, ~end_date,
#   "Carrie Austin", "2003-01-01", "2019-05-19",
#   "Pat Dowell", "2019-05-20", "2023-05-14",
#   "Jason Ervin", "2023-05-15", "2025-06-24"
# ) %>%
#   mutate(across(c(start_date, end_date), as.Date), budget_chair = 1)


# Create a month-year panel using zoo::as.yearmon
panel_grid <- expand_grid(
  year_month = as.yearmon(seq(as.Date("1998-01-01"), as.Date("2025-06-01"), by = "months")),
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

final_panel <- final_panel %>%
  rename(month = year_month)

# Write to CSV
write_csv(final_panel, "../output/chicago_alderman_panel.csv")

print("CSV file created successfully: chicago_alderman_panel.csv")