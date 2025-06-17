# source("/Users/jacobherbstman/Desktop/aldermanic_privilege/source_script.R")

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")


####################################################################################

#───────────────────────────────────────────────────────────────────────────────
# 1. Core “keep” list (the only fields you truly need for FAR + basic mapping)
#───────────────────────────────────────────────────────────────────────────────
keep_vars_history <- c(
  # ─ Identifiers & Geography ──────────────────────────────────────────────────
  "attom_id",
  "ah_history_yr",            

  # ─ Value & Tax History ──────────────────────────────────────────────────────
  "sa_val_assd",              # total assessed value
  "sa_val_assd_land",
  "sa_val_assd_imprv",
  "sa_val_market",
  "sa_val_market_land",
  "sa_val_market_imprv",
  "sa_tax_val",               # tax‐assessed value
  "taxyear",
  
  # ─ Address stuff ───────────────────────────────────────────────────────────────
  "sa_mail_house_nbr", 
  "sa_mail_dir", 
  "sa_mail_street_name",
  "sa_mail_suf",
  "sa_mail_post_dir",
  "sa_mail_city",
  "sa_mail_state",
  "sa_mail_zip",
  "sa_mail_plus_4",
  

  # ─ Exemptions ───────────────────────────────────────────────────────────────
  # "sa_exemp_val_1","sa_exemp_val_2", "sa_exemp_val_3",
  # "sa_exemp_val_4", "sa_exemp_val_5", "sa_exemp_val_6",
  
  # ─ Lot & Building Areas ────────────────────────────────────────────────────
  "sa_lotsize",
  "sa_bldg_sqft",
  "sa_fin_sqft_tot",
  "sa_sqft",
  
  # ─ Rooms, Baths & Units ─────────────────────────────────────────────────────
  "sa_nbr_bedrms",
  "sa_nbr_rms",
  "sa_nbr_bath", 
  "sa_nbr_stories",
  "sa_nbr_units",
  
  # ─ Year Built ────────────────────────────────────────────────────────────────
  "sa_yr_blt",
  
  # ─ Sale / Transfer History ──────────────────────────────────────────────────
  "sa_date_transfer",
  "sa_val_transfer"
)


####################################################################################
sql <- "
CREATE TABLE assessor_history AS
  SELECT *
  FROM read_csv_auto(
    '../input/assessor_history/*.csv',
    IGNORE_ERRORS = TRUE
  )
  WHERE SA_SITE_CITY = 'CHICAGO';
"

# 2. setup db file
db_file <- "../temp/assessor_history.duckdb"
dir.create(dirname(db_file), showWarnings = FALSE, recursive = TRUE)


con     <- dbConnect(duckdb(), dbdir = db_file)
dbExecute(con, sql)


# 3a. Now pull just Chicago rows
df_chicago <- dbGetQuery(con, "
  SELECT *
  FROM assessor_history
  WHERE SA_SITE_CITY = 'CHICAGO';
")

df_chicago <- df_chicago %>% 
  janitor::clean_names() %>% 
  dplyr::select(any_of(keep_vars_history)) # Use any_of() for robustness

write_parquet(df_chicago,  "../output/chicago_attom_history.parquet")

dbDisconnect(con, shutdown = TRUE)

## counts of observations by year with small x axis text
# df_chicago %>% 
#   filter(taxyear > 0) %>% 
#   group_by(ah_history_yr) %>% 
#   summarise(n = n()) %>% 
#   ggplot(aes(x = ah_history_yr, y = n)) +
#   geom_col() +
#   theme_minimal() +
#   labs(title = "Number of Observations by Year",
#        x = "Year",
#        y = "Count") +
#   scale_x_continuous(breaks = seq(2000, 2020, by = 10))
# 
# sort(unique(df_chicago$ah_history_yr))


