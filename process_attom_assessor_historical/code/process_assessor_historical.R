source("/Users/jacobherbstman/Desktop/aldermanic_privilege/source_script.R")


####################################################################################

#───────────────────────────────────────────────────────────────────────────────
# 1. Core “keep” list (the only fields you truly need for FAR + basic mapping)
#───────────────────────────────────────────────────────────────────────────────
keep_vars_history <- c(
  # ─ Identifiers & Geography ──────────────────────────────────────────────────
  "sa_property_id",
  "attom_id",
  "ah_history_yr",            
  "ah_history_yr_version",    
  "mm_muni_name",
  # standard property‐use code
  
  # ─ Value & Tax History ──────────────────────────────────────────────────────
  "sa_val_assd",              # total assessed value
  "sa_val_assd_land",
  "sa_val_assd_imprv",
  "sa_val_market",
  "sa_val_market_land",
  "sa_val_market_imprv",
  "sa_tax_val",               # tax‐assessed value
  "taxyear",
  "sa_imprv_pct_appraise",    # % improvements in appraised value
  
  # ─ Exemptions ───────────────────────────────────────────────────────────────
  "sa_exemp_val_1","sa_exemp_val_2", "sa_exemp_val_3",
  "sa_exemp_val_4", "sa_exemp_val_5", "sa_exemp_val_6",
  
  # ─ Lot & Building Areas ────────────────────────────────────────────────────
  "sa_lotsize",
  "sa_bldg_sqft",
  "sa_bsmt_fin_sqft",
  "sa_bsmt_unfin_sqft",
  "sa_fin_sqft_tot",
  "sa_sqft",
  
  # ─ Rooms, Baths & Units ─────────────────────────────────────────────────────
  "sa_nbr_bedrms",
  "sa_nbr_rms",
  "sa_nbr_bath", 
  "sa_nbr_bath_half",
  "sa_nbr_bath_1qtr",
  "sa_nbr_stories",
  "sa_nbr_units",
  
  # ─ Year Built ────────────────────────────────────────────────────────────────
  "sa_yr_blt",
  
  # ─ Sale / Transfer History ──────────────────────────────────────────────────
  "sa_date_transfer",
  "sa_val_transfer"
)


####################################################################################


db_file <- file.path(root, "process_attom_assessor_historical/input/assessor_history.duckdb")
con     <- dbConnect(duckdb(), dbdir = db_file)

# 2. Create a single table over all 64 CSVs in that folder
# sql <- sprintf("
#   CREATE TABLE assessor_history AS
#     SELECT *
#     FROM read_csv_auto(
#       '%s/raw_data/assessor_history/*.csv',
#       IGNORE_ERRORS=TRUE    -- drop rows with conversion problems
#     )
#     WHERE SA_SITE_CITY = 'CHICAGO';
# ", root)

# dbExecute(con, sql)

# 3a. Now pull just Chicago rows
df_chicago <- dbGetQuery(con, "
  SELECT *
  FROM assessor_history
  WHERE SA_SITE_CITY = 'CHICAGO';
")

df_chicago <- df_chicago %>% 
  janitor::clean_names() %>% 
  select(keep_vars_history)

write_parquet(df_chicago, paste0(root, "process_attom_assessor_historical/output/chicago_attom_history.parquet"))

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


