### this code processes the ATTOM assessor data, downloaded from dewey with a uchicago sign-in:
# https://app.deweydata.io/products?categories%5B0%5D%5Btitle%5D=ATTOM%20Data&categories%5B0%5D%5Bvalue%5D=7395056b-2d7d-454a-a258-565344ff61d7&page=1


## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")


tax_files <- list.files(
  path   = "../input/assessor",          # folder to search
  pattern = "^Tax_Assessor_Cook_County-\\d+\\.csv$",  # the 64 files
  full.names = TRUE,                              # return full paths
  ignore.case = FALSE
)

tax_list <- lapply(tax_files, function(f) {
  fread(
    input       = f,
    integer64   = "numeric",   # convert int64 → double
    showProgress= FALSE
  )
})

tax_df <- rbindlist(tax_list, use.names=TRUE, fill=TRUE)





#───────────────────────────────────────────────────────────────────────────────
# 1. Core “keep” list (the only fields we truly need for FAR + basic mapping)
#───────────────────────────────────────────────────────────────────────────────
keep_vars <- c(
  # ─ Identifiers & Location ───────────────────────────────────────────────────
  "attom_id",
  "propertylatitude",
  "propertylongitude",        
  
  # ─ Lot & Building Areas (FAR inputs) ───────────────────────────────────────
  "arealotacres",  
  "arealotsf",  
  "arealotdepth",
  "arealotwidth",
  "areabuilding",  
  "areagross",     
  "censusfipsplacecode",
  "censusblockgroup",
  "censusblock",
  "companyflag",
  "ownertypedescription1",
  "ownershipvestingrelationcode",
  "taxyearassessed",
  "taxassessedvaluetotal",
  "taxassessedvalueimprovements",
  "taxassessedvalueland",
  "taxassessedimprovementsperc",
  "previousassessedvalue",
  "taxmarketvalueyear",
  "taxmarketvaluetotal",
  "taxmarketvalueimprovements",
  "taxmarketvalueland",
  "taxmarketimprovementsperc",
  "taxfiscalyear",
  "taxratearea",
  "taxbilledamount",
  "lastassessortaxrollupdate",
  "assrlastupdated",
  "taxexemptionhomeownerflag",
  "taxexemptionadditional",
  "deedlastsaledate",
  "deedlastsaleprice",
  "zonedcodelocal",
  "propertyusemuni",
  "yearbuilt",
  "construction",
  "bathcount",
  "bathpartialcount",
  "bedroomscount",
  "roomscount",
  "storiescount",
  "unitscount",
  "statusowneroccupiedflag"
)



## clean names, chicago only
tax_df <- tax_df %>% 
  janitor::clean_names() %>% 
  filter(minorcivildivisionname == "CHICAGO") %>% 
  filter(!is.na(propertylatitude) & !is.na(propertylongitude)) %>% 
  filter(arealotacres > 0) %>% 
  filter(areabuilding > 0 ) %>% 
  select(all_of(keep_vars)) %>% 
  mutate(FAR = areabuilding / arealotsf) 



# write_csv(tax_df, paste0(root, "process_attom_accessor/output/chicago_attom_2023.parquet"))
write_parquet(tax_df,  "../output/chicago_attom_2023.parquet")



