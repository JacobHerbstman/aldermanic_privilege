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
  "propertyusegroup",
  "yearbuilt",
  "construction",
  "bathcount",
  "bathpartialcount",
  "bedroomscount",
  "roomscount",
  "storiescount",
  "unitscount",
  "statusowneroccupiedflag",
  "assessorpriorsaledate", 
  "assessorpriorsaleamount",
  "parcelnumberyearchange", 
  "construction", 
  "foundation",
  "buildingscount",
  "parkingspacecount",
  "propertyusestandardized"
)



## clean names, chicago only
tax_df_new <- tax_df %>% 
  janitor::clean_names() %>% 
  filter(yearbuilt >= 2003) %>% ## year built lining up with permit data
  filter(propertyaddresscity == "CHICAGO") %>% #chicago only
  filter(!is.na(propertylatitude) & !is.na(propertylongitude)) %>% ##need lat/lon for border discontinuity
  mutate(deedlastsaleprice = na_if(deedlastsaleprice, -1)) %>% ## -1 means missing
  filter(propertyusegroup %in% c("Residential")) %>% # residential only
  select(all_of(keep_vars)) %>% ## keep vars of interest for memory reason
  st_as_sf(coords = c("propertylongitude", "propertylatitude"), crs = 4326) %>% 
  st_transform(crs = 3435) ## state plane illinois east

st_write(tax_df_new,  "../output/chicago_attom_2023.gpkg", delete_dsn = TRUE)

# rm(tax_df)
# gc()
# 
# # ─────────────────────────────────────────────────────────────────────────────
# # Imputation: Simple units imputation (OLS on log(unitscount))
# # ─────────────────────────────────────────────────────────────────────────────
# 
# # 1) Flatten sf to a plain data.frame
# tax_df_tbl <- tax_df_new %>% sf::st_drop_geometry() %>% as.data.frame()
# 
# # 1a) Normalize unitscount if prior joins created .x/.y
# if (!"unitscount" %in% names(tax_df_tbl)) {
#   alt <- intersect(names(tax_df_tbl), c("unitscount.x","unitscount.y"))
#   if (length(alt) == 0) stop("unitscount column not found after st_drop_geometry().")
#   ux <- tax_df_tbl[[alt[1]]]
#   uy <- if (length(alt) > 1) tax_df_tbl[[alt[2]]] else NULL
#   tax_df_tbl$unitscount <- if (!is.null(uy)) dplyr::coalesce(ux, uy) else ux
#   tax_df_tbl <- tax_df_tbl %>% dplyr::select(-dplyr::any_of(alt))
# }
# 
# # 2) Treat 0 as missing where appropriate
# zero_is_missing <- intersect(c("arealotsf","areabuilding","storiescount","unitscount"),
#                              names(tax_df_tbl))
# tax_df_tbl <- tax_df_tbl %>%
#   dplyr::mutate(dplyr::across(dplyr::all_of(zero_is_missing), ~ dplyr::na_if(., 0)))
# 
# # 3) Minimal feature engineering needed for your RHS set
# safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den <= 0, NA_real_, num / den)
# 
# tax_df_tbl <- tax_df_tbl %>%
#   dplyr::mutate(
#     ln_bldg    = log1p(areabuilding),
#     ln_lot     = log1p(arealotsf),
#     far_proxy  = safe_divide(areabuilding, arealotsf),
#     area_per_st= safe_divide(areabuilding, storiescount),
#     ln_val_imp = log1p(taxassessedvalueimprovements),
#     ln_val_land= log1p(taxassessedvalueland)
#   )
# 
# # 4) Train on rows with observed positive units and complete predictors
# cand_x <- c("ln_bldg","ln_lot","storiescount",
#             "bedroomscount","bathcount","roomscount",
#             "far_proxy","area_per_st","yearbuilt",
#             "ln_val_imp","ln_val_land")
# have_vars <- intersect(cand_x, names(tax_df_tbl))
# 
# train <- tax_df_tbl %>%
#   dplyr::filter(!is.na(unitscount), unitscount > 0) %>%
#   dplyr::select(unitscount, dplyr::all_of(have_vars)) %>%
#   tidyr::drop_na()
# 
# # Fallbacks if too many NAs (shrink RHS)
# if (nrow(train) == 0 && "ln_lot" %in% have_vars) {
#   have_vars <- setdiff(have_vars, "ln_lot")
#   train <- tax_df_tbl %>%
#     dplyr::filter(!is.na(unitscount), unitscount > 0) %>%
#     dplyr::select(unitscount, dplyr::all_of(have_vars)) %>%
#     tidyr::drop_na()
# }
# if (nrow(train) == 0 && "storiescount" %in% have_vars) {
#   have_vars <- setdiff(have_vars, "storiescount")
#   train <- tax_df_tbl %>%
#     dplyr::filter(!is.na(unitscount), unitscount > 0) %>%
#     dplyr::select(unitscount, dplyr::all_of(have_vars)) %>%
#     tidyr::drop_na()
# }
# if (nrow(train) == 0 && "ln_bldg" %in% names(tax_df_tbl)) {
#   have_vars <- "ln_bldg"
#   train <- tax_df_tbl %>%
#     dplyr::filter(!is.na(unitscount), unitscount > 0) %>%
#     dplyr::select(unitscount, dplyr::all_of(have_vars)) %>%
#     tidyr::drop_na()
# }
# stopifnot(nrow(train) > 0)
# 
# form <- stats::as.formula(paste0("log(unitscount) ~ ", paste(have_vars, collapse = " + ")))
# fit  <- stats::lm(form, data = train)
# 
# # 5) Predict for rows needing imputation (units NA or 0) where predictors exist
# need_imp  <- which(is.na(tax_df_tbl$unitscount) | tax_df_tbl$unitscount == 0)
# pred_ok   <- if (length(need_imp)) stats::complete.cases(tax_df_tbl[need_imp, have_vars, drop = FALSE]) else logical(0)
# pred_rows <- if (length(need_imp)) need_imp[pred_ok] else integer(0)
# 
# pred_units_aligned <- rep(NA_integer_, nrow(tax_df_tbl))
# if (length(pred_rows) > 0) {
#   pred_log <- stats::predict(fit, newdata = tax_df_tbl[pred_rows, , drop = FALSE],
#                              na.action = stats::na.pass)
#   pred_u   <- round(pmax(1, pmin(50, exp(pred_log))))  # back-transform, clamp, integer
#   pred_units_aligned[pred_rows] <- pred_u
# }
# 
# # 6) Attach imputed values + flags, and final units
# tax_df_tbl$unitscount_imputed <- tax_df_tbl$unitscount
# tax_df_tbl$units_impute_flag  <- FALSE
# 
# use_idx <- !is.na(pred_units_aligned)
# tax_df_tbl$unitscount_imputed[use_idx] <- pred_units_aligned[use_idx]
# tax_df_tbl$units_impute_flag[use_idx]  <- TRUE
# 
# write_idx <- is.na(tax_df_tbl$unitscount) | tax_df_tbl$unitscount == 0
# tax_df_tbl$unitscount_final <- tax_df_tbl$unitscount
# tax_df_tbl$unitscount_final[write_idx] <- tax_df_tbl$unitscount_imputed[write_idx]
# 
# # 7) Merge back (replace any prior impute cols if they exist)
# tax_df_new <- tax_df_new %>%
#   dplyr::select(-dplyr::any_of(c("unitscount_imputed","unitscount_final","units_impute_flag"))) %>%
#   dplyr::left_join(
#     tax_df_tbl %>% dplyr::select(attom_id, unitscount_imputed, unitscount_final, units_impute_flag),
#     by = "attom_id"
#   )
# 
# cat("\nImputation summary (TRUE = imputed):\n")
# print(table(tax_df_new$units_impute_flag, useNA = "ifany"))
# 
# 
# 
# # Long data for plots
# plot_df <- tax_df_tbl %>%
#   transmute(
#     source = if_else(units_impute_flag, "Imputed", "Observed"),
#     units  = if_else(units_impute_flag,
#                      as.integer(unitscount_imputed),
#                      as.integer(unitscount))
#   ) %>%
#   filter(!is.na(units), units >= 1, units <= 50)
# 
# summary_tbl <- plot_df %>%
#   group_by(source) %>%
#   summarise(
#     n      = n(),
#     mean   = mean(units),
#     median = median(units),
#     p25    = quantile(units, 0.25),
#     p75    = quantile(units, 0.75),
#     .groups = "drop"
#   )
# print(summary_tbl)
# 
# 
# p_hist <- plot_df %>%
#   ggplot(aes(x = units)) +
#   geom_histogram(binwidth = 1, boundary = 0.5, closed = "right", fill = "#4e79a7") +
#   facet_wrap(~ source, ncol = 1, scales = "free_y") +
#   scale_x_continuous(breaks = c(1,2,3,4,5,10,20,30,40,50), limits = c(1,50)) +
#   scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
#   labs(title = "Units distribution by source", x = "Units", y = "Parcels") +
#   theme_minimal(base_size = 12)
# print(p_hist)
# 
# 
# p_ecdf <- plot_df %>%
#   ggplot(aes(x = units, color = source)) +
#   stat_ecdf(geom = "step", linewidth = 0.9) +
#   scale_x_continuous(breaks = c(1,2,3,4,5,10,20,30,40,50), limits = c(1,50)) +
#   labs(title = "ECDF of Units: Observed vs Imputed",
#        x = "Units", y = "Cumulative share", color = "") +
#   theme_minimal(base_size = 12) +
#   theme(legend.position = "top")
# print(p_ecdf)
# 
