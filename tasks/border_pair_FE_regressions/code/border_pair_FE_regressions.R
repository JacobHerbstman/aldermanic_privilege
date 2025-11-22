  # border_pair_FE_tables_by_bw.R
  # One table per bandwidth (in miles) with multiple outcomes as columns.
  # Regressions: y ~ strictness_own_std | construction_year + ward_pair, clustered by ward_pair
  
  ## run this line when editing code in Rstudio
  # setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
  
  source("../../setup_environment/code/packages.R")
  
  # =======================================================================================
  # --- Interactive Test Block --- (uncomment to run in RStudio)
  bw_ft           <- 528
  yvars           <- c("density_dupac", "density_far", "density_lapu", "density_bcr", "density_lps",
   "unitscount", "storiescount", "areabuilding", "arealotsf", "bedroomscount")
  output_filename  <- "../output/fe_table_bw1056.tex"
  # =======================================================================================
  
  # ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
  # Args: <bw_feet> <output_filename> <yvar1> [<yvar2> ...]
  # args <- commandArgs(trailingOnly = TRUE)
  # if (length(args) >= 3) {
  #   bw_ft           <- suppressWarnings(as.integer(args[1]))
  #   output_filename <- args[2]
  #   # space-separated yvars
  #   yvars <- args[3:length(args)]
  #   
  #   # backward-compat: if exactly 3 args and the 3rd has commas, split them
  #   if (length(args) == 3 && grepl(",", args[3])) {
  #     yvars <- strsplit(args[3], ",")[[1]] |> trimws()
  #   }
  # } else {
  #   # allow interactive testing with objects already defined in the session
  #   if (!exists("bw_ft") || !exists("output_filename") || !exists("yvars")) {
  #     stop("FATAL: need args: <bw_feet> <output_filename> <yvar1> [<yvar2> ...]", call. = FALSE)
  #   }
  # }
  
  if (!is.finite(bw_ft) || bw_ft <= 0) stop("bw_feet must be a positive integer/numeric.")
  if (length(yvars) == 0) stop("No yvars provided.")
  
  bw_mi <- round(bw_ft / 5280, 2)
  
  
  # ── 2) DATA ──────────────────────────────────────────────────────────────────
  parcels <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
    mutate(strictness_own_std = strictness_own / sd(strictness_own, na.rm = TRUE)) %>% 
    filter(arealotsf > 1) %>% 
    filter(unitscount > 1)
  
  
  # --- Sample restriction helper: keep modal zone that exists on both sides within the bw ---
  restrict_to_modal_zone <- function(df, bw) {
    # 1) limit to current bandwidth + non-missing zone
    df_bw <- df %>%
      filter(dist_to_boundary <= bw, !is.na(zone_code))
    
    # 2) grouping keys: use (boundary_year, ward_pair) when available, else ward_pair
    group_keys <- intersect(c("boundary_year", "ward_pair"), names(df_bw))
    
    # 3) count parcels by zone within each group, requiring presence on BOTH sides
    #    prefer signed distance if available; else fall back to ward on each side
    zone_counts <-
      if ("signed_distance" %in% names(df_bw)) {
        df_bw %>%
          group_by(across(all_of(c(group_keys, "zone_code")))) %>%
          summarise(n = n(),
                    n_sides = n_distinct(sign(signed_distance)),
                    .groups = "drop") %>%
          filter(n_sides == 2)
      } else if ("ward" %in% names(df_bw)) {
        df_bw %>%
          group_by(across(all_of(c(group_keys, "zone_code")))) %>%
          summarise(n = n(),
                    n_sides = n_distinct(ward),
                    .groups = "drop") %>%
          filter(n_sides == 2)
      } else {
        # if neither side indicator is present, just compute modal zone
        df_bw %>%
          group_by(across(all_of(c(group_keys, "zone_code")))) %>%
          summarise(n = n(), .groups = "drop")
      }
    
    # 4) pick the modal zone (tie-breaker: alphabetical zone_code)
    modal_zone <- zone_counts %>%
      arrange(across(all_of(group_keys)), desc(n), zone_code) %>%
      group_by(across(all_of(group_keys))) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      select(all_of(group_keys), zone_code) %>%
      rename(modal_zone_code = zone_code)
    
    # 5) keep parcels in the modal zone for each group
    df_bw %>%
      inner_join(modal_zone, by = group_keys) %>%
      filter(zone_code == modal_zone_code) %>%
      select(-modal_zone_code)
  }
  
  parcels_fe <- parcels
  parcels_fe <- restrict_to_modal_zone(parcels_fe, bw_ft)
  # parcels_fe <- parcels_fe %>%
  # filter(abs(strictness_own - strictness_neighbor) > .1) # Drop parcels where the two sides are too similar in strictness
  
  
  # ── 3) HELPERS ───────────────────────────────────────────────────────────────
  is_log_spec <- function(v) str_detect(v, "^log\\(.+\\)$")
  base_name   <- function(v) gsub("^log\\(|\\)$", "", v)
  
  pretty_label <- function(v) {
    b <- base_name(v)
    dict <- c(
      "density_far"  = "Floor Area Ratio (FAR)",
      "density_lapu" = "Lot Area Per Unit (LAPU)",
      "density_bcr"  = "Building Coverage Ratio (BCR)",
      "density_lps"  = "Lot Size Per Story (LPS)",
      "density_spu"  = "Square Feet Per Unit (SPU)",
      "arealotsf"    = "Lot Area (sf)",
      "areabuilding" = "Building Area (sf)",
      "storiescount" = "Stories",
      "unitscount"   = "Units", 
      "bedroomscount"= "Bedrooms", 
      "bathcount"= "Bathrooms"
    )
    lab <- ifelse(b %in% names(dict), dict[[b]], b)
    if (is_log_spec(v)) paste("Log", lab) else lab
  }
  
  # fitstat: mean of *level* DV for the estimation sample
  mean_y_level <- function(x) {
    dat <- x$custom_data
    y_lhs <- deparse(x$fml[[2]])
    y0 <- if (grepl("^log\\(", y_lhs)) gsub("^log\\(|\\)$", "", y_lhs) else y_lhs
    mean(dat[[y0]], na.rm = TRUE)
  }
  fitstat_register("myo", mean_y_level, alias = "Dep. Var. Mean")
  
  #fitstat: n ward pairs
  n_ward_pairs <- function(x) {
    mf <- tryCatch(model.frame(x), error = function(e) NULL)
    if (!is.null(mf) && "ward_pair" %in% names(mf)) {
      return(length(unique(mf$ward_pair)))
    }
    # Fallbacks if needed:
    if (!is.null(x$cluster) && "ward_pair" %in% names(x$cluster)) {
      return(length(unique(x$cluster$ward_pair)))
    }
    if (!is.null(x$custom_data) && "ward_pair" %in% names(x$custom_data)) {
      return(length(unique(stats::na.omit(x$custom_data$ward_pair))))
    }
    NA_integer_
  }
  
  fitstat_register("nwp", n_ward_pairs, alias = "Ward Pairs")
  
  rename_dict <- c(
    "strictness_own_std" = "Strictness Score",
    "construction_year"  = "Year",
    "ward_pair"          = "Ward Pair",
    "ward"               = "Ward",
    "density_far"        = "Floor Area Ratio (FAR)",
    "density_lapu"       = "Lot Area Per Unit (LAPU)",
    "density_bcr"        = "Building Coverage Ratio (BCR)",
    "density_lps"        = "Lot Size Per Story (LPS)",
    "density_spu"        = "Square Feet Per Unit (SPU)",
    "arealotsf"          = "Lot Area (sf)",
    "areabuilding"       = "Building Area (sf)",
    "storiescount"       = "Stories",
    "unitscount"         = "Units", 
    "bedroomscount"      = "Bedrooms", 
    "bathcount"          = "Bathrooms"
  )
  
  # ── 4) MODELS (ONE PER OUTCOME), SAME BW ─────────────────────────────────────
  models <- list()
  col_headers <- c()
  
  for (yv in yvars) {
    b <- base_name(yv)
    if (!b %in% names(parcels)) {
      warning(sprintf("Skipping '%s' (base var '%s' not found).", yv, b))
      next
    }
    
    df <- parcels_fe %>%
      filter(dist_to_boundary <= bw_ft)
    # filter(.data[[b]] > 0) %>% 
    # filter(density_far > 0 & density_lapu > 0)
    
    if (nrow(df) == 0) {
      warning(sprintf("Skipping '%s' (no rows after filtering).", yv))
      next
    }
    
    fml_txt <- paste0(yv, " ~ strictness_own_std | construction_year^ward_pair ")
    m <- feols(as.formula(fml_txt), data = df, cluster = ~ ward_pair)
    m$custom_data <- df
    
    models[[length(models) + 1]] <- m
    col_headers <- c(col_headers, pretty_label(yv))
  }
  
  if (length(models) == 0) stop("No models estimated; check yvars and data.")
  names(models) <- col_headers
  
  # ── 5) TITLE & TABLE OUTPUT ──────────────────────────────────────────────────
  table_title <- sprintf("Border-Pair FE estimates (bw = %f mi)", bw_mi)
  
  etable(models,
         keep         = "Strictness Score",
         fitstat      = ~ n + myo + nwp,
         style.tex    = style.tex("aer", model.format = ""),
         depvar       = FALSE,
         digits       = 2,
         dict         = rename_dict,
         headers      = names(models),
         signif.code  = c("***"=0.01, "**"=0.05, "*"=0.1),
         fixef.group = list("Ward-pair × Year FE" = "construction_year\\^ward_pair"),
         title        = table_title,
         # file         = output_filename,
         replace      = TRUE)


