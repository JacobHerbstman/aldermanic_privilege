  #this code runs regression discontinuities on each alderman pair for the land share variable
  
  # setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
  source("../../setup_environment/code/packages.R")
  
  # ---- Load Data ----
  aldermen_pairs <- read_csv("../input/rd_alderman_pair_episodes.csv")
  chicago_alderman_panel <- read_csv("../input/chicago_alderman_panel.csv")
  
## get alderman names
  current <- chicago_alderman_panel %>%
    # mutate(month = ymd(month)) %>%                
    arrange(ward, desc(month)) %>%
    distinct(ward, .keep_all = TRUE) %>%          # keep most recent row per ward
    transmute(ward, alderman = alderman)
  
  current_names <- current$alderman 
  
  dt <- as.data.table(aldermen_pairs)
  
  # keep usable BC estimates
  dt <- dt[is.finite(coef_bc) & is.finite(se_bc) & se_bc > 0]
  
  # (optional) drop episodes that fail your own quality gates
  dt <- dt[ok_min_sideN == TRUE]
  
  # orient an unordered pair label, but KEEP the hi/lo orientation for y
  dt[, pair := paste(pmin(ald_lo, ald_hi), pmax(ald_lo, ald_hi), sep = " | ")]
  dt[, v := se_bc^2]
  
  # Inverse-variance collapse to ONE edge per pair (keeps orientation hi−lo)
  pair_dt <- dt[, .(
    coef_bc = sum(coef_bc / v) / sum(1 / v),
    se_bc   = sqrt( 1 / sum(1 / v) ),
    n_ep    = .N,
    ald_i   = ald_hi[1],  # orientation: i = ald_hi, j = ald_lo
    ald_j   = ald_lo[1]
  ), by = pair]
  
  # weights
  pair_dt[, `:=`(y = coef_bc, v = se_bc^2, w = 1 / (se_bc^2))]
  
  
  edges <- pair_dt
  aldermen <- sort(unique(c(edges$ald_i, edges$ald_j)))
  id <- setNames(seq_along(aldermen), aldermen)
  
  m <- nrow(edges); nA <- length(aldermen)
  i_idx <- id[edges$ald_i]; j_idx <- id[edges$ald_j]
  
  # sparse design: +1 for ald_hi, -1 for ald_lo
  D <- sparseMatrix(i = rep(seq_len(m), 2),
                    j = c(i_idx, j_idx),
                    x = c(rep(1, m), rep(-1, m)),
                    dims = c(m, nA))
  
  # (optional) cap extreme weights so no single border dominates
  w_cap <- quantile(edges$w, 0.95, na.rm = TRUE)
  w <- pmin(edges$w, w_cap)
  
  A <- t(D) %*% Diagonal(x = w) %*% D
  b <- t(D) %*% (w * edges$y)
  
  # Identify: drop the densest node as reference, then mean-center for display
  deg <- tabulate(c(i_idx, j_idx), nbins = nA)
  ref <- which.max(deg); keep <- setdiff(seq_len(nA), ref)
  
  s_hat_k <- solve(A[keep, keep, drop=FALSE], b[keep, , drop=FALSE])
  s_hat <- numeric(nA); s_hat[keep] <- as.numeric(s_hat_k); s_hat[ref] <- 0
  s_hat <- s_hat - mean(s_hat)  # presentation
  
  # SEs of scores (reference node has NA)
  V_k <- solve(A[keep, keep, drop=FALSE])
  se_hat <- numeric(nA); se_hat[keep] <- sqrt(diag(V_k)); se_hat[ref] <- NA
  
  ranking <- data.table(
    alderman   = aldermen,
    strictness = s_hat,
    se         = se_hat,
    degree     = deg
  )[order(-strictness)]
  
  sd_s <- sd(ranking$strictness, na.rm = TRUE)
  
  ranking[, `:=`(
    strictness_sd = strictness / sd_s,               # z-score: SDs from average alderman
    ci_lo = strictness - 1.96*se,                    # 95% CI in share points
    ci_hi = strictness + 1.96*se
  )]
  
  
  write_csv(ranking, "../output/aldermen_strictness_scores.csv")
  
  
  
  # --- Setup ---
  current_names <- current$alderman |> na.omit() |> stringr::str_squish()
  
  
  # --- PLOT 1: All Aldermen with Bold Current Names ---
  
  # 1. Create data for the full plot
  plot_dt <- ranking %>%
    mutate(
      sign = factor(if_else(strictness >= 0, "Positive", "Negative"),
                    levels = c("Negative","Positive")),
      alderman = factor(alderman, levels = .$alderman)
    ) %>%
    mutate(is_current = alderman %in% current_names)
  
  # 2. Get the ordered levels from the factor
  y_axis_levels <- levels(plot_dt$alderman)
  
  # 3. Create a matching vector of font faces
  face_vector <- ifelse(y_axis_levels %in% current_names, "bold", "plain")
  
  # 4. Build the plot
  p <- ggplot(plot_dt, aes(x = strictness, y = alderman, fill = sign)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, linewidth = 0.6, color = "grey30") +
    scale_fill_manual(values = c(Negative = "#2C7FB8", Positive = "#D62728")) +
    scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
    labs(x = "Score", y = NULL, fill = NULL,
         title = "Alderman Strictness Rankings") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      axis.text.y = element_text(
        face = face_vector,
        size = 6,
        lineheight = 0.9,
        margin = margin(r = 3)
      )
    )
  
  # 5. Save Plot 1
  ggsave("../output/strictness_barplot.pdf", p,
         width = 7.5, height = 9)
  
  # (Optional) Display the plot in your R environment
  # print(p)
  
  
  # --- PLOT 2: Current Aldermen Only (No Bold) ---
  
  # 1. Create new plot data, filtering for current aldermen
  # Re-leveling the factor is key to ensure correct sorting and no empty levels
  plot_dt_current <- ranking %>%
    filter(alderman %in% current_names) %>%
    mutate(
      sign = factor(if_else(strictness >= 0, "Positive", "Negative"),
                    levels = c("Negative","Positive")),
      # This re-levels the factor based on the filtered, sorted data
      alderman = factor(alderman, levels = .$alderman) 
    )
  
  # 2. Build Plot 2
  p_current <- ggplot(plot_dt_current, aes(x = strictness, y = alderman, fill = sign)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, linewidth = 0.6, color = "grey30") +
    scale_fill_manual(values = c(Negative = "#2C7FB8", Positive = "#D62728")) +
    scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
    labs(x = "Score", y = NULL, fill = NULL,
         title = "Current Alderman Strictness Rankings") + # Updated title
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      axis.text.y = element_text(
        face = "plain",  # All labels are plain
        size = 9,        # Increased size for better readability (fewer labels)
        lineheight = 0.9,
        margin = margin(r = 3)
      )
    )
  
  # 3. Save Plot 2
  ggsave("../output/strictness_barplot_current_only.pdf", p_current,
         width = 7.5, height = 9)
  

# g <- graph_from_data_frame(data.frame(u = edges$ald_i, v = edges$ald_j), directed = FALSE)
# comp <- components(g)$membership
# table(comp)  # if >1 component, ranks are relative within each component only
# ranking[, component := comp[match(alderman, names(comp))]]

