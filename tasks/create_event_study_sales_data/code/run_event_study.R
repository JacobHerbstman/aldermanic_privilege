# run_event_study.R
# Event study regressions for home sales around the 2015 ward redistricting
# Uses block-level panels to estimate effect of switching aldermen

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/event_study_sales/code")
source("../../setup_environment/code/packages.R")

# =============================================================================
# 1. LOAD DATA
# =============================================================================
message("Loading block-year panel...")
block_year <- read_csv("../output/sales_block_year_panel.csv", show_col_types = FALSE) %>%
    filter(!is.na(switched_2015)) %>%
    mutate(
        treat = as.integer(switched_2015),
        post = as.integer(relative_year >= 0),
        # Continuous treatment: change in strictness
        strictness_change = replace_na(strictness_change, 0),
        # Create treatment groups
        treat_stricter = switch_type == "Moved to Stricter",
        treat_lenient = switch_type == "Moved to More Lenient"
    )

message(sprintf("Loaded %s block-years", format(nrow(block_year), big.mark = ",")))

# Focus on event window: 5 years before/after
block_year <- block_year %>%
    filter(year >= 2010, year <= 2020)

message(sprintf("Event window: %s block-years", format(nrow(block_year), big.mark = ",")))

# Summary stats
message("\nSummary of sales by treatment status:")
block_year %>%
    filter(n_sales > 0) %>%
    group_by(switch_type) %>%
    summarise(
        n_block_years = n(),
        total_sales = sum(n_sales),
        avg_price = weighted.mean(mean_price, n_sales, na.rm = TRUE)
    ) %>%
    print()

# =============================================================================
# 2. TWFE / DiD REGRESSIONS
# =============================================================================
message("\n\n=== TWFE / DiD Regressions ===")

# Weight by number of sales
block_year_weighted <- block_year %>%
    filter(n_sales > 0)

# Model 1: Simple DiD
m1 <- feols(log(mean_price) ~ treat * post | block_id + year,
    data = block_year_weighted,
    weights = ~n_sales,
    cluster = ~block_id
)

# Model 2: Using continuous treatment (strictness change)
m2 <- feols(log(mean_price) ~ strictness_change * post | block_id + year,
    data = block_year_weighted %>% filter(switched_2015),
    weights = ~n_sales,
    cluster = ~block_id
)

# Model 3: Heterogeneous effects - stricter vs lenient
m3_stricter <- feols(log(mean_price) ~ treat * post | block_id + year,
    data = block_year_weighted %>%
        filter(switch_type %in% c("No Significant Change", "Moved to Stricter")),
    weights = ~n_sales,
    cluster = ~block_id
)

m3_lenient <- feols(log(mean_price) ~ treat * post | block_id + year,
    data = block_year_weighted %>%
        filter(switch_type %in% c("No Significant Change", "Moved to More Lenient")),
    weights = ~n_sales,
    cluster = ~block_id
)

message("\nModel 1: Simple DiD (treat × post)")
summary(m1, vcov = ~block_id)

message("\nModel 2: Continuous treatment (strictness_change × post) - switching blocks only")
summary(m2, vcov = ~block_id)

message("\nModel 3a: Moved to Stricter Alderman")
summary(m3_stricter, vcov = ~block_id)

message("\nModel 3b: Moved to More Lenient Alderman")
summary(m3_lenient, vcov = ~block_id)

# =============================================================================
# 3. EVENT STUDY
# =============================================================================
message("\n\n=== Event Study ===")

# Cap relative years for cleaner bins
block_year_es <- block_year_weighted %>%
    mutate(relative_year_capped = pmax(pmin(relative_year, 5), -5))

# Event study: all switchers
m_es_all <- feols(
    log(mean_price) ~ i(relative_year_capped, treat, ref = -1) | block_id + year,
    data = block_year_es,
    weights = ~n_sales,
    cluster = ~block_id
)

# Event study: moved to stricter
m_es_stricter <- feols(
    log(mean_price) ~ i(relative_year_capped, treat, ref = -1) | block_id + year,
    data = block_year_es %>%
        filter(switch_type %in% c("No Significant Change", "Moved to Stricter")),
    weights = ~n_sales,
    cluster = ~block_id
)

# Event study: moved to more lenient
m_es_lenient <- feols(
    log(mean_price) ~ i(relative_year_capped, treat, ref = -1) | block_id + year,
    data = block_year_es %>%
        filter(switch_type %in% c("No Significant Change", "Moved to More Lenient")),
    weights = ~n_sales,
    cluster = ~block_id
)

message("\nEvent Study - All Switchers:")
summary(m_es_all)

message("\nEvent Study - Moved to Stricter:")
summary(m_es_stricter)

message("\nEvent Study - Moved to More Lenient:")
summary(m_es_lenient)

# =============================================================================
# 4. PLOTS
# =============================================================================
message("\n\n=== Creating Event Study Plots ===")

# Helper to extract iplot data and convert to ggplot-friendly format
extract_iplot_data <- function(model, group_label) {
    iplot_data <- iplot(model, .plot = FALSE)[[1]]
    if (is.null(iplot_data) || nrow(iplot_data) == 0) {
        return(NULL)
    }
    iplot_data %>%
        mutate(
            group = group_label,
            # Convert coefficients to percentage
            estimate_pct = estimate * 100,
            ci_low_pct = ci_low * 100,
            ci_high_pct = ci_high * 100
        )
}

# Extract data from all models
plot_data_all <- extract_iplot_data(m_es_all, "All Switchers")
plot_data_stricter <- extract_iplot_data(m_es_stricter, "Moved to Stricter")
plot_data_lenient <- extract_iplot_data(m_es_lenient, "Moved to More Lenient")

# Combine for faceted plot
plot_data_combined <- bind_rows(
    plot_data_stricter,
    plot_data_lenient
) %>%
    filter(!is.na(estimate))

# Create plot
if (nrow(plot_data_combined) > 0) {
    p <- ggplot(plot_data_combined, aes(x = x, y = estimate_pct, color = group)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray70") +
        geom_errorbar(aes(ymin = ci_low_pct, ymax = ci_high_pct), width = 0.2, alpha = 0.6) +
        geom_point(size = 2.5) +
        geom_line(alpha = 0.5) +
        facet_wrap(~group, ncol = 1) +
        scale_color_manual(values = c(
            "Moved to Stricter" = "#D55E00",
            "Moved to More Lenient" = "#0072B2"
        )) +
        scale_x_continuous(breaks = -5:5) +
        labs(
            title = "Event Study: Effect of Ward Switch on Home Prices",
            subtitle = "2015 Redistricting, Reference: Year Before Switch (t = -1)",
            x = "Years Relative to Ward Switch",
            y = "Effect on Log(Sale Price) (%)",
            caption = "Clustered SEs at block level. Weighted by number of sales."
        ) +
        theme_minimal(base_size = 12) +
        theme(
            legend.position = "none",
            plot.title = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold")
        )

    print(p)
    ggsave("../output/event_study_sales_price.pdf", p, width = 8, height = 8, bg = "white")
    message("Saved plot to ../output/event_study_sales_price.pdf")
}

# =============================================================================
# 5. SAVE TABLES
# =============================================================================
message("\n\n=== Saving Tables ===")

rename_dict <- c(
    "treat" = "Treatment (Switched)",
    "post" = "Post (2015+)",
    "treat:post" = "Treatment × Post",
    "strictness_change" = "Strictness Change",
    "post:strictness_change" = "Post × Strictness Change"
)

# DiD table
etable(
    list(m1, m3_stricter, m3_lenient),
    keep = c("treat:post", "post:strictness_change"),
    fitstat = ~ n + r2,
    style.tex = style.tex("aer",
        model.format = "",
        fixef.title = "",
        fixef.suffix = "",
        yesNo = c("$\\checkmark$", "")
    ),
    depvar = FALSE,
    digits = 3,
    dict = rename_dict,
    headers = c("All", "To Stricter", "To More Lenient"),
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    fixef.group = list(
        "Block FE" = "block_id",
        "Year FE" = "year"
    ),
    float = FALSE,
    file = "../output/did_table.tex",
    replace = TRUE
)
message("Saved DiD table to ../output/did_table.tex")

message("\n\nDone!")
