# this code cleans multifamily data
# to include only buildings built on or after the year 1999 and in townships 70-77 (Chicago)

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")

# ---------- 1) Load and keep Chicago ----------
data <- readr::read_csv("../input/commercial_value_raw.csv") %>% 
  janitor::clean_names()

data <- data %>%
  dplyr::filter(township %in% c("West Chicago","South Chicago","Jefferson","North Chicago","Lake View","Rogers Park","Hyde Park","Lake"))

data <-data %>% 
  filter(yearbuilt >= 1999)


multifamily_data <- data %>% 
  filter(str_detect(modelgroup, "(?i)Multifamily|Class3|Class9|Condos")) %>%
  mutate(calculated_sum = rowSums(select(., studiounits, x1brunits, x2brunits, x3brunits, x4brunits), na.rm = TRUE)) %>% 
  mutate(tot_units = coalesce(tot_units, if_else(calculated_sum > 0, calculated_sum, NA_real_))) %>% 
  filter(!is.na(tot_units)) %>% 
  select(-calculated_sum) %>% 
  select(
    keypin,
    address,
    yearbuilt,
    tot_units,
    bldgsf,
    landsf,
    modelgroup,
    class_es, # usage class
    studiounits,
    x1brunits,
    x2brunits,
    x3brunits,
    x4brunits
  ) %>% 
  mutate(keypin = str_remove_all(keypin, "-")) %>% 
  rename(pin = keypin)


multifamily_data_deduped <- multifamily_data %>%
  # Create a helper flag for "Has Land"
  mutate(has_land = landsf > 0) %>% 
  group_by(pin) %>% 
  arrange(desc(has_land), desc(tot_units)) %>% 
  slice(1) %>%
  ungroup() %>% 
  select(-has_land)

write_csv(multifamily_data_deduped, "../output/multifamily_data_cleaned.csv")


# multifamily_data_deduped %>%
#   # Filter to the relevant window to avoid skewing the chart with high-rises
#   filter(tot_units >= 5 & tot_units <= 25) %>% 
#   ggplot(aes(x = tot_units)) +
#   geom_bar(fill = "steelblue", color = "black", width = 0.8) +
#   # Force x-axis to show every integer so you can clearly see 9 vs 10
#   scale_x_continuous(breaks = seq(5, 25, 1)) + 
#   theme_minimal() +
#   labs(
#     title = "Bunching Estimator: Unit Counts (5-25 Units)",
#     subtitle = "Look for the 'Cliff' between 9 and 10 units (ARO Threshold)",
#     x = "Total Units",
#     y = "Number of Buildings"
#   )  
# time_series_counts <- multifamily_data_deduped %>%
#   # Filter to the "Bunching Window" + a control group (12 units)
#   filter(tot_units %in% c(7, 8, 9, 10, 12)) %>%
#   count(yearbuilt, tot_units) %>% 
#   complete(yearbuilt = 1999:2024, tot_units, fill = list(n = 0)) %>%
#   group_by(tot_units) %>%
#   arrange(yearbuilt) %>%
#   mutate(
#     # k=2, align="right": The value for 2002 is the average of 2001 and 2002.
#     n_smooth = rollmean(n, k = 2, fill = NA, align = "right")
#   ) %>%
#   ungroup()
# 
# 
# ggplot(time_series_counts, aes(x = yearbuilt, y = n_smooth, color = factor(tot_units))) +
#   geom_line(linewidth = 1.2) +
#   
#   # 2007 Shock
#   geom_vline(xintercept = 2007, linetype = "dotted", color = "black") +
#   annotate("text", x = 2007, y = max(time_series_counts$n_smooth, na.rm=TRUE), 
#            label = "2007: Zoning Trigger", angle = 90, vjust = -0.5) +
#   
#   # 2015 Shock
#   geom_vline(xintercept = 2015, linetype = "dashed", color = "red") +
#   annotate("text", x = 2015, y = max(time_series_counts$n_smooth, na.rm=TRUE), 
#            label = "2015 Reform", angle = 90, vjust = -0.5) +
#   
#   scale_color_manual(values = c(
#     "7" = "#33a02c",
#     "8" = "#a6cee3",  
#     "9" = "#1f78b4",  # Dark Blue
#     "10" = "#e31a1c", # Red
#     "12" = "#33a02c"  
#   )) +
#   theme_minimal() +
#   labs(
#     title = "The Two Eras of ARO Avoidance (2-Year Smoothed)",
#     subtitle = "Note the divergence of 9 units (Blue) vs 10 units (Red)",
#     y = "New Buildings (2-Yr Avg)",
#     x = "Year Built",
#     color = "Unit Count"
#   )
# 
