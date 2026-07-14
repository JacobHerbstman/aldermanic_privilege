# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/corrected_permit_event_study/code")

source("../../../setup_environment/code/packages.R")

data <- read_parquet("../output/corrected_permit_block_year_panel.parquet") %>%
  filter(
    ward_pair_id == "46-48",
    dist_m <= 152.4,
    relative_year >= -5L,
    relative_year <= 5L
  ) %>%
  mutate(
    block_group = case_when(
      alderman_origin_2014 == "Harry Osterman" &
        alderman_dest_2014 == "James Cappleman" ~
        "Reassigned: Osterman to Cappleman",
      alderman_origin_2014 == alderman_dest_2014 ~
        "Comparison: same alder throughout",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(block_group))

raw_summary <- data %>%
  group_by(year, block_group) %>%
  summarise(
    blocks = n_distinct(block_id),
    total_permits = sum(n_high_discretion_issue),
    permits_per_block = mean(n_high_discretion_issue),
    blocks_with_permit = sum(n_high_discretion_issue > 0),
    share_blocks_with_permit = mean(n_high_discretion_issue > 0),
    .groups = "drop"
  ) %>%
  mutate(
    block_group = factor(
      block_group,
      levels = c(
        "Comparison: same alder throughout",
        "Reassigned: Osterman to Cappleman"
      )
    )
  )

write_csv(raw_summary, "../output/ward_46_48_raw_permits_500ft.csv")

colors <- c(
  "Comparison: same alder throughout" = "#3B6F8F",
  "Reassigned: Osterman to Cappleman" = "#C6463D"
)

common_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  )

total_plot <- ggplot(
  raw_summary,
  aes(year, total_permits, color = block_group, group = block_group)
) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "grey45") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  labs(title = "Total permits", x = NULL, y = "High-discretion permits") +
  common_theme

mean_plot <- ggplot(
  raw_summary,
  aes(year, permits_per_block, color = block_group, group = block_group)
) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "grey45") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  labs(title = "Permits per block", x = NULL, y = "Mean permit count") +
  common_theme

share_plot <- ggplot(
  raw_summary,
  aes(year, share_blocks_with_permit, color = block_group, group = block_group)
) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "grey45") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Blocks with any permit",
    x = "Year",
    y = "Share of blocks"
  ) +
  common_theme

figure <- total_plot / mean_plot / share_plot +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Raw high-discretion permits along the Ward 46-48 boundary",
    subtitle = paste(
      "Blocks within 500 feet of the boundary; dashed line marks the",
      "2015 ward reassignment"
    ),
    caption = paste(
      "Notes: Annual issue-date counts include block-years with zero permits.\n",
      "The comparison contains 64 stable blocks; 14 blocks moved from",
      "Osterman to Cappleman. Four opposite-direction movers are excluded."
    )
  ) &
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

ggsave(
  "../output/ward_46_48_raw_permits_500ft.pdf",
  figure,
  width = 9,
  height = 10
)
ggsave(
  "../output/ward_46_48_raw_permits_500ft.png",
  figure,
  width = 9,
  height = 10,
  dpi = 180
)
