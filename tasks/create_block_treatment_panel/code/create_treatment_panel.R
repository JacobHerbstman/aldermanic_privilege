# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_block_treatment_panel/code")

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

suppressMessages(sf_use_s2(FALSE))

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)

alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
    mutate(month_date = as.Date(paste("01", month), format = "%d %b %Y")) %>%
    filter(month(month_date) == 6) %>%
    mutate(year = year(month_date)) %>%
    select(year, ward, alderman)
if (anyDuplicated(alderman_panel[c("year", "ward")]) > 0) {
    stop("Alderman panel must be unique by ward-year after June filtering.", call. = FALSE)
}

blocks_2010 <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_transform(st_crs(ward_panel)) %>%
    rename(block_id = GEOID10) %>%
    mutate(block_id = as.character(block_id)) %>%
    distinct(block_id, .keep_all = TRUE)

blocks_2020 <- read_csv("../input/census_blocks_2020.csv", show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_transform(st_crs(ward_panel)) %>%
    rename(block_id = GEOID20) %>%
    mutate(block_id = as.character(block_id)) %>%
    distinct(block_id, .keep_all = TRUE)

ward_map_2003_2014 <- aggregate_ward_map(ward_panel, canonical_map_year_for_era("2003_2014"))
ward_map_2015_2023 <- aggregate_ward_map(ward_panel, canonical_map_year_for_era("2015_2023"))
ward_map_post_2023 <- aggregate_ward_map(ward_panel, canonical_map_year_for_era("post_2023"))

block_area_2010 <- tibble(
    block_id = blocks_2010$block_id,
    block_area = as.numeric(st_area(blocks_2010))
)

intersections_2010_pre_2015 <- suppressWarnings(
    st_intersection(
        blocks_2010 %>% select(block_id),
        ward_map_2003_2014 %>% select(ward)
    )
)

if (nrow(intersections_2010_pre_2015) == 0) {
    joined_2014 <- tibble(
        block_id = blocks_2010$block_id,
        ward_pre_2015 = NA_integer_,
        ward_pre_2015_share = NA_real_,
        ward_pre_2015_n_wards = NA_integer_
    )
} else {
    joined_2014 <- intersections_2010_pre_2015 %>%
        mutate(intersection_area = as.numeric(st_area(geometry))) %>%
        st_drop_geometry() %>%
        left_join(block_area_2010, by = "block_id", relationship = "many-to-one") %>%
        group_by(block_id) %>%
        arrange(desc(intersection_area), ward, .by_group = TRUE) %>%
        summarise(
            ward_pre_2015 = first(as.integer(ward)),
            ward_pre_2015_share = first(intersection_area) / first(block_area),
            ward_pre_2015_n_wards = n_distinct(ward),
            .groups = "drop"
        )

    joined_2014 <- tibble(block_id = blocks_2010$block_id) %>%
        left_join(joined_2014, by = "block_id", relationship = "one-to-one")
}

intersections_2010_post_2015 <- suppressWarnings(
    st_intersection(
        blocks_2010 %>% select(block_id),
        ward_map_2015_2023 %>% select(ward)
    )
)

if (nrow(intersections_2010_post_2015) == 0) {
    joined_2015 <- tibble(
        block_id = blocks_2010$block_id,
        ward_post_2015 = NA_integer_,
        ward_post_2015_share = NA_real_,
        ward_post_2015_n_wards = NA_integer_
    )
} else {
    joined_2015 <- intersections_2010_post_2015 %>%
        mutate(intersection_area = as.numeric(st_area(geometry))) %>%
        st_drop_geometry() %>%
        left_join(block_area_2010, by = "block_id", relationship = "many-to-one") %>%
        group_by(block_id) %>%
        arrange(desc(intersection_area), ward, .by_group = TRUE) %>%
        summarise(
            ward_post_2015 = first(as.integer(ward)),
            ward_post_2015_share = first(intersection_area) / first(block_area),
            ward_post_2015_n_wards = n_distinct(ward),
            .groups = "drop"
        )

    joined_2015 <- tibble(block_id = blocks_2010$block_id) %>%
        left_join(joined_2015, by = "block_id", relationship = "one-to-one")
}

assignments_2010 <- tibble(block_id = blocks_2010$block_id) %>%
    left_join(joined_2014, by = "block_id", relationship = "one-to-one") %>%
    left_join(joined_2015, by = "block_id", relationship = "one-to-one") %>%
    mutate(
        switched_2015 = (ward_pre_2015 != ward_post_2015) &
            !is.na(ward_pre_2015) & !is.na(ward_post_2015),
        block_vintage = "2010"
    )

block_area_2020 <- tibble(
    block_id = blocks_2020$block_id,
    block_area = as.numeric(st_area(blocks_2020))
)

intersections_2020_post_2015 <- suppressWarnings(
    st_intersection(
        blocks_2020 %>% select(block_id),
        ward_map_2015_2023 %>% select(ward)
    )
)

if (nrow(intersections_2020_post_2015) == 0) {
    joined_2022 <- tibble(
        block_id = blocks_2020$block_id,
        ward_post_2015 = NA_integer_,
        ward_post_2015_share = NA_real_,
        ward_post_2015_n_wards = NA_integer_
    )
} else {
    joined_2022 <- intersections_2020_post_2015 %>%
        mutate(intersection_area = as.numeric(st_area(geometry))) %>%
        st_drop_geometry() %>%
        left_join(block_area_2020, by = "block_id", relationship = "many-to-one") %>%
        group_by(block_id) %>%
        arrange(desc(intersection_area), ward, .by_group = TRUE) %>%
        summarise(
            ward_post_2015 = first(as.integer(ward)),
            ward_post_2015_share = first(intersection_area) / first(block_area),
            ward_post_2015_n_wards = n_distinct(ward),
            .groups = "drop"
        )

    joined_2022 <- tibble(block_id = blocks_2020$block_id) %>%
        left_join(joined_2022, by = "block_id", relationship = "one-to-one")
}

intersections_2020_post_2023 <- suppressWarnings(
    st_intersection(
        blocks_2020 %>% select(block_id),
        ward_map_post_2023 %>% select(ward)
    )
)

if (nrow(intersections_2020_post_2023) == 0) {
    joined_2024 <- tibble(
        block_id = blocks_2020$block_id,
        ward_post_2023 = NA_integer_,
        ward_post_2023_share = NA_real_,
        ward_post_2023_n_wards = NA_integer_
    )
} else {
    joined_2024 <- intersections_2020_post_2023 %>%
        mutate(intersection_area = as.numeric(st_area(geometry))) %>%
        st_drop_geometry() %>%
        left_join(block_area_2020, by = "block_id", relationship = "many-to-one") %>%
        group_by(block_id) %>%
        arrange(desc(intersection_area), ward, .by_group = TRUE) %>%
        summarise(
            ward_post_2023 = first(as.integer(ward)),
            ward_post_2023_share = first(intersection_area) / first(block_area),
            ward_post_2023_n_wards = n_distinct(ward),
            .groups = "drop"
        )

    joined_2024 <- tibble(block_id = blocks_2020$block_id) %>%
        left_join(joined_2024, by = "block_id", relationship = "one-to-one")
}

assignments_2020 <- tibble(block_id = blocks_2020$block_id) %>%
    left_join(joined_2022, by = "block_id", relationship = "one-to-one") %>%
    left_join(joined_2024, by = "block_id", relationship = "one-to-one") %>%
    mutate(
        switched_2023 = (ward_post_2015 != ward_post_2023) &
            !is.na(ward_post_2015) & !is.na(ward_post_2023),
        block_vintage = "2020"
    )

ward_turnover_2015 <- alderman_panel %>%
    filter(year %in% c(2014, 2015)) %>%
    select(ward, year, alderman) %>%
    pivot_wider(names_from = year, values_from = alderman, names_prefix = "alderman_") %>%
    mutate(ward_had_turnover_2015 = alderman_2014 != alderman_2015) %>%
    select(ward, ward_had_turnover_2015)
if (anyDuplicated(ward_turnover_2015$ward) > 0) {
    stop("2015 ward turnover lookup must be unique by ward.", call. = FALSE)
}

ward_turnover_2023 <- alderman_panel %>%
    filter(year %in% c(2022, 2023)) %>%
    select(ward, year, alderman) %>%
    pivot_wider(names_from = year, values_from = alderman, names_prefix = "alderman_") %>%
    mutate(ward_had_turnover_2023 = alderman_2022 != alderman_2023) %>%
    select(ward, ward_had_turnover_2023)
if (anyDuplicated(ward_turnover_2023$ward) > 0) {
    stop("2023 ward turnover lookup must be unique by ward.", call. = FALSE)
}

treatment_2015 <- assignments_2010 %>%
    left_join(ward_turnover_2015, by = c("ward_pre_2015" = "ward"), relationship = "many-to-one") %>%
    mutate(
        has_complete_ward_assignment_2015 = !is.na(ward_pre_2015) & !is.na(ward_post_2015),
        valid_2015 = has_complete_ward_assignment_2015,
        valid_2015 = replace_na(valid_2015, FALSE)
    )

treatment_2023 <- assignments_2020 %>%
    left_join(ward_turnover_2023, by = c("ward_post_2015" = "ward"), relationship = "many-to-one") %>%
    mutate(
        has_complete_ward_assignment_2023 = !is.na(ward_post_2015) & !is.na(ward_post_2023),
        valid_2023 = has_complete_ward_assignment_2023 &
            (switched_2023 | (!switched_2023 & !ward_had_turnover_2023)),
        valid_2023 = replace_na(valid_2023, FALSE)
    )

panel_2015 <- treatment_2015 %>%
    select(
        block_id, block_vintage,
        ward_pre_2015, ward_post_2015,
        ward_pre_2015_share, ward_post_2015_share,
        ward_pre_2015_n_wards, ward_post_2015_n_wards,
        switched_2015,
        ward_had_turnover_2015, valid_2015, has_complete_ward_assignment_2015
    ) %>%
    mutate(cohort = "2015")

panel_2023 <- treatment_2023 %>%
    select(
        block_id, block_vintage,
        ward_post_2015, ward_post_2023,
        ward_post_2015_share, ward_post_2023_share,
        ward_post_2015_n_wards, ward_post_2023_n_wards,
        switched_2023,
        ward_had_turnover_2023, valid_2023, has_complete_ward_assignment_2023
    ) %>%
    mutate(cohort = "2023")

panel_2015_renamed <- panel_2015 %>%
    rename(
        ward_origin = ward_pre_2015,
        ward_dest = ward_post_2015,
        ward_origin_share = ward_pre_2015_share,
        ward_dest_share = ward_post_2015_share,
        ward_origin_n_wards = ward_pre_2015_n_wards,
        ward_dest_n_wards = ward_post_2015_n_wards,
        switched = switched_2015,
        ward_had_turnover = ward_had_turnover_2015,
        valid = valid_2015,
        has_complete_ward_assignment = has_complete_ward_assignment_2015
    )

panel_2023_renamed <- panel_2023 %>%
    rename(
        ward_origin = ward_post_2015,
        ward_dest = ward_post_2023,
        ward_origin_share = ward_post_2015_share,
        ward_dest_share = ward_post_2023_share,
        ward_origin_n_wards = ward_post_2015_n_wards,
        ward_dest_n_wards = ward_post_2023_n_wards,
        switched = switched_2023,
        ward_had_turnover = ward_had_turnover_2023,
        valid = valid_2023,
        has_complete_ward_assignment = has_complete_ward_assignment_2023
    )

block_treatment_pre_scores <- bind_rows(panel_2015_renamed, panel_2023_renamed)
block_treatment_pre_scores <- block_treatment_pre_scores %>%
    mutate(
        min_assignment_share = pmin(ward_origin_share, ward_dest_share, na.rm = TRUE)
    )

write_csv(block_treatment_pre_scores, "../output/block_treatment_pre_scores.csv")
