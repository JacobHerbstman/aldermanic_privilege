# ─────────────────────────────────────────────────────────────────────────────
# extract_developer_identities.R
# Extract and standardize developer identities from building permits
# ─────────────────────────────────────────────────────────────────────────────

source("../../setup_environment/code/packages.R")

library(stringdist)

message("=== Step 1: Loading raw permit data ===")

permits_raw <- read_csv("../input/Building_Permits_20251121.csv", 
                        show_col_types = FALSE) %>%
  janitor::clean_names()

message(sprintf("Loaded %d permits", nrow(permits_raw)))

# ─────────────────────────────────────────────────────────────────────────────
# Filter to high discretion permits only
# Focus on big developers doing substantial work (renovations, demolitions, new construction)
# ─────────────────────────────────────────────────────────────────────────────

high_discretion_permits <- c(
  "PERMIT - NEW CONSTRUCTION",
  "PERMIT - RENOVATION/ALTERATION",
  "PERMIT - WRECKING/DEMOLITION",
  "PERMIT - REINSTATE REVOKED PMT"
)

permits_raw <- permits_raw %>%
  filter(permit_type %in% high_discretion_permits)

message(sprintf("Filtered to %d high discretion permits", nrow(permits_raw)))
message("Permit type breakdown:")
print(permits_raw %>% count(permit_type, sort = TRUE))

# ─────────────────────────────────────────────────────────────────────────────
# Step 1: Reshape contact columns from wide to long
# ─────────────────────────────────────────────────────────────────────────────

message("=== Step 2: Reshaping contact columns to long format ===")

contacts_long <- permits_raw %>%
  select(id, permit_type, ward, application_start_date, processing_time,
         reported_cost, permit_status,
         matches("^contact_\\d+_(type|name)$")) %>%
  pivot_longer(
    cols = matches("^contact_\\d+"),
    names_to = c("contact_num", ".value"),
    names_pattern = "contact_(\\d+)_(.*)"
  ) %>%
  filter(!is.na(name) & name != "")

message(sprintf("Found %d permit-contact pairs", nrow(contacts_long)))

# ─────────────────────────────────────────────────────────────────────────────
# Step 2: Explore contact types
# ─────────────────────────────────────────────────────────────────────────────

message("=== Step 3: Exploring contact types ===")

type_counts <- contacts_long %>% 
  count(type, sort = TRUE)

message("Contact type frequencies:")
print(type_counts, n = 30)

write_csv(type_counts, "../output/contact_type_counts.csv")
message("Wrote contact_type_counts.csv for manual review")

# ─────────────────────────────────────────────────────────────────────────────
# Step 3: Define developer hierarchy and identify primary developer
# ─────────────────────────────────────────────────────────────────────────────

message("=== Step 4: Identifying primary developer per permit ===")

# Define hierarchy: who counts as "the developer"
# Lower rank = higher priority
# Based on actual contact types in the data
developer_types <- c(
  "RESIDENTAL REAL ESTATE DEV",        # 608 entries - actual developers
  "CONTRACTOR-GENERAL CONTRACTOR",      # 318,217 entries - this is the main GC type
  "OWNER AS GENERAL CONTRACTOR",        # 72,339 entries - owner acting as GC
  "GENERAL CONTRACTOR",                 # 17,947 entries
  "OWNER AS ARCHITECT & CONTRACTR",     # 6,187 entries - owner multi-role
  "BUILDING OWNER",                     # 7,750 entries - explicit owner
  "OWNER",                              # 363,098 entries - generic owner (lower priority)
  "OWNER OCCUPIED"                      # 106,032 entries - individual homeowners (lowest)
)

# Check which of our expected types exist
existing_developer_types <- type_counts %>%
  filter(type %in% developer_types) %>%
  pull(type)

message("Developer types found in data:")
print(existing_developer_types)

# Filter to developer types and pick highest priority per permit
permit_developers <- contacts_long %>%
  filter(type %in% developer_types) %>%
  mutate(type_rank = match(type, developer_types)) %>%
  group_by(id) %>%
  slice_min(type_rank, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(id, developer_name_raw = name, developer_type = type)

message(sprintf("Identified developers for %d permits (%.1f%% of total)",
                nrow(permit_developers),
                100 * nrow(permit_developers) / n_distinct(permits_raw$id)))

# Developer type distribution
message("Developer type distribution:")
print(permit_developers %>% count(developer_type, sort = TRUE))

# ─────────────────────────────────────────────────────────────────────────────
# Step 4: Standardize company names
# ─────────────────────────────────────────────────────────────────────────────

message("=== Step 5: Standardizing company names ===")

clean_company_name <- function(name) {
  name %>%
    str_to_upper() %>%
    # Remove punctuation
    str_remove_all("[.,'/\"\\-]") %>%
    # Remove common prefixes
    str_remove("^THE\\s+") %>%
    # Collapse multiple spaces
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    # Standardize common suffixes/abbreviations
    str_replace("\\bL\\.?L\\.?C\\.?\\b", "LLC") %>%
    str_replace("\\bINC\\.?\\b", "INC") %>%
    str_replace("\\bCORP\\.?(ORATION)?\\b", "CORP") %>%
    str_replace("\\bCO\\.?\\b$", "CO") %>%
    str_replace("\\bCOMPANY\\b", "CO") %>%
    str_replace("\\bCONSTRUCTION\\b", "CONST") %>%
    str_replace("\\bCONSTR\\b", "CONST") %>%
    str_replace("\\bDEVELOPMENT(S)?\\b", "DEV") %>%
    str_replace("\\bPROPERT(Y|IES)\\b", "PROP") %>%
    str_replace("\\bGENERAL CONTRACTOR\\b", "GC") %>%
    str_replace("\\bGENERAL\\b", "GEN") %>%
    str_replace("\\bBUILDERS?\\b", "BLDR") %>%
    str_replace("\\bSERVICES?\\b", "SVC") %>%
    str_replace("\\bMANAGEMENT\\b", "MGMT") %>%
    str_replace("\\bENTERPRISES?\\b", "ENT") %>%
    str_replace("\\bASSOCIATES?\\b", "ASSOC") %>%
    str_replace("\\bGROUP\\b", "GRP") %>%
    str_replace("\\bCHICAGO\\b", "CHI") %>%
    str_replace("\\bUNIVERSITY\\b", "UNIV") %>%
    str_replace("\\bINTERNATIONAL\\b", "INTL") %>%
    str_replace("\\bAMERICAN?\\b", "AMER") %>%
    str_replace("\\bNATIONAL\\b", "NATL") %>%
    # Final cleanup
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

permit_developers <- permit_developers %>%
  mutate(developer_name_clean = clean_company_name(developer_name_raw))

# Count unique names before and after cleaning
n_raw <- n_distinct(permit_developers$developer_name_raw)
n_clean <- n_distinct(permit_developers$developer_name_clean)
message(sprintf("Unique names: %d raw -> %d cleaned (%.1f%% reduction)",
                n_raw, n_clean, 100 * (1 - n_clean/n_raw)))

# ─────────────────────────────────────────────────────────────────────────────
# Step 5: Cluster similar names using fuzzy matching
# ─────────────────────────────────────────────────────────────────────────────

message("=== Step 6: Clustering similar names with fuzzy matching ===")

# Get frequency counts
name_counts <- permit_developers %>%
  count(developer_name_clean, sort = TRUE, name = "permit_count")

# Only cluster names appearing >= 3 times (computational efficiency)
min_permits_for_clustering <- 3
frequent_names <- name_counts %>%
  filter(permit_count >= min_permits_for_clustering) %>%
  pull(developer_name_clean)

message(sprintf("Clustering %d names with >= %d permits (covers %d permits)",
                length(frequent_names),
                min_permits_for_clustering,
                sum(name_counts$permit_count[name_counts$developer_name_clean %in% frequent_names])))

# Compute Jaro-Winkler distance matrix
message("Computing string distance matrix...")
dist_matrix <- stringdistmatrix(frequent_names, method = "jw")

# Hierarchical clustering
message("Performing hierarchical clustering...")
hc <- hclust(as.dist(dist_matrix), method = "complete")

# Cut tree at threshold (conservative to avoid false merges)
# 0.15 was too aggressive - merged different companies like "BEAR CONST CO" with "EMTRAK CONST CO"
cluster_threshold <- 0.08
clusters <- cutree(hc, h = cluster_threshold)

message(sprintf("Threshold %.2f created %d clusters from %d names",
                cluster_threshold, 
                n_distinct(clusters),
                length(frequent_names)))

# Create crosswalk: pick first (most frequent) name in each cluster as canonical
name_crosswalk <- tibble(
  developer_name_clean = frequent_names,
  developer_cluster = clusters
) %>%
  left_join(name_counts, by = "developer_name_clean") %>%
  group_by(developer_cluster) %>%
  # Use the most frequent name in the cluster as the standard name
  mutate(developer_name_std = developer_name_clean[which.max(permit_count)]) %>%
  ungroup() %>%
  select(developer_name_clean, developer_name_std, developer_cluster, permit_count)

# Show some example clusters for inspection
message("\n=== Sample clusters with multiple names (for inspection) ===")
multi_name_clusters <- name_crosswalk %>%
  group_by(developer_cluster) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(developer_cluster, desc(permit_count))

sample_clusters <- unique(multi_name_clusters$developer_cluster)[1:min(10, n_distinct(multi_name_clusters$developer_cluster))]
for (cl in sample_clusters) {
  cluster_names <- multi_name_clusters %>%
    filter(developer_cluster == cl) %>%
    pull(developer_name_clean)
  message(sprintf("\nCluster %d: %s", cl, paste(cluster_names, collapse = " | ")))
}

# ─────────────────────────────────────────────────────────────────────────────
# Step 6: Create developer IDs and final output
# ─────────────────────────────────────────────────────────────────────────────

message("\n=== Step 7: Creating final outputs ===")

# Join standardized names back to all permits
permit_developers_final <- permit_developers %>%
  left_join(name_crosswalk %>% select(developer_name_clean, developer_name_std), 
            by = "developer_name_clean") %>%
  mutate(
    # For infrequent names not in crosswalk, use cleaned name as-is
    developer_name_std = coalesce(developer_name_std, developer_name_clean),
    developer_id = as.integer(factor(developer_name_std))
  )

# Summary stats on standardization
n_std <- n_distinct(permit_developers_final$developer_name_std)
message(sprintf("Final standardized names: %d (from %d cleaned, %d raw)",
                n_std, n_clean, n_raw))

# Merge back to full permit data for output
permits_with_developers <- permits_raw %>%
  select(id, permit_type, ward, application_start_date, processing_time, 
         reported_cost, permit_status) %>%
  left_join(permit_developers_final %>% 
              select(id, developer_name_raw, developer_name_clean, 
                     developer_name_std, developer_type, developer_id),
            by = "id") %>%
  mutate(has_developer = !is.na(developer_id))

# Summary
message("\n=== Final dataset summary ===")
message(sprintf("Total permits: %d", nrow(permits_with_developers)))
message(sprintf("Permits with developer: %d (%.1f%%)",
                sum(permits_with_developers$has_developer),
                100 * mean(permits_with_developers$has_developer)))
message(sprintf("Unique developers: %d", n_distinct(permits_with_developers$developer_id, na.rm = TRUE)))

# Top developers
message("\n=== Top 20 developers by permit count ===")
top_developers <- permits_with_developers %>%
  filter(!is.na(developer_name_std)) %>%
  count(developer_name_std, developer_id, sort = TRUE) %>%
  head(20)
print(top_developers)

# ─────────────────────────────────────────────────────────────────────────────
# Write outputs
# ─────────────────────────────────────────────────────────────────────────────

message("\n=== Writing output files ===")

write_csv(permits_with_developers, "../output/permits_with_developers.csv")
message("Wrote permits_with_developers.csv")

write_csv(name_crosswalk, "../output/developer_name_crosswalk.csv")
message("Wrote developer_name_crosswalk.csv")

# Also write a top developers summary
write_csv(top_developers, "../output/top_developers.csv")
message("Wrote top_developers.csv")

# Write multi-name clusters for manual review
write_csv(multi_name_clusters, "../output/fuzzy_match_clusters.csv")
message("Wrote fuzzy_match_clusters.csv (for manual inspection)")

message("\n=== Done! ===")
