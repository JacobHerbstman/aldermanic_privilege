# Aldermanic Privilege

This repository contains the analysis pipeline for a paper and slide deck on aldermanic discretion, housing supply, rents, and home prices in Chicago.

The project uses a task-based workflow. Each active task lives in `tasks/<task_name>/` and typically contains:

- `code/`: scripts and a task `Makefile`
- `input/`: symlinked upstream inputs
- `output/`: generated artifacts

The top-level contract is:

- run each task with `make` from its `code/` directory
- compile the paper with `make` in `paper/`
- compile the slides with `make` in `slides/`
- edit generating scripts, not generated tables or figures

## Task Graph

The workflow is organized as a series of tasks. Each task folder contains `input`, `code`, and `output`. A task's output is used as an input by one or more downstream tasks.

The repository includes a task graph in the Dingel-Neiman style: it is generated from explicit `../input <- ../../upstream/output` symlink rules in task Makefiles, so it stays dynamic and readable as the workflow changes.

Build it with:

```bash
cd tasks/symlink_graph/code
make
```

Current graph:

![Task graph](tasks/symlink_graph/output/task_flow.png)

## Current Repository Structure

### Top-level directories

- `data_raw/`: raw source data, not tracked in git
- `paper/`: LaTeX paper and section files
- `slides/`: Beamer slide deck, section files, and a small number of local/manual assets
- `tasks/`: active analysis tasks plus archive and maintenance utilities
- `tmp/`: local scratch space, ignored by git

### Task namespaces

- `tasks/<name>/`: active task folders used by the current workflow
- `tasks/_lib/`: shared helpers used across multiple tasks
- `tasks/_deprecated/`: scripts moved out of active task surfaces but kept for reference
- `tasks/archive/`: archived task folders from older iterations of the project
- `tasks/symlink_graph/`: generated task graph based on symlink dependencies

## Workflow Conventions

- Each active task should have a simple `Makefile` with a default `all` target and a `link-inputs` target.
- Downstream inputs are connected by symlinks in `input/`, not by copying files.
- Task outputs are mostly ignored by git; the maintained workflow is represented by the task code and Makefiles.
- The current slide deck points directly to task outputs under `../tasks/.../output/...`, so slide-linked figures and tables update when their upstream tasks are rerun.
- Some local slide assets in `slides/images/` and `slides/figures/` are intentionally manual; only the files actually used by the deck are tracked.

## Active Analytical Pipelines

### 1. Alderman stringency / uncertainty index

This branch constructs the alderman-level regulatory stringency score from building permit processing times and validates it.

Core tasks:

- `building_permits_scraping`
- `clean_building_permits`
- `download_residential_improvements_full`
- `create_alderman_data`
- `create_ward_controls`
- `data_for_alderman_uncertainty_index`
- `create_alderman_uncertainty_index`
- `uncertainty_validation_checks`
- `strictness_score_map`
- `within_ward_strictness`
- `permit_summary_stats`

Main maintained outputs include:

- the alderman uncertainty index CSV and distribution plot
- the ward-level strictness map
- validation tables
- permit-processing summary tables and figures

### 2. Canonical border geometry

This branch creates the canonical ward-boundary geometry, segment buffers, parcel/sale/rent boundary distances, and merged score files used downstream.

Core tasks:

- `ward_panel_create`
- `border_segment_creation`
- `calculate_ward_boundary_distances`
- `assign_segment_ids`
- `merge_in_scores`
- `calculate_sale_distances`
- `calculate_rent_distances`
- `assign_segment_ids_sales_rental`
- `merge_event_study_scores`
- `geometry_pipeline_validation`

These tasks define the shared geography used by the density, rental, sales, and event-study branches.

### 3. Density at ward boundaries

This branch studies multifamily development intensity near ward boundaries.

Core tasks:

- `summary_stats_new_construction`
- `spatial_rd_fe`
- `border_pair_FE_regressions`
- `border_confound_pruning`
- `pruned_boundary_maps`
- `uncertainty_score_density_robustness`
- `uncertainty_permit_border500_robustness`

Current slide-facing density outputs come from `spatial_rd_fe`, `border_pair_FE_regressions`, `summary_stats_new_construction`, and `pruned_boundary_maps`.

Key maintained slide-facing outputs:

- `tasks/spatial_rd_fe/output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.pdf`
- `tasks/spatial_rd_fe/output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.pdf`
- `tasks/border_pair_FE_regressions/output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex`

### 4. Rental and home-sales border FE results

These tasks estimate local-border reduced-form effects on rents, listed rental supply, and home prices using the canonical geometry and current stringency score.

Core tasks:

- `rental_border_pair_fe`
- `sales_border_pair_fe`
- `rental_characteristics_at_borders`
- `rental_border_fe_sensitivity`
- `prep_sales_border_data`
- `full_sample_border_pair_fe`
- `full_sample_rental_border_fe`

Current maintained mechanism outputs include:

- rental FE tables and plots
- sales FE tables
- rental quality-sorting and amenity-sorting tables
- rental listed-units PPML tables and plots
- rental permutation-test appendix outputs

Key maintained slide-facing outputs:

- `tasks/rental_border_pair_fe/output/fe_table_rental_bw500_pre_2023_clust_ward_pair.tex`
- `tasks/sales_border_pair_fe/output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.tex`
- `tasks/rental_characteristics_at_borders/output/rent_amenity_attenuation_bw500_pre_2023_clust_ward_pair.tex`
- `tasks/rental_characteristics_at_borders/output/listing_units_ppml_fe_table_bw500_pre_2023_multifamily_only_pct0_clust_ward_pair.tex`

### 5. Event studies and diagnostics

These tasks build treatment panels and event-study datasets for sales and rentals, then estimate dynamic effects around redistricting-driven alderman changes.

Core tasks:

- `process_residential_improvements_full`
- `process_rent_data`
- `create_block_treatment_panel`
- `create_event_study_sales_data`
- `create_event_study_sales_data_disaggregate`
- `create_event_study_rental_data_disaggregate`
- `run_event_study_sales_disaggregate`
- `run_event_study_rental_disaggregate`
- `event_study_sales_diagnostics`
- `repeat_sales_event_study`
- `event_study_audit`
- `combine_did_tables`
- `summary_stats_event_study`
- `create_appendix_a_sumstats`

These tasks are active, but not all outputs are used in the current slide deck.

### 6. Audit and maintenance tasks

These tasks exist to audit the current deck/workflow and keep the project readable.

Core tasks:

- `slide_deck_audit`
- `project_cleanup_audit`
- `setup_environment`

## Paper and Slide Structure

### Paper

- Main file: `paper/paper.tex`
- Sections live in `paper/sections/`
- Compile with:

```bash
cd paper
make
```

### Slides

- Main file: `slides/slides.tex`
- Sections live in `slides/sections/`
- Compile with:

```bash
cd slides
make
```

The current slide deck mixes task-linked empirical outputs with a small number of local/manual assets in `slides/images/` and `slides/figures/`.

## Data Layout

Raw data are not tracked in git and live under `data_raw/`. The current raw-data tree includes material for:

- assessor parcel, sales, and improvement records
- building permits
- ward and city boundary files
- zoning and geographic overlays
- OSM water shapefiles
- census blocks and block-level geography
- rent downloads and processing inputs

Task outputs that are expensive or canonical are kept in task `output/` folders and then symlinked forward into downstream `input/` folders.

## Current Practical Entry Points

If you are trying to orient yourself quickly, these are the best entry points:

- strictness construction: `tasks/create_alderman_uncertainty_index`
- canonical border geometry: `tasks/border_segment_creation`
- parcel boundary distances: `tasks/calculate_ward_boundary_distances`
- main density RD plots: `tasks/spatial_rd_fe`
- main density FE tables: `tasks/border_pair_FE_regressions`
- main rental FE tables: `tasks/rental_border_pair_fe`
- rental mechanisms: `tasks/rental_characteristics_at_borders`
- main sales FE tables: `tasks/sales_border_pair_fe`
- geometry validation: `tasks/geometry_pipeline_validation`
- slide audit: `tasks/slide_deck_audit`
- cleanup manifest: `tasks/project_cleanup_audit`

## Notes on Git Tracking

- `data_raw/` is ignored.
- Most task `input/`, `output/`, and `temp/` directories are ignored.
- Local scratch under `tmp/` is ignored.
- `slides/images/` and `slides/figures/` are mostly ignored, with only the required tracked assets explicitly unignored.

If a table or figure is important for the current workflow, the source of truth should be the generating task code and its maintained output path, not a copied artifact elsewhere in the repo.
