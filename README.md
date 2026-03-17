# Aldermanic Privilege

This repository contains the code underlying a paper and slide deck on aldermanic discretion, housing supply, rents, and home prices in Chicago. The current workflow builds a regulatory stringency index from permit processing times, merges that index into canonical border geometry, and estimates density, rental, and home-sales results at ward boundaries.

The project uses a task-based workflow. Each active task lives in `tasks/<task>/` and contains `input/`, `code/`, and `output/` folders.

## Code Organization

The workflow is organized as a series of tasks whose outputs feed downstream tasks through explicit symlinks. Each task is run from its own `code/` folder with `make`, and the task graph is generated directly from those `../input <- ../../upstream/output` dependency rules.

Task graph: [tasks/symlink_graph/output/task_flow.png](tasks/symlink_graph/output/task_flow.png)

![Task graph](tasks/symlink_graph/output/task_flow.png)

## Current Maintained Outputs

The most useful current outputs for readers are:

- Main stringency score: [tasks/create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH.csv](tasks/create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH.csv)
- Main density RD plots: [tasks/spatial_rd_fe/output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.pdf](tasks/spatial_rd_fe/output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.pdf) and [tasks/spatial_rd_fe/output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.pdf](tasks/spatial_rd_fe/output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.pdf)
- Main density FE table: [tasks/border_pair_FE_regressions/output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex](tasks/border_pair_FE_regressions/output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex)
- Main rental table: [tasks/rental_border_pair_fe/output/fe_table_rental_bw500_pre_2023_clust_ward_pair.tex](tasks/rental_border_pair_fe/output/fe_table_rental_bw500_pre_2023_clust_ward_pair.tex)
- Main sales table: [tasks/sales_border_pair_fe/output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.tex](tasks/sales_border_pair_fe/output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.tex)
- Current slide deck: [slides/slides.pdf](slides/slides.pdf)

## Replication Notes

The project is implemented through task-level Makefiles, R scripts, shell scripts, and symbolic links. Raw source data live in `data_raw/` and are not tracked in git.

Run a task from its own `code/` folder:

```bash
cd tasks/<task>/code
make
```

Build the paper:

```bash
cd paper
make
```

Build the slides:

```bash
cd slides
make
```

Rebuild the task graph:

```bash
cd tasks/symlink_graph/code
make
```
