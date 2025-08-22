All data needed to begin the project from scratch is in the `data_raw` folder (not pushed to this repo for space concerns) Everything else made is in a task output folder, and is symbolically linked to the next corresponding task input folder using makefiles.

All tasks are within the `tasks` folder, and each task has its own `makefile` and R script that performs the task.

There are currently two sets of output, the event study plots and diff-in-diff results in `run_test_regressions_permits`. 
The task order for this output is:

Using raw data (these can be run in any order): 
`clean_building_permits`
`ward_panel_create`
`create_alderman_data`
`create_block_group_controls`

Using intermediate inputs (run in this order): 
`create_ward_controls`
`data_for_alderman_strictness_scores`
`create_alderman_strictness_scores`
`merge_permits_blocks`

Makes final outputs: 
`run_test_regressions_permits`


The other is the regression discontinuity plots in `spatial_rd`. 

Using raw data (these can be run in any order): 
`process_attom_assessor`
`process_attom_assessor_historical`
`ward_panel_create`
`create_alderman_data`

Using intermediate inputs (run in this order): 
`geocode_assessor_history`
`clean_attom_historical`
`data_for_alderman_strictness_scores`
`create_alderman_strictness_scores`
`calculate_ward_boundary_distances`

Makes final outputs:
`spatial_rd`



