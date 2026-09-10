# Construction review footprint acquisition

This restores the original Cook County 2008/2022 acquisition from
`research-archive:tasks/audits/new_construction_project_verification/code/download_official_footprint_snapshot.R`
at commit `010a1f8497c1f32e2c79b5933d1c5baf9af44be3`.

The downloader queries the official feature services around the 795-project
review cohort, then retains footprints near their project search sites. The code
records the tiling, buffers, requested fields, paging, geometry normalization,
and feature filtering. It retains the original acquisition manifest.

Running this task creates a fresh source extract. Replication instead reads the
preserved extracts in `data_raw/construction_review/`, whose hashes are recorded.
A refreshed extract must be compared with those preserved inputs before adoption.
This task has not yet been executed during restoration because its upstream
project-geometry producers are still being restored.
