# Historical construction-review footprints

Run `make` from `code/` to read the recorded Cook County 2008 and 2022 footprint
extracts and produce their data reports. The exact source files are included in
the recorded-source release restored by `make -C replication` at the repository
root. This task does not refresh the source vintage.

The original downloader is preserved in
`archive/download_official_footprint_snapshot.R`. It queried tiles around a fixed
795-project review cohort, retained nearby footprints, and recorded the requests.
Its original code and task context are also preserved at commit
`010a1f8497c1f32e2c79b5933d1c5baf9af44be3` in
`tasks/audits/new_construction_project_verification/`.

That historical cohort is not the current construction sample. Its later
restoration depended on 21 intermediate files with neither a producer nor a
preserved output in this checkout. The obsolete restoration rules were retired;
they are available at commit `1b5dae29`. The archived downloader retains its
original paths as a record of acquisition and is not an active build target.
A future source refresh requires an explicit new query cohort.
