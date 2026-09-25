# Aldermanic Privilege

This repository contains the code and data needed to reproduce the paper's
analysis of aldermanic discretion, housing supply, rents, and home prices in
Chicago. The paper measures differences in alderman stringency using permit
processing times and compares outcomes across ward boundaries.

## Reproduce the Paper

From the repository root, run:

```bash
make
```

This runs three steps in order:

1. `make -C tasks/setup_environment/code` installs any missing R packages, builds
   the Python environment for the data reports, and records their versions.
2. `make -C replication` downloads the recorded source archive from the
   [GitHub release](https://github.com/JacobHerbstman/aldermanic_privilege/releases/tag/recorded-sources-2026-09-10),
   verifies its checksum, and restores the recorded raw files under `data_raw/`.
3. `make -C paper` builds every task the paper depends on, in dependency order,
   and compiles `paper/paper.pdf`, `paper/online_appendix.pdf`,
   `paper/working_paper.pdf` and `paper/word_count.pdf`.

If a download is interrupted, run `make` again: completed files are kept, the
release downloads resume where they stopped, and Make continues from the first
missing or out-of-date file. An unchanged second
`make` runs no analysis. Run one build at a time in a checkout.

### Requirements

- R (tested with 4.5.2) with the system libraries needed by `sf`, `units` and
  `arrow` (GDAL, GEOS, PROJ, UDUNITS).
- Python 3.11 or later on the PATH (tested with 3.13 and 3.14). The setup step
  builds its own virtual environment in `tasks/setup_environment/output/python-env`
  with the pinned report packages (`pandas==3.0.0`, `duckdb==1.4.4`); nothing is
  installed into the system Python. If no Python 3.11+ is found, setup stops and
  says how to install one.
- GNU Make 3.81 or later, Bash, `curl`, `tar`, `gzip` and `shasum` (the optional source-refresh targets also use `wget`).
- A LaTeX installation providing `pdflatex` and `bibtex`.
- About 60 GB of free disk space for downloads and intermediate files.

### Credentials

RentHub listings are downloaded from Dewey and require a `DEWEY_API_KEY`
environment variable (for example in `~/.Renviron`). Every RentHub file is checked
against the recorded list of sizes and checksums in
`tasks/download_rent_data/sources/renthub_manifest.csv`. A deliberate refresh of
the recorded Census inputs requires `CENSUS_API_KEY`; ordinary builds reuse the
committed Census sources and do not need it.

## Data Inputs

Most sources are recorded snapshots whose bytes are checked against a SHA-256
checksum before use:

- **Recorded source archive** (`make -C replication`): Cook County parcel sales and
  residential improvement records, the Chicago CPI-U series, the full Chicago
  building-permit extract and Assessor records used to build the construction
  data, Cook County parcel addresses and the 2025 parcel universe, historical
  zoning maps, OpenStreetMap layers from the September 19, 2025 Geofabrik Illinois
  extract, and the 2014 and 2015 ward maps.
- **Release asset**: the 2006–2022 building-permit extract used for processing
  times (`tasks/download_building_permits`).
- **Committed files**: Census counts and geography (`tasks/create_ward_controls/sources`,
  `tasks/create_block_group_controls/sources`), major streets, historical sale-parcel
  coordinates, the RentHub file list, and the hand-research tables (alderman terms,
  construction decisions, permit-block and rental-location reviews, preserved
  zoning history).

The following sources are downloaded live on every fresh build and are not
checksum-pinned, so a later build can differ slightly from the recorded data
reports if the publisher has revised them: Cook County condominium
characteristics and parcel centroids, 2022 building footprints, Chicago Park
District boundaries, CTA stations, 2014–15 CPS school locations, community areas,
and 2010 Census blocks from the City of Chicago data portal.

Every data-producing script writes a report on each saved dataset to its task's
`report/` folder (row counts, column summaries, key checks and a SHA-256 of the
saved bytes). These reports are committed, so `git diff` after a rebuild shows
whether any dataset changed.

## Task Graph

Each task lives in `tasks/<task>/` with its own `code/`, `input/` and `output/`
folders. Task Makefiles connect tasks through explicit file dependencies, and
`paper/Makefile` lists every table and figure the paper uses. The graph shows every
task required by the paper; arrows point from a task to the next task that uses
its output.

[![Paper task dependency graph](task_graph/paper_task_flow.svg)](task_graph/paper_task_flow.svg)

The diagram is generated from the Makefiles. After changing dependencies, rebuild
it with `make -C task_graph` (requires Graphviz). The new-construction data are
built by `tasks/prepare_permit_construction` (see its README) and prepared for the
density analysis by `tasks/new_construction_analysis_data`.

## Research Archive

The `main` branch contains only the data and code needed to reproduce the
paper. Audits, exploratory specifications, rezoning work, old analyses, and
presentation files are preserved on the
[`research-archive`](https://github.com/JacobHerbstman/aldermanic_privilege/tree/research-archive)
branch. To recover one of those files without switching branches, use:

```bash
git restore --source research-archive -- path/to/file
```
