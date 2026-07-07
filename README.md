# Aldermanic Privilege

This repository contains the code underlying a paper and slide deck on aldermanic discretion, housing supply, rents, and home prices in Chicago. The current workflow builds a regulatory stringency index from permit processing times, merges that index into canonical border geometry, and estimates density, rental, and home-sales results at ward boundaries.

The project uses a task-based workflow. Each active task lives in `tasks/<task>/` and contains `input/`, `code/`, and `output/` folders.

## Code Organization

The workflow is organized as a series of tasks whose outputs feed downstream tasks through explicit symlinks. Each task is run from its own `code/` folder with `make`, and the task graph is generated directly from those `../input <- ../../upstream/output` dependency rules.

Task graph: [tasks/symlink_graph/output/task_flow.png](tasks/symlink_graph/output/task_flow.png)

![Task graph](tasks/symlink_graph/output/task_flow.png)

## Replication Notes

The project is implemented through task-level Makefiles, R scripts, shell scripts, and symbolic links. A fresh clone should be run from the repository root or from task-level `code/` folders; task inputs are resolved through Makefile dependencies and symlinks.

### Data Inputs

The main paper pipeline uses a mix of frozen/local inputs and live downloads:

- Tracked static inputs: small ward and zoning GeoJSON files in `data_raw/` are committed to git.
- Required untracked local input: the Geofabrik Illinois OpenStreetMap vintage `data_raw/illinois-250919-free/` is not committed because it is large. To run the paper from a fresh clone, place that directory locally under `data_raw/`. It must include the `gis_osm_roads_free_1`, `gis_osm_landuse_a_free_1`, `gis_osm_water_a_free_1`, and `gis_osm_waterways_free_1` shapefile sidecars (`.shp`, `.dbf`, `.shx`, `.prj`, `.cpg`).
- Live downloads: several tasks download current source data at build time, including Chicago building permits, Chicago spatial/open-data endpoints, ACS/NHGIS-derived inputs, Zillow/FRED benchmark series, and RentHub files from Dewey. These sources can change over time unless their downloaded outputs are archived separately.
- Dewey/RentHub: `tasks/download_rent_data` requires `DEWEY_API_KEY`. Replicators need their own Dewey credentials. The downloader skips existing parquet files, so interrupted Dewey downloads can be resumed by rerunning `make`.

The paper has been tested from a clean clone with empty task outputs, the local `data_raw/illinois-250919-free/` directory supplied, and live-download credentials available. Exact last-decimal equality is only expected when the live downloaded sources are the same snapshots.

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
