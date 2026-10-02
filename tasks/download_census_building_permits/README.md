# Census building permits for Chicago

`download_census_building_permits.R` downloads the Census Bureau Building
Permits Survey annual place files for the Midwest region, 1990–2025
(`https://www2.census.gov/econ/bps/Place/Midwest%20Region/mwYYYYa.txt`), and
saves Chicago's row for each year in `output/chicago_building_permits_survey.csv`:
permitted buildings, units and construction value in 1-unit, 2-unit, 3–4-unit
and 5+-unit buildings, with the months the City reported. The first set of counts
includes the Census Bureau's imputation for months not reported; the `_reported`
set counts only reported months. Chicago is the Illinois place named "Chicago";
place IDs were reassigned in the early 1990s.

These annual files are revised rarely; a later run can still differ from the
recorded report. Run `make` in `code/`.
