# download_park_boundaries_data

Purpose: Downloads the official Chicago Park District park-boundary polygons from the City of Chicago Socrata API.

Source: `https://data.cityofchicago.org/d/ej32-qgdr`, backed by dataset `ejsh-fztr`.

The output is used as the main park-access amenity source. CPD facility points are not used for the main park distance because they measure distance to facility points, not distance to park land.
