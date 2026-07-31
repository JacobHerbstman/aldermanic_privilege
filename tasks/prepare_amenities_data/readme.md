# Prepare Amenity Locations

This task prepares the CTA, major-street, park, and school locations used in the
rent and sales controls. All four output files use EPSG:3435.

Main amenity sources:

- Lake Michigan: OSM water geometry is read downstream and treated as static.
- Major streets: City of Chicago Major Streets shapefile, treated as a stable
  measure of access to major roads.
- Parks: official Chicago Park District park-boundary polygons from
  `download_park_boundaries_data`, not CPD facility points.
- Schools: CPS SY2014-15 school locations, treated as a fixed measure of school
  access.
- CTA stops: CTA current station points plus Washington/State, Madison/Wabash,
  and Randolph/Wabash, the three stations that closed during 2006--2022. The
  output records opening and closing dates so the rent and sales controls use
  the stations open in each observation month. Opening dates are also recorded
  for Morgan, Oakton-Skokie, Cermak-McCormick Place, Washington/Wabash, and the
  Damen Green Line station.
