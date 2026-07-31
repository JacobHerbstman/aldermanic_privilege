# Clean RentHub Listings

This task converts the Illinois Dewey/RentHub files into the Chicago
floorplan-month panel used in the rent analysis.

The default window is January 2014 through December 2022. The output,
`output/chicago_rent_panel.parquet`, contains one row per observed floorplan
and month. Ward, distance, boundary-segment, and stringency-score assignments
are added in later tasks.
