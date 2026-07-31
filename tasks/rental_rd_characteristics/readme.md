# Rental Regression Data

This task combines the RentHub floorplan-month panel with housing
characteristics and distances to schools, parks, major streets, CTA stations,
and Lake Michigan. The output is
`output/rental_rd_characteristics_panel_bw<bandwidth>.parquet`.

The default bandwidth is 1,500 feet so the paper can estimate the main
500-foot comparison and move the cutoff 1,000 feet into either ward.
