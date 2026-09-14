# Rent and Home-Price Boundary Results

This task compares listed rents and home sale prices on opposite sides of ward
boundaries. It uses 100-foot distance bands within 500 feet of each boundary.
The band immediately inside the less-stringent side is the reference group.

Rental regressions use listings with reliable geographic assignments, housing
characteristics, nearby amenities, boundary-segment-by-month fixed effects, and
standard errors clustered by ward pair. Sales regressions use the
corresponding property and amenity controls, boundary-segment-by-quarter fixed
effects, and ward-pair clustering.

Each panel reports the difference between the two bands nearest the cutoff.
The additional figures move the cutoff 1,000 feet into either ward, restrict the
sample to locally straight boundaries, and exclude observations within 25 or 50
feet of the boundary.

The official conditional-price specifications include categorical dwelling-type
fixed effects. For rental listings, the categorical control is the cleaned
listing building type. For sales, it is the Cook County Assessor property class.
The separate `tasks/audits/sales_record_quality_audit` task compares specifications
with and without these controls using the same distance-bin estimator.

Every sales specification reads the same property-cleaned, amenity-enriched
panel. The rooms rule is applied in `prep_sales_border_data`, before amenity
enrichment, not separately in each regression. Missing apartment counts are
allowed and no upper-tail price trimming is applied to the baseline. Annual
99.9th-percentile price-per-square-foot trimming remains a separate audit
sensitivity check, not a dependency of the paper figures.

The task produces the four paper figures. Years, windows, bin width, controls,
fixed effects and clustering are specified in its Makefile. The distance-bin
estimator is shared with the sales audit; input preparation remains visible in
each script. If a figure is missing, Make regenerates the paper figure set.
