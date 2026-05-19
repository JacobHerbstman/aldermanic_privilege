# Sales RD bandwidth path

This diagnostic task estimates flat sales-price discontinuities across ward borders using the corrected parcel-to-border geometry and segment assignments. It is not in the paper path yet.

The main specification uses 2006-2022 home sales within each bandwidth, `log(sale_price)` as the outcome, a stricter-side indicator as the treatment, segment-by-quarter fixed effects, and clustering by segment. The main bandwidth plot reports no controls, hedonic controls, and hedonic plus amenity controls. The companion plot compares fixed-effect choices using the hedonic plus amenity specification because sales are much thinner than RentHub listings.

The task reads the amenity-enriched hedonic sales panel from `sales_border_pair_fe`, so it inherits the audited sales distance assignment, residential-improvement controls, and amenity-distance construction.
