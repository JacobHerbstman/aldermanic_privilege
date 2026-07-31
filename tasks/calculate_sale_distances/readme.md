# Sale Locations and Ward Boundaries

This task cleans 2006--2022 residential sales, assigns coordinates from the
sale-year parcel record when available, and uses the current parcel location
only as a fallback. It assigns the ward and alderman in office on the sale date,
measures distance to the nearest boundary of that ward, and expresses
winsorized prices in 2022 dollars.

It writes:
- `output/sales_pre_scores.csv`
