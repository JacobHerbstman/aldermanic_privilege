# Sale Locations and Ward Boundaries

This task cleans 2006--2022 residential sales, assigns coordinates from the
sale-year parcel record when available, and uses the current parcel location
only as a fallback. It assigns the ward and alderman in office on the sale date,
measures distance to the nearest boundary of that ward, and expresses
uncensored prices in 2022 dollars.

Cook County's historical ingest sometimes records an unrefined transaction on
the first of its execution month. When either adjoining ward changes aldermen
within that month, the date does not identify who was serving. This task drops
those observations rather than assigning them to the first day. It applies the
same rule when the ward map can change within the month. Unrefined dates shown
on another day are retained.

It writes:
- `output/sales_pre_scores.csv`
