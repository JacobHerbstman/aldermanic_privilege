# Refilings of zoning map amendments

`link_refilings.R` links each zoning map amendment that stalled, was withdrawn or failed
(`tasks/clean_zoning_map_amendments`) to the later amendment that filed it again, in `output/zoning_refilings.csv`
(one row per refiled amendment). Run `make` in `code/`.

A later amendment refiles an earlier one when it is filed by the same kind of filer (an applicant or an alderman),
is introduced within four years (one council term), and carries the same application number or shares a street
segment of the title address: the same street and direction with overlapping house numbers on the same side of the
street (odd or even numbers, unless a range spans both). The first such amendment is the refiling.

Every refiling chosen has been read by hand in `adjudication/refiling_reviews.csv` (one row per pair of matter IDs,
with the record numbers, a decision and a reason), and the build stops if one has not. A pair judged to be
different projects is not a refiling and the next candidate is considered: 18 pairs were rejected, mostly a single
site against a corridor or assemblage rezoning around it, or opposite changes on the same parcel. Reviewed
September 28, 2026, from the titles, districts and outcomes of both amendments.

94 amendments were refiled (92 stalls and 2 withdrawals), a median of 553 days after the first filing, and 85 of
the refilings passed. Consumer: `tasks/estimate_alderman_zoning_measures`.
