# Recorded CTA history

These two CSVs preserve the eight station records formerly written inside
`prepare_amenities_data.R`. `cta_opening_dates.csv` supplies dates for five
stations in the current CTA source; `cta_historical_stations.csv` supplies
three closed stations absent from that source. Each row retains the original
source description. Coordinates, dates, and the existing treatment of all
other stations are unchanged. The producing script reads these inputs before
calculating the period for which each station is available.
