# Add Property Characteristics to Sales

This task joins each sale to the Assessor's residential characteristics for the
same PIN and tax year. It constructs the housing controls used in the sales
regressions and writes `output/sales_with_hedonics.parquet`. The join does not
roll to a different tax year. The output is the frozen comparable sales panel:
it retains only single-building, non-prorated PINs with positive building
square footage. It retains same-year construction and records class mismatches
instead of silently dropping them. Model-specific requirements for logged age,
other controls, boundary distance, and alderman scores are imposed downstream.
