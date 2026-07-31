# Add Property Characteristics to Sales

This task joins each sale to the Assessor's residential characteristics for the
same PIN and tax year. It constructs the housing controls used in the sales
regressions and writes `output/sales_with_hedonics.parquet`. The join does not
roll to a different tax year.
