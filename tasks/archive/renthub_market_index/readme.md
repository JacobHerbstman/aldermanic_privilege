This task builds a canonical internal RentHub market-rent task from the raw RentHub parquet shards.

The task produces two citywide series:
- a smooth repeat-rent style index
- a news-style asking-rent series based on first observations in 90-day listing cycles

It also produces neighborhood asking-rent growth outputs for wards and community areas, with conditional shrinkage applied only when support is thin.

The main benchmark inputs are the Zillow city and metro ZORI series plus the Chicago rent CPI already used in the RentHub audit task. ACS is used only as a secondary long-run plausibility check on cumulative growth.

Optional news-style benchmark input:
- place a CSV symlink at `input/news_benchmarks.csv`
- expected columns:
  - `month_start`
  - `benchmark_id`
  - `benchmark_label`
  - `benchmark_family`
  - `benchmark_value`
  - `benchmark_yoy_pct`

If that optional file is absent, the task still runs and benchmarks only against the existing Zillow/FRED series.
