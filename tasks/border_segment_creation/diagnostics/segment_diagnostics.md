# Border Segment Diagnostics

## Phase 1
- Wards per era:
  - 1998_2002: rows=50, unique_wards=50, invalid_before=0, invalid_after=0, epsg=3435
  - 2003_2014: rows=50, unique_wards=50, invalid_before=0, invalid_after=0, epsg=3435
  - 2015_2023: rows=50, unique_wards=50, invalid_before=0, invalid_after=0, epsg=3435
  - post_2023: rows=50, unique_wards=50, invalid_before=0, invalid_after=0, epsg=3435

## Phase 2
- Boundary-pair stats by era:
  - 1998_2002: pairs=116 | len_ft min/med/mean/max=353.5/9702.5/12075.4/43173.5 | geom=LINESTRING
  - 2003_2014: pairs=102 | len_ft min/med/mean/max=665.4/11041.7/12831.6/44371.1 | geom=LINESTRING,MULTILINESTRING
  - 2015_2023: pairs=122 | len_ft min/med/mean/max=331.0/10556.7/13965.6/63249.4 | geom=LINESTRING,MULTILINESTRING
  - post_2023: pairs=119 | len_ft min/med/mean/max=666.4/11465.9/13634.7/37975.3 | geom=LINESTRING,MULTILINESTRING
- Outlier boundary rows (<100ft or >30000ft): 34

## Phase 3
- Segment validation by target length:
  - 1320 ft: n_pair_rows=459 | max_abs_len_diff_ft=0.000 | max_overlap_or_gap_ft=0.000
  - 2640 ft: n_pair_rows=459 | max_abs_len_diff_ft=0.000 | max_overlap_or_gap_ft=0.000

## Phase 4
- Segment type counts (1320ft reference):
  - 2015_2023:
    - arterial: 482 (37.3%)
    - collector: 379 (29.3%)
    - no_feature: 219 (16.9%)
    - mixed: 129 (10.0%)
    - park_water: 77 (6.0%)
    - residential: 7 (0.5%)
  - post_2023:
    - arterial: 431 (35.0%)
    - collector: 369 (30.0%)
    - no_feature: 195 (15.9%)
    - mixed: 146 (11.9%)
    - park_water: 78 (6.3%)
    - residential: 11 (0.9%)
  - 2003_2014:
    - arterial: 391 (39.3%)
    - collector: 287 (28.8%)
    - no_feature: 132 (13.3%)
    - mixed: 109 (11.0%)
    - park_water: 65 (6.5%)
    - residential: 11 (1.1%)
  - 1998_2002:
    - arterial: 377 (35.5%)
    - collector: 308 (29.0%)
    - no_feature: 173 (16.3%)
    - mixed: 119 (11.2%)
    - park_water: 68 (6.4%)
    - residential: 16 (1.5%)

## Phase 5
- Assignment smoke test rows: 3
- Non-missing nearest_segment_id: 3

## Gate Status
- [x] Phase 1 gate passed
- [x] Phase 2 gate passed
- [x] Phase 3 gate passed
- [x] Phase 4 gate passed
- [x] Phase 5 gate passed
