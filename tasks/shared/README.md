# Shared research code

This directory owns the execution and recursive Make rules and the canonical
Chicago geometry routines used by the restored construction pipeline and
existing production callers. It does not produce a dataset. The remaining
score and amenity helpers have not been migrated in this construction change.

`report.py` adapts the DuckDB summary approach in Jacob's project-template
report helper. It reports saved CSV bytes, exact row/non-missing counts and
declared keys, approximate distinct counts, and inferred numeric summaries.
It does not rewrite or reorder data. CSV types are explicitly labeled as
inferred. Its Python dependencies are DuckDB and pandas.
