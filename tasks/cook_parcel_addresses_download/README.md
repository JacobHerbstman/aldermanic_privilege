# Chicago parcel addresses

The ordinary build restores the recorded 2025 Cook County Property Locations
extract (`3723-97qp`) from the replication source archive, verifying its checksum.
The snapshot was preserved from this task's existing output; July 10, 2026 is
its local modification date, not an independently established retrieval date.
It contains the Chicago rows and the original source columns.

For a deliberate refresh, run `make download-current` in `code/`.
That downloads a separately named current file. Compare row counts and contents
before adopting a new snapshot; ordinary builds never replace the recorded vintage.
