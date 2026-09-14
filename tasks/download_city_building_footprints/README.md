# City building footprints, August 2015

This task downloads the original City of Chicago building shapefile attached to
dataset `syp8-uezg`. Its archive members are dated August 10, 2015. The literal
attachment URL is preserved in the Makefile; the checksum pins the same bytes
as `data_raw/construction_review/chicago_building_footprints_2015.zip`.

Run `make` from `code/`. A failed download, checksum mismatch, or damaged ZIP
cannot replace the last valid output. An unchanged build reuses that output.
Changing the source vintage requires deliberately updating the recipe and hash
and reviewing dependent evidence. The historical attachment may require a
preserved copy if the publisher removes it; this is not a query for current
building records.

Construction cleaning reads the output through an input symlink. It selects
footprints relevant to project geometries and constructs the evidence fields.
The downloaded ZIP itself is unchanged. Its layer contains 820,606 source rows;
City IDs are not universally unique, and some shapes are empty or invalid.
Consumers must validate the records in their requested scope rather than
arbitrarily deleting duplicate IDs or interpreting missing footprints as proof
that construction did not occur. A 2015 footprint cannot establish the absence
of a building constructed later.
