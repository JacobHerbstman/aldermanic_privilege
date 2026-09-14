# Recorded historical parcels

Run `make` in `code/` to combine the preserved Cook County parcel polygons and
parcel-coordinate histories used to locate new construction. One R script reads
the recorded requests and responses, checks their identifiers and geometry, and
combines their coverage. It preserves the existing source priority: the preferred
extract takes precedence where it already covers an initial year/parcel request.
Reviewed predecessor duplicates must identify the same parcel and geometry.

The seven outputs are inputs to `prepare_new_construction`. Original queries,
including requests with no returned polygon, remain in the source files. The
Makefile verifies the recorded checksums before combining them. Standard data
reports are written alongside the outputs.

Sources are in `data_raw/construction_review/` and the committed `sources/`
folder. Ordinary builds use these recorded files. Acquisition code and detailed
research history are preserved under
[`tasks/audits/construction_review_history/source_acquisition/`](../audits/construction_review_history/source_acquisition/).
A deliberate source refresh requires comparing and recording new dated inputs
and their checksums; there is no refresh command in this production task.
