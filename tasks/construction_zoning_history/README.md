# Preserved construction zoning history

The two files in `output/` are the unchanged historical zoning sources approved
for this replication. They were previously stored with construction adjudications.
The CSV contains 9,609 project-year records; the GeoPackage contains the preserved
2006 zoning polygons. These are source history, not 9,609 manual corrections.

Both files are versioned inputs. There is no recipe that reconstructs the original
ordinance research. Attribute reports are committed with the sources. Construction
cleaning reads the sources through explicit local input links and combines them
with the recorded zoning snapshots and approved construction-year decisions.

The [review history](../audits/construction_review_history/history.md) records the
provenance and limitations of the original reconstruction. Moving these files
does not resolve the separate limitation that the original ordinance history
cannot yet be rebuilt from raw sources.
