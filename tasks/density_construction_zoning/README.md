# Construction-year zoning groups

This task contains the frozen broad zoning group used for each new-construction
project in the density analysis. The file covers all 9,603 production PINs
built from 2006 through 2022 that lie within 1,500 feet of a sampled ward
boundary. That distance covers the 500-foot main sample and the two placebo
windows centered 1,000 feet from the boundary.

Each row records the original whole-building PIN, its construction year, and
the broad zoning group in force on June 15 of that year. The file does not
claim to recover an exact historical zoning code, zoned FAR, or the internal
rules of a planned development.

The reconstruction starts from official Chicago zoning maps from October 2012,
July 2014, and November 2015, then applies passed zoning amendments using City
Council journals and ordinance records. Annual Cook County parcel polygons
link parcels whose PINs changed over time. The complete reconstruction,
validation against later official maps, reviewed cases, and source links are
preserved in `tasks/audits/historical_zoning_validation`.

The lookup is committed so the paper build does not depend on revisions to
live City or County services. Its SHA-256 checksum is
`50c84dbf114bc5f3467a269354bdf6052557d9eb35a1ca54fc51eae3d275215e`.
