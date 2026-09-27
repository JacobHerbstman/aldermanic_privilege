# Hand-Reviewed Permit Block Assignments

This task stores the hand-reviewed decisions for permit coordinates that do not
fall inside a Chicago Census block polygon. The spreadsheet records explicit
drops for 158 permits against the 2010 blocks.

The first 110, for applications in 2010--2020, are all at least 18.7 meters
outside the block coverage. The other 48, for applications in 2006--2009 and
2021--2022 (added September 2026 when block counts were extended to every year
of the permit data), are 10.8 to 63.3 meters outside it; 7 are high-discretion
permits. None is a boundary-rounding case: the nearest are about half a street
width from a block, on the public way between blocks, and several share one
coordinate. Assigning them to a block would mean guessing which side of the
street they belong to, so they are dropped.

The Makefile verifies that the committed spreadsheet is present.
