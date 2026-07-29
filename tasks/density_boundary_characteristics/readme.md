# Density Boundary Characteristics

This task stores the project-level inputs used for the density boundary checks. The
file contains the four location measures reported in the appendix, an indicator for
projects near locally straight boundary segments, and two boundary-feature
classifications.

The location measures are distances to downtown Chicago, the nearest school, the
nearest park, and Lake Michigan. The straight-segment indicator follows the geometric
test in Kulka, Sood, and Chiumenti (2022). The feature indicators use the overlap of
each assigned boundary segment with expressways, water, parks, cemeteries, and major
arterials.

The file was constructed and checked in
`tasks/audits/density_boundary_design_checks`. It is frozen here so the paper build
does not depend on exploratory geometry code or live spatial sources.

Its SHA-256 hash is
`6314345aa121c925d3ee24e7aad47a8e431f58ed09ebb59891bcf1a8192eb1d9`.
