# Permit Block Hand Adjudications

This task publishes the committed review decisions for permit coordinates that
do not fall inside a Chicago Census block polygon. The file records explicit
drops for 110 permits against the 2010 blocks and 19 permits against the 2020
blocks. All are at least 18.7 meters outside the corresponding block coverage,
so none is treated as a boundary-rounding case.

The Makefile intentionally only verifies that the frozen output is present.
