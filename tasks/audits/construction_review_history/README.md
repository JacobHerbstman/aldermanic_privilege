# Construction review history

This directory preserves superseded construction reviews and their supporting
notes. It is an archive, with no Makefile or default build. The paper reads the
[current decision inputs](../../new_construction_cleaning/README.md).

[The earlier research record](history.md) contains the recovery history, source
references, recorded decisions, and explanations of later replacements. Files
under `records/` retain their exact original bytes. Production consumers use the approved instructions in the source task.
Release-audit consumers still read the original research tables where needed.

The archive records what was considered and why. Its older decisions do not
override the currently applied instructions.

`code/` preserves the four former commercial review-table scripts. They use the
old construction task's paths and are historical source, not a runnable task in
this directory. Their original dependency graph is available at Git commit
`1b5dae29`. General unit-count and parcel-coverage rules remain in production in
`tasks/construction_commercial_measurements/code/select_commercial_measurements.R`; the discarded calculations assembled
review displays and alternative recommendations that the final ledger did not
use. No automatically selected building was converted into a manual exception.

The replacement was checked against all 1,128 source projects. Both rule flags,
the resulting source decisions, and the 810 commercial-building records agree
with the preserved version. This comparison holds the recorded sources and
approved manual decisions fixed.

The September 11 source separation also preserves the original four measurement
instruction tables and both building-type review spreadsheets in `records/`.
Their normalized instructions are in `new_construction_cleaning/output/`.
All six original files retain their exact bytes; the new source README documents
the field mapping and production owners. The other historical tables moved here
remain available to their release-audit readers.

The five superseded commercial unit, land, and permit review displays are preserved in `records/`, with their original data reports in `report/`. They have no production consumers.

The other superseded inspection reports from the former construction folder are retained in `report/` as historical records. Their original production code and Make rules remain available in Git history.
