# Commercial construction review evidence

This audit reproduces the longer permit, building-footprint and residential-history
summaries used to investigate commercial Assessor records. Run `make` in `code/`.
The two datasets retain their original contents and filenames.

Production cleaning reads the committed decisions and calculates whether a
selected Assessor report follows an issued residential construction permit in
`select_commercial_evidence_rules.R`. It no longer builds these review summaries.
The shorter calculation preserves the original permit matches and classification
rule. Its completion flags agree for all 800 in-period source projects, and the
resulting decisions agree for all 1,128 commercial source records.

The current decision inputs are documented in
[new_construction_cleaning](../../new_construction_cleaning/README.md).
These audit outputs supply research context and do not override those decisions.
