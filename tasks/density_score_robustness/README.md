# Density score checks

This task checks two concerns about how aldermanic stringency classifies the two sides of a ward boundary.

First, it recalculates the two alderman scores for each construction project after removing permit records matched to that project. The permit-level adjustment is estimated once, and the alderman scores are then recalculated without the project's own permit outcomes. This prevents a project's processing time from directly affecting the score used to classify its boundary.

Second, it excludes boundaries where the two alderman scores differ by less than 0.25 or 0.50 standard deviations. These restrictions test whether close rankings drive the main result.

The committed file `adjudication/project_permit_matches.csv` records the project-to-permit matches established while constructing the final new-construction sample. Keeping this two-column file here makes the check reproducible without relying on the separate audit branch. Its SHA-256 checksum is `7de81866eb4fd1f017117e8f8ce33cb4a6e5a152277a1c631565514254d41975`.

Running `make` from `code/` creates `output/density_score_robustness.tex`, which appears in Appendix D.
