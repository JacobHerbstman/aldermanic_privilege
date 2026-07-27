# New-construction analysis data

`output/new_construction_analysis_data.csv` is the frozen project-level input
for the density analysis. It contains 8,648 projects built from 2006 through
2022 and located within 1,500ft of a ward boundary. Of these, 3,710 are within
the 500ft main bandwidth.

The source combines Assessor residential and commercial new-construction
records, completed new-building permit chains, historical parcel coordinates,
construction-year zoning, and project-level duplicate and multicard
adjudication. Every project within the main bandwidth that was retained
principally from the Assessor's new-construction designation received a final
row-level review.

The complete derivation, review evidence, and validation checks are in:

- `tasks/audits/commercial_new_construction_sample_audit/`
- `tasks/audits/new_construction_universe_validation/`
- `tasks/audits/new_construction_project_verification/`

The production file intentionally omits audit URLs, review notes, intermediate
matches, and diagnostics. Its SHA-256 hash is
`64cdb00adb87fcea6c49646983ba4346125f855eb2c49658508701202677fbff`.
