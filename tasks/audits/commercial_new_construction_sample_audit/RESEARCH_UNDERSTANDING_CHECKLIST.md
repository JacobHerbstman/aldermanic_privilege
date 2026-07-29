# Research understanding checklist

## Session goal

- [x] Define the intended observation as a physical residential construction project.
- [x] Separate mechanical cleaning from substantive adjudication.
- [x] Freeze decisions before re-estimating the density models.

## Data provenance

- [x] Verify hashes for every raw and supporting source.
- [x] Commercial records are mutable entity-level reports beginning in 2021.
- [x] Residential cards represent improvements; tieback PINs can represent pieces of one building.
- [x] Historical parcel polygons are available annually from Cook County.

## Cleaning and construction logic

- [x] Write the residential tieback, multicard, and class 2-97 rules.
- [x] Write the commercial cross-vintage entity rules.
- [x] Generate project candidates without treatment or outcome fields.
- [ ] Complete ambiguous-case adjudication.

## Joins and geography

- [ ] Validate every join cardinality.
- [x] Fetch construction-year polygons for all component PINs.
- [ ] Resolve or exclude projects with ambiguous boundary assignment.

## Final validation

- [ ] Confirm unique project IDs and component membership.
- [ ] Reconcile the complete citywide, 1,500-foot, and 500-foot funnels.
- [ ] Confirm field-level provenance for every retained project.
- [ ] Freeze the preferred project file before estimating any model.

## Open decisions

- [ ] Resolve commercial construction-year conflicts using permit and completion evidence.
- [ ] Resolve class 2-97 project groupings rather than excluding the class wholesale.
- [ ] Resolve multicard land allocation where card proration is incomplete.
- [ ] Decide the small set of commercial cases whose source vintages change entity boundaries.
