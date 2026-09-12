import pandas as pd

cohort = pd.read_csv("../reference/construction_reconciliation_cohort.csv", dtype=str)
questions = pd.read_csv("../output/current_construction_questions.csv", dtype=str).fillna("")
residential = pd.read_csv("../input/preferred_residential_project_candidates.csv", dtype=str).fillna("")
commercial = pd.read_csv("../input/preferred_commercial_project_ledger.csv", dtype=str).fillna("")
assert len(cohort) == 268 and cohort.project_id.is_unique
assert questions.project_id.is_unique and residential.project_id.is_unique and commercial.project_id.is_unique
questions = questions.set_index("project_id")
residential = residential.set_index("project_id")
commercial = commercial.set_index("project_id")
resolutions = pd.read_csv("../input/residential_review_source_dispositions.csv", dtype=str).fillna("").set_index("source_project_id")
selected = pd.read_csv("../input/preferred_residential_project_ledger.csv", dtype=str).fillna("").set_index("project_id")
assert resolutions.index.is_unique and selected.index.is_unique
condo_requests = pd.read_csv("../input/residential_successor_condo_requests.csv", dtype={"pin10": str})
condos = pd.read_csv("../input/construction_condominium_history.csv", dtype={"pin": str, "pin10": str})
assert not condos.duplicated(["pin", "year"]).any()
assert not condo_requests.duplicated(["project_id", "pin10"]).any()
condo_reviews = {}
for project_id, requests in condo_requests.groupby("project_id"):
    descriptions = []
    for request in requests.itertuples(index=False):
        history = condos[condos.pin10 == request.pin10]
        if history.empty:
            descriptions.append("No condominium source records returned for " + request.pin10)
            continue
        assessment = 2022 if (history.year == 2022).any() else 2025
        condo_cohort = history[history.year == assessment]
        homes = condo_cohort[(condo_cohort.is_parking_space == False) & (condo_cohort.is_common_area == False)]
        years = sorted(homes.char_yrblt.dropna().unique())
        floors = sorted(homes.char_building_sf.dropna().unique())
        lands = sorted(homes.char_land_sf.dropna().unique())
        complete = (condo_cohort.char_building_pins.eq(len(condo_cohort)).all()
            and condo_cohort.char_building_non_units.eq(len(condo_cohort) - len(homes)).all()
            and condo_cohort.is_parking_space.notna().all() and condo_cohort.is_common_area.notna().all())
        same_year = len(years) == 1 and years[0] == request.target_year
        measurements = len(floors) == 1 and floors[0] > 1 and len(lands) == 1 and lands[0] > 1
        if not complete:
            verdict = "The available unit records do not establish a complete building."
        elif len(years) == 1 and years[0] > 2022:
            verdict = "The completed condominium records report construction after the study period; check the earlier date before retaining."
        elif same_year and measurements and len(requests) == 1 and request.projects_per_condo_base == 1:
            verdict = "One complete condominium building reports the same construction year and consistent areas; verify its identity before replacing the old record."
        else:
            verdict = "The completed records have a year, area, or building-grouping difference that needs reconciliation."
        descriptions.append(f"Base {request.pin10}, assessment {assessment}: {len(homes)} homes; "
            f"construction years {years}; building sq ft {floors}; land sq ft {lands}. " + verdict)
    condo_reviews[project_id] = " | ".join(descriptions)
snapshots = pd.read_csv("../input/residential_tieback_temporal_snapshots.csv")
assert not snapshots.duplicated(["tieback_lineage_id", "tax_year"]).any()
last_snapshots = snapshots.sort_values("tax_year").groupby("tieback_lineage_id").tail(1).set_index("tieback_lineage_id")
frozen = pd.read_csv("../input/frozen_construction_analysis.csv").set_index("project_id")
assert frozen.index.is_unique
rows = []
for case in cohort.itertuples(index=False):
    targets = [case.project_id]
    if case.project_id in residential.index:
        record = residential.loc[case.project_id]
        replacement = record.replacement_project_ids
        if replacement and replacement != "NA":
            targets = replacement.split("/")
        state = record.candidate_status
        reason = record.decision_reason
    else:
        record = commercial.loc[case.project_id]
        state = "retained" if record.location_resolved.lower() == "true" else "location_missing"
        reason = record.decision_reason
    if case.project_id in resolutions.index:
        decision = resolutions.loc[case.project_id]
        if decision.final_disposition != "review_required":
            targets = [p for p in decision.final_project_ids.split("/") if p]
            state = decision.final_disposition
            reason = decision.decision_reason
    remaining = [p for p in targets if p in questions.index]
    if case.project_id in questions.index and case.project_id not in remaining:
        remaining.append(case.project_id)
    unknown_targets = [p for p in targets if p not in residential.index and p not in commercial.index and p not in selected.index]
    pending = " | ".join(questions.loc[p, "question"] for p in remaining)
    if unknown_targets:
        pending += " Replacement absent from current selected projects: " + "/".join(unknown_targets)
    assessor_review = ""
    if case.project_id in last_snapshots.index:
        a = last_snapshots.loc[case.project_id]
        assessor_review = (f"Latest linked assessment: {int(a.tax_year)}; {int(a.member_pin_count)} parcels; "
            f"{int(a.cards)} cards; {int(a.construction_year_values)} construction-year values; "
            f"{int(a.building_area_values)} floor-area values; recorded parcel fractions sum to {a.pin_proration_sum:g}. "
            f"Assessment check: {a.snapshot_review_reason}.")
    rows.append(dict(project_id=case.project_id, initial_issue=case.initial_issue,
        in_frozen_paper_data=case.project_id in frozen.index,
        in_frozen_500ft_sample=case.project_id in frozen.index and bool(frozen.loc[case.project_id, "within_500ft"]),
        assessor_record_review=assessor_review,
        frozen_replacement_ids="/".join(p for p in targets if p != case.project_id and p in frozen.index),
        frozen_contains_source_and_all_replacements=(case.project_id in frozen.index and
            bool(targets) and case.project_id not in targets and all(p in frozen.index for p in targets) and not remaining and not unknown_targets),
        current_status=state, selected_project_ids="/".join(targets),
        reconciliation_status="still_open" if remaining or unknown_targets else "closed",
        applied_reason=reason, remaining_question=pending,
        condominium_source_review=condo_reviews.get(case.project_id, "")))
result = pd.DataFrame(rows)
assert len(result) == 268 and result.project_id.is_unique
result.to_csv("../output/construction_reconciliation_progress.csv", index=False)
