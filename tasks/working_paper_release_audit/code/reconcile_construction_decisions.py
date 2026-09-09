"""One current list of unfinished construction records; no production decisions."""
import pandas as pd

residential = pd.read_csv("../input/preferred_residential_project_candidates.csv", dtype={"project_id": str, "component_pins": str})
commercial = pd.read_csv("../input/preferred_commercial_projects.csv", dtype={"project_id": str, "component_pins": str})
locations = pd.read_csv("../input/preferred_commercial_project_ledger.csv", dtype={"project_id": str})
geography = pd.read_csv("../input/preferred_project_boundary_scope.csv", dtype={"project_id": str})
questions = pd.read_csv("../reference/remaining_building_decision_questions.csv", dtype=str).set_index("project_id")
addresses = pd.read_csv("../input/parcel_addresses_2025_chicago.csv", dtype=str, usecols=["pin", "prop_address_full"])
assert questions.index.is_unique
assert addresses.pin.is_unique
address_by_pin = addresses.set_index("pin").prop_address_full.to_dict()
for frame in [residential, commercial, locations, geography]:
    assert frame.project_id.is_unique
assert set(questions.index) <= set(residential.project_id) | set(commercial.project_id)

# These are existing decisions, not permission to apply every old measurement.
# Their presence distinguishes unfinished implementation from missing evidence.
recorded = []
for filename, table, id_column, action_column, reason_column in [
    ("residential_tieback_no_snapshot_decisions.csv", pd.read_csv("../input/residential_tieback_no_snapshot_decisions.csv", dtype=str).fillna(""), "source_project_id", "decision_action", "decision_reason"),
    ("residential_class297_exceptions.csv", pd.read_csv("../input/residential_class297_exceptions.csv", dtype=str).fillna(""), "source_project_id", "override_action", "decision_reason"),
    ("residential_overlap_decisions.csv", pd.read_csv("../input/residential_overlap_decisions.csv", dtype=str).fillna(""), "source_project_id", "overlap_action", "decision_reason"),
    ("residential_remaining_case_decisions.csv", pd.read_csv("../input/residential_remaining_case_decisions.csv", dtype=str).fillna(""), "source_project_id", "decision_action", "decision_reason"),
    ("residential_unresolved_source_dispositions.csv", pd.read_csv("../input/residential_unresolved_source_dispositions.csv", dtype=str).fillna(""), "source_project_id", "disposition", "decision_reason"),
    ("eligibility_manual_exceptions.csv", pd.read_csv("../input/eligibility_manual_exceptions.csv", dtype=str).fillna(""), "project_id", "manual_action", "reason"),
    ("residential_successor_condo_overrides.csv", pd.read_csv("../input/residential_successor_condo_overrides.csv", dtype=str).fillna(""), "project_id", "reason", "evidence"),
    ("project_manual_reviews.csv", pd.read_csv("../input/project_manual_reviews.csv", dtype=str).fillna(""), "project_id", "review_status", "notes"),
]:
    assert table[id_column].is_unique
    for _, decision in table.iterrows():
        recorded.append({"project_id": decision[id_column], "recorded_decision": filename + ": " + decision[action_column] + "; " + decision[reason_column]})
recorded = pd.DataFrame(recorded).groupby("project_id", as_index=False).agg(recorded_decision=("recorded_decision", " | ".join))
residential = residential.merge(recorded, on="project_id", how="left", validate="one_to_one")
residential = residential.merge(geography[["project_id", "location_source", "distance_to_boundary_ft"]], on="project_id", how="left", validate="one_to_one")
commercial = commercial.merge(locations[["project_id", "location_resolved"]], on="project_id", how="left", validate="one_to_one")
assert commercial.location_resolved.notna().all()

reason_text = {
    "tieback_contains_multicard_pin": "Combined property contains multiple Assessor cards; establish the distinct buildings and their measurements.",
    "no_complete_contemporaneous_lineage_snapshot": "No assessment contains complete measurements for all components at the same time.",
    "missing_or_nonpositive_building_area": "Building floor area is missing. Check whether supported units and land allow apartments-per-acre alone.",
    "tieback_contains_class_297": "Combined property includes condo-development records; match them to the finished building without counting it twice.",
    "class_297_units_unresolved": "The condo-development record does not establish the apartment count.",
    "class_297_pins_share_permit_chain": "Several property records share a permit; establish how many buildings they describe.",
    "conflicting_permit_unit_mentions": "The linked permits give different apartment counts."
}
open_records = []
for _, row in residential.iterrows():
    if row.candidate_status.startswith("exclude_"):
        continue
    issues, explanation = [], []
    decision = row.recorded_decision if pd.notna(row.recorded_decision) else ""
    if row.project_id in questions.index:
        issues.append("building_decision_conflict")
        explanation.append(questions.loc[row.project_id, "question"])
    elif row.candidate_status == "review_required":
        issues.append("recorded_decision_needs_application" if decision else "building_evidence_needed")
        explanation.append(reason_text.get(row.decision_reason, row.decision_reason))
    elif row.candidate_status == "defer_to_commercial_reconciliation":
        issues.append("residential_commercial_overlap")
        explanation.append("Decide which source supplies this property and suppress the other record; do not count both.")
    if row.candidate_status == "retain_mechanical":
        if row.location_source == "former_parcel_centroid_unresolved_individual":
            issues.append("individual_location_rule_needed")
            explanation.append("Distance uses the center of an older larger parcel, not an accepted individual home location.")
        elif pd.isna(row.distance_to_boundary_ft):
            issues.append("location_missing")
            explanation.append("No construction-year boundary distance is available.")
    if not issues:
        continue
    address = " / ".join(sorted({address_by_pin[p] for p in row.component_pins.split("/") if p in address_by_pin and pd.notna(address_by_pin[p])}))
    open_records.append(dict(project_id=row.project_id, source_family="residential", address=address,
        construction_year=row.construction_year, issue=" / ".join(issues), question=" ".join(explanation),
        recorded_decision=decision, component_pins=row.component_pins, dwelling_units=row.dwelling_units,
        building_sqft=row.building_sqft, land_sqft=row.land_sqft, distance_to_boundary_ft=row.distance_to_boundary_ft))
for _, row in commercial.iterrows():
    issues, explanation = [], []
    if row.project_id in questions.index:
        issues.append("building_decision_conflict")
        explanation.append(questions.loc[row.project_id, "question"])
    if (row.allow_far or row.allow_dupac) and not row.location_resolved:
        issues.append("location_missing")
        explanation.append("Selected building measurements have not been linked to a supported final location. Preserve settled measurements while resolving location.")
    if not issues:
        continue
    open_records.append(dict(project_id=row.project_id, source_family="commercial", address=row.selected_source_addresses,
        construction_year=row.construction_year, issue=" / ".join(issues), question=" ".join(explanation),
        recorded_decision=row.decision_reason, component_pins=row.component_pins, dwelling_units=row.dwelling_units,
        building_sqft=row.building_sqft, land_sqft=row.land_sqft, distance_to_boundary_ft=float("nan")))
review = pd.DataFrame(open_records).sort_values(["issue", "source_family", "project_id"])
assert review.project_id.is_unique
assert set(residential.loc[residential.candidate_status == "review_required", "project_id"]) <= set(review.project_id)
assert set(commercial.loc[(commercial.allow_far | commercial.allow_dupac) & ~commercial.location_resolved, "project_id"]) <= set(review.project_id)
review.to_csv("../output/current_construction_questions.csv", index=False)
