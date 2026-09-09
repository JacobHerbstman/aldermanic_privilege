"""Compare existing map denominators with recorded Assessor measurements."""
import re
import pandas as pd

selected = pd.read_csv("../input/preferred_commercial_projects.csv", dtype={"project_id": str, "component_pins": str})
candidates = pd.read_csv("../input/preferred_commercial_project_candidates.csv", dtype={"project_id": str, "component_pins": str})
versions = pd.read_csv("../input/commercial_entity_version_candidates.csv", dtype={"project_family_id": str, "raw_row": str, "pins": str})
assert selected.project_id.is_unique and candidates.project_id.is_unique and versions.raw_row.is_unique
review = selected.loc[selected.land_source.isin([
    "construction_year_parcel_union", "construction_year_parcel_polygon",
    "construction_year_union_of_2021_components"
])].merge(candidates[["project_id", "land_sqft", "building_sqft", "dwelling_units", "component_pins", "land_source"]],
    on="project_id", suffixes=("", "_candidate"), validate="one_to_one")
review["map_minus_assessor_sqft"] = review.land_sqft - review.land_sqft_candidate
review["map_minus_assessor_percent"] = (100 * (review.land_sqft / review.land_sqft_candidate - 1)).where(review.land_sqft_candidate > 1)
review["same_candidate_components"] = review.component_pins == review.component_pins_candidate
review["assessor_land_available"] = review.land_sqft_candidate > 1
# Earlier rows can partition a later rollup. Add their reported areas only when
# every component is covered exactly once; never add repeated site measurements.
for i, project in review.iterrows():
    rows = versions.loc[(versions.project_family_id == project.project_id) & (versions.valuation_year == 2021)]
    components = [re.sub(r"[^0-9]", "", p) for value in rows.pins.dropna() for p in value.split(",")]
    target = project.component_pins.split("/")
    complete = bool(len(rows)) and len(components) == len(set(components)) and set(components) == set(target)
    review.loc[i, "earlier_source_rows"] = "/".join(rows.raw_row)
    review.loc[i, "earlier_exact_disjoint_components"] = complete
    if complete and rows.landsf.notna().all() and (rows.landsf > 1).all():
        review.loc[i, "earlier_reported_land_sqft"] = rows.landsf.sum()
    if complete and rows.bldgsf.notna().all() and (rows.bldgsf > 1).all():
        review.loc[i, "earlier_reported_building_sqft"] = rows.bldgsf.sum()
    if complete and rows.reported_units.notna().all() and (rows.reported_units > 0).all():
        review.loc[i, "earlier_reported_units"] = rows.reported_units.sum()
review = review.sort_values("project_id")
review.to_csv("../output/commercial_reported_land_comparison.csv", index=False)
