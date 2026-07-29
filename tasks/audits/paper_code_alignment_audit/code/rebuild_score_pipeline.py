from pathlib import Path
import os
import shutil
import subprocess

import numpy as np
import pandas as pd


# setwd("tasks/audits/paper_code_alignment_audit/code")

repo = Path("../../../..").resolve()
audit_task = repo / "tasks/audits/paper_code_alignment_audit"
sandbox = audit_task / "temp/score_rebuild"
sandbox_tasks = sandbox / "tasks"

if sandbox.exists():
    shutil.rmtree(sandbox)
sandbox_tasks.mkdir(parents=True)

os.symlink(repo / "tasks/setup_environment", sandbox_tasks / "setup_environment")
os.symlink(repo / "tasks/_lib", sandbox_tasks / "_lib")

data_task = sandbox_tasks / "data_for_alderman_uncertainty_index"
(data_task / "code").mkdir(parents=True)
(data_task / "input").mkdir()
(data_task / "output").mkdir()

for source in sorted((repo / "tasks/data_for_alderman_uncertainty_index/input").iterdir()):
    os.symlink(source.resolve(), data_task / "input" / source.name)

subprocess.run(
    [
        "Rscript",
        str(repo / "tasks/data_for_alderman_uncertainty_index/code/prepare_uncertainty_data.R"),
    ],
    cwd=data_task / "code",
    check=True,
)

fresh_permits_path = data_task / "output/permits_for_uncertainty_index.csv"
old_permits_path = repo / "tasks/data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv"

score_task = sandbox_tasks / "create_alderman_uncertainty_index"
(score_task / "code").mkdir(parents=True)
(score_task / "input").mkdir()
(score_task / "output").mkdir()
os.symlink(fresh_permits_path, score_task / "input/permits_for_uncertainty_index.csv")

score_script = repo / "tasks/create_alderman_uncertainty_index/code/create_uncertainty_index.R"
common_args = ["TRUE", "TRUE", "TRUE", "FALSE", "TRUE", "N_PERMITS", "LAG1", "BOTH"]

subprocess.run(
    ["Rscript", str(score_script), *common_args, "2022", "FALSE"],
    cwd=score_task / "code",
    check=True,
)
subprocess.run(
    ["Rscript", str(score_script), *common_args, "2014", "FALSE"],
    cwd=score_task / "code",
    check=True,
)

score_suffixes = {
    "through2022": "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022",
    "through2014": "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2014",
}

comparison_frames = []
summary_rows = []
for vintage, suffix in score_suffixes.items():
    old_path = repo / f"tasks/create_alderman_uncertainty_index/output/alderman_uncertainty_index_{suffix}.csv"
    fresh_path = score_task / f"output/alderman_uncertainty_index_{suffix}.csv"
    shutil.copy2(fresh_path, audit_task / f"output/rebuilt_alderman_uncertainty_index_{vintage}.csv")

    old_score = pd.read_csv(old_path).rename(
        columns={
            "uncertainty_index": "old_uncertainty_index",
            "mean_resid": "old_mean_resid",
            "n_permits": "old_n_permits",
        }
    )
    fresh_score = pd.read_csv(fresh_path).rename(
        columns={
            "uncertainty_index": "fresh_uncertainty_index",
            "mean_resid": "fresh_mean_resid",
            "n_permits": "fresh_n_permits",
        }
    )
    merged = old_score[
        ["alderman", "old_uncertainty_index", "old_mean_resid", "old_n_permits"]
    ].merge(
        fresh_score[
            ["alderman", "fresh_uncertainty_index", "fresh_mean_resid", "fresh_n_permits"]
        ],
        on="alderman",
        how="outer",
        indicator=True,
        validate="one_to_one",
    )
    merged["vintage"] = vintage
    merged["score_difference"] = (
        merged["fresh_uncertainty_index"] - merged["old_uncertainty_index"]
    )
    merged["old_rank"] = merged["old_uncertainty_index"].rank(method="average")
    merged["fresh_rank"] = merged["fresh_uncertainty_index"].rank(method="average")
    merged["rank_difference"] = merged["fresh_rank"] - merged["old_rank"]
    comparison_frames.append(merged)

    common = merged[merged["_merge"] == "both"].copy()
    summary_rows.append(
        {
            "component": "alderman_score",
            "vintage": vintage,
            "old_rows": len(old_score),
            "fresh_rows": len(fresh_score),
            "common_rows": len(common),
            "old_only_rows": int((merged["_merge"] == "left_only").sum()),
            "fresh_only_rows": int((merged["_merge"] == "right_only").sum()),
            "pearson_correlation": common[
                ["old_uncertainty_index", "fresh_uncertainty_index"]
            ].corr(method="pearson").iloc[0, 1],
            "spearman_correlation": common[
                ["old_uncertainty_index", "fresh_uncertainty_index"]
            ].corr(method="spearman").iloc[0, 1],
            "mean_absolute_difference": common["score_difference"].abs().mean(),
            "max_absolute_difference": common["score_difference"].abs().max(),
            "changed_value_rows": int((common["score_difference"].abs() > 1e-12).sum()),
        }
    )

score_comparison = pd.concat(comparison_frames, ignore_index=True)
score_comparison.to_csv(audit_task / "output/score_rebuild_comparison.csv", index=False)

input_columns = [
    "id",
    "ward",
    "alderman",
    "month",
    "year",
    "processing_time",
    "map_version",
    "pop_total",
    "median_hh_income",
    "share_black",
    "share_hisp",
    "share_white",
    "homeownership_rate",
    "dist_cbd_km",
    "dist_lake_km",
    "n_rail_stations_800m",
]
old_permits = pd.read_csv(old_permits_path, usecols=input_columns, low_memory=False)
fresh_permits = pd.read_csv(fresh_permits_path, usecols=input_columns, low_memory=False)

old_permits = old_permits.rename(columns={column: f"old_{column}" for column in input_columns if column != "id"})
fresh_permits = fresh_permits.rename(columns={column: f"fresh_{column}" for column in input_columns if column != "id"})
permit_comparison = old_permits.merge(
    fresh_permits,
    on="id",
    how="outer",
    indicator=True,
    validate="one_to_one",
)

common_permits = permit_comparison[permit_comparison["_merge"] == "both"].copy()
changed_assignment = (
    common_permits["old_ward"].fillna(-1) != common_permits["fresh_ward"].fillna(-1)
) | (
    common_permits["old_alderman"].fillna("") != common_permits["fresh_alderman"].fillna("")
) | (
    common_permits["old_map_version"].fillna(-1)
    != common_permits["fresh_map_version"].fillna(-1)
)

changed_rows = common_permits.loc[
    changed_assignment,
    [
        "id",
        "old_month",
        "old_ward",
        "fresh_ward",
        "old_alderman",
        "fresh_alderman",
        "old_map_version",
        "fresh_map_version",
        "old_processing_time",
    ],
].sort_values(["old_month", "id"])

if changed_rows.empty:
    changed_rows = pd.DataFrame(
        columns=[
            "id",
            "old_month",
            "old_ward",
            "fresh_ward",
            "old_alderman",
            "fresh_alderman",
            "old_map_version",
            "fresh_map_version",
            "old_processing_time",
        ]
    )
changed_rows.to_csv(audit_task / "output/score_input_changed_assignments.csv", index=False)

numeric_change_counts = []
for column in [
    "pop_total",
    "median_hh_income",
    "share_black",
    "share_hisp",
    "share_white",
    "homeownership_rate",
    "dist_cbd_km",
    "dist_lake_km",
    "n_rail_stations_800m",
]:
    old_values = pd.to_numeric(common_permits[f"old_{column}"], errors="coerce")
    fresh_values = pd.to_numeric(common_permits[f"fresh_{column}"], errors="coerce")
    different = ~np.isclose(old_values, fresh_values, rtol=1e-12, atol=1e-12, equal_nan=True)
    numeric_change_counts.append(int(different.sum()))

summary_rows.append(
    {
        "component": "permit_score_input",
        "vintage": "all",
        "old_rows": len(old_permits),
        "fresh_rows": len(fresh_permits),
        "common_rows": len(common_permits),
        "old_only_rows": int((permit_comparison["_merge"] == "left_only").sum()),
        "fresh_only_rows": int((permit_comparison["_merge"] == "right_only").sum()),
        "pearson_correlation": np.nan,
        "spearman_correlation": np.nan,
        "mean_absolute_difference": np.nan,
        "max_absolute_difference": np.nan,
        "changed_value_rows": int(changed_assignment.sum()),
    }
)
summary_rows.append(
    {
        "component": "permit_score_input_numeric_controls",
        "vintage": "all",
        "old_rows": len(common_permits),
        "fresh_rows": len(common_permits),
        "common_rows": len(common_permits),
        "old_only_rows": 0,
        "fresh_only_rows": 0,
        "pearson_correlation": np.nan,
        "spearman_correlation": np.nan,
        "mean_absolute_difference": np.nan,
        "max_absolute_difference": np.nan,
        "changed_value_rows": max(numeric_change_counts),
    }
)

pd.DataFrame(summary_rows).to_csv(audit_task / "output/score_rebuild_summary.csv", index=False)
