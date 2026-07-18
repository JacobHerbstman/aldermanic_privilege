from pathlib import Path
import os
import shutil
import subprocess

import pandas as pd
import pyarrow.parquet as pq


# setwd("tasks/audits/paper_code_alignment_audit/code")

repo = Path("../../../..").resolve()
audit_task = repo / "tasks/audits/paper_code_alignment_audit"
sandbox = audit_task / "temp/permit_event_rebuild"
sandbox_tasks = sandbox / "tasks"

if sandbox.exists():
    shutil.rmtree(sandbox)
sandbox_tasks.mkdir(parents=True)

os.symlink(repo / "tasks/setup_environment", sandbox_tasks / "setup_environment")
os.symlink(repo / "tasks/_lib", sandbox_tasks / "_lib")


def make_task(name):
    task = sandbox_tasks / name
    (task / "code").mkdir(parents=True)
    (task / "input").mkdir()
    (task / "output").mkdir()
    return task


def link(source, destination):
    os.symlink(Path(source).resolve(), destination)


treatment_task = make_task("create_block_treatment_panel")
for name in [
    "census_blocks_2010.csv",
    "census_blocks_2020.csv",
    "ward_panel.gpkg",
    "chicago_alderman_panel.csv",
]:
    link(repo / "tasks/create_block_treatment_panel/input" / name, treatment_task / "input" / name)

subprocess.run(
    ["Rscript", str(repo / "tasks/create_block_treatment_panel/code/create_treatment_panel.R")],
    cwd=treatment_task / "code",
    check=True,
)

merge_task = make_task("merge_event_study_scores")
link(repo / "tasks/merge_event_study_scores/input/sales_pre_scores.csv", merge_task / "input/sales_pre_scores.csv")
link(treatment_task / "output/block_treatment_pre_scores.csv", merge_task / "input/block_treatment_pre_scores.csv")
for name in [
    "chicago_alderman_panel.csv",
    "aldermen_uncertainty_scores_through2022.csv",
    "aldermen_uncertainty_scores_through202604.csv",
]:
    link(repo / "tasks/merge_event_study_scores/input" / name, merge_task / "input" / name)

subprocess.run(
    ["Rscript", str(repo / "tasks/merge_event_study_scores/code/merge_sales_treatment_scores.R")],
    cwd=merge_task / "code",
    check=True,
)

current_treatment = pd.read_csv(merge_task / "output/block_treatment_panel_through202604.csv")
all_turnover_treatment = current_treatment.copy()
all_turnover_treatment["valid"] = all_turnover_treatment["has_complete_ward_assignment"]
if all_turnover_treatment["valid"].isna().any():
    raise RuntimeError("Complete-assignment lookup is missing treatment rows.")
all_turnover_path = sandbox / "block_treatment_panel_all_turnover.csv"
all_turnover_treatment.to_csv(all_turnover_path, index=False)


def run_event_panel(label, treatment_path):
    task = make_task(f"create_event_study_permit_data_{label}")
    event_input = repo / "tasks/create_event_study_permit_data/input"
    for name in [
        "building_permits_clean.gpkg",
        "building_permits_text_features.csv.gz",
        "census_blocks_2010.csv",
        "census_blocks_2020.csv",
        "manual_permit_block_assignments.csv",
        "ward_panel.gpkg",
        "boundary_segments_1320ft.gpkg",
        "chicago_alderman_panel.csv",
        "alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2014.csv",
    ]:
        link(event_input / name, task / "input" / name)
    link(treatment_path, task / "input/block_treatment_panel.csv")
    subprocess.run(
        [
            "Rscript",
            str(repo / "tasks/create_event_study_permit_data/code/create_permit_block_year_panel.R"),
            "304.8",
            "800",
            "2010",
            "2020-12",
        ],
        cwd=task / "code",
        check=True,
    )
    return task / "output/permit_block_year_panel_2015.parquet"


fresh_current = run_event_panel(
    "current",
    merge_task / "output/block_treatment_panel_through202604.csv",
)
fresh_all_turnover = run_event_panel("all_turnover", all_turnover_path)

status = []
for label, path in [
    ("production", repo / "tasks/create_event_study_permit_data/output/permit_block_year_panel_2015.parquet"),
    ("fresh_current_rule", fresh_current),
    ("fresh_all_turnover", fresh_all_turnover),
]:
    metadata = pq.read_metadata(path)
    status.append(
        {
            "panel": label,
            "path": str(path),
            "rows": metadata.num_rows,
            "columns": metadata.num_columns,
            "file_bytes": path.stat().st_size,
        }
    )

pd.DataFrame(status).to_csv(audit_task / "output/permit_event_rebuild_status.csv", index=False)
