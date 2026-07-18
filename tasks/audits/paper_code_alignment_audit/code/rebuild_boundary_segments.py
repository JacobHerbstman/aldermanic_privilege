from pathlib import Path
import os
import shutil
import subprocess


# setwd("tasks/audits/paper_code_alignment_audit/code")

repo = Path("../../../..").resolve()
audit_task = repo / "tasks/audits/paper_code_alignment_audit"
sandbox = audit_task / "temp/segment_rebuild"
sandbox_tasks = sandbox / "tasks"

if sandbox.exists():
    shutil.rmtree(sandbox)
sandbox_tasks.mkdir(parents=True)

os.symlink(repo / "tasks/setup_environment", sandbox_tasks / "setup_environment")
os.symlink(repo / "tasks/_lib", sandbox_tasks / "_lib")

segment_task = sandbox_tasks / "border_segment_creation"
(segment_task / "code").mkdir(parents=True)
(segment_task / "input").mkdir()
(segment_task / "output").mkdir()

for source in sorted((repo / "tasks/border_segment_creation/input").iterdir()):
    os.symlink(source.resolve(), segment_task / "input" / source.name)

subprocess.run(
    [
        "Rscript",
        str(repo / "tasks/border_segment_creation/code/build_boundary_segments.R"),
        "1320",
        "100 250 400",
        "30",
    ],
    cwd=segment_task / "code",
    check=True,
)

assignment_task = sandbox_tasks / "assign_segment_ids"
(assignment_task / "code").mkdir(parents=True)
(assignment_task / "input").mkdir()
(assignment_task / "output").mkdir()

os.symlink(
    (repo / "tasks/assign_segment_ids/input/parcels_pre_scores.csv").resolve(),
    assignment_task / "input/parcels_pre_scores.csv",
)
os.symlink(
    (repo / "tasks/assign_segment_ids/input/parcels_with_geometry.gpkg").resolve(),
    assignment_task / "input/parcels_with_geometry.gpkg",
)
os.symlink(
    segment_task / "output/boundary_segments_1320ft.gpkg",
    assignment_task / "input/boundary_segments_1320ft.gpkg",
)

subprocess.run(
    [
        "Rscript",
        str(repo / "tasks/assign_segment_ids/code/assign_segment_ids.R"),
        "1320",
        "250",
    ],
    cwd=assignment_task / "code",
    check=True,
)
