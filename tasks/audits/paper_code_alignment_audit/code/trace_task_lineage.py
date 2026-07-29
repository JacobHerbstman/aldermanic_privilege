from pathlib import Path
import csv
import hashlib
import re


# setwd("tasks/audits/paper_code_alignment_audit/code")

repo = Path("../../../..").resolve()

with (repo / "tasks/audits/paper_code_alignment_audit/output/artifact_inventory.csv").open() as source:
    paper_tasks = sorted({row["producer_task"] for row in csv.DictReader(source)})

upstream_pattern = re.compile(r"\.\./\.\./([A-Za-z0-9_-]+)/output/([^\s|\\]+)")
raw_pattern = re.compile(r"(?:\.\./){3}data_raw/([^\s|\\]+)")

queue = list(paper_tasks)
seen = set()
rows = []

while queue:
    task = queue.pop(0)
    if task in seen:
        continue
    seen.add(task)

    makefile = repo / "tasks" / task / "code" / "Makefile"
    if not makefile.exists():
        rows.append(
            {
                "downstream_task": task,
                "source_type": "missing_makefile",
                "upstream_task_or_raw_path": "",
                "declared_path": "",
            }
        )
        continue

    make_text = makefile.read_text()
    for upstream_task, output_path in upstream_pattern.findall(make_text):
        rows.append(
            {
                "downstream_task": task,
                "source_type": "task_output",
                "upstream_task_or_raw_path": upstream_task,
                "declared_path": output_path,
            }
        )
        if upstream_task not in seen:
            queue.append(upstream_task)

    for raw_path in raw_pattern.findall(make_text):
        rows.append(
            {
                "downstream_task": task,
                "source_type": "data_raw",
                "upstream_task_or_raw_path": raw_path,
                "declared_path": raw_path,
            }
        )

rows.sort(
    key=lambda row: (
        row["downstream_task"],
        row["source_type"],
        row["upstream_task_or_raw_path"],
        row["declared_path"],
    )
)

with (repo / "tasks/audits/paper_code_alignment_audit/output/task_lineage_edges.csv").open(
    "w", newline=""
) as output:
    writer = csv.DictWriter(output, fieldnames=rows[0].keys())
    writer.writeheader()
    writer.writerows(rows)

source_rows = []
for task in sorted(seen):
    for source_file in sorted((repo / "tasks" / task / "code").iterdir()):
        if not source_file.is_file():
            continue
        contents = source_file.read_bytes()
        source_rows.append(
            {
                "task": task,
                "source_file": str(source_file.relative_to(repo)),
                "lines": contents.count(b"\n") + bool(contents),
                "size_bytes": len(contents),
                "sha256": hashlib.sha256(contents).hexdigest(),
            }
        )

with (repo / "tasks/audits/paper_code_alignment_audit/output/task_source_inventory.csv").open(
    "w", newline=""
) as output:
    writer = csv.DictWriter(output, fieldnames=source_rows[0].keys())
    writer.writeheader()
    writer.writerows(source_rows)
