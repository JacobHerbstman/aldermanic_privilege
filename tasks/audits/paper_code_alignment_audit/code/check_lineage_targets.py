from pathlib import Path
import csv
import subprocess


# setwd("tasks/audits/paper_code_alignment_audit/code")

repo = Path("../../../..").resolve()

with (repo / "tasks/audits/paper_code_alignment_audit/output/task_lineage_edges.csv").open() as source:
    edges = list(csv.DictReader(source))

targets = sorted(
    {
        (row["upstream_task_or_raw_path"], row["declared_path"])
        for row in edges
        if row["source_type"] == "task_output"
        and "$" not in row["declared_path"]
        and "%" not in row["declared_path"]
    }
)

rows = []
for task, output_path in targets:
    task_code = repo / "tasks" / task / "code"
    target = f"../output/{output_path}"
    if not task_code.exists():
        rows.append(
            {
                "producer_task": task,
                "target": target,
                "status": "missing_task_code",
                "return_code": "",
                "dry_run_output": "",
                "stderr": "",
            }
        )
        continue

    try:
        result = subprocess.run(
            ["make", "-n", target],
            cwd=task_code,
            capture_output=True,
            text=True,
            timeout=120,
        )
        dry_run_output = result.stdout.strip()
        informative_lines = [
            line.strip()
            for line in dry_run_output.splitlines()
            if line.strip()
            and "is up to date." not in line
            and "Nothing to be done" not in line
            and "Entering directory" not in line
            and "Leaving directory" not in line
        ]
        rows.append(
            {
                "producer_task": task,
                "target": target,
                "status": (
                    "error"
                    if result.returncode != 0
                    else "would_rebuild"
                    if informative_lines
                    else "locally_up_to_date"
                ),
                "return_code": result.returncode,
                "dry_run_output": dry_run_output.replace("\n", " | "),
                "stderr": result.stderr.strip().replace("\n", " | "),
            }
        )
    except subprocess.TimeoutExpired as error:
        rows.append(
            {
                "producer_task": task,
                "target": target,
                "status": "timeout",
                "return_code": "",
                "dry_run_output": (error.stdout or "").strip().replace("\n", " | "),
                "stderr": (error.stderr or "").strip().replace("\n", " | "),
            }
        )

with (repo / "tasks/audits/paper_code_alignment_audit/output/lineage_target_dry_run_status.csv").open(
    "w", newline=""
) as output:
    writer = csv.DictWriter(output, fieldnames=rows[0].keys())
    writer.writeheader()
    writer.writerows(rows)
