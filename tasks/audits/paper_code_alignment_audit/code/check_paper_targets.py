from pathlib import Path
import csv
import subprocess


# setwd("tasks/audits/paper_code_alignment_audit/code")

repo = Path("../../../..").resolve()

with (repo / "tasks/audits/paper_code_alignment_audit/output/artifact_inventory.csv").open() as source:
    artifacts = list(csv.DictReader(source))

rows = []
for artifact in artifacts:
    target = "../output/" + Path(artifact["artifact"]).name
    result = subprocess.run(
        ["make", "-q", target],
        cwd=repo / "tasks" / artifact["producer_task"] / "code",
        capture_output=True,
        text=True,
    )
    rows.append(
        {
            "artifact": artifact["artifact"],
            "producer_task": artifact["producer_task"],
            "make_q_status": result.returncode,
            "status": {0: "up_to_date", 1: "stale"}.get(result.returncode, "error"),
            "stdout": result.stdout.strip().replace("\n", " | "),
            "stderr": result.stderr.strip().replace("\n", " | "),
        }
    )

with (repo / "tasks/audits/paper_code_alignment_audit/output/paper_target_status.csv").open(
    "w", newline=""
) as output:
    writer = csv.DictWriter(output, fieldnames=rows[0].keys())
    writer.writeheader()
    writer.writerows(rows)
