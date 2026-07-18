from pathlib import Path
import csv
import re


# setwd("tasks/audits/paper_code_alignment_audit/code")

repo = Path("../../../..").resolve()
paper_dir = repo / "short_paper"

tex_files = [paper_dir / "paper.tex"] + sorted((paper_dir / "sections").glob("*.tex"))
artifact_pattern = re.compile(
    r"\\(?:input|includegraphics)(?:\[[^]]*\])?\{(\.\./tasks/[^}]+)\}"
)
makefile_artifacts = set(re.findall(r"\.\./tasks/[^\s\\]+", (paper_dir / "Makefile").read_text()))

artifact_rows = []
line_rows = []

for tex_file in tex_files:
    for line_number, line in enumerate(tex_file.read_text().splitlines(), start=1):
        stripped = line.strip()
        if not stripped or stripped.startswith("%"):
            continue

        task_references = artifact_pattern.findall(line)
        line_rows.append(
            {
                "source_file": str(tex_file.relative_to(repo)),
                "line": line_number,
                "text": stripped,
                "has_task_artifact": bool(task_references),
                "has_number": bool(re.search(r"\d", stripped)),
                "has_method_term": bool(
                    re.search(
                        r"regress|fixed effect|cluster|bandwidth|sample|control|resid|"
                        r"spatial|distance|join|match|permit|zoning|ward|block|parcel|"
                        r"stringency|construction|rent|sale|price|FAR|DUPAC|PPML|RD",
                        stripped,
                        flags=re.IGNORECASE,
                    )
                ),
            }
        )

        for reference in task_references:
            artifact = (paper_dir / reference).resolve()
            relative_artifact = artifact.relative_to(repo)
            parts = relative_artifact.parts
            task_name = parts[1] if len(parts) > 1 and parts[0] == "tasks" else ""
            producer_makefile = repo / "tasks" / task_name / "code" / "Makefile"
            artifact_rows.append(
                {
                    "source_file": str(tex_file.relative_to(repo)),
                    "line": line_number,
                    "kind": "table" if stripped.startswith("\\input") else "figure",
                    "artifact": str(relative_artifact),
                    "exists": artifact.exists(),
                    "listed_in_paper_makefile": reference in makefile_artifacts,
                    "producer_task": task_name,
                    "producer_makefile_exists": producer_makefile.exists(),
                    "artifact_named_in_producer_makefile": (
                        artifact.name in producer_makefile.read_text()
                        if producer_makefile.exists()
                        else False
                    ),
                    "size_bytes": artifact.stat().st_size if artifact.exists() else "",
                    "modified_ns": artifact.stat().st_mtime_ns if artifact.exists() else "",
                }
            )

artifact_rows.sort(key=lambda row: (row["source_file"], row["line"], row["artifact"]))
line_rows.sort(key=lambda row: (row["source_file"], row["line"]))

with (repo / "tasks/audits/paper_code_alignment_audit/output/artifact_inventory.csv").open(
    "w", newline=""
) as output:
    writer = csv.DictWriter(output, fieldnames=artifact_rows[0].keys())
    writer.writeheader()
    writer.writerows(artifact_rows)

with (repo / "tasks/audits/paper_code_alignment_audit/output/manuscript_line_inventory.csv").open(
    "w", newline=""
) as output:
    writer = csv.DictWriter(output, fieldnames=line_rows[0].keys())
    writer.writeheader()
    writer.writerows(line_rows)
