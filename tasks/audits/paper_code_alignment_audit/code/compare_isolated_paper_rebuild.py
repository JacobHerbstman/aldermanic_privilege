from __future__ import annotations

import csv
import hashlib
import subprocess
import sys
from difflib import unified_diff
from pathlib import Path


if len(sys.argv) != 2:
    raise SystemExit("Usage: compare_isolated_paper_rebuild.py <isolated_repo_root>")

repo = Path(__file__).resolve().parents[4]
isolated_repo = Path(sys.argv[1]).resolve()
inventory_path = repo / "tasks/audits/paper_code_alignment_audit/output/artifact_inventory.csv"


def file_hash(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def comparable_text(path: Path) -> str:
    if path.suffix.lower() == ".pdf":
        result = subprocess.run(
            ["pdftotext", "-layout", str(path), "-"],
            check=True,
            capture_output=True,
            text=True,
        )
        text = result.stdout
    else:
        text = path.read_text(errors="replace")
    return "\n".join(line.rstrip() for line in text.splitlines()).strip()


with inventory_path.open(newline="") as source:
    inventory = list(csv.DictReader(source))

artifact_paths = sorted({row["artifact"] for row in inventory})
comparison_rows: list[dict[str, object]] = []
difference_rows: list[dict[str, object]] = []

for artifact in artifact_paths:
    original = repo / artifact
    rebuilt = isolated_repo / artifact
    original_exists = original.exists()
    rebuilt_exists = rebuilt.exists()
    binary_equal = False
    text_equal = False

    if original_exists and rebuilt_exists:
        binary_equal = file_hash(original) == file_hash(rebuilt)
        original_text = comparable_text(original)
        rebuilt_text = comparable_text(rebuilt)
        text_equal = original_text == rebuilt_text
        if not text_equal:
            diff = list(
                unified_diff(
                    original_text.splitlines(),
                    rebuilt_text.splitlines(),
                    fromfile="current",
                    tofile="isolated_rebuild",
                    lineterm="",
                )
            )
            for line_number, line in enumerate(diff[:200], start=1):
                difference_rows.append(
                    {
                        "artifact": artifact,
                        "diff_line": line_number,
                        "text": line,
                    }
                )

    comparison_rows.append(
        {
            "artifact": artifact,
            "suffix": original.suffix.lower(),
            "original_exists": original_exists,
            "rebuilt_exists": rebuilt_exists,
            "binary_equal": binary_equal,
            "extracted_text_equal": text_equal,
            "original_size_bytes": original.stat().st_size if original_exists else "",
            "rebuilt_size_bytes": rebuilt.stat().st_size if rebuilt_exists else "",
        }
    )

with (repo / "tasks/audits/paper_code_alignment_audit/output/isolated_rebuild_artifact_comparison.csv").open(
    "w", newline=""
) as destination:
    writer = csv.DictWriter(destination, fieldnames=comparison_rows[0].keys())
    writer.writeheader()
    writer.writerows(comparison_rows)

with (repo / "tasks/audits/paper_code_alignment_audit/output/isolated_rebuild_text_differences.csv").open(
    "w", newline=""
) as destination:
    fieldnames = ["artifact", "diff_line", "text"]
    writer = csv.DictWriter(destination, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(difference_rows)
