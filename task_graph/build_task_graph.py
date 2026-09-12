"""Draw the production task graph from the file rules reached by paper/Makefile."""
from pathlib import Path
import os
import subprocess

repository = Path("..").resolve()
edges = set()
visited = set()
visiting = set()


def visit(task, target):
    key = (task, target)
    if key in visiting:
        raise ValueError(f"File dependency cycle at {task}: {target}")
    if key in visited:
        return
    visiting.add(key)
    directory = repository / ("paper" if task == "paper" else f"tasks/{task}/code")

    # Ask Make to expand variables and pattern rules without running producers.
    # Suppress only the shared recursive check while inspecting this task's rules.
    result = subprocess.run(
        ["make", "-qpRr", "--no-print-directory", "-o", "FORCE_UPSTREAM", target],
        cwd=directory, capture_output=True, text=True,
    )
    if result.returncode not in (0, 1):
        raise RuntimeError(f"Cannot read {task}: {target}\n{result.stderr}")
    declarations = [line for line in result.stdout.splitlines()
                    if line.startswith(target + ":")]
    if len(declarations) != 1:
        raise ValueError(f"Expected one expanded rule for {task}: {target}")
    dependencies = declarations[0].split(":", 1)[1].split("|", 1)[0].split()

    for dependency in dependencies:
        path = Path(os.path.normpath(directory / dependency))
        if path.is_relative_to(repository / "tasks"):
            parts = path.relative_to(repository / "tasks").parts
            folders = [i for i, part in enumerate(parts) if part in ("output", "report")]
            if folders:
                position = folders[0]
                parent = "/".join(parts[:position])
                parent_target = "../" + "/".join(parts[position:])
                if parent != task:
                    edges.add((parent, task))
                visit(parent, parent_target)
                continue
        if dependency.startswith("../input/") or (task == "paper" and dependency.endswith(".pdf")):
            visit(task, dependency)

    visiting.remove(key)
    visited.add(key)


visit("paper", "all")
assert edges, "No production task dependencies found"
assert not any("audit" in task for edge in edges for task in edge), "Paper depends on an audit task"
lines = [
    "digraph paper_tasks {",
    "  rankdir=LR;",
    "  graph [bgcolor=white, pad=0.2, nodesep=0.25, ranksep=0.55];",
    '  node [shape=box, style="rounded,filled", fillcolor="#f3f4f6", color="#6b7280", fontname=Helvetica, fontsize=9];',
    '  edge [color="#9ca3af", arrowsize=0.6];',
    '  paper [shape=box, style="rounded,filled,bold", fillcolor="#dbeafe", color="#2563eb", label="paper"];',
]
lines.extend(f'  "{parent}" -> "{child}";' for parent, child in sorted(edges))
lines.append("}")
Path("paper_task_flow.dot").write_text("\n".join(lines) + "\n")
