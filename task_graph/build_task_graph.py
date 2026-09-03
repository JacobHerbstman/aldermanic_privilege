"""Draw task-level dependencies declared by the paper and reachable task Makefiles.

Run through make from task_graph/. Recipe commands and non-output helper-file
dependencies are excluded. Missing task Makefiles and cycles fail explicitly.
"""
from pathlib import Path
import re


def upstream_tasks(path, pattern):
    declarations = "\n".join(
        line.split("#", 1)[0]
        for line in path.read_text().replace("\\\n", " ").splitlines()
        if not line.startswith("\t")
    )
    return set(re.findall(pattern, declarations))


edges = set()
visited = set()
visiting = set()


def visit(task):
    if task in visiting:
        raise ValueError(f"Task dependency cycle at {task}")
    if task in visited:
        return
    visiting.add(task)
    for parent in upstream_tasks(
        Path(f"../tasks/{task}/code/Makefile"), r"\.\./\.\./([^/\s]+)/output/"
    ):
        edges.add((parent, task))
        visit(parent)
    visiting.remove(task)
    visited.add(task)


paper_inputs = upstream_tasks(Path("../paper/Makefile"), r"\.\./tasks/([^/\s]+)/output/")
if not paper_inputs:
    raise ValueError("No task-output prerequisites found in paper/Makefile")
for task in paper_inputs:
    edges.add((task, "paper"))
    visit(task)

assert not any("audit" in task for task in visited), "Paper depends on an audit task"
lines = [
    "digraph paper_tasks {",
    "  rankdir=LR;",
    '  graph [bgcolor=white, pad=0.2, nodesep=0.25, ranksep=0.55];',
    '  node [shape=box, style="rounded,filled", fillcolor="#f3f4f6", color="#6b7280", fontname=Helvetica, fontsize=9];',
    '  edge [color="#9ca3af", arrowsize=0.6];',
    '  paper [shape=box, style="rounded,filled,bold", fillcolor="#dbeafe", color="#2563eb", label="paper"];',
]
lines.extend(f'  "{parent}" -> "{child}";' for parent, child in sorted(edges))
lines.append("}")
Path("paper_task_flow.dot").write_text("\n".join(lines) + "\n")
