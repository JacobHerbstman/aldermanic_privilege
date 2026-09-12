"""Draw construction task and script dependencies from their literal Make rules."""
from pathlib import Path
import os
import re
import sys

mode = sys.argv[1]
assert mode in ("tasks", "scripts", "steps")
repository = Path("..").resolve()
rules = {}
scripts = {}

# Construction producers use literal paths; scalar settings do not alter their names.
for makefile in sorted((repository / "tasks").glob("construction_*/code/Makefile")):
    for line in makefile.read_text().replace("\\\n", " ").splitlines():
        if line.startswith(("\t", "#", "include")) or ":" not in line or "=" in line:
            continue
        targets, inputs = line.split(":", 1)
        targets = targets.replace("../%/", "../output/")
        if "%" in targets:
            continue
        dependencies = [Path(os.path.normpath(makefile.parent / x))
                        for x in inputs.split("|", 1)[0].split()]
        for target in targets.split():
            path = Path(os.path.normpath(makefile.parent / target))
            rules.setdefault(path, []).extend(dependencies)
            code = [x for x in dependencies if x.suffix == ".R" and x.parent == makefile.parent]
            if code:
                scripts[path] = code[0]

script_inputs = {}
script_outputs = {}
source_nodes = set()
resolving = set()


def producer(path):
    if path in resolving:
        raise ValueError(f"Construction file dependency cycle: {path}")
    resolving.add(path)
    dependencies = rules.get(path, [])
    if path in scripts:
        result = scripts[path]
        script_outputs.setdefault(result, set()).add(path)
        if result not in script_inputs:
            script_inputs[result] = set()
            for item in dependencies:
                if item.parent.name in ("input", "output"):
                    script_inputs[result].add(producer(item))
    elif path.parent.name == "input" and len(dependencies) == 1:
        result = producer(dependencies[0])
    else:
        result = path
        source_nodes.add(path)
    resolving.remove(path)
    return result


root = repository / "tasks/construction_boundary_distances/code/all"
for target in rules[root]:
    producer(target)
assert script_inputs, "No construction producers found"
levels = {}
visiting = set()


def level(node):
    if node in visiting:
        raise ValueError(f"Construction producer cycle: {node}")
    if node not in levels:
        visiting.add(node)
        levels[node] = 1 + max((level(x) for x in script_inputs.get(node, [])), default=-1)
        visiting.remove(node)
    return levels[node]


for node in script_inputs:
    level(node)
if mode == "steps":
    lines = ["# Construction data: execution order", "",
             "Generated from the current Makefile prerequisites. Scripts at the same level are independent.", ""]
    for step in sorted(set(levels.values())):
        nodes = sorted(x for x in script_inputs if levels[x] == step)
        if not nodes:
            continue
        lines += [f"## Dependency level {step}", ""]
        for node in nodes:
            relative = node.relative_to(repository)
            lines += [f"- [{relative}](../{relative}): " +
                      ", ".join(f"`{x.name}`" for x in sorted(script_outputs[node]))]
        lines.append("")
    Path("construction_steps.md").write_text("\n".join(lines).rstrip() + "\n")
else:
    edges = {(parent, child) for child, parents in script_inputs.items() for parent in parents}
    if mode == "tasks":
        edges = {(x.parent.parent.name, y.parent.parent.name) for x, y in edges
                 if x.parent.parent != y.parent.parent}
    else:
        edges = {(str(x.relative_to(repository)), str(y.relative_to(repository))) for x, y in edges}
    lines = ["digraph construction {", "rankdir=LR;", "graph [bgcolor=white];",
             'node [shape=box, fontname="Helvetica", fontsize=9];']
    for node in sorted({x for pair in edges for x in pair}):
        label = node.replace("tasks/", "").replace("/code/", "\\n").replace("/output/", "\\n")
        lines.append(f'"{node}" [label="{label}"];')
    for parent, child in sorted(edges):
        lines.append(f'"{parent}" -> "{child}";')
    lines.append("}")
    Path(f"construction_{mode}.dot").write_text("\n".join(lines) + "\n")
