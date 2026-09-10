"""Display the current construction build from its literal Make prerequisites."""
from pathlib import Path
import re
import sys

mode = sys.argv[1]
assert mode in ("tasks", "scripts", "steps")
root = Path("../tasks/new_construction_cleaning/code")
rules = {}
read_files = set()


def read_make(path):
    if path in read_files:
        return
    read_files.add(path)
    for line in path.read_text().replace("\\\n", " ").splitlines():
        if line.startswith("\t"):
            continue
        line = line.split("#", 1)[0].strip()
        if line.startswith("include "):
            name = line.split()[1]
            if "/shared/" not in name:
                read_make(path.parent / name)
        elif ":" in line and not re.search(r"[:?+]?=", line):
            targets, inputs = line.split(":", 1)
            if "%" not in targets and not targets.startswith(".PHONY"):
                for target in targets.split():
                    rules.setdefault(target, []).extend(inputs.split("|")[0].split())


read_make(root / "Makefile")
assert rules.get("all"), "The current build has no default products"
script_inputs = {}
script_outputs = {}
source_nodes = {}
resolving = set()


def producer(target):
    if target in resolving:
        raise ValueError(f"Construction dependency cycle: {target}")
    resolving.add(target)
    dependencies = rules.get(target, [])
    if target.startswith("../report/"):
        result = producer(target.replace("../report/", "../output/").removesuffix(".log"))
    elif target.startswith("../input/") and len(dependencies) == 1:
        result = producer(dependencies[0])
    elif target.startswith("../output/"):
        scripts = [x for x in dependencies if "/" not in x and x.endswith(".R")]
        if scripts:
            result = scripts[0]
            script_outputs.setdefault(result, set()).add(target)
            if result not in script_inputs:
                script_inputs[result] = set()
                for item in dependencies:
                    if item.startswith(("../output/", "../input/", "../adjudication/")):
                        script_inputs[result].add(producer(item))
        else:
            parents = [x for x in dependencies if x.startswith("../output/")]
            if len(parents) != 1:
                raise ValueError(f"No unique producer for {target}")
            result = producer(parents[0])
            script_outputs.setdefault(result, set()).add(target)
    else:
        match = re.match(r"../../([^/]+)/output/", target)
        result = match.group(1) if match else (
            "Recorded decisions" if target.startswith("../adjudication/") else "Pinned local sources")
        source_nodes.setdefault(result, set()).add(target)
    resolving.remove(target)
    return result


for target in rules["all"]:
    producer(target)
levels = {}


def level(node):
    if node not in levels:
        levels[node] = 1 + max((level(x) for x in script_inputs.get(node, [])), default=-1)
    return levels[node]


for node in script_inputs:
    level(node)
if mode == "steps":
    lines = ["# Current construction cleaning: execution order", "",
             "Generated from the current Make targets. Steps at the same level are independent.",
             "The chronological construction outputs feed the density analyses; older unused final-assembly rules are excluded.", ""]
    for step in sorted(set(levels.values())):
        scripts = sorted(x for x in script_inputs if levels[x] == step)
        if not scripts:
            continue
        lines += [f"## Dependency level {step}", ""]
        for script in scripts:
            lines += [f"- [{script}](../tasks/new_construction_cleaning/code/{script}): " +
                      ", ".join(f"`{x.removeprefix('../output/')}`" for x in sorted(script_outputs[script]))]
        lines.append("")
    Path("construction_steps.md").write_text("\n".join(lines).rstrip() + "\n")
else:
    edges = {(parent, script) for script, parents in script_inputs.items() for parent in parents}
    if mode == "tasks":
        edges = {(parent, "new_construction_cleaning") for parent in source_nodes}
        # These are the actual direct task handoffs to the current construction build.
    nodes = set(x for edge in edges for x in edge)
    lines = ["digraph construction {", "rankdir=LR;",
             'graph [bgcolor="white", pad=.25, nodesep=.25, ranksep=.5];',
             'node [shape=box, style="rounded,filled", fillcolor="#eef4fa", fontname="Helvetica", fontsize=10];',
             'edge [color="#64748b"];']
    for node in sorted(nodes):
        label = node.removesuffix(".R").replace("_", " ")
        lines.append(f'"{node}" [label="{label}"];')
    for parent, child in sorted(edges):
        lines.append(f'"{parent}" -> "{child}";')
    lines += ['label="Current construction build — feeds the density analyses";', "}"]
    Path(f"construction_{mode}.dot").write_text("\n".join(lines) + "\n")
