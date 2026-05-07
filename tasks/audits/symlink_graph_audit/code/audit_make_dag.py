#!/usr/bin/env python3
from __future__ import annotations

import csv
import fnmatch
import glob
import re
from collections import defaultdict
from pathlib import Path


ROOT = Path(__file__).resolve().parents[4]
TASKS = ROOT / "tasks"
PAPER = ROOT / "paper"
OUT = ROOT / "tasks" / "audits" / "symlink_graph_audit" / "output"


def read(path: Path) -> str:
    return path.read_text(errors="ignore")


def logical_lines(text: str) -> list[str]:
    lines: list[str] = []
    buf = ""
    for raw in text.splitlines():
        line = raw.rstrip()
        if line.endswith("\\"):
            buf += line[:-1] + " "
            continue
        lines.append(buf + line)
        buf = ""
    if buf:
        lines.append(buf)
    return lines


def strip_make_comment(line: str) -> str:
    if "#" not in line:
        return line
    if line.lstrip().startswith("#"):
        return ""
    return re.sub(r"\s+#.*$", "", line)


VAR_REF_RE = re.compile(r"\$\(([^)]+)\)|\${([^}]+)\}")


def expand_make_vars(text: str, variables: dict[str, str], depth: int = 8) -> str:
    out = text
    for _ in range(depth):
        changed = False

        def repl(match: re.Match[str]) -> str:
            nonlocal changed
            name = match.group(1) or match.group(2)
            if name in variables:
                changed = True
                return variables[name]
            return match.group(0)

        nxt = VAR_REF_RE.sub(repl, out)
        out = nxt
        if not changed:
            break
    return out


def make_variables(lines: list[str]) -> dict[str, str]:
    variables: dict[str, str] = {}
    assignment = re.compile(r"^([A-Za-z_][A-Za-z0-9_]*)\s*([:+?]?=)\s*(.*)$")
    for line in lines:
        stripped = strip_make_comment(line).strip()
        if not stripped or stripped.startswith("\t"):
            continue
        match = assignment.match(stripped)
        if not match:
            continue
        name, op, value = match.groups()
        value = expand_make_vars(value.strip(), variables)
        if op == "+=":
            variables[name] = (variables.get(name, "") + " " + value).strip()
        elif op == "?=":
            variables.setdefault(name, value)
        else:
            variables[name] = value
    return variables


def expanded_make_text(text: str) -> str:
    lines = logical_lines(text)
    variables = make_variables(lines)
    return "\n".join(expand_make_vars(strip_make_comment(line), variables) for line in lines)


def make_pattern_matches(pattern: str, target: str) -> bool:
    if "%" not in pattern:
        return pattern == target
    return fnmatch.fnmatchcase(target, pattern.replace("%", "*"))


def pattern_stem(pattern: str, target: str) -> str | None:
    if "%" not in pattern:
        return "" if pattern == target else None
    prefix, suffix = pattern.split("%", 1)
    if not target.startswith(prefix) or not target.endswith(suffix):
        return None
    return target[len(prefix) : len(target) - len(suffix) if suffix else len(target)]


def apply_stem(text: str, stem: str | None) -> str:
    if stem is None:
        return text
    return text.replace("%", stem)


def make_rules(makefile: Path) -> list[dict[str, object]]:
    rules: list[dict[str, object]] = []
    text = read(makefile)
    lines = logical_lines(text)
    variables = make_variables(lines)
    current: dict[str, object] | None = None
    assignment = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*\s*[:+?]?=")

    for raw in lines:
        if raw.startswith("\t"):
            if current is not None:
                current["recipe"].append(expand_make_vars(raw.strip(), variables))
            continue

        current = None
        stripped = strip_make_comment(raw).strip()
        if (
            not stripped
            or assignment.match(stripped)
            or stripped.startswith("include ")
            or stripped.startswith("-include ")
            or ":" not in stripped
        ):
            continue

        expanded = expand_make_vars(stripped, variables)
        if expanded.startswith(".") and not expanded.startswith("../"):
            continue

        parts = expanded.split(":")
        static_pattern = None
        if len(parts) >= 3 and "%" in parts[1]:
            lhs = parts[0].strip()
            static_pattern = parts[1].strip()
            prereq = ":".join(parts[2:]).strip()
        else:
            lhs, prereq = expanded.split(":", 1)
            lhs = lhs.strip()
            prereq = prereq.strip()

        targets = lhs.split()
        if not targets:
            continue
        current = {"targets": targets, "static_pattern": static_pattern, "prereq": prereq, "recipe": []}
        rules.append(current)
    return rules


def matching_rules(rules: list[dict[str, object]], target: str) -> list[tuple[dict[str, object], str | None]]:
    matches: list[tuple[dict[str, object], str | None]] = []
    for rule in rules:
        targets = rule["targets"]
        assert isinstance(targets, list)
        static_pattern = rule["static_pattern"]
        if isinstance(static_pattern, str) and target in targets:
            matches.append((rule, pattern_stem(static_pattern, target)))
            continue
        for pattern in targets:
            stem = pattern_stem(pattern, target)
            if stem is not None:
                matches.append((rule, stem))
                break
    return matches


def local_prereqs(prereq_text: str, stem: str | None) -> list[str]:
    prereq_text = apply_stem(prereq_text.split("|", 1)[0], stem)
    out: list[str] = []
    for token in prereq_text.split():
        token = token.strip("\"'\\")
        if "$(" in token or "${" in token or ")" in token or "," in token:
            continue
        if token in {"../input", "../output", "../temp", "slurmlogs"}:
            continue
        if token.startswith(("../input/", "../output/", "../temp/")):
            out.append(token)
    return out


def active_tex_files() -> list[Path]:
    seen: list[Path] = []

    def walk(path: Path) -> None:
        if path in seen or not path.exists():
            return
        seen.append(path)
        for match in re.finditer(r"\\(?:input|include)\{([^}]+)\}", read(path)):
            target = match.group(1)
            if target.startswith("../tasks/"):
                continue
            nxt = path.parent / target
            if not nxt.suffix:
                nxt = nxt.with_suffix(".tex")
            walk(nxt)

    walk(PAPER / "paper.tex")
    return seen


def paper_task_refs() -> set[str]:
    refs: set[str] = set()
    for path in active_tex_files():
        text = read(path)
        for pattern in (
            r"\\(?:input|include)\{(\.\./tasks/[^}]+)\}",
            r"\\includegraphics(?:\[[^\]]*\])?\{(\.\./tasks/[^}]+)\}",
        ):
            refs.update(re.findall(pattern, text))
    return refs


def task_makefiles() -> dict[str, Path]:
    out: dict[str, Path] = {}
    for makefile in TASKS.glob("*/code/Makefile"):
        task = makefile.parents[1].name
        if task.startswith("_") or makefile.match("tasks/archive/*") or makefile.match("tasks/audits/*"):
            continue
        out[task] = makefile
    return out


def explicit_targets(makefile: Path) -> set[str]:
    targets: set[str] = set()
    text = read(makefile)
    lines = logical_lines(text)
    variables = make_variables(lines)
    for line in lines:
        stripped = strip_make_comment(line).strip()
        if not stripped or stripped.startswith("#") or stripped.startswith("\t"):
            continue
        if re.match(r"^[A-Za-z_][A-Za-z0-9_]*\s*[:+?]?=", stripped):
            continue
        if ":" not in stripped:
            continue
        lhs = stripped.split(":", 1)[0].strip()
        if not lhs:
            continue
        first_target = lhs.split()[0]
        if first_target.startswith(".") and not first_target.startswith("../"):
            continue
        lhs = expand_make_vars(lhs, variables)
        for target in lhs.split():
            targets.add(target)
    return targets


def target_exists(targets: set[str], target: str) -> bool:
    if target in targets:
        return True
    for candidate in targets:
        if make_pattern_matches(candidate, target):
            return True
    return False


def has_dynamic_targets(targets: set[str]) -> bool:
    return any("$(" in target or "${" in target for target in targets)


def task_ref_to_target(ref: str) -> tuple[str, str] | None:
    match = re.match(r"\.\./tasks/([^/]+)/output/(.+)$", ref)
    if not match:
        return None
    return match.group(1), "../output/" + match.group(2)


def upstream_refs(text: str) -> list[tuple[str, str]]:
    refs = []
    for task, file in re.findall(r"\.\./\.\./([^/\s:]+)/output/([^\s:|]+)", text):
        file = file.strip("\"'\\")
        if "$(" in file or "${" in file or "," in file or ")" in file:
            continue
        refs.append((task, "../output/" + file))
    return refs


def raw_refs(text: str) -> list[str]:
    refs = []
    for match in re.findall(r"\.\./\.\./\.\./data_raw/[^\s:|]+", text):
        refs.append(match.strip("\"'\\"))
    return refs


def raw_ref_exists(makefile: Path, raw: str) -> bool:
    glob_raw = raw.replace("%", "*")
    raw_path = makefile.parent / glob_raw
    if any(char in glob_raw for char in "*?["):
        return bool(glob.glob(str(raw_path)))
    return raw_path.resolve().exists()


def has_non_idempotent_symlink(text: str) -> bool:
    for line in logical_lines(text):
        if "ln -sf" in line or "ln -sfn" in line:
            if "readlink" not in line:
                return True
    return False


def live_api_hits(task: str, makefile: Path) -> list[str]:
    hits = []
    files = [makefile, *makefile.parent.glob("*.R"), *makefile.parent.glob("*.py"), *makefile.parent.glob("*.sh")]
    patterns = {
        "census_api": r"get_acs|get_decennial|tidycensus",
        "fred_api": r"fredr::|fredr\(|fred\.stlouisfed",
        "socrata_api": r"Socrata|data\.cityofchicago\.org|api\.cookcountyil\.gov|datacatalog\.cookcountyil\.gov",
        "dewey_api": r"dewey|deweydatar",
        "generic_web": r"\bcurl\s|wget|httr|httr2|read_csv\(['\"]https?://",
    }
    for path in files:
        text = "\n".join(
            line for line in read(path).splitlines()
            if not line.lstrip().startswith(("#", "##", "###"))
        )
        for label, pattern in patterns.items():
            if re.search(pattern, text, flags=re.I):
                hits.append(f"{label}:{path.relative_to(ROOT)}")
    return sorted(set(hits))


def cycles(edges: dict[str, set[str]]) -> list[list[str]]:
    found: list[list[str]] = []
    stack: list[str] = []
    seen: set[str] = set()

    def visit(node: str) -> None:
        if node in stack:
            found.append(stack[stack.index(node) :] + [node])
            return
        if node in seen:
            return
        seen.add(node)
        stack.append(node)
        for nxt in sorted(edges.get(node, set())):
            visit(nxt)
        stack.pop()

    for node in sorted(edges):
        visit(node)
    return found


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    makefiles = task_makefiles()
    targets_cache: dict[str, set[str]] = {}
    rules_cache: dict[str, list[dict[str, object]]] = {}
    issues: list[dict[str, str]] = []
    issue_keys: set[tuple[str, str, str, str]] = set()
    edges: dict[str, set[str]] = defaultdict(set)
    reachable: set[str] = set()
    reachable_targets: set[tuple[str, str]] = set()
    active_rule_text: dict[str, list[str]] = defaultdict(list)

    def add_issue(severity: str, kind: str, task: str, detail: str) -> None:
        key = (severity, kind, task, detail)
        if key in issue_keys:
            return
        issue_keys.add(key)
        issues.append({"severity": severity, "kind": kind, "task": task, "detail": detail})

    def targets_for(task: str) -> set[str]:
        if task not in targets_cache and task in makefiles:
            targets_cache[task] = explicit_targets(makefiles[task])
        return targets_cache.get(task, set())

    def rules_for(task: str) -> list[dict[str, object]]:
        if task not in rules_cache and task in makefiles:
            rules_cache[task] = make_rules(makefiles[task])
        return rules_cache.get(task, [])

    def walk_target(task: str, target: str, suppress_dynamic_warning: bool = False) -> None:
        state = (task, target)
        if state in reachable_targets:
            return
        reachable_targets.add(state)
        reachable.add(task)
        if task not in makefiles:
            add_issue("error", "missing_task_makefile", task, target)
            return

        matches = matching_rules(rules_for(task), target)
        if not matches:
            if not target_exists(targets_for(task), target):
                if has_dynamic_targets(targets_for(task)):
                    if not suppress_dynamic_warning:
                        add_issue("warn", "dynamic_producer_unverified", task, target)
                else:
                    add_issue("error", "missing_target_rule", task, target)
            return

        for rule, stem in matches:
            prereq = rule["prereq"]
            recipe = rule["recipe"]
            assert isinstance(prereq, str)
            assert isinstance(recipe, list)
            block_text = apply_stem(prereq + "\n" + "\n".join(str(line) for line in recipe), stem)
            active_rule_text[task].append(block_text)

            for up_task, up_target in upstream_refs(block_text):
                edges[task].add(up_task)
                if up_task not in makefiles:
                    add_issue("error", "missing_upstream_makefile", task, f"{up_task}:{up_target}")
                    continue
                if not target_exists(targets_for(up_task), up_target):
                    if has_dynamic_targets(targets_for(up_task)):
                        add_issue("warn", "dynamic_upstream_producer_unverified", task, f"{up_task}:{up_target}")
                    else:
                        add_issue("error", "missing_upstream_producer", task, f"{up_task}:{up_target}")
                walk_target(up_task, up_target)

            for local_target in local_prereqs(prereq, stem):
                walk_target(task, local_target)

    for ref in sorted(paper_task_refs()):
        parsed = task_ref_to_target(ref)
        if not parsed:
            add_issue("error", "paper_ref_parse", "", ref)
            continue
        task, target = parsed
        reachable.add(task)
        if task not in makefiles:
            add_issue("error", "missing_task_makefile", task, ref)
        elif not target_exists(targets_for(task), target):
            if has_dynamic_targets(targets_for(task)):
                add_issue("warn", "dynamic_paper_producer_unverified", task, f"{target} from {ref}")
            else:
                add_issue("error", "missing_paper_producer", task, f"{target} from {ref}")
        walk_target(task, target, suppress_dynamic_warning=True)

    for task in sorted(reachable):
        makefile = makefiles.get(task)
        if not makefile:
            continue
        text = read(makefile)
        active_text = "\n".join(active_rule_text.get(task, []))
        if re.search(r"\.\./\.\./[^/\s:]+/input/", active_text):
            add_issue("warn", "upstream_input_link", task, str(makefile.relative_to(ROOT)))
        if has_non_idempotent_symlink(text):
            add_issue("warn", "non_idempotent_symlink", task, str(makefile.relative_to(ROOT)))
        for raw in sorted(set(raw_refs(active_text))):
            if not raw_ref_exists(makefile, raw):
                add_issue("error", "missing_raw_root", task, raw)
            else:
                add_issue("info", "raw_root", task, raw)
        for hit in live_api_hits(task, makefile):
            severity = "info" if hit.startswith("census_api") else "warn"
            add_issue(severity, "live_api", task, hit)

    for cyc in cycles(edges):
        add_issue("error", "cycle", cyc[0], " -> ".join(cyc))

    csv_path = OUT / "make_dag_audit.csv"
    with csv_path.open("w", newline="") as fh:
        writer = csv.DictWriter(fh, fieldnames=["severity", "kind", "task", "detail"])
        writer.writeheader()
        writer.writerows(issues)

    md_path = OUT / "make_dag_audit.md"
    counts = defaultdict(int)
    for issue in issues:
        counts[(issue["severity"], issue["kind"])] += 1
    lines = [
        "# Make DAG Audit",
        "",
        f"- active paper task refs: {len(paper_task_refs())}",
        f"- reachable tasks: {len(reachable)}",
        f"- issues/notes: {len(issues)}",
        "",
        "## Counts",
        "",
        "| Severity | Kind | Count |",
        "|---|---|---:|",
    ]
    for (severity, kind), count in sorted(counts.items()):
        lines.append(f"| {severity} | {kind} | {count} |")
    lines.extend(["", "## Errors And Warnings", ""])
    for issue in issues:
        if issue["severity"] in {"error", "warn"}:
            lines.append(f"- **{issue['severity']} / {issue['kind']} / {issue['task']}**: `{issue['detail']}`")
    md_path.write_text("\n".join(lines) + "\n")


if __name__ == "__main__":
    main()
