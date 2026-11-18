"""Collect and compare test and line coverage evidence at a fixed revision."""

import argparse
import ast
from collections import Counter, defaultdict
from importlib.util import find_spec
import json
import os
from pathlib import Path
import platform
import re
import subprocess
import sys
import tempfile
from xml.etree import ElementTree

if __package__:
    from .ci_baseline import junit_id, write_json
else:
    from ci_baseline import junit_id, write_json


def tree_record(overlay_paths):
    root = Path(subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).rstrip("\n"))
    status = subprocess.check_output(["git", "status", "--porcelain=v1", "-z", "--untracked-files=all",
                                      "--no-renames", "--ignore-submodules=none"], cwd=root)
    # With renames disabled each NUL-delimited entry is "XY path", relative to the root.
    changed = sorted(os.fsdecode(entry[3:]) for entry in status.split(b"\0") if entry)
    overlay = {}
    for name in overlay_paths:
        path = Path(name)
        if (path.is_absolute() or ".." in path.parts or not (root / path).is_file()
                or (root / path).resolve() != root / path):
            raise ValueError(f"expected a repository-relative regular file for --overlay: {name}")
        blob = subprocess.check_output(["git", "hash-object", "--", path], cwd=root, text=True).strip()
        overlay[path.as_posix()] = blob
    return dict(clean=not changed, changed_paths=changed, overlay=overlay)


def collect(args):
    if args.env in ("", ".", "..") or Path(args.env).name != args.env:
        raise ValueError("expected a single environment name")
    if args.pytest[:1] != ["--"]:
        raise ValueError("supply pytest arguments after --")
    cov_plugin = find_spec("pytest_cov") is not None
    if not cov_plugin and find_spec("coverage") is None:
        raise ValueError("install pytest-cov or coverage")
    sha = subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip()
    tree = tree_record(args.overlay)
    dest = (args.out / args.env).resolve()
    dest.mkdir(parents=True, exist_ok=False)
    xml, cov = dest / "junit.xml", dest / "coverage.json"
    py = [sys.executable, "-m"]
    pytest_args = [*args.pytest[1:], "-o", "junit_family=xunit1", f"--junitxml={xml}"]
    if cov_plugin:
        if not any(arg == "--cov" or arg.startswith("--cov=") for arg in pytest_args):
            pytest_args.append("--cov")
        command = py + ["pytest", *pytest_args, f"--cov-report=json:{cov}", "--cov-fail-under=0"]
    else:
        command = py + ["coverage", "run", "--parallel-mode", "-m", "pytest", *pytest_args]
    record = dict(commit=sha, **tree, command=command, python_version=platform.python_version(),
                  obligation=args.obligation, pytest_exit_code=None, complete=False)
    write_json(dest / "env.json", record)
    with tempfile.TemporaryDirectory(dir=dest) as tmp:
        env = dict(os.environ, COVERAGE_FILE=str(Path(tmp) / ".coverage"))
        try:
            code = subprocess.run(command, env=env).returncode
        except KeyboardInterrupt:
            code = 2
        record["pytest_exit_code"] = code
        write_json(dest / "env.json", record)
        if not cov_plugin and code in (0, 1):
            subprocess.run(py + ["coverage", "combine", tmp], env=env, check=True)
            subprocess.run(py + ["coverage", "json", "--fail-under=0", "-o", cov], env=env, check=True)
        record["complete"] = code in (0, 1) and xml.is_file() and cov.is_file()
    write_json(dest / "env.json", record)
    return code if code else int(not record["complete"])


def string_list(value):
    return isinstance(value, list) and all(isinstance(s, str) and s for s in value)


def tree_matches_record(meta):
    changed = meta.get("changed_paths", [])
    if meta.get("clean") is True:
        return changed == []
    overlay = meta.get("overlay")
    return (meta.get("clean") is False and string_list(changed) and bool(changed)
            and isinstance(overlay, dict) and set(changed) <= overlay.keys()
            and all(isinstance(blob, str) and re.fullmatch(r"[0-9a-f]{40}|[0-9a-f]{64}", blob)
                    for blob in overlay.values()))


def read_json(path):
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except ValueError as exc:
        raise ValueError(f"{path}: {exc}") from exc


def names(path):
    if path is None:
        return set()
    value = read_json(path)
    if not string_list(value):
        raise ValueError(f"{path}: expected a JSON string list")
    return set(value)


def sample(values):
    values = sorted(values)
    tail = f" (+{len(values) - 12} cut)" if len(values) > 12 else ""
    return ", ".join(f"`{value}`" for value in values[:12]) + tail


def read_nodes(path, issues, side):
    nodes = {}
    if path.stat().st_size == 0:
        issues["empty baseline" if side == "before" else "empty after run"].add(str(path))
        return nodes
    root = ElementTree.parse(path).getroot()
    if root.tag not in ("testsuite", "testsuites"):
        raise ValueError(f"{path}: expected JUnit")
    for suite in root.iter("testsuite"):
        cases = suite.findall("testcase")
        counts = Counter(t.tag for case in cases for t in case)
        for key, n in [("tests", len(cases)), ("errors", counts["error"]),
                       ("failures", counts["failure"]), ("skipped", counts["skipped"])]:
            if suite.get(key) != str(n):
                raise ValueError(f"{path}: JUnit {key} total disagrees")
        for case in cases:
            tags = [t.tag for t in case if t.tag not in ("system-out", "system-err", "properties")]
            if tags not in ([], ["skipped"], ["error"], ["failure"]):
                raise ValueError(f"{path}: ambiguous testcase outcome {tags}")
            skip, error = case.find("skipped"), case.find("error")
            if skip is not None and skip.get("message") == "collection skipped" and case.get("file"):
                # pytest serializes (checkout-specific filename, line, reason).
                node = case.attrib["file"]
                try:
                    _, _, reason = ast.literal_eval(skip.text or "")
                except (SyntaxError, TypeError, ValueError) as exc:
                    raise ValueError(f"{path}: invalid collection skip for {node}") from exc
            else:
                node = junit_id(case)
                reason = (skip.get("message") or (skip.text or "").strip()) if skip is not None else ""
            if node in nodes:
                raise ValueError(f"{path}: duplicate node {node}")
            if error is not None and error.get("message") == "collection failure":
                issues["collection error"].add(node)
            if skip is not None and (not isinstance(reason, str) or not reason):
                raise ValueError(f"{path}: missing skip reason for {node}")
            state = "skipped" if skip is not None else "failed" if tags else "passed"
            nodes[node] = (state, reason)
    if len(nodes) != sum(1 for _ in root.iter("testcase")):
        raise ValueError(f"{path}: testcase outside a suite")
    if not nodes:
        issues["empty baseline" if side == "before" else "empty after run"].add(str(path))
    return nodes


def evidence(folder, side, args, required, issues):
    if not folder.is_dir():
        issues["missing environment"].add(f"{side}/{folder.name}")
        return None
    record, xml, cov = (folder / f for f in ("env.json", "junit.xml", "coverage.json"))
    for path in (record, xml, cov):
        if not path.is_file():
            issues["missing commit record" if path == record else "missing report file"].add(str(path))
    if not all(path.is_file() for path in (record, xml, cov)):
        return None
    meta = read_json(record)
    if not isinstance(meta, dict):
        raise ValueError(f"{record}: expected environment record")
    if meta.get("commit") != args.commit:
        issues["commit mismatch"].add(str(record))
    if not tree_matches_record(meta):
        issues["dirty worktree"].add(str(record))
    code = meta.get("pytest_exit_code")
    if meta.get("complete") is not True or type(code) is not int or code not in (0, 1):
        issues["incomplete run"].add(str(record))
    if (meta.get("obligation") not in ("pre-merge", "daily")
            or not isinstance(meta.get("python_version"), str) or not meta["python_version"]
            or not string_list(meta.get("command")) or not meta["command"]):
        raise ValueError(f"{record}: invalid environment record")
    nodes = read_nodes(xml, issues, side)
    if code in (0, 1) and bool(code) != any(state == "failed" for state, _ in nodes.values()):
        issues["pytest exit disagrees with XML"].add(str(xml))
    coverage = read_json(cov)
    files = coverage.get("files") if isinstance(coverage, dict) else None
    if not isinstance(files, dict):
        raise ValueError(f"{cov}: expected coverage files")
    if not files:
        issues["empty coverage"].add(str(cov))
    for module, data in files.items():
        lines = data.get("executed_lines") if isinstance(data, dict) else None
        if not isinstance(lines, list) or any(type(n) is not int or n < 1 for n in lines):
            raise ValueError(f"{cov}: invalid executed_lines for {module}")
    issues["missing required module"].update(f"{side}: {m}" for m in required - files.keys())
    return meta, nodes, files


def compare_env(args, env, required, touched, mapping, paths, issues, notes):
    before, after = (evidence(getattr(args, side) / env, side, args, required | touched, issues)
                     for side in ("before", "after"))
    if not before or not after:
        sizes = [len(run[1]) if run else "?" for run in (before, after)]
        return [env, *sizes, *(["?"] * 7)]
    old_meta, old, old_files = before
    meta, new, files = after
    for field, reason in [("obligation", "obligation changed"), ("python_version", "Python version changed")]:
        if old_meta[field] != meta[field]:
            issues[reason].add(env)
    lost = old.keys() - new.keys()
    passed = {n for n, (state, _) in new.items() if state == "passed"}
    failed = {n for n, (state, _) in new.items() if state == "failed"}
    old_failed = {n for n, (state, _) in old.items() if state == "failed"}
    skipped = new.keys() - passed - failed
    changed = {n for n in skipped & old.keys() if old[n][0] == "skipped" and old[n] != new[n]}
    mapped = {n for n in lost if mapping.get(n) in passed}
    issues["missing passing replacement" if args.kind == "removal" else "dropped node"].update(lost - mapped)
    issues["hidden baseline failure"].update(old_failed - passed - failed - mapped)
    issues["new failure"].update(failed - old_failed)
    issues["passing node now skipped"].update(n for n in skipped & old.keys() if old[n][0] == "passed")
    issues["skip reason changed"].update(changed)
    closure, pr, nightly = paths
    if not args.closure and "pre-merge" in (old_meta["obligation"], meta["obligation"]):
        notes.append(f"{env}: pre-merge closure: not checked (no closure supplied)")
        if args.kind == "consolidation":
            issues["pre-merge closure not checked"].add(env)
    if old_meta["obligation"] == "pre-merge":
        replacements = {n for n in mapped if mapping[n] in pr}
        missing = closure - (pr & new.keys()) - replacements
        issues["pre-merge node moved to nightly"].update(missing & nightly)
        issues["missing pre-merge closure node"].update(missing - nightly)
    elif args.closure:
        issues["expected pre-merge evidence"].add(env)
    line_counts = [f"{m}: ?" for m in sorted(touched - (old_files.keys() & files.keys()))]
    for module in sorted(touched & old_files.keys() & files.keys()):
        lines = set(old_files[module]["executed_lines"]) - set(files[module]["executed_lines"])
        line_counts.append(f"{module}: {len(lines)}")
        if lines:
            issues["lost covered lines"].add(f"{module}: {sample(lines)}")
    added = new.keys() - old.keys()
    if added:
        notes.append(f"Added nodes in {env} ({len(added)}): {sample(added)}")
    return [env, len(old), len(new), len(passed), len(failed), len(skipped), len(lost),
            len(failed - old_failed), len(changed), ", ".join(line_counts) or "—"]


def compare(args):
    issues, notes, rows = defaultdict(set), [], []
    code = 1
    try:
        if not re.fullmatch(r"[0-9a-f]{40}|[0-9a-f]{64}", args.commit):
            raise ValueError("expected a full git commit SHA")
        closure, pr, nightly = (names(p) for p in (args.closure, args.pr_path_nodes, args.nightly_path_nodes))
        if args.kind == "selection":
            if not args.closure or any((args.pr_path_nodes, args.nightly_path_nodes, args.mapping,
                                        args.required_envs, args.required_modules, args.touched_modules)):
                raise ValueError("selection requires shard lists for before, after and closure")
            before, after = (names(p / "selected-shards.json" if p.is_dir() else p)
                             for p in (args.before, args.after))
            if not before:
                issues["empty baseline"].add(str(args.before))
            issues["missing closure shard"].update(closure - after)
            savings = before - after - closure
            notes.append(f"Shards dropped outside the closure (intended saving, {len(savings)}): {sample(savings)}")
        else:
            if args.touched_modules is None:
                raise ValueError(f"--touched-modules is required for {args.kind}")
            paths = (args.closure, args.pr_path_nodes, args.nightly_path_nodes)
            if any(paths) and not all(paths):
                raise ValueError("supply closure, pr-path-nodes and nightly-path-nodes together")
            if bool(args.mapping) != (args.kind == "removal"):
                raise ValueError("--mapping is required exclusively for removal")
            mapping = read_json(args.mapping) if args.mapping else {}
            if not isinstance(mapping, dict) or not string_list(list(mapping.values())) or "" in mapping:
                raise ValueError("expected mapping of node IDs to node IDs")
            required, touched = names(args.required_modules), names(args.touched_modules)
            if not touched:
                notes.append("line check: no touched modules")
            envs = names(args.required_envs)
            for folder in (args.before, args.after):
                envs.update(p.name for p in folder.iterdir() if p.is_dir())
            if not envs:
                issues["empty baseline"].add(str(args.before))
            for env in sorted(envs):
                if env in (".", "..") or Path(env).name != env:
                    raise ValueError(f"invalid environment name {env}")
                local = defaultdict(set)
                try:
                    rows.append(compare_env(args, env, required, touched, mapping, (closure, pr, nightly), local, notes))
                except (OSError, ValueError, KeyError, TypeError, ElementTree.ParseError) as exc:
                    local["invalid input"].add(str(exc))
                    rows.append([env, *(["?"] * 9)])
                    code = 2
                for reason, details in local.items():
                    issues[reason].update(f"{env}: {d}" for d in details)
    except (OSError, ValueError, KeyError, TypeError, ElementTree.ParseError) as exc:
        issues["invalid input"].add(str(exc))
        code = 2
    reasons = sorted(reason for reason, values in issues.items() if values)
    verdict = f"FAIL ({', '.join(reasons)})" if reasons else "PASS"
    lines = [f"Coverage gate at {args.commit[:8]}: {verdict}", ""]
    if args.kind != "selection":
        rows.insert(0, ["Environment", "Nodes before", "Nodes after", "Passed", "Failed", "Skipped",
                        "Lost nodes", "New failures", "Skip-reason changes", "Lost lines per touched module"])
        rows.insert(1, ["---"] * len(rows[0]))
        lines.extend("| " + " | ".join(str(cell).replace("|", "\\|") for cell in row) + " |" for row in rows)
    lines.extend(["", *(f"- {r}: {sample(issues[r])}" for r in reasons), *notes])
    args.report.write_text("\n".join(lines).rstrip() + "\n")
    return code if reasons else 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="action", required=True)
    run = commands.add_parser("collect")
    run.add_argument("--env", required=True)
    run.add_argument("--out", required=True, type=Path)
    run.add_argument("--overlay", nargs="+", action="extend", default=[], metavar="PATH",
                     help="repository-relative files deliberately overlaid on the base commit")
    run.add_argument("--obligation", choices=["pre-merge", "daily"], default="pre-merge")
    run.add_argument("pytest", nargs=argparse.REMAINDER)
    diff = commands.add_parser("compare")
    for name in ("before", "after", "report", "required-envs", "required-modules", "touched-modules",
                 "closure", "pr-path-nodes", "nightly-path-nodes", "mapping"):
        diff.add_argument("--" + name, type=Path, required=name in ("before", "after", "report"))
    diff.add_argument("--commit", required=True)
    diff.add_argument("--kind", choices=["selection", "consolidation", "removal"], required=True)
    try:
        args = parser.parse_args()
    except SystemExit as exc:
        # Replace a previous verdict even if argparse rejects the comparison.
        if exc.code == 2 and sys.argv[1:2] == ["compare"]:
            locator = argparse.ArgumentParser(add_help=False, allow_abbrev=False)
            locator.add_argument("--report", type=Path)
            locator.add_argument("--commit", default="unknown")
            known, _ = locator.parse_known_args(sys.argv[2:])
            if known.report:
                try:
                    known.report.write_text(f"Coverage gate at {known.commit[:8]}: FAIL (invalid usage)\n")
                except OSError as error:
                    parser.exit(2, f"{error}\n")
        raise
    try:
        return collect(args) if args.action == "collect" else compare(args)
    except (ValueError, OSError, subprocess.CalledProcessError) as exc:
        parser.exit(2, f"{exc}\n")


if __name__ == "__main__":
    sys.exit(main())
