#!/usr/bin/env python3
# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Replay candidate selection against historical ownership and saved GitHub job evidence.

The evidence directory holds push-runs.tsv and sweep-runs.tsv (SHA, run, name,
conclusion, attempt), jobs/ and sweep-jobs/ (SHA-run-attempt.json job lists),
and failed-reports.json and sweep-failed-reports.json (result and JUnit summaries).
"verified" counts shard membership with complete observed outcomes. "with_gate_operands"
also requires a pytest operand for each changed test. Either shortfall exits 1.
"""

import argparse
import fnmatch
import json
import re
import shlex
from collections import defaultdict
from functools import cache
from pathlib import Path

import yaml

import detect_touched_domains as detector
from measure_shard_selection import git


@cache
def ownership(ref):
    """Declared owners and pytest operands at this revision, including workflow additions."""
    groups = {}
    for line in git("show", f"{ref}:tests/DOMAINS.md").splitlines():
        cells = line.strip().strip("|").split("|")
        if len(cells) > 1 and (roots := detector.ROOT_RE.findall(cells[1])):
            groups[cells[0].strip()] = set(roots)
    gates = yaml.safe_load(git("show", f"{ref}:.claude/quality-gates.yaml"))["gates"]
    workflow = git("show", f"{ref}:.github/workflows/quality-gates-by-domain.yml")
    extra = re.findall(r'if os.environ\["GATE_NAME"\] == "tests-([^"]+)":\s+command \+= "([^"]+)"', workflow)
    commands = {name: gates["tests-" + name]["command"] for name in groups}
    for name, operands in extra:
        commands[name] += operands
    operands = {}
    for name, command in commands.items():
        args = shlex.split(command)
        roots = {arg for arg in args if arg.startswith("tests/")}
        ignored = {arg.removeprefix("--ignore=") for arg in args if arg.startswith("--ignore=")}
        groups[name].update(roots)
        operands[name] = (roots, ignored)
    return groups, operands


def under_root(path, spec):
    root = Path(spec)
    prefix = path.parts[:len(root.parts)]
    return (len(prefix) == len(root.parts)
            and all(fnmatch.fnmatchcase(a, b) for a, b in zip(prefix, root.parts)))


def owns(path, roots):
    return any(under_root(path, root) or
               (path.name == "conftest.py" and Path(root).is_relative_to(path.parent))
               for root in roots)


def changed_owners(paths, parent, sha):
    owners, unowned, without_operand = set(), [], []
    for path in map(Path, paths):
        if path.parts[0] != "tests" or path.suffix != ".py":
            continue
        matches = set()
        has_operand = False
        for ref in (parent, sha):
            groups, commands = ownership(ref)
            matches.update(shard for shard, roots in groups.items() if owns(path, roots))
            has_operand |= any(owns(path, roots) and not any(under_root(path, r) for r in ignored)
                               for roots, ignored in commands.values())
        if path.name.startswith("test_") or path.name.endswith("_test.py") or path.name == "conftest.py":
            if not matches:
                unowned.append(str(path))
            elif not has_operand:
                without_operand.append(str(path))
        owners.update(matches)
    return sorted(owners), unowned, without_operand


def run_attempt(run, attempt):
    return run if int(attempt) == 1 else f"{run}/{attempt}"


def failure_reports(evidence):
    """Index only parsed, SHA/run-bound failures with complete JUnit counts."""
    reports = {}
    for filename in ("failed-reports.json", "sweep-failed-reports.json"):
        for row in json.loads((evidence / filename).read_text()):
            result = row.get("result") or {}
            key = (row["sha"], row["run"], row["shard"])
            if (key != (result.get("sha"), result.get("run_id"), result.get("shard"))
                    or not result.get("parsed") or result.get("conclusion") != "failure"
                    or result.get("attempt") != int(row["attempt"])
                    or row.get("nnodes") != result.get("collected") or not row.get("nnodes")):
                continue
            nodes = {node: state for state in ("failed", "errored") for node in result[state]}
            if nodes and len(nodes) == row.get("nfailed"):
                reports[row["sha"], run_attempt(row["run"], row["attempt"]), row["shard"]] = nodes
    return reports


def assess(sha, parent, *, selected, owners, unowned, problems, failures, reports, reviewed):
    required = set(owners)
    issues = list(problems)
    baseline = []
    for run, shard in failures:
        nodes = reports.get((sha, run, shard), {})
        if not nodes:
            issues.append(f"missing failure report for {shard} in {run}")
        parent_run = reviewed.get((run, shard))
        previous = reports.get((parent, parent_run, shard), {})
        shared = sorted(node for node in nodes if nodes[node] == previous.get(node))
        if shared:
            baseline.append(dict(shard=shard, run=run, parent_run=parent_run, nodes=shared))
        if not nodes or len(shared) != len(nodes):
            required.add(shard)
    missing = sorted(required - set(selected))
    return dict(sha=sha, owners=owners, selected=sorted(selected), missing=missing,
                failed=sorted({shard for _, shard in failures}), baseline=baseline,
                unowned=unowned, issues=issues, verified=not (issues or unowned or missing))


def run_index(path):
    index = defaultdict(list)
    for line in path.read_text().splitlines():
        sha, run, _, _, attempt = line.split("\t")
        index[sha].extend((run, i) for i in range(1, int(attempt) + 1))
    return index


def outcomes(evidence, sha, domain_runs, sweep_runs):
    issues, failures, runs, other_failures = [], [], [], []
    workflow = yaml.safe_load(git("show", f"{sha}:.github/workflows/full-matrix-sweep.yml"))
    expects_sweep = "push" in workflow.get("on", workflow.get(True, {}))
    groups, _ = ownership(sha)
    expected = set(groups)
    for folder, attempts, prefix, detect, aggregate, required in (
        ("jobs", domain_runs, "tests-", "Detect touched test domains", "Domain test aggregate", True),
        ("sweep-jobs", sweep_runs, "sweep-tests-", "Build full shard matrix", "Full matrix sweep result", expects_sweep),
    ):
        if required and not attempts:
            issues.append(f"missing {folder} run")
        for run, attempt in attempts:
            label = run_attempt(run, attempt)
            runs.append(label)
            path = evidence / folder / f"{sha}-{run}-{attempt}.json"
            jobs = json.loads(path.read_text()) if path.exists() else []
            if not jobs or any((j["head_sha"], str(j["run_id"]), j["run_attempt"]) !=
                               (sha, run, attempt) for j in jobs):
                issues.append(f"missing or mismatched jobs in {run}")
                continue
            states = {j["name"]: j["conclusion"] for j in jobs}
            shards = {n[len(prefix):]: state for n, state in states.items() if n.startswith(prefix)}
            if (states.get(aggregate) not in ("success", "failure") or states.get(detect) != "success"
                    or not shards or not shards.keys() <= expected
                    or (folder == "sweep-jobs" and shards.keys() != expected)
                    or any(s not in ("success", "failure") for s in shards.values())):
                issues.append(f"incomplete matrix in {run}")
            failures.extend((label, n) for n, state in shards.items() if state == "failure")
            other_failures.extend(f"{label} {n}" for n, state in states.items()
                                  if state == "failure" and not n.startswith(prefix))
    return issues, failures, runs, other_failures


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--evidence", type=Path, required=True)
    parser.add_argument("--base-ref", required=True)
    parser.add_argument("--merges", type=int, default=50)
    parser.add_argument("--baseline", action="append", default=[], metavar="SHARD:RUN:PARENT_RUN",
                        help="compare failed nodes only for a reviewed shard and run pair")
    args = parser.parse_args()
    if args.merges < 1:
        parser.error("--merges must be positive")
    reviewed = {}
    for pair in args.baseline:
        shard, run, parent_run = pair.split(":")
        reviewed[run, shard] = parent_run
    reports = failure_reports(args.evidence)
    domains = detector.parse_domains(Path("tests/DOMAINS.md"))
    table = detector.load_src_routes()
    push = run_index(args.evidence / "push-runs.tsv")
    sweep = run_index(args.evidence / "sweep-runs.tsv")

    @cache
    def pins_at(sha):
        return detector.git_pinned_domains(sha, domains)

    rows = []
    for sha in git("rev-list", "--first-parent", "-n", str(args.merges), args.base_ref).split():
        parent = git("rev-parse", sha + "^1")
        paths = detector.git_changed_files(parent, sha)
        pins = defaultdict(set)
        for ref in (parent, sha):
            for path, names in pins_at(ref).items():
                pins[path].update(names)
        selected = detector.touched_domains(paths, domains, parent, sha, pinned_domains=pins,
                                             src_routes=table)
        owners, unowned, without_operand = changed_owners(paths, parent, sha)
        issues, failures, runs, other_failures = outcomes(args.evidence, sha, push[sha], sweep[sha])
        result = assess(sha, parent, selected=[d.name for d in selected], owners=owners,
                        unowned=unowned, problems=issues, failures=failures, reports=reports,
                        reviewed=reviewed)
        rows.append(dict(result, runs=runs, other_failures=other_failures,
                         declared_only=without_operand))
    verified = sum(row["verified"] for row in rows)
    with_operands = sum(row["verified"] and not row["declared_only"] for row in rows)
    print(json.dumps(dict(verified=verified, with_gate_operands=with_operands,
                          requested=args.merges, merges=rows), indent=2))
    return int(with_operands != args.merges)


if __name__ == "__main__":
    raise SystemExit(main())
