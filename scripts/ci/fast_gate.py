#!/usr/bin/env python3
"""Fast gate in shadow: select the test files a change needs, run them, compare verdicts.

Three commands, one per workflow step of the advisory ``fast-gate (shadow)`` job:

``select``  lists the changed paths (fork point to head, as the detector does
            for a pull request) and maps each one to test files through
            ``tests/OWNERS.toml``:

            * a changed test file selects itself;
            * a change under a source unit selects the test files that import
              the unit;
            * any other path, and a source path with no unit in the file, falls
              back to every test file of the domains the detector gives it
              (``scripts/ci/detect_touched_domains.py``), which for source is the
              full matrix;
            * a path the detector routes to no domain selects nothing.

``run``     runs the selected files in one pytest process and records the
            outcome and the time it took.

``agree``   records the fast gate's verdict beside the domain matrix's verdict
            for the same head, as JSON and as a job summary.
"""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time
import tomllib
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts/ci"))

import detect_touched_domains as detector  # noqa: E402
from generate_owners import OUTPUT as OWNERS_PATH, expand, unit_of  # noqa: E402

TEST_NAME = re.compile(r"(?:^|/)(?:test_[^/]*|[^/]*_test)\.py$")
LISTED = 200  # paths listed in the JSON record; counts are always complete


def load_owners(text: str) -> dict:
    owners = tomllib.loads(text)
    if owners.get("version") != 1:
        raise ValueError("unsupported OWNERS.toml version")
    return owners


def source_unit(path: str, sources: dict) -> str | None:
    """The unit in OWNERS.toml for a source path, or None when it has no row.

    A data file in a folder no module imports (``x/data/table.yml``) is read by
    the package around it, so it takes the package folder's row. A Python file
    with no row of its own has no static importer and falls back."""
    unit = unit_of(path)
    if unit in sources:
        return unit
    package = "/".join(path.split("/")[:3]) + "/"
    if not path.endswith(".py") and unit != package and package in sources:
        return package
    return None


def select(changed: list[str], owners: dict, domains: list[detector.Domain], root: Path = ROOT) -> dict:
    """Map changed paths to test files. Every path gets a recorded reason."""
    shards = {name: set(entry["tests"]) for name, entry in owners["shards"].items()}
    universe = set().union(*shards.values()) if shards else set()
    sources = owners.get("sources", {})
    selected: set[str] = set()
    reasons: dict[str, str] = {}
    fallback: dict[str, list[str]] = {}

    def domain_fallback(path: str, why: str) -> None:
        names = [domain.name for domain in detector.touched_domains([path], domains)]
        if not names:
            reasons[path] = "no domain owns this path"
            return
        missing = [name for name in names if name not in shards]
        if missing:
            # A domain without a row cannot be expanded; select everything.
            names = sorted(shards)
        for name in names:
            selected.update(shards[name])
        fallback[path] = names
        reasons[path] = f"{why}: every test file of {', '.join(names)}"

    for raw in changed:
        path = detector.normalize_root(raw)
        if detector.is_no_domain_path(path):
            reasons[path] = "routed outside the domain matrix"
        elif path in universe:
            selected.add(path)
            reasons[path] = "changed test file"
        elif path.startswith("tests/") and TEST_NAME.search(path) and not (root / path).exists():
            reasons[path] = "deleted test file"
        elif path.startswith("src/digitalmodel/") and detector.package_document_domains(path, domains) is None:
            unit = source_unit(path, sources)
            if unit is None:
                domain_fallback(path, "source path with no unit in OWNERS.toml")
            else:
                found = expand(sources[unit]["tests"], universe)
                selected.update(found)
                reasons[path] = f"source unit {unit}: {len(found)} test files"
        else:
            domain_fallback(path, "path with no OWNERS.toml row")

    involved = sorted(name for name, tests in shards.items() if tests & selected)
    extras = sorted({extra for name in involved for extra in owners["shards"][name].get("extras", [])})
    return {
        "changed": sorted(reasons),
        "reasons": reasons,
        "fallback": fallback,
        "selected": sorted(selected),
        "shards": involved,
        "extras": extras,
        "node": "contracts" in involved,
        "gmsh": bool({"solver-smoke", "specialized"} & set(involved)),
    }


def git(*args: str) -> str:
    return subprocess.run(["git", *args], check=True, capture_output=True, text=True).stdout


def changed_files(base: str, head: str) -> list[str]:
    start = git("merge-base", base, head).strip()
    return [path for path in git("diff", "--name-only", "--no-renames", "-z", start, head).split("\0") if path]


def owners_text(root: Path) -> tuple[str, bool]:
    """The committed OWNERS.toml, or a fresh generation when it is stale.

    The shadow measures the gate as it would run if required, when the
    generator's --check would keep the file current."""
    from generate_owners import generate

    fresh = generate(root)
    committed = root / OWNERS_PATH
    current = committed.is_file() and committed.read_text(encoding="utf-8") == fresh
    return fresh, current


def write_outputs(values: dict) -> None:
    target = os.environ.get("GITHUB_OUTPUT")
    if not target:
        return
    with open(target, "a", encoding="utf-8") as handle:
        for key, value in values.items():
            handle.write(f"{key}={value}\n")


def command_select(args: argparse.Namespace) -> int:
    text, current = owners_text(ROOT)
    domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
    changed = changed_files(args.base, args.head)
    result = select(changed, load_owners(text), domains)
    result.update(base=args.base, head=args.head, owners_current=current)
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    write_outputs({
        "count": len(result["selected"]),
        "extras": ",".join(["test", *[e for e in result["extras"] if e != "test"]]),
        "node": str(result["node"]).lower(),
        "gmsh": str(result["gmsh"]).lower(),
    })
    print(f"{len(changed)} changed paths -> {len(result['selected'])} test files "
          f"in {len(result['shards'])} shards ({', '.join(result['shards']) or 'none'})")
    if not current:
        print("tests/OWNERS.toml is stale; selected from a fresh generation")
    return 0


def junit_counts(path: Path) -> dict:
    counts = {"tests": 0, "failures": 0, "errors": 0, "skipped": 0}
    try:
        tree = ET.parse(path)
    except (OSError, ET.ParseError):
        return counts
    for suite in tree.iter("testsuite"):
        for key in counts:
            counts[key] += int(suite.get(key, 0))
    return counts


def verdict_of(exit_code: int | None, selected: int) -> str:
    if selected == 0:
        return "pass"  # nothing to run, as an empty domain matrix passes
    if exit_code in (0, 5):
        return "pass"  # 5: the files collected no test
    return "fail"


def command_run(args: argparse.Namespace) -> int:
    selection = json.loads(args.selection.read_text(encoding="utf-8"))
    files = selection["selected"]
    started = time.monotonic()
    code = None
    if files:
        command = [sys.executable, "-m", "pytest", *files, "-rfE", "-p", "no:randomly", "-p", "no:sugar",
                   "--no-header", "-q", "--tb=line", f"--junitxml={args.junit}", "-o", "junit_family=xunit2"]
        code = subprocess.run(command, cwd=ROOT).returncode
    seconds = round(time.monotonic() - started, 1)
    result = {
        "selected_count": len(files),
        "shards": selection["shards"],
        "exit_code": code,
        "duration_seconds": seconds,
        "verdict": verdict_of(code, len(files)),
        "junit": junit_counts(args.junit),
    }
    args.out.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(f"fast gate (shadow): {result['verdict']} in {seconds} s over {len(files)} test files")
    return 0  # advisory: the verdict is recorded, never enforced


def matrix_verdict(result: str, has_domains: str) -> str:
    if result == "success":
        return "pass"
    if result == "failure":
        return "fail"
    if result == "skipped" and has_domains == "false":
        return "pass"  # no shard selected; the aggregate passes
    return "unknown"


def agreement(fast: str, matrix: str) -> dict:
    known = {"pass", "fail"}
    return {
        "pair": f"{fast}/{matrix}",
        "agree": fast == matrix if fast in known and matrix in known else None,
    }


def read_json(path: Path | None) -> dict:
    try:
        return json.loads(path.read_text(encoding="utf-8")) if path else {}
    except (OSError, ValueError):
        return {}


def command_agree(args: argparse.Namespace) -> int:
    selection = read_json(args.selection)
    result = read_json(args.result)
    fast = result.get("verdict", "unknown") if args.event == "pull_request" else "not-run"
    matrix = matrix_verdict(args.matrix_result, args.has_domains)
    try:
        matrix_domains = [item["domain"] for item in json.loads(args.matrix or "{}").get("include", [])]
    except (ValueError, AttributeError, TypeError):
        matrix_domains = []
    record = {
        "schema": 1,
        "event": args.event,
        "pull_request": args.pull_request,
        "head_sha": args.head_sha,
        "base_tip": selection.get("base"),
        "run_id": args.run_id,
        "run_attempt": args.run_attempt,
        "owners_current": selection.get("owners_current"),
        "changed_count": len(selection.get("changed", [])),
        "changed": selection.get("changed", [])[:LISTED],
        "fallback": selection.get("fallback", {}),
        "selected_count": len(selection.get("selected", [])),
        "selected": selection.get("selected", [])[:LISTED],
        "fast_gate": {
            "job_result": args.fast_result,
            "verdict": fast,
            "shards": result.get("shards", selection.get("shards", [])),
            "duration_seconds": result.get("duration_seconds"),
            "exit_code": result.get("exit_code"),
            "junit": result.get("junit"),
        },
        "domain_matrix": {"job_result": args.matrix_result, "verdict": matrix, "shards": matrix_domains},
        **agreement(fast, matrix),
    }
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(record, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    lines = [
        "## fast-gate (shadow) agreement",
        "",
        "Advisory record for decision A3; this check is not required and gates nothing.",
        "",
        "| | Verdict | Shards | Test files | Time |",
        "| --- | --- | --- | --- | --- |",
        f"| fast gate | {fast} | {len(record['fast_gate']['shards'])} | {record['selected_count']} "
        f"| {record['fast_gate']['duration_seconds'] if record['fast_gate']['duration_seconds'] is not None else '-'} s |",
        f"| domain matrix | {matrix} | {len(matrix_domains)} | - | - |",
        "",
        f"Agreement: **{record['pair']}** ({'agree' if record['agree'] else 'disagree' if record['agree'] is False else 'not comparable'}).",
    ]
    if record["fallback"]:
        lines += ["", f"{len(record['fallback'])} changed paths fell back to whole domains."]
    if record["owners_current"] is False:
        lines += ["", "tests/OWNERS.toml was stale; the selection used a fresh generation."]
    if args.summary:
        with open(args.summary, "a", encoding="utf-8") as handle:
            handle.write("\n".join(lines) + "\n")
    print("\n".join(lines))
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    commands = parser.add_subparsers(dest="command", required=True)
    chooser = commands.add_parser("select")
    chooser.add_argument("--base", required=True)
    chooser.add_argument("--head", required=True)
    chooser.add_argument("--out", type=Path, required=True)
    runner = commands.add_parser("run")
    runner.add_argument("--selection", type=Path, required=True)
    runner.add_argument("--junit", type=Path, required=True)
    runner.add_argument("--out", type=Path, required=True)
    judge = commands.add_parser("agree")
    judge.add_argument("--selection", type=Path)
    judge.add_argument("--result", type=Path)
    judge.add_argument("--event", required=True)
    judge.add_argument("--pull-request", default="")
    judge.add_argument("--head-sha", default="")
    judge.add_argument("--run-id", default="")
    judge.add_argument("--run-attempt", default="")
    judge.add_argument("--fast-result", default="")
    judge.add_argument("--matrix-result", default="")
    judge.add_argument("--has-domains", default="")
    judge.add_argument("--matrix", default="")
    judge.add_argument("--out", type=Path, required=True)
    judge.add_argument("--summary")
    args = parser.parse_args(argv)
    return {"select": command_select, "run": command_run, "agree": command_agree}[args.command](args)


if __name__ == "__main__":
    raise SystemExit(main())
