# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Extract, publish and compare exact CI failures without masking shard results."""

from __future__ import annotations

import argparse
import io
import json
import re
import subprocess
import zipfile
from datetime import datetime, timezone
from pathlib import Path
from typing import Any
from xml.etree import ElementTree


def instant(value: str) -> datetime:
    """Parse an explicitly zoned observation timestamp."""
    parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    if parsed.tzinfo is None:
        raise ValueError("Timestamp has no timezone")
    return parsed


def read_json(path: Path) -> dict[str, Any]:
    """Read an object, rejecting absent or malformed evidence."""
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError("Expected a JSON object")
    return value


def write_json(path: Path, value: dict[str, Any]) -> None:
    """Write a deterministic, readable artifact."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def junit_id(case: ElementTree.Element) -> str:
    """Rebuild pytest node ids from xunit2 module and class attributes."""
    name = case.get("name", "")
    classname = case.get("classname", "")
    file = case.get("file", "")
    if not classname and name.endswith(".py"):
        return name
    error = case.find("error")
    if not classname and error is not None and error.get("message") == "collection failure":
        if not name or not all(part.isidentifier() for part in name.split(".")):
            raise ValueError("Ambiguous collection identity")
        return file or name.replace(".", "/") + ".py"
    parts = classname.split(".")
    if file:
        module = file.removesuffix(".py").replace("/", ".")
        if classname == module:
            classes = []
        elif classname.startswith(module + "."):
            classes = classname[len(module) + 1:].split(".")
        else:
            raise ValueError("File and classname disagree")
    else:
        positions = [i for i, part in enumerate(parts) if part.startswith("test_") or part.endswith("_test")]
        if not positions:
            raise ValueError("Cannot establish test module")
        end = positions[-1] + 1
        file, classes = "/".join(parts[:end]) + ".py", parts[end:]
    if not name or not file.endswith(".py"):
        raise ValueError("Incomplete node identity")
    return "::".join([file, *classes, name])


def parse_junit(path: Path) -> tuple[list[str], list[str], int]:
    """Read complete JUnit evidence and check declared failure totals."""
    root = ElementTree.parse(path).getroot()
    if root.tag not in {"testsuite", "testsuites"}:
        raise ValueError("Not JUnit")
    suites = [root] if root.tag == "testsuite" else list(root.iter("testsuite"))
    cases = list(root.iter("testcase"))
    failed, errored = [], []
    for case in cases:
        if case.find("failure") is not None:
            failed.append(junit_id(case))
        if case.find("error") is not None:
            errored.append(junit_id(case))
    for key, found in (("failures", len(failed)), ("errors", len(errored)), ("tests", len(cases))):
        if sum(int(suite.attrib[key]) for suite in suites) != found:
            raise ValueError("JUnit totals disagree")
    if not suites or not cases:
        raise ValueError("Empty JUnit")
    return sorted(set(failed)), sorted(set(errored)), len(cases)


def summary_id(text: str) -> str:
    """Separate a short-summary reason while preserving parameter whitespace."""
    depth = 0
    for index, char in enumerate(text):
        if depth == 0 and text[index:index + 3] == " - ":
            return text[:index]
        depth += (char == "[") - (char == "]")
    return text.rstrip()


def parse_log(path: Path) -> tuple[list[str], list[str], int]:
    """Read pytest short summaries from raw or timestamped hosted logs."""
    failed: set[str] = set()
    errored: set[str] = set()
    collected = 0
    for line in path.read_text(encoding="utf-8").splitlines():
        line = re.sub(r"\x1b\[[0-9;]*m", "", line)
        line = re.sub(r"^\d{4}-\d\d-\d\dT[\d:.]+Z ", "", line)
        match = re.match(r"^(FAILED|ERROR) (.+)$", line)
        if match:
            node = summary_id(match[2])
            if not re.match(r"[^\s]+\.py(?:::.*)?$", node):
                raise ValueError("Invalid summary identity")
            (failed if match[1] == "FAILED" else errored).add(node)
        count = re.search(r"collected (\d+) items?", line)
        if count:
            collected = int(count[1])
    if not failed and not errored:
        raise ValueError("No failure summary ids")
    return sorted(failed), sorted(errored), max(collected, len(failed | errored))


def extract(args: argparse.Namespace) -> None:
    """Persist extraction failure as explicit unparsed evidence."""
    row = dict(shard=args.shard, sha=args.sha, run_id=args.run, attempt=args.attempt,
               completed_at=args.completed_at, conclusion=args.conclusion,
               parsed=False, failed=[], errored=[], collected=0)
    try:
        parser = parse_junit if args.command == "extract" else parse_log
        failed, errored, collected = parser(args.source)
        row.update(parsed=True, failed=failed, errored=errored, collected=collected)
    except (OSError, ValueError, KeyError, ElementTree.ParseError):
        pass
    write_json(args.out, row)


def load_results(directory: Path) -> list[dict[str, Any]]:
    """Load all downloaded shard artifacts, rejecting duplicate shards."""
    rows = [read_json(path) for path in sorted(directory.rglob("result-*.json"))]
    if len({row["shard"] for row in rows}) != len(rows):
        raise ValueError("Duplicate shard observations")
    return rows


def publish(results: Path, previous: str, output: Path) -> None:
    """Replace observed shards while carrying absent or cancelled shards forward."""
    baseline = read_json(Path(previous)) if previous != "none" else {"tests": {}, "shards": {}}
    previous_tests = dict(baseline["tests"])
    for row in load_results(results):
        if row["conclusion"] == "cancelled":
            continue
        shard = row["shard"]
        metadata = {key: row[key] for key in ("sha", "run_id", "completed_at", "conclusion", "parsed")}
        baseline["shards"][shard] = metadata
        if row["parsed"] is not True:
            continue
        baseline["tests"] = {key: value for key, value in baseline["tests"].items() if value["shard"] != shard}
        for field, status in (("failed", "failed"), ("errored", "error")):
            for node in row[field]:
                old = previous_tests.get(node, {})
                baseline["tests"][node] = dict(
                    status=status, shard=shard, sha=row["sha"], run_id=row["run_id"],
                    completed_at=row["completed_at"],
                    first_seen_run=old.get("first_seen_run", old.get("run_id", row["run_id"])))
    write_json(output, baseline)



def classify(row: dict[str, Any], baseline: dict[str, Any], max_age: float, now: datetime) -> tuple[bool, str]:
    """Require fresh, parsed, same-shard main failures for every current id."""
    shard = baseline["shards"].get(row["shard"], {})
    ids = row["failed"] + row["errored"]
    if row["conclusion"] != "failure" or row["parsed"] is not True or not ids:
        return False, "failed shard has incomplete evidence"
    if shard.get("conclusion") != "failure" or shard.get("parsed") is not True:
        return False, "main shard is not a parsed failure"
    age = (now - instant(shard["completed_at"])).total_seconds() / 3600
    if not 0 <= age <= max_age:
        return False, "main shard is stale or future-dated"
    for node in ids:
        record = baseline["tests"].get(node, {})
        if record.get("status") not in {"failed", "error"} or record.get("shard") != row["shard"]:
            return False, "new or unproven test id"
        if any(record.get(key) != shard.get(key) for key in ("sha", "run_id", "completed_at")):
            return False, "test and shard observations disagree"
    return True, "all failing ids are pre-existing on main"


def compare_results(baseline_path: Path, results: Path, max_age: float, summary: Path,
                    verdict: Path, now: datetime | None = None) -> int:
    """Write comparison evidence; return one for every unprovable failure."""
    now = now or datetime.now(timezone.utc)
    report: dict[str, Any] = {"passed": False, "pre_existing": [], "shards": []}
    lines = ["## Main baseline comparison", "", "| Shard | Comparison | Main run |", "| --- | --- | --- |"]
    try:
        baseline = read_json(baseline_path)
        rows = load_results(results)
        if not rows:
            raise ValueError("No shard results")
        outcomes = []
        for row in rows:
            if row["conclusion"] == "success":
                continue
            ok, reason = classify(row, baseline, max_age, now)
            outcomes.append(ok)
            main = baseline["shards"].get(row["shard"], {})
            report["shards"].append(dict(shard=row["shard"], pre_existing=ok, reason=reason))
            lines.append(f"| {row['shard']} | {reason} | {main.get('run_id', 'unavailable')} |")
            if ok:
                for node in sorted(set(row["failed"] + row["errored"])):
                    report["pre_existing"].append(node)
                    message = f"pre-existing on main: {node} (main {main['sha']} run {main['run_id']}, see main-health issue)"
                    lines.append(f"- {message}")
                    escaped = message.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")
                    print(f"::warning::{escaped}")
        report["passed"] = all(outcomes)
    except (OSError, ValueError, KeyError, TypeError) as error:
        lines.append(f"\nComparison fails closed: {type(error).__name__}.")
    summary.write_text("\n".join(lines) + "\n", encoding="utf-8")
    write_json(verdict, report)
    return 0 if report["passed"] else 1


def gh_json(endpoint: str) -> dict[str, Any]:
    """Read authenticated workflow metadata through the hosted CLI."""
    value: dict[str, Any] = json.loads(
        subprocess.check_output(["gh", "api", endpoint], text=True)
    )
    return value


def download_baseline(repository: str, output: Path, require_latest: bool = False) -> bool:
    """Download the newest baseline only from the main sweep workflow."""
    artifacts = gh_json(f"repos/{repository}/actions/artifacts?name=main-baseline&per_page=100")["artifacts"]
    candidates = [a for a in artifacts if not a["expired"] and a.get("workflow_run", {}).get("head_branch") == "main"]
    for artifact in sorted(candidates, key=lambda a: a["id"], reverse=True):
        run = gh_json(f"repos/{repository}/actions/runs/{artifact['workflow_run']['id']}")
        if run["path"] != ".github/workflows/full-matrix-sweep.yml" or run["head_branch"] != "main":
            continue
        if require_latest:
            latest = gh_json(
                f"repos/{repository}/actions/workflows/full-matrix-sweep.yml/runs"
                "?branch=main&status=completed&per_page=1"
            )["workflow_runs"]
            if not latest or latest[0]["id"] != run["id"]:
                return False
        raw = subprocess.check_output(["gh", "api", f"repos/{repository}/actions/artifacts/{artifact['id']}/zip"])
        with zipfile.ZipFile(io.BytesIO(raw)) as archive:
            names = [n for n in archive.namelist() if n == "baseline.json"]
            if len(names) != 1:
                raise ValueError("Baseline artifact is incomplete")
            output.write_bytes(archive.read(names[0]))
        read_json(output)
        return True
    return False


def reconcile_results(directory: Path, jobs: list[dict[str, Any]], prefix: str,
                      sha: str, run_id: str, attempt: int) -> None:
    """Bind artifacts to actual jobs and materialize missing failed-shard evidence."""
    rows = {row["shard"]: row for row in load_results(directory)}
    directory.mkdir(parents=True, exist_ok=True)
    for job in jobs:
        if not job["name"].startswith(prefix):
            continue
        shard = job["name"][len(prefix):]
        row = rows.get(shard, {})
        identity = (row.get("sha"), str(row.get("run_id")), row.get("attempt"))
        if identity != (sha, str(run_id), attempt):
            row = dict(shard=shard, parsed=False, failed=[], errored=[], collected=0)
        row.update(sha=sha, run_id=run_id, attempt=attempt,
                   completed_at=job["completed_at"], conclusion=job["conclusion"])
        for path in directory.rglob("result-*.json"):
            if read_json(path)["shard"] == shard:
                path.unlink()
        write_json(directory / f"result-{shard}.json", row)


def main(argv: list[str] | None = None) -> int:
    """Dispatch the four artifact operations."""
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    for command, source in (("extract", "--junit"), ("extract-log", "--log")):
        sub = commands.add_parser(command)
        sub.add_argument(source, dest="source", type=Path, required=True)
        for flag in ("shard", "sha", "run", "conclusion"):
            sub.add_argument(f"--{flag}", required=True)
        sub.add_argument("--attempt", type=int, required=True)
        sub.add_argument("--completed-at", default=datetime.now(timezone.utc).isoformat())
        sub.add_argument("--out", type=Path, required=True)
    sub = commands.add_parser("publish")
    sub.add_argument("--results", type=Path, required=True)
    sub.add_argument("--previous", required=True)
    sub.add_argument("--out", type=Path, required=True)
    sub = commands.add_parser("compare")
    sub.add_argument("--baseline", type=Path, required=True)
    sub.add_argument("--results", type=Path, required=True)
    sub.add_argument("--max-age-hours", type=float, default=24)
    sub.add_argument("--summary", type=Path, required=True)
    sub.add_argument("--verdict", type=Path, required=True)
    args = parser.parse_args(argv)
    if args.command in {"extract", "extract-log"}:
        extract(args)
    elif args.command == "publish":
        publish(args.results, args.previous, args.out)
    else:
        return compare_results(args.baseline, args.results, args.max_age_hours, args.summary, args.verdict)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
