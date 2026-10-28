# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Extract, publish and compare exact CI failures without masking shard results."""

from __future__ import annotations

import argparse
import io
import json
import os
import re
import subprocess
import tempfile
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
    return parse_log_text(path.read_text(encoding="utf-8"))


def parse_log_text(content: str) -> tuple[list[str], list[str], int]:
    """Extract exact failed and errored IDs from pytest short-summary lines."""
    failed: set[str] = set()
    errored: set[str] = set()
    collected = 0
    for line in content.splitlines():
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
    return sorted(failed), sorted(errored), collected


def extract(args: argparse.Namespace) -> None:
    """Persist extraction failure as explicit unparsed evidence."""
    row = dict(shard=args.shard, sha=args.sha, run_id=args.run, attempt=args.attempt,
               completed_at=args.completed_at, conclusion=args.conclusion,
               parsed=False, failed=[], errored=[], collected=0, exit_code=args.exit_code,
               first_attempt_exit_code=args.exit_code, first_attempt_failed={}, recovered=[])
    try:
        parser = parse_junit if args.command == "extract" else parse_log
        failed, errored, collected = parser(args.source)
        row.update(parsed=True, failed=failed, errored=errored, collected=collected)
        messages = {node: "" for node in failed + errored}
        if args.command == "extract":
            for case in ElementTree.parse(args.source).getroot().iter("testcase"):
                failure = next((child for child in case if child.tag in {"failure", "error"}), None)
                if failure is not None:
                    messages[junit_id(case)] = (failure.get("message", "") + "\n" + (failure.text or "")).strip()[:500]
        row["first_attempt_failed"] = messages
        evidence = args.source.parent / f"flaky-{args.shard}.json"
        if evidence.exists():
            retry = read_json(evidence)
            if (retry.get("sha") != args.sha or str(retry.get("run_id")) != args.run
                    or retry.get("exit_code") != args.exit_code
                    or set(retry["first_attempt_failed"]) != set(messages)
                    or not set(retry["recovered"]) <= set(messages)):
                raise ValueError("Retry evidence disagrees with first attempt")
            row.update(first_attempt_failed=retry["first_attempt_failed"],
                       first_attempt_exit_code=retry["first_attempt_exit_code"], recovered=retry["recovered"])
            for field in ("failed", "errored"):
                row[field] = [node for node in row[field] if node not in row["recovered"]]
    except (OSError, ValueError, KeyError, TypeError, ElementTree.ParseError):
        row["parsed"] = False
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
        metadata = {key: row.get(key) for key in ("sha", "run_id", "completed_at", "conclusion", "parsed", "exit_code", "collected")}
        baseline["shards"][shard] = metadata
        if row["parsed"] is not True:
            continue
        baseline["tests"] = {key: value for key, value in baseline["tests"].items() if value["shard"] != shard}
        for field, status in (("failed", "failed"), ("errored", "error")):
            for node in row[field]:
                if node in row.get("recovered", []):
                    continue
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
    if (row["conclusion"] != "failure" or row["parsed"] is not True or not ids
            or row.get("exit_code") != 1 or not row.get("collected", 0)
            or any("::" not in node for node in ids)):
        return False, "failed shard has incomplete evidence"
    if (shard.get("conclusion") != "failure" or shard.get("parsed") is not True
            or shard.get("exit_code") != 1 or not shard.get("collected", 0)):
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


def verified_evidence(url: str, shard: str, node: str,
                      log_cache: dict[str, tuple[list[str], list[str], int] | None] | None = None) -> bool:
    """Require this exact ID to fail in a same-shard job on this repository's main."""
    repository = os.environ.get("GITHUB_REPOSITORY", "")
    if not re.fullmatch(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+", repository):
        return False
    escaped = re.escape(repository)
    match = re.fullmatch(
        rf"https://(?:github\.com/{escaped}|api\.github\.com/(?:repos/{escaped}|repositories/([0-9]+)))"
        r"/actions/(runs|jobs)/([0-9]+)", url)
    if not match:
        return False
    repository_id, kind, number = match.groups()
    prefix = f"repos/{repository}/actions"
    if log_cache is None:
        log_cache = {}
    try:
        if repository_id is not None and str(gh_json(f"repos/{repository}")["id"]) != repository_id:
            return False
        job = gh_json(f"{prefix}/jobs/{number}") if kind == "jobs" else None
        if job is not None and str(job.get("id")) != number:
            return False
        run_id = job["run_id"] if job is not None else int(number)
        run = gh_json(f"{prefix}/runs/{run_id}")
        if (str(run.get("id")) != str(run_id) or run.get("head_branch") != "main"
                or run.get("event") not in MAIN_RUN_EVENTS or run.get("status") != "completed"
                or not isinstance(run.get("head_repository"), dict)
                or run["head_repository"].get("full_name") != repository):
            return False
        jobs = [job] if job is not None else gh_json(f"{prefix}/runs/{run_id}/jobs?per_page=100")["jobs"]
        for item in jobs:
            if (isinstance(item, dict)
                    and item.get("name") in {f"tests-{shard}", f"sweep-tests-{shard}"}
                    and item.get("status") == "completed" and item.get("conclusion") == "failure"
                    and str(item.get("run_id")) == str(run_id)
                    and item.get("head_sha") == run.get("head_sha")
                    and re.fullmatch(r"[0-9]+", str(item.get("id")))):
                endpoint = f"{prefix}/jobs/{item['id']}/logs"
                if endpoint not in log_cache:
                    log_cache[endpoint] = None
                    log_cache[endpoint] = parse_log_text(gh_text(endpoint))
                summary = log_cache[endpoint]
                if summary is None:
                    return False
                if node in summary[0] or node in summary[1]:
                    return True
        return False
    except (OSError, ValueError, KeyError, TypeError, subprocess.CalledProcessError):
        return False


def register_decisions(path: Path, ids: list[str], shard: str) -> dict[str, str]:
    """Read trusted entries and verify every evidence URL before clearing an ID."""
    try:
        import yaml
        with tempfile.TemporaryDirectory(prefix="ci-register-") as scratch:
            if os.environ.get("GITHUB_EVENT_NAME") == "pull_request":
                path = Path(scratch) / "register.yml"
                select_register(path)
            rows = yaml.safe_load(path.read_text(encoding="utf-8"))
        if not isinstance(rows, list) or not all(isinstance(row, dict) for row in rows):
            raise ValueError("Invalid register")
        entries = {row["id"]: row for row in rows}
        if len(entries) != len(rows):
            raise ValueError("Duplicate register IDs")
    except (OSError, ValueError, KeyError, TypeError, ImportError, subprocess.CalledProcessError):
        return {node: "register unreadable" for node in ids}
    except yaml.YAMLError:
        return {node: "register unreadable" for node in ids}
    decisions = {}
    checked: dict[tuple[str, str], bool] = {}
    log_cache: dict[str, tuple[list[str], list[str], int] | None] = {}
    for node in ids:
        row = entries.get(node, {})
        evidence = row.get("evidence")
        approved = (row.get("status") == "open" and isinstance(evidence, list) and bool(evidence)
                    and all(isinstance(item, str) and item.strip() for item in evidence))
        if approved:
            for url in evidence:
                if (url, node) not in checked:
                    checked[url, node] = verified_evidence(url, shard, node, log_cache)
            approved = all(checked[url, node] for url in evidence)
        decisions[node] = "registered with evidence" if approved else "missing open entry with evidence"
    return decisions


def compare_results(baseline_path: Path, results: Path, max_age: float, summary: Path,
                    verdict: Path, now: datetime | None = None,
                    register: Path = Path(".github/ci/flaky-register.yml")) -> int:
    """Write comparison evidence; return one for every unprovable failure."""
    now = now or datetime.now(timezone.utc)
    report: dict[str, Any] = {"passed": False, "pre_existing": [], "shards": []}
    lines = ["## Main baseline comparison", "", "| Shard | Comparison | Main run |", "| --- | --- | --- |"]
    try:
        try:
            baseline = read_json(baseline_path)
            if not isinstance(baseline.get("tests"), dict) or not isinstance(baseline.get("shards"), dict):
                raise ValueError("Invalid baseline structure")
        except (OSError, ValueError):
            baseline = {"tests": {}, "shards": {}}
            lines.append("\nMain baseline unavailable; no id can be proven pre-existing.")
        rows = load_results(results)
        if not rows:
            raise ValueError("No shard results")
        outcomes = []
        for row in rows:
            first = row.get("first_attempt_failed", {})
            ids = sorted(set(row["failed"] + row["errored"]) | set(first))
            if row["conclusion"] == "success" and not ids:
                outcomes.append(row["parsed"] is True and row.get("exit_code") == 0
                                and row.get("collected", 0) > 0
                                and row.get("first_attempt_exit_code", 0) == 0)
                continue
            choices = register_decisions(register, ids, row["shard"])
            classifications = {}
            reasons = []
            main = baseline["shards"].get(row["shard"], {})
            for node in ids:
                observation = dict(row, failed=[node], errored=[],
                                   exit_code=row.get("first_attempt_exit_code", row.get("exit_code")))
                # Success is accepted only as a completed, explicitly recovered attempt.
                if (row["conclusion"] == "success" and node in first
                        and node in row.get("recovered", []) and row.get("exit_code") == 0):
                    observation["conclusion"] = "failure"
                ok, reason = classify(observation, baseline, max_age, now)
                registered = (node in row.get("recovered", []) and node in first
                              and observation["conclusion"] == "failure"
                              and observation.get("exit_code") == 1 and row.get("exit_code") in {0, 1}
                              and row["parsed"] is True and row.get("collected", 0) > 0
                              and "::" in node and choices[node] == "registered with evidence")
                classifications[node] = "PRE-EXISTING" if ok else "FLAKY" if registered else "NEW"
                if ok:
                    report["pre_existing"].append(node)
                    message = f"pre-existing on main: {node} (main {main['sha']} run {main['run_id']}, see main-health issue)"
                    lines.append(f"- {message}")
                    escaped = message.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")
                    print(f"::warning::{escaped}")
                reasons.append(reason)
                lines.append(f"| {row['shard']}: {node} | {classifications[node]} | {main.get('run_id', 'unavailable')} |")
            ok = bool(ids) and all(value != "NEW" for value in classifications.values())
            if not ids:
                reasons.append("failed shard has incomplete evidence")
            outcomes.append(ok)
            reason = "; ".join(dict.fromkeys(reasons))
            report["shards"].append(dict(shard=row["shard"], pre_existing=bool(ids) and all(
                value == "PRE-EXISTING" for value in classifications.values()), reason=reason, ids=classifications))
        report["passed"] = all(outcomes)
    except (OSError, ValueError, KeyError, TypeError) as error:
        lines.append(f"\nComparison fails closed: {type(error).__name__}.")
    summary.write_text("\n".join(lines) + "\n", encoding="utf-8")
    write_json(verdict, report)
    return 0 if report["passed"] else 1


# Events that can produce a main baseline. The Main full run is dispatched by our
# own cron (workflow_dispatch); push and schedule keep older baselines readable.
MAIN_RUN_EVENTS = frozenset({"push", "schedule", "workflow_dispatch"})


def gh_text(endpoint: str) -> str:
    """Read authenticated GitHub metadata or job logs through the hosted CLI."""
    command = ["gh", "api"]
    if re.search(r"/actions/jobs/[0-9]+/logs$", endpoint):
        command.append("--allow-escape-sequences")
    command.append(endpoint)
    return subprocess.check_output(command, text=True)


def gh_json(endpoint: str) -> dict[str, Any]:
    """Read authenticated workflow metadata through the hosted CLI."""
    value: dict[str, Any] = json.loads(gh_text(endpoint))
    return value


def is_main_sweep(run: dict[str, Any], repository: str) -> bool:
    """Require a completed main run from this repository's own main.

    The Main full run is dispatched by our own cron (workflow_dispatch); push and
    schedule stay accepted so older published baselines remain readable.
    """
    return (run.get("event") in MAIN_RUN_EVENTS
            and run.get("head_branch") == "main"
            and run.get("head_repository", {}).get("full_name") == repository
            and run.get("path") == ".github/workflows/full-matrix-sweep.yml"
            and run.get("status") == "completed")


def select_baseline_run(runs: list[dict[str, Any]], artifacts: list[dict[str, Any]],
                        repository: str, now: datetime, max_age: float = 24) -> dict[str, Any] | None:
    """Stop at the newest eligible result; missing evidence forbids fallback."""
    for run in sorted(runs, key=lambda row: row["id"], reverse=True):
        if not is_main_sweep(run, repository) or run.get("conclusion") == "cancelled":
            continue
        age = (now - instant(run["updated_at"])).total_seconds() / 3600
        if not 0 <= age <= max_age:
            continue
        available = [item for item in artifacts if item.get("name") == "main-baseline"
                     and item.get("expired") is False
                     and item.get("workflow_run", {}).get("id") == run["id"]
                     and instant(item["created_at"]) <= now]
        if available:
            return run
        print(f"Comparison fails closed: main sweep run {run['id']} has no available baseline.")
        return None
    return None


def recent_metadata(repository: str, kind: str, now: datetime) -> list[dict[str, Any]]:
    """Page recent run or artifact metadata without assuming API ordering by completion."""
    if kind == "workflow_runs":
        endpoint = "actions/workflows/full-matrix-sweep.yml/runs?branch=main"
    else:
        endpoint = "actions/artifacts?name=main-baseline"
    rows = []
    for page in range(1, 11):
        batch = gh_json(f"repos/{repository}/{endpoint}&per_page=100&page={page}")[kind]
        rows.extend(batch)
        if len(batch) < 100:
            break
    return rows


def download_baseline(repository: str, output: Path, require_latest: bool = False,
                      now: datetime | None = None) -> bool:
    """Download only the newest eligible published sweep; invalid evidence fails closed."""
    now = now or datetime.now(timezone.utc)
    artifacts = recent_metadata(repository, "artifacts", now)
    runs = recent_metadata(repository, "workflow_runs", now)
    selected = select_baseline_run(runs, artifacts, repository, now)
    if selected is None:
        return False
    print(f"Reading baseline from main sweep run {selected['id']}.")
    run = gh_json(f"repos/{repository}/actions/runs/{selected['id']}")
    identity = ("id", "path", "head_branch", "event",
                "head_sha", "status", "conclusion", "updated_at")
    if not is_main_sweep(run, repository) or any(run.get(key) != selected.get(key) for key in identity):
        raise ValueError("Sweep metadata changed during baseline selection")
    candidates = [item for item in artifacts if item.get("workflow_run", {}).get("id") == run["id"]
                  and item.get("name") == "main-baseline" and item.get("expired") is False
                  and instant(item["created_at"]) <= now]
    artifact = max(candidates, key=lambda row: row["id"])
    if artifact["workflow_run"].get("head_sha") != run["head_sha"]:
        raise ValueError("Artifact and sweep identities disagree")
    raw = subprocess.check_output(["gh", "api", f"repos/{repository}/actions/artifacts/{artifact['id']}/zip"])
    with zipfile.ZipFile(io.BytesIO(raw)) as archive:
        if archive.namelist().count("baseline.json") != 1:
            raise ValueError("Baseline artifact is incomplete")
        value = json.loads(archive.read("baseline.json"))
    if not isinstance(value, dict) or not isinstance(value.get("shards"), dict) or not isinstance(value.get("tests"), dict):
        raise ValueError("Invalid baseline structure")
    write_json(output, value)
    return True


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


def select_register(output: Path) -> None:
    """Copy the base-tip register for PRs, or the checkout for other events.

    Fetch and object-read failures propagate. An absent file is an empty
    register, never a reason to fall back to the pull request's own entries.
    """
    path = ".github/ci/flaky-register.yml"
    content = "[]\n"
    if os.environ.get("GITHUB_EVENT_NAME") == "pull_request":
        base = os.environ.get("GITHUB_BASE_REF", "")
        if not base or not re.fullmatch(r"[A-Za-z0-9_./-]+", base):
            raise ValueError("Invalid base branch")
        env = {key: value for key, value in os.environ.items()
               if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}

        def git(*args: str) -> str:
            return subprocess.check_output(["git", *args], text=True, env=env)

        git("fetch", "--no-tags", "origin", f"refs/heads/{base}")
        tip = git("rev-parse", "FETCH_HEAD").strip()
        if not re.fullmatch(r"[0-9a-f]{40,64}", tip):
            raise ValueError("Invalid fetched base tip")
        if git("ls-tree", "--name-only", tip, "--", path).strip():
            content = git("show", f"{tip}:{path}")
    elif Path(path).is_file():
        content = Path(path).read_text(encoding="utf-8")
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(content, encoding="utf-8")


def main(argv: list[str] | None = None) -> int:
    """Dispatch artifact operations and trusted-register selection."""
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    sub = commands.add_parser("register")
    sub.add_argument("--out", type=Path, required=True)
    for command, source in (("extract", "--junit"), ("extract-log", "--log")):
        sub = commands.add_parser(command)
        sub.add_argument(source, dest="source", type=Path, required=True)
        for flag in ("shard", "sha", "run", "conclusion"):
            sub.add_argument(f"--{flag}", required=True)
        sub.add_argument("--attempt", type=int, required=True)
        sub.add_argument("--exit-code", type=int)
        sub.add_argument("--completed-at", default=datetime.now(timezone.utc).isoformat())
        sub.add_argument("--out", type=Path, required=True)
    sub = commands.add_parser("publish")
    sub.add_argument("--results", type=Path, required=True)
    sub.add_argument("--previous", required=True)
    sub.add_argument("--out", type=Path, required=True)
    sub = commands.add_parser("compare")
    sub.add_argument("--baseline", type=Path, required=True)
    sub.add_argument("--results", type=Path, required=True)
    sub.add_argument("--register", type=Path, default=Path(".github/ci/flaky-register.yml"))
    sub.add_argument("--max-age-hours", type=float, default=24)
    sub.add_argument("--summary", type=Path, required=True)
    sub.add_argument("--verdict", type=Path, required=True)
    sub.add_argument("--now", type=instant)
    args = parser.parse_args(argv)
    if args.command == "register":
        select_register(args.out)
    elif args.command in {"extract", "extract-log"}:
        extract(args)
    elif args.command == "publish":
        publish(args.results, args.previous, args.out)
    else:
        return compare_results(args.baseline, args.results, args.max_age_hours, args.summary, args.verdict, args.now, args.register)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
