"""Fetch Actions shard timing evidence or summarize a saved snapshot offline."""

import argparse
import gzip
import json
import math
import re
import statistics
import subprocess
import sys
import time
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timedelta, timezone
from pathlib import Path
from urllib.parse import urlencode

if __package__:
    from .geotechnical_timing import solver_timing
    from .shard_log_timings import fetch_run_phases
else:
    from geotechnical_timing import solver_timing
    from shard_log_timings import fetch_run_phases

WORKFLOWS = {"sweep": "full-matrix-sweep.yml", "pr": "quality-gates-by-domain.yml"}
API = "repos/{owner}/{repo}/"


def get(path):
    for delay in (1, 2, 4, 0):
        # Raw job logs contain ANSI bytes. Capture them only into snapshot files.
        raw = ["--allow-escape-sequences"] if re.fullmatch(r"actions/jobs/\d+/logs", path) else []
        call = subprocess.run(["gh", "api", "--method", "GET", *raw, API + path], capture_output=True)
        if not call.returncode:
            return call.stdout
        if not delay or not re.search(rb"HTTP 5..|TLS handshake timeout|unexpected EOF|connection reset|stream error: stream ID", call.stderr):
            print(call.stderr.decode(), file=sys.stderr, flush=True)
            call.check_returncode()
        time.sleep(delay)


def pages(path, key, **query):
    rows = []
    for page in range(1, 10000):
        data = json.loads(get(path + "?" + urlencode(dict(query, page=page, per_page=100))))
        rows.extend(data[key])
        if len(rows) == data["total_count"]:
            if len({row["id"] for row in rows}) != len(rows):
                raise ValueError(f"Duplicate IDs in {path}")
            return rows
        if len(data[key]) < 100:
            raise ValueError(f"Incomplete {path}: {len(rows)} of {data['total_count']}")
    raise ValueError(f"Pagination limit for {path}")


def save_run(folder, run):
    path = folder / f"{run['id']}.json"
    saved = json.loads(path.read_text()) if path.exists() else {}
    refresh = run.get("status", "completed") != "completed" and saved.get("status") != "completed"
    if refresh:
        state = json.loads(get(f"actions/runs/{run['id']}"))["status"]
        if state != "completed":
            path.write_text(json.dumps({"jobs": [], "status": state}))
            return
    if saved and not refresh:
        jobs = saved["jobs"]
    else:
        rows = pages(f"actions/runs/{run['id']}/jobs", "jobs", filter="all")
        jobs = [{k: job[k] for k in ("name", "id", "conclusion", "run_attempt",
                                     "started_at", "completed_at", "steps")}
                for job in rows if re.fullmatch(r"(?:sweep-)?tests-[\w-]+", job["name"])]
        path.write_text(json.dumps({"jobs": jobs, "status": "completed"}))
    for job in jobs:
        log = folder / f"{job['id']}.log.gz"
        if log.exists() or job.get("log_error"):
            continue
        if any(s["name"] == "Run geotechnical solver benchmarks" and s["conclusion"] != "skipped"
               for s in job["steps"]):
            try:
                log.write_bytes(gzip.compress(get(f"actions/jobs/{job['id']}/logs")))
            except subprocess.CalledProcessError as exc:
                if not re.search(rb"HTTP (404|410)\b", exc.stderr):
                    raise
                job["log_error"] = exc.stderr.decode().strip()
                path.write_text(json.dumps({"jobs": jobs, "status": "completed"}))
    if refresh or not (folder / f"{run['id']}-phases.json").exists():
        fetch_run_phases(folder, run, jobs, get)


def fetch(folder, end, resume=False):
    folder.mkdir(exist_ok=resume, parents=True)
    if resume:
        runs = json.loads((folder / "index.json").read_text())["runs"]
    else:
        runs = run_index(folder, end)
    with ThreadPoolExecutor(max_workers=4) as pool:
        for _ in pool.map(lambda run: save_run(folder, run), runs):
            pass


def run_index(folder, end):
    start = end - timedelta(days=7)
    window = "..".join(t.strftime("%Y-%m-%dT%H:%M:%SZ") for t in (start, end))
    runs = []
    for kind, name in WORKFLOWS.items():
        query = {"event": "pull_request"} if kind == "pr" else {}
        for row in pages(f"actions/workflows/{name}/runs", "workflow_runs", created=window, **query):
            runs.append(dict(workflow=kind, **{k: row[k] for k in (
                "id", "created_at", "head_sha", "status", "run_attempt", "event")}))
    (folder / "index.json").write_text(json.dumps({"window": window, "runs": runs}, indent=1))
    return runs


STEP_CLASSES = {
    "Checkout code": "checkout", "Set up Python": "python", "Install uv": "uv",
    "Set up Node for source locator formatter": "node",
    "Provision source locator formatter": "formatter_install",
    "Install Gmsh runtime libraries": "system_libs",
    "Read domain gate command": "read_gate", "Set up job": "runner_setup",
    "Build and guard local pull request merge": "merge",
    "Create reports directory": "reports_dir", "Select trusted register": "register",
    "Run domain gate": "domain_gate", "Run geotechnical solver benchmarks": "solver",
    "Upload domain artifacts": "upload",
}
SETUP = {"checkout", "python", "uv", "node", "formatter_install", "system_libs",
         "read_gate", "runner_setup", "merge", "reports_dir", "register", "dependency_install"}


def elapsed(row):
    try:
        a, b = (datetime.fromisoformat(row[k].replace("Z", "+00:00"))
                for k in ("started_at", "completed_at"))
        seconds = (b - a).total_seconds()
        return seconds if seconds >= 0 and a.year > 2000 else None
    except (ValueError, TypeError, KeyError, AttributeError):
        return None


def total(values):
    return None if None in values else sum(values)


def measure(job, run, folder, phases=None):
    classes = defaultdict(list)
    steps = []
    for step in job.get("steps", []):
        name = step["name"]
        kind = STEP_CLASSES.get(name, "other")
        if kind == "other" and re.search(r"install.*dependenc", name, re.I):
            kind = "dependency_install"
        seconds = elapsed(step) if step.get("conclusion") in ("success", "failure") else None
        steps.append({"name": name, "class": kind, "s": seconds, "conclusion": step.get("conclusion")})
        if step.get("conclusion") != "skipped":
            classes[kind].append(seconds)
    costs = {k: total(v) for k, v in classes.items()}
    wall = elapsed(job) if job.get("conclusion") in ("success", "failure") else None
    complete = wall and "domain_gate" in costs
    def share(keys):
        values = [v for k, v in costs.items() if k in keys]
        seconds = total(values)
        return round(100 * seconds / wall, 3) if complete and values and seconds is not None else None
    result = dict(run=run.get("id"), job=job["id"], attempt=job.get("run_attempt", 1),
                  shard=re.sub(r"^(sweep-)?tests-", "", job["name"]), workflow=run.get("workflow"),
                  conclusion=job.get("conclusion"), depth=None, wall_s=wall,
                  steps=steps, classes=costs, setup_pct=share(SETUP),
                  test_step_pct=share({"domain_gate", "solver"}))
    if phases:
        result.update(phases)
    if "solver" in costs:
        log = folder / f"{job['id']}.log.gz"
        content = gzip.decompress(log.read_bytes()).decode(errors="replace") if log.exists() else ""
        result["solver_phases"] = solver_timing(content)
    solver = result.get("solver_phases", {"install_s": 0, "pytest_s": 0})
    setup = [v for k, v in costs.items() if k in SETUP]
    for key, values in (("setup_with_install_pct", setup + [result.get("install_s"), solver["install_s"]]),
                        ("pytest_pct", [result.get("pytest_s"), solver["pytest_s"]])):
        seconds = total(values)
        result[key] = round(100 * seconds / wall, 3) if complete and seconds is not None else None
    return result


def stats(values):
    known = sorted(v for v in values if v is not None)
    return dict(n=len(known), unknown=len(values) - len(known),
                median=statistics.median(known) if known else None,
                p90=known[math.ceil(0.9 * len(known)) - 1] if known else None)


def report(folder):
    index = json.loads((folder / "index.json").read_text())
    groups = defaultdict(lambda: defaultdict(list))
    jobs = []
    pending = []
    for run in index["runs"]:
        snap = json.loads((folder / f"{run['id']}.json").read_text())
        if snap.get("status", run.get("status", "completed")) != "completed":
            pending.append(run["id"])
            continue
        phase_file = folder / f"{run['id']}-phases.json"
        phases = json.loads(phase_file.read_text()) if phase_file.exists() else {}
        for row in snap["jobs"]:
            job = measure(row, run, folder, phases.get(str(row["id"])))
            jobs.append(job)
            fields = dict(job["classes"], **{k: job.get(k) for k in
                          ("wall_s", "setup_pct", "test_step_pct", "install_s", "pytest_s",
                           "setup_with_install_pct", "pytest_pct")})
            for key in (job["shard"], "*"):
                for metric, value in fields.items():
                    groups[f"{run['workflow']}/{key}"][metric].append(value)
    summary = {group: {k: stats(v) for k, v in fields.items()} for group, fields in groups.items()}
    return dict(window=index.get("window"), runs=index["runs"], pending_runs=pending, jobs=jobs, summary=summary)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    collect = commands.add_parser("fetch", help="download evidence into a new directory")
    window = collect.add_mutually_exclusive_group()
    window.add_argument("--resume", action="store_true", help="finish the saved window in this checkout")
    window.add_argument("--end", type=datetime.fromisoformat, default=datetime.now(timezone.utc))
    for sub in (collect, commands.add_parser("report", help="write offline JSON to stdout")):
        sub.add_argument("folder", type=Path)
    args = parser.parse_args()
    if args.command == "fetch":
        if args.end.tzinfo is None:
            parser.error("--end needs a UTC offset")
        fetch(args.folder, args.end.astimezone(timezone.utc), args.resume)
    else:
        json.dump(report(args.folder), sys.stdout, allow_nan=False)
        print()


if __name__ == "__main__":
    main()
