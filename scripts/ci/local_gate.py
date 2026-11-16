#!/usr/bin/env python3
# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Run the pull-request gates on a local host and post a ``local-gate`` commit status.

The gate tests the same tree the hosted workflows test: the pull-request head
merged into the current base tip. It then runs the static checks, the CI harness
tests and only the domain shards that ``detect_touched_domains.py`` selects,
module by module, in parallel, inside a time budget.

The result is fail-closed. ``success`` is posted only when every planned module
ran and returned 0. A failed, timed-out or unstarted module posts ``failure``;
a host problem (low disk, fetch, detection) posts ``error``. A head that moved
posts nothing, because the requested commit is no longer the pull-request head.

Only same-repository pull requests are accepted: the gate executes repository
code, exactly as the self-hosted workflow jobs do. See ``docs/ci/local-gate.md``.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import dataclasses
import datetime as dt
import json
import os
import re
import shlex
import shutil
import signal
import subprocess
import sys
import time
from pathlib import Path
from typing import Callable

CONTEXT = "local-gate"
DESCRIPTION_LIMIT = 140
HARNESS_JOB = "ci-harness-tests"
HARNESS_STEP = "Run harness tests"
DOMAIN_WORKFLOW = ".github/workflows/quality-gates-by-domain.yml"
GATES_FILE = ".claude/quality-gates.yaml"
EXIT_PASS, EXIT_FAIL, EXIT_ERROR, EXIT_STALE = 0, 1, 2, 3

# Reads the shard commands and the harness command from the tree under test, so
# the local gate cannot drift from the workflow definitions it mirrors.
PLAN_READER = r"""
import json, sys, yaml
gates = yaml.safe_load(open(sys.argv[1], encoding="utf-8"))["gates"]
workflow = yaml.safe_load(open(sys.argv[2], encoding="utf-8"))
steps = workflow["jobs"][sys.argv[3]]["steps"]
harness = next(s["run"] for s in steps if s.get("name") == sys.argv[4])
commands = {k: v["command"] for k, v in gates.items() if isinstance(v, dict) and v.get("command")}
print(json.dumps({"gates": commands, "harness": harness}))
"""


class GateError(Exception):
    """A host or setup problem: the gate result is ``error``, never ``success``."""


@dataclasses.dataclass
class Module:
    """One independently reported unit: a list of shell commands run in order."""

    name: str
    commands: list[str]


@dataclasses.dataclass
class Result:
    """Outcome of one module. ``status`` is pass, fail, timeout or not-run."""

    name: str
    status: str
    seconds: float = 0.0
    code: int | None = None


Runner = Callable[..., "subprocess.CompletedProcess[str]"]


def capture(cmd: list[str], cwd: Path | None = None, env: dict[str, str] | None = None,
            check: bool = True) -> subprocess.CompletedProcess[str]:
    """Run a short helper command and capture its text output."""
    done = subprocess.run(cmd, cwd=cwd, env=env, capture_output=True, text=True,
                          encoding="utf-8", errors="replace", check=False)
    if check and done.returncode != 0:
        tail = (done.stderr or done.stdout).strip()[-300:]
        raise GateError(f"{' '.join(cmd[:3])} exited {done.returncode}: {tail}")
    return done


def run_shell(command: str, cwd: Path, env: dict[str, str], log: Path, timeout: float) -> int:
    """Run one shell command in its own process group; 124 means timed out."""
    with log.open("a", encoding="utf-8") as handle:
        handle.write(f"\n$ {command}\n")
        handle.flush()
        proc = subprocess.Popen(["bash", "-c", command], cwd=cwd, env=env, stdout=handle,
                                stderr=subprocess.STDOUT, start_new_session=True)
        try:
            return proc.wait(timeout=max(timeout, 0.1))
        except subprocess.TimeoutExpired:
            for sig in (signal.SIGTERM, signal.SIGKILL):
                try:
                    os.killpg(proc.pid, sig)
                except (ProcessLookupError, PermissionError, AttributeError):
                    proc.kill()
                try:
                    proc.wait(timeout=15)
                    break
                except subprocess.TimeoutExpired:
                    continue
            return 124


def free_gb(path: Path) -> float:
    """Free space on the volume holding ``path``."""
    return shutil.disk_usage(path).free / 1024 ** 3


def plan_modules(matrix: dict, plan: dict, tip: str, scratch: str) -> list[Module]:
    """Translate the detector matrix into runnable modules, mirroring the workflow.

    A touched domain without a gate command raises: an unknown shard can never
    be skipped silently.
    """
    lint = Module("lint", [
        "uv run --no-project --with ruff==0.15.1 python scripts/ci/check_text_encoding.py "
        f"--base {shlex.quote(tip)}",
        "uv run --no-project python scripts/enforcement/check-report-provenance.py",
        "uv run --no-project --with pyyaml python scripts/maintenance/verify_repo_structure.py "
        "--repo . --config config/repo_structure.yml",
        "uv run --no-project python scripts/enforcement/check-engineering-register.py "
        f"--diff {shlex.quote(tip)}",
    ])
    modules = [lint, Module("ci-harness", [plan["harness"]])]
    for entry in matrix.get("include", []):
        domain = entry["domain"]
        if not re.fullmatch(r"[A-Za-z0-9_.-]+", domain):
            raise GateError(f"unexpected domain name: {domain!r}")
        command = plan["gates"].get(f"tests-{domain}")
        if not command:
            raise GateError(f"no gate command for touched domain {domain}")
        junit = f"reports/junit-{domain}.xml"
        suffix = f" --junitxml={junit} -o junit_family=xunit2"
        commands = ["mkdir -p reports"]
        if domain == "subsea":
            command += " tests/riser_fatigue/ tests/riser_bom/"
        if domain == "contracts":
            prefix = f"{scratch}/source-locators-formatter"
            commands.append(
                f"npm install --prefix {shlex.quote(prefix)} --cache {shlex.quote(scratch + '/npm-cache')} "
                "--no-save --no-package-lock --no-audit --no-fund prettier@4.0.0-alpha.8")
            command = (f"SOURCE_LOCATORS_PRETTIER={shlex.quote(prefix + '/node_modules/.bin/prettier')} "
                       + command)
        if domain in ("solver-smoke", "specialized"):
            commands.append("dpkg -s libglu1-mesa >/dev/null")
        if domain == "orcaflex":
            register = f"{scratch}/flaky-register.yml"
            commands.append(f"python3 scripts/ci/ci_baseline.py register --out {shlex.quote(register)}")
            command = ("uv run --no-sources --with pyyaml==6.0.2 python scripts/ci/run_shard.py "
                       f"--register {shlex.quote(register)} --mode report --shard orcaflex "
                       f"--junit {junit} --max-retry-ids 20 -- {command}")
        commands.append(command + suffix)
        if domain == "specialized":
            commands.append(
                "PYTHONPATH=src uv run --no-sources --with-editable '.[test]' --with 'xslope==1.0.3' "
                "--with 'gmsh>=4.15' python -m pytest tests/geotechnical -q -rA -p no:cacheprovider")
        modules.append(Module(domain, commands))
    return modules


def run_module(module: Module, cwd: Path, env: dict[str, str], logs: Path, deadline: float,
               module_timeout: float, shell: Callable[..., int] = run_shell,
               clock: Callable[[], float] = time.monotonic) -> Result:
    """Run one module unless the budget is already spent."""
    started = clock()
    if started >= deadline:
        return Result(module.name, "not-run")
    limit = started + module_timeout
    for command in module.commands:
        remaining = min(limit, deadline) - clock()
        if remaining <= 0:
            return Result(module.name, "timeout", clock() - started, 124)
        code = shell(command, cwd, env, logs / f"{module.name}.log", remaining)
        if code != 0:
            status = "timeout" if code == 124 else "fail"
            return Result(module.name, status, clock() - started, code)
    return Result(module.name, "pass", clock() - started, 0)


def run_modules(modules: list[Module], jobs: int, run_one: Callable[[Module], Result]) -> list[Result]:
    """Run modules in parallel within the job cap, keeping plan order in the result."""
    with concurrent.futures.ThreadPoolExecutor(max_workers=max(1, jobs)) as pool:
        return list(pool.map(run_one, modules))


def summarize(results: list[Result], seconds: float, tip: str) -> tuple[str, str]:
    """Return the commit-status state and a description within the API limit.

    The description starts with the full base commit the head was merged into,
    so a merge step can require a result for the base tip it is about to merge into.
    """
    bad = [r for r in results if r.status != "pass"]
    total = len(results)
    if not results:
        return "error", "no module was planned"
    if not bad:
        names = ", ".join(r.name for r in results)
        text = f"base {tip}: {total}/{total} modules passed in {seconds:.0f} s ({names})"
        return "success", text[:DESCRIPTION_LIMIT]
    detail = ", ".join(f"{r.name} ({r.status})" for r in bad)
    text = f"base {tip}: {total - len(bad)}/{total} modules passed in {seconds:.0f} s; not passed: {detail}"
    return "failure", text[:DESCRIPTION_LIMIT]


def post_status(repo: str, sha: str, state: str, description: str, run: Runner = capture) -> None:
    """Post the commit status; a failed post is an error for the caller."""
    run(["gh", "api", "-X", "POST", f"repos/{repo}/statuses/{sha}", "-f", f"state={state}",
         "-f", f"context={CONTEXT}", "-f", f"description={description[:DESCRIPTION_LIMIT]}"])


def pull_request(repo: str, number: int, run: Runner = capture) -> dict:
    """Read the head, base and origin of the pull request."""
    out = run(["gh", "api", f"repos/{repo}/pulls/{number}", "--jq",
               "{head: .head.sha, head_repo: .head.repo.full_name, base: .base.ref, state: .state}"])
    return dict(json.loads(out.stdout))


def prepare_tree(args: argparse.Namespace, base: str, worktree: Path) -> str:
    """Fetch into the gate's own mirror and build the merged tree; return the base tip."""
    mirror = args.work_root / "mirror.git"
    url = f"https://github.com/{args.repo}.git"
    if not mirror.exists():
        # A complete clone, as the workflows use (fetch-depth: 0): citation and pin tests read
        # old revisions, and a partial clone would fetch each missing object over the network.
        capture(["git", "clone", "--quiet", "--bare", url, str(mirror)])
    git = ["git", "-C", str(mirror)]
    capture(git + ["fetch", "--quiet", "--no-tags", url,
                   f"+refs/heads/{base}:refs/local-gate/base",
                   f"+refs/pull/{args.pr}/head:refs/local-gate/pr-{args.pr}"])
    fetched = capture(git + ["rev-parse", f"refs/local-gate/pr-{args.pr}"]).stdout.strip()
    if fetched != args.sha:
        raise StaleHead(f"fetched head {fetched[:8]} is not the requested {args.sha[:8]}")
    tip = capture(git + ["rev-parse", "refs/local-gate/base"]).stdout.strip()
    capture(git + ["worktree", "prune"])
    capture(git + ["worktree", "add", "--quiet", "--detach", str(worktree), tip])
    merge = capture(["git", "-C", str(worktree), "-c", "user.name=local-gate",
                     "-c", "user.email=local-gate@users.noreply.github.com",
                     "merge", "--no-ff", "--no-edit", args.sha], check=False)
    if merge.returncode != 0:
        raise MergeConflict(f"head does not merge into base {tip[:8]}")
    return tip


class StaleHead(GateError):
    """The requested commit is not the pull-request head."""


class MergeConflict(GateError):
    """The head conflicts with the current base tip."""


def remove_tree(work_root: Path, tree_root: Path) -> None:
    """Remove only the worktrees this run created (one per module, under ``tree_root``)."""
    mirror = work_root / "mirror.git"
    if tree_root.is_dir():
        for child in tree_root.iterdir():
            capture(["git", "-C", str(mirror), "worktree", "remove", "--force", str(child)], check=False)
        shutil.rmtree(tree_root, ignore_errors=True)
    capture(["git", "-C", str(mirror), "worktree", "prune"], check=False)


def parse_args(argv: list[str]) -> argparse.Namespace:
    """Parse and validate the command line."""
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--pr", type=int, required=True)
    parser.add_argument("--sha", required=True, help="full 40-character head commit")
    parser.add_argument("--repo", default="vamseeachanta/digitalmodel")
    parser.add_argument("--work-root", type=Path, required=True,
                        help="directory on a volume with room for the mirror, worktrees and caches")
    parser.add_argument("--jobs", type=int, default=4, help="modules run in parallel")
    parser.add_argument("--budget-seconds", type=int, default=3600)
    parser.add_argument("--module-timeout", type=int, default=1500)
    parser.add_argument("--min-free-gb", type=float, default=20.0,
                        help="refuse to start when the work-root volume has less free space")
    parser.add_argument("--python", default="3.11",
                        help="version or interpreter path; give the path of the interpreter the hosted jobs use")
    parser.add_argument("--no-post", action="store_true", help="run and report, post no status")
    parser.add_argument("--keep", action="store_true", help="keep the worktree for inspection")
    args = parser.parse_args(argv)
    if not re.fullmatch(r"[0-9a-f]{40}", args.sha):
        parser.error("--sha must be a full lower-case commit id")
    if not re.fullmatch(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+", args.repo):
        parser.error("--repo must be owner/name")
    if args.pr <= 0 or args.jobs <= 0 or args.budget_seconds <= 0 or args.module_timeout <= 0:
        parser.error("--pr, --jobs, --budget-seconds and --module-timeout must be positive")
    return args


def gate(args: argparse.Namespace, run_dir: Path, tree_root: Path) -> tuple[str, str, list[Result]]:
    """Run the whole gate; return state, description and module results.

    Every module gets its own checkout of the merged commit, as every hosted job
    does: a file one shard writes into the tree cannot change another shard's result.
    """
    started = time.monotonic()
    deadline = started + args.budget_seconds
    available = free_gb(args.work_root)
    if available < args.min_free_gb:
        raise GateError(f"work volume has {available:.0f} GB free, below the {args.min_free_gb:.0f} GB floor")
    info = pull_request(args.repo, args.pr)
    if info["head"] != args.sha or info["state"] != "open":
        raise StaleHead(f"pull request head is {str(info['head'])[:8]} ({info['state']})")
    if info["head_repo"] != args.repo:
        raise GateError("pull request comes from another repository; local gate refused")
    if not re.fullmatch(r"[A-Za-z0-9._/-]{1,100}", info["base"]):
        raise GateError("unexpected base branch name")
    worktree = tree_root / "plan"
    tip = prepare_tree(args, info["base"], worktree)
    merged = capture(["git", "-C", str(worktree), "rev-parse", "HEAD"]).stdout.strip()
    mirror = args.work_root / "mirror.git"

    scratch = run_dir / "scratch"
    logs = run_dir / "logs"
    scratch.mkdir(parents=True)
    logs.mkdir()
    env = dict(os.environ)
    env.update(UV_CACHE_DIR=str(args.work_root / "uv-cache"),
               UV_PYTHON_INSTALL_DIR=str(args.work_root / "uv-python"),
               UV_PYTHON=args.python, PYTHONDONTWRITEBYTECODE="1", TMPDIR=str(scratch),
               GITHUB_BASE_REF=info["base"], GITHUB_SHA=args.sha)
    # Prefer an interpreter installed on the host over a downloaded standalone build: the
    # standalone 3.11 build lacks fcntl.F_SEAL_WRITE, which tests/workflows needs.
    env.setdefault("UV_PYTHON_PREFERENCE", "system")
    uv = ["uv", "run", "--no-sources"]
    detect = capture(uv + ["python", "scripts/ci/detect_touched_domains.py", "--mode", "touched",
                           "--base", tip, "--head", args.sha, "--domains-file", "tests/DOMAINS.md",
                           "--output-format", "json-matrix"], cwd=worktree, env=env)
    matrix = json.loads(detect.stdout.strip().splitlines()[-1])
    reader = capture(uv + ["--with", "pyyaml==6.0.2", "python", "-c", PLAN_READER, GATES_FILE,
                           DOMAIN_WORKFLOW, HARNESS_JOB, HARNESS_STEP], cwd=worktree, env=env)
    plan = json.loads(reader.stdout.strip().splitlines()[-1])
    modules = plan_modules(matrix, plan, tip, str(scratch))
    print(f"local-gate: {len(modules)} modules: {', '.join(m.name for m in modules)}", flush=True)

    def run_one(module: Module) -> Result:
        tree = tree_root / module.name
        added = capture(["git", "-C", str(mirror), "worktree", "add", "--quiet", "--detach",
                         str(tree), merged], check=False)
        if added.returncode != 0:
            print(f"local-gate: {module.name} checkout failed", flush=True)
            return Result(module.name, "fail", 0.0, added.returncode)
        result = run_module(module, tree, env, logs, deadline, args.module_timeout)
        print(f"local-gate: {result.name} {result.status} in {result.seconds:.0f} s", flush=True)
        return result

    results = run_modules(modules, args.jobs, run_one)
    shutil.rmtree(scratch, ignore_errors=True)
    state, description = summarize(results, time.monotonic() - started, tip)
    return state, description, results


def main(argv: list[str] | None = None) -> int:
    """Run the gate, post the status and return 0 only for ``success``."""
    args = parse_args(sys.argv[1:] if argv is None else argv)
    args.work_root = args.work_root.resolve()
    tag = f"pr-{args.pr}-{args.sha[:8]}"
    stamp = dt.datetime.now(dt.timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    run_dir = args.work_root / "runs" / f"{tag}-{stamp}"
    worktree = args.work_root / "wt" / tag
    results: list[Result] = []
    created = False
    try:
        if not args.work_root.is_dir():
            raise GateError("work root does not exist")
        (args.work_root / "wt").mkdir(exist_ok=True)
        if worktree.exists():
            raise GateError("a gate run for this head is already present")
        run_dir.mkdir(parents=True)
        created = True
        state, description, results = gate(args, run_dir, worktree)
    except StaleHead as exc:
        print(f"local-gate: stale, nothing posted: {exc}", flush=True)
        return EXIT_STALE
    except MergeConflict as exc:
        state, description = "failure", str(exc)
    except (GateError, OSError, ValueError, KeyError, StopIteration) as exc:
        state, description = "error", f"gate could not run: {exc}"[:DESCRIPTION_LIMIT]
    finally:
        if created and not args.keep:
            remove_tree(args.work_root, worktree)
    if run_dir.is_dir():
        payload = {"pr": args.pr, "sha": args.sha, "state": state, "description": description,
                   "modules": [dataclasses.asdict(r) for r in results]}
        (run_dir / "result.json").write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    print(f"local-gate: {state}: {description}", flush=True)
    if not args.no_post:
        try:
            post_status(args.repo, args.sha, state, description)
        except GateError as exc:
            print(f"local-gate: status post failed: {exc}", flush=True)
            return EXIT_ERROR
    return {"success": EXIT_PASS, "failure": EXIT_FAIL}.get(state, EXIT_ERROR)


if __name__ == "__main__":
    sys.exit(main())
