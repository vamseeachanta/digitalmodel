"""Collect exact node IDs and measure each runnable file in its own pytest process."""
import hashlib
import json
import os
import subprocess
import sys
import time
import xml.etree.ElementTree as ET
from collections import Counter
from pathlib import Path


def _event(kind, key, value):
    with open(os.environ["ORPHAN_EVENTS"], "a", encoding="utf8") as stream:
        stream.write(json.dumps([kind, key, value]) + "\n")


# Pytest loads this module with -p in the child. Keep exact IDs independently
# of terminal verbosity, parametrization, and JUnit's classname conversion.
def pytest_collectreport(report):
    if report.failed:
        text = getattr(getattr(report.longrepr, "reprcrash", None), "message", None)
        lines = str(report.longrepr).splitlines()
        cause = next((line[1:].strip() for line in lines if line.startswith("E ")), None)
        _event("error", report.nodeid, cause or (text or lines[0]).splitlines()[0])
    elif report.skipped:
        _event("skip", report.nodeid, str(report.longrepr[-1]))
    elif report.nodeid.endswith(".py"):
        _event("seen", report.nodeid, None)


def pytest_collection_finish(session):
    _event("nodes", "", [item.nodeid for item in session.items])


def pytest_runtest_logfinish(nodeid, location):
    _event("finished", nodeid, None)


def _env():
    return {k: v for k, v in os.environ.items()
            if k not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}


def _head(root):
    return subprocess.check_output(
        ["git", "rev-parse", "HEAD"], cwd=root, env=_env(), text=True).strip()


def tree_changes(root):
    # The requested --no-sources environment re-resolves the tracked lock.
    # Measurement outputs and new tooling belong outside the measured tree.
    return subprocess.check_output(
        ["git", "status", "--porcelain", "--", ".", ":(exclude)uv.lock"],
        cwd=root, env=_env(), text=True,
    ).strip()


def _save(path, data):
    tmp = path.with_suffix(".tmp")
    tmp.write_text(json.dumps(data, indent=2) + "\n", encoding="utf8")
    tmp.replace(path)


def _pytest(root, stem, args, limit):
    events = stem.with_suffix(".jsonl").resolve()
    events.write_text("")
    env = _env()
    paths = [str(Path(__file__).resolve().parent), env.get("PYTHONPATH", "")]
    env.update(ORPHAN_EVENTS=str(events), PYTHONPATH=os.pathsep.join(paths))
    cmd = ["timeout", "--kill-after=2s", f"{max(0.001, limit):.3f}s", sys.executable,
           "-m", "pytest", "-q", "-p", "orphan_measure", *args]
    start = time.monotonic()
    with stem.with_suffix(".log").open("wb") as log:
        rc = subprocess.run(cmd, cwd=root, env=env, stdout=log, stderr=subprocess.STDOUT).returncode
    return rc, round(time.monotonic() - start, 3), [
        json.loads(line) for line in events.read_text().splitlines() if line.endswith("]")]


def _collect(root, out, selected, deadline):
    files = {path: {"nodes": [], "status": "unknown"} for path in selected}
    if not files:
        return {"files": files, "returncode": 0, "seconds": 0, "unknown": []}
    rc, seconds, events = _pytest(root, out / "collection", [
        "--collect-only", "--continue-on-collection-errors", "--", *files], deadline - time.time())
    unknown = []
    finished = False
    for kind, key, value in events:
        if kind == "nodes":
            finished = True
            for node in value:
                path = node.split("::", 1)[0]
                if path in files:
                    files[path]["nodes"].append(node)
                else:
                    unknown.append(node)
        elif key.split("::", 1)[0] in files:
            row = files[key.split("::", 1)[0]]
            if kind == "error":
                row.update(status="error", error=value)
            elif kind == "skip":
                row.update(status="module_skip", reason=value)
            elif kind == "seen" and row["status"] == "unknown":
                row["status"] = "seen"
        elif kind in ("error", "skip"):
            unknown.append([key, value])
    for row in files.values():
        if row["status"] == "seen":
            row["status"] = ("collected" if row["nodes"] else "zero") if finished else "unknown"
    return {"files": files, "returncode": rc, "seconds": seconds, "unknown": unknown}


def _run_file(root, out, index, nodes, limit):
    stem = out / "junit" / str(index)
    xml = stem.with_suffix(".xml")
    rc, seconds, events = _pytest(root, stem, [
        "--durations=0", f"--junitxml={xml}", "--", *nodes], limit)
    row = {"returncode": rc, "seconds": seconds, "status": "unknown"}
    done = [key for kind, key, _ in events if kind == "finished"]
    row["finished_nodes"] = len(done)
    if rc == 124 or (rc == 137 and seconds >= limit):
        row["status"] = "timeout"
    try:
        cases = ET.parse(xml).findall(".//testcase")
    except (ET.ParseError, OSError):
        return row
    counts = dict.fromkeys(("pass", "fail", "error", "skip"), 0)
    reasons = Counter()
    for case in cases:
        skip = case.find("skipped")
        if case.find("error") is not None:
            counts["error"] += 1
        elif case.find("failure") is not None:
            counts["fail"] += 1
        elif skip is not None:
            counts["skip"] += 1
            reasons[skip.get("message", "")] += 1
        else:
            counts["pass"] += 1
    row["junit"] = str(xml.relative_to(out))
    expected_rc = int(bool(counts["fail"] or counts["error"]))
    if rc == expected_rc and Counter(done) == Counter(nodes) and len(cases) == len(nodes):
        row.update(status="complete", outcomes=counts, skip_reasons=reasons)
    return row


def _run(root, out, collected, binding):
    path = out / "run.json"
    report = json.loads(path.read_text()) if path.exists() else {**binding, "files": {}}
    if any(report[k] != v for k, v in binding.items()):
        raise ValueError("Stored run belongs to a different collection")
    (out / "junit").mkdir(exist_ok=True)
    for index, (name, source) in enumerate(collected["files"].items()):
        files = report["files"]
        if not source["nodes"]:
            files[name] = {"status": "collection_" + source["status"]}
            if source["status"] == "module_skip":
                files[name]["reason"] = source["reason"]
            continue
        if name in files and files[name]["status"] != "unstarted":
            continue
        seconds = min(300, collected["deadline"] - time.time())
        if seconds <= 0:
            files[name] = {"status": "unstarted"}
            continue
        files[name] = {"status": "started"}
        _save(path, report)  # An interrupted process may already have executed nodes.
        files[name] = _run_file(root, out, index, source["nodes"], seconds)
        if changes := tree_changes(root):
            files[name].update(status="source_changed", changes=changes)
            files[name].pop("outcomes", None)
            files[name].pop("skip_reasons", None)
            _save(path, report)
            raise ValueError(f"Checkout changed during measurement of {name}: {changes}")
        _save(path, report)
    rows = report["files"].values()
    report["outcomes"] = {k: sum(r.get("outcomes", {}).get(k, 0) for r in rows)
                          for k in ("pass", "fail", "error", "skip")}
    report["skip_reasons"] = dict(sum((Counter(r.get("skip_reasons", {})) for r in rows), Counter()))
    report["module_skip_reasons"] = Counter(
        r["reason"] for r in rows if r["status"] == "collection_module_skip")
    report["seconds"] = round(sum(r.get("seconds", 0) for r in rows), 3)
    _save(path, report)
    return any(r["status"] not in {"complete", "collection_zero", "collection_module_skip"}
               for r in rows) or report["outcomes"]["fail"] or report["outcomes"]["error"]


def measure(root, out, mode):
    import fcntl  # GNU timeout and advisory locking are used on the Linux runner.

    root, out = Path(root).resolve(), Path(out).resolve()
    manifest = (out / "list.json").read_bytes()
    inventory = json.loads(manifest)
    binding = {"base_commit": _head(root), "list_sha256": hashlib.sha256(manifest).hexdigest()}
    if inventory["base_commit"] != binding["base_commit"]:
        raise ValueError("Checkout differs from inventory base commit")
    if changes := tree_changes(root):
        raise ValueError(f"Measurement requires a clean base checkout: {changes}")
    selected = inventory["automatic_orphans"]
    if any(not (root / p).resolve().is_relative_to(root) or p.startswith("-") for p in selected):
        raise ValueError("Orphan file outside repository or option in inventory")
    with (out / "measurement.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        path = out / "collect.json"
        if mode == "collect" and not path.exists():
            deadline = time.time() + 90 * 60
            result = _collect(root, out, selected, deadline)
            if changes := tree_changes(root):
                result["unknown"].append(f"Checkout changed during collection: {changes}")
            _save(path, {**binding, "deadline": deadline, **result})
        collected = json.loads(path.read_text())
        if any(collected[k] != v for k, v in binding.items()):
            raise ValueError("Stored collection belongs to a different inventory")
        failed = (inventory["unknown"] or collected["unknown"] or collected["returncode"] not in (0, 5)
                  or any(r["status"] in {"error", "unknown"} for r in collected["files"].values()))
        if mode == "run":
            binding["collection_sha256"] = hashlib.sha256(path.read_bytes()).hexdigest()
            failed = _run(root, out, collected, binding) or failed
    return int(bool(failed))
