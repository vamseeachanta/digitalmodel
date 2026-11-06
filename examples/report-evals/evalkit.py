# Copyright (c) 2026 AceEngineer. Licensed under the MIT License.
"""Shared helpers for report evals (results.json contract, schema version 1.0).

Standard library only. The same file is kept in each public repository that
holds report evals (digitalmodel, worldenergydata, aceengineer-website), so a
reader can run one eval without installing anything for it.

An eval script supplies a function ``compute(args) -> (values, notes, context)``:
  values  - dict of result id -> value it rebuilt, or None if not evaluated
  notes   - dict of result id -> one-line note (why not evaluated, what drifted)
  context - dict of facts about the run (commit, input file digests, versions)

``main()`` compares the values with results.json and prints a table. Exit codes:
0 PASS, 2 PARTIAL, 1 FAIL. With ``--compare FILE`` it grades a reader's own
results file (same ``results`` list, ``id`` and ``value`` only) instead of
rebuilding, which is the prompt-level eval.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import platform
import subprocess
import sys
import tempfile
import urllib.request
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable

SCHEMA_VERSION = "1.0"
STATUSES = ("MATCH", "DRIFT", "MISMATCH", "NOT_EVALUATED", "ERROR")
EXIT = {"PASS": 0, "FAIL": 1, "PARTIAL": 2}
REQUIRED_TOP = ("schema_version", "report", "code", "inputs", "missing_data",
                "cannot_do", "eval", "results")
REQUIRED_RESULT = ("id", "label", "value", "unit", "period", "source", "method",
                   "repo", "commit", "tolerance", "basis", "limits")


def utc_now() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def fetch(url: str, dest: Path, headers: dict[str, str] | None = None,
          timeout: int = 120) -> dict[str, str]:
    """Download url to dest unless dest exists; return path, sha256 and time."""
    dest.parent.mkdir(parents=True, exist_ok=True)
    retrieved = "cached"
    if not dest.exists():
        req = urllib.request.Request(url, headers=headers or {"User-Agent": "report-eval"})
        with urllib.request.urlopen(req, timeout=timeout) as resp:  # nosec B310 (fixed https URL)
            dest.write_bytes(resp.read())
        retrieved = utc_now()
    return {"url": url, "path": str(dest), "sha256": sha256_file(dest),
            "retrieved_utc": retrieved}


def git_head(path: Path) -> str | None:
    try:
        out = subprocess.run(["git", "-C", str(path), "rev-parse", "HEAD"],
                             capture_output=True, text=True, timeout=30)
        return out.stdout.strip() or None
    except (OSError, subprocess.SubprocessError):
        return None


def validate(doc: dict[str, Any]) -> list[str]:
    """Structural check of results.json without third-party packages.

    The JSON Schema (results.schema.json) is the full contract; this check
    covers the fields an eval relies on, so a run never needs jsonschema.
    """
    errors = [f"missing top-level field: {k}" for k in REQUIRED_TOP if k not in doc]
    if doc.get("schema_version") != SCHEMA_VERSION:
        errors.append(f"schema_version is {doc.get('schema_version')!r}, expected {SCHEMA_VERSION}")
    seen = set()
    for i, r in enumerate(doc.get("results", [])):
        for k in REQUIRED_RESULT:
            if k not in r:
                errors.append(f"results[{i}] missing {k}")
        rid = r.get("id")
        if rid in seen:
            errors.append(f"duplicate result id {rid}")
        seen.add(rid)
        tol = r.get("tolerance", {})
        if not (isinstance(tol, dict) and len(tol) == 1 and set(tol) <= {"abs", "rel_pct"}):
            errors.append(f"results[{i}] tolerance must be one of {{abs}} or {{rel_pct}}")
        if r.get("basis") not in ("fixed", "live"):
            errors.append(f"results[{i}] basis must be fixed or live")
        if not r.get("limits"):
            errors.append(f"results[{i}] limits must list at least one limit")
        commit = str(r.get("commit", ""))
        if len(commit) != 40 or any(c not in "0123456789abcdef" for c in commit):
            errors.append(f"results[{i}] commit must be a full 40-character SHA")
    return errors


def compare_one(expected: dict[str, Any], got: Any) -> tuple[str, str]:
    """Return (status, difference text) for one result."""
    if got is None:
        return "NOT_EVALUATED", ""
    want = expected["value"]
    if isinstance(want, (int, float)) and not isinstance(want, bool):
        try:
            got_f = float(got)
        except (TypeError, ValueError):
            return "MISMATCH", f"non-numeric {got!r}"
        if not math.isfinite(got_f):
            return "MISMATCH", f"non-finite {got!r}"
        diff = got_f - float(want)
        tol = expected["tolerance"]
        if "abs" in tol:
            ok = abs(diff) <= float(tol["abs"]) + 1e-12
        else:
            base = abs(float(want)) or 1.0
            ok = abs(diff) / base * 100.0 <= float(tol["rel_pct"]) + 1e-12
        rel = (diff / abs(float(want)) * 100.0) if want else float("nan")
        text = f"{diff:+.6g} ({rel:+.3g} %)" if want else f"{diff:+.6g}"
    else:
        ok = got == want
        text = "" if ok else f"got {got!r}"
    if ok:
        return "MATCH", text
    return ("DRIFT" if expected["basis"] == "live" else "MISMATCH"), text


def grade(rows: list[dict[str, Any]]) -> str:
    statuses = {r["status"] for r in rows}
    if statuses & {"MISMATCH", "ERROR"}:
        return "FAIL"
    if statuses & {"DRIFT", "NOT_EVALUATED"}:
        return "PARTIAL"
    return "PASS"


def evaluate(doc: dict[str, Any], values: dict[str, Any],
             notes: dict[str, str]) -> list[dict[str, Any]]:
    rows = []
    for r in doc["results"]:
        rid = r["id"]
        got = values.get(rid)
        if isinstance(got, Exception):
            status, diff, got = "ERROR", f"{type(got).__name__}: {got}", None
        else:
            status, diff = compare_one(r, got)
        rows.append({"id": rid, "expected": r["value"], "got": got,
                     "unit": r["unit"], "status": status, "difference": diff,
                     "note": notes.get(rid, "")})
    return rows


def print_table(report_id: str, rows: list[dict[str, Any]], verdict: str) -> None:
    print(f"report: {report_id}")
    width = max(len(r["id"]) for r in rows) if rows else 10
    for r in rows:
        got = "-" if r["got"] is None else r["got"]
        line = f"  {r['status']:<13} {r['id']:<{width}}  expected {r['expected']} {r['unit']}  got {got}"
        if r["difference"]:
            line += f"  diff {r['difference']}"
        if r["note"]:
            line += f"  [{r['note']}]"
        print(line)
    counts = {s: sum(r["status"] == s for r in rows) for s in STATUSES}
    print("  counts: " + ", ".join(f"{k} {v}" for k, v in counts.items() if v))
    print(f"GRADE: {verdict}")


def scrub(value: Any) -> Any:
    """Replace local temp and home folders in the run record with placeholders."""
    if isinstance(value, dict):
        return {k: scrub(v) for k, v in value.items()}
    if isinstance(value, list):
        return [scrub(v) for v in value]
    if isinstance(value, str):
        for prefix, label in ((tempfile.gettempdir(), "<tmp>"), (str(Path.home()), "~")):
            for form in (prefix, prefix.replace("\\", "/")):
                value = value.replace(form, label)
    return value


def reader_values(path: Path) -> dict[str, Any]:
    data = json.loads(Path(path).read_text(encoding="utf-8"))
    items = data.get("results", data) if isinstance(data, dict) else data
    if isinstance(items, dict):
        return dict(items)
    return {item["id"]: item.get("value") for item in items}


def main(results_path: Path,
         compute: Callable[[argparse.Namespace], tuple[dict, dict, dict]],
         add_args: Callable[[argparse.ArgumentParser], None] | None = None,
         argv: list[str] | None = None) -> int:
    doc = json.loads(Path(results_path).read_text(encoding="utf-8"))
    ap = argparse.ArgumentParser(description=f"Eval for {doc['report']['title']}")
    ap.add_argument("--out", type=Path, help="write the eval record (JSON) here")
    ap.add_argument("--compare", type=Path,
                    help="grade a reader's results file instead of rebuilding")
    ap.add_argument("--validate-only", action="store_true",
                    help="check results.json against the contract and stop")
    if add_args:
        add_args(ap)
    args = ap.parse_args(argv)
    sys.stdout.reconfigure(encoding="utf-8")
    errors = validate(doc)
    if errors:
        print("results.json does not meet the contract:")
        for e in errors:
            print("  " + e)
        return EXIT["FAIL"]
    if args.validate_only:
        print(f"results.json valid ({len(doc['results'])} results)")
        return EXIT["PASS"]
    started = utc_now()
    if args.compare:
        mode = "prompt-level (reader results file)"
        values, notes, context = reader_values(args.compare), {}, {"reader_file": str(args.compare)}
    else:
        mode = "rebuild"
        values, notes, context = compute(args)
        if "repo_root" in context:  # keep the folder name only, not the local path
            context["repo_root"] = Path(context["repo_root"]).name
    rows = evaluate(doc, values, notes)
    verdict = grade(rows)
    print_table(doc["report"]["id"], rows, verdict)
    record = {
        "schema_version": SCHEMA_VERSION,
        "report_id": doc["report"]["id"],
        "mode": mode,
        "started_utc": started,
        "finished_utc": utc_now(),
        "grade": verdict,
        "python": platform.python_version(),
        "platform": platform.system(),
        "context": scrub(context),
        "results": rows,
    }
    if args.out:
        args.out.parent.mkdir(parents=True, exist_ok=True)
        args.out.write_text(json.dumps(record, indent=1, default=str) + "\n", encoding="utf-8")
    return EXIT[verdict]
