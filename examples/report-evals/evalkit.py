# Copyright (c) 2026 AceEngineer. Licensed under the MIT License.
"""Shared helpers for report evals (results.json contract, schema version 1.0).

Standard library only. The same file is kept, byte for byte, in each public
repository that holds report evals (digitalmodel, worldenergydata,
aceengineer-website), so a reader can run one eval without installing anything
for it.

An eval script supplies ``compute(args, values, notes, context)``, which fills:
  values  - result id -> rebuilt value; leave an id out if it was not evaluated,
            or store an Exception to mark it ERROR
  notes   - result id -> one-line note (why not evaluated, what drifted)
  context - facts about the run (code revision, input file digests, versions)
The dicts are filled in place, so values computed before a failure are kept.

``main()`` grades the values against results.json, prints a table and writes
the run record (``--out``) even when compute() fails. Exit codes: 0 PASS,
2 PARTIAL, 1 FAIL.

Comparison contract (also in the schema description):
  - Every result in results.json is graded; a missing value is NOT_EVALUATED.
  - A numeric result needs a finite int or float (a boolean is rejected); a
    boolean result needs a boolean; a string result needs an equal string.
  - abs tolerance t:     |got - expected| <= t
    rel_pct tolerance r: |got - expected| <= r / 100 * |expected|
    Both bounds are inclusive. When both values are integers the comparison
    is exact integer arithmetic. Otherwise a floating allowance of
    1e-9 * |expected| is added for binary representation; it is zero when the
    expected value is 0, so a zero reference is exact under a relative
    tolerance or an absolute tolerance of 0.
  - Outside tolerance: DRIFT on a live basis, MISMATCH on a fixed basis.
  - Grade: FAIL if any MISMATCH or ERROR, a reader-file structural error, or
    an unexpected exception from compute() (even after every value was
    filled); else PARTIAL if any DRIFT or NOT_EVALUATED; else PASS.
  - Revision (rebuild mode): --revision candidate (default) uses the code and
    repository inputs of the checkout; --revision pinned reads them from
    code.commit through git (repo_file()). The record states which.
  - The run record is always written: to --out, or to a new, uniquely named
    file under <temp>/report-evals-runs/ (created exclusively, never replaced).
    It is bound to the bytes of the files it graded;
    --check-record reports a changed or missing file as stale.
  - Scope: "both" (default) is graded in every mode; "rebuild" only when the
    eval rebuilds; "pinned" only in a --revision pinned rebuild.
  - ``--compare FILE`` (prompt-level eval) grades only results whose scope is
    "both" (the default). Unknown or duplicate ids in the reader file make the
    grade FAIL; an empty file grades PARTIAL.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import platform
import subprocess
import sys
import tempfile
import traceback
import urllib.request
from datetime import datetime, timezone
from fractions import Fraction
from pathlib import Path
from typing import Any, Callable

SCHEMA_VERSION = "1.0"
STATUSES = ("MATCH", "DRIFT", "MISMATCH", "NOT_EVALUATED", "ERROR")
EXIT = {"PASS": 0, "FAIL": 1, "PARTIAL": 2}
REQUIRED_TOP = ("schema_version", "report", "code", "inputs", "missing_data",
                "cannot_do", "eval", "results")
REQUIRED_REPORT = ("id", "title", "url", "published", "family")
REQUIRED_CODE = ("repo", "commit", "paths")
REQUIRED_RESULT = ("id", "label", "value", "unit", "period", "source", "method",
                   "repo", "commit", "tolerance", "basis", "limits")
# Files whose bytes a rebuild record is bound to; a prompt-level record also binds PROMPT.md.
REBUILD_ARTIFACTS = ("results.json", "eval.py", "../evalkit.py")
PROMPT_ARTIFACTS = REBUILD_ARTIFACTS + ("PROMPT.md",)


def utc_now() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def sha256_text(path: Path) -> str:
    """SHA-256 of a text file with line endings normalized to LF.

    Git may store LF and check out CRLF, so artifact identity uses LF bytes.
    """
    data = Path(path).read_bytes().replace(b"\r\n", b"\n")
    return hashlib.sha256(data).hexdigest()


def fetch(url: str, dest: Path, headers: dict[str, str] | None = None,
          timeout: int = 120) -> dict[str, str]:
    """Download url to dest unless dest exists; return path, sha256 and time."""
    dest.parent.mkdir(parents=True, exist_ok=True)
    retrieved = "cached"
    if not dest.exists():
        req = urllib.request.Request(url, headers=headers or {"User-Agent": "report-eval"})
        with urllib.request.urlopen(req, timeout=timeout) as resp:  # nosec B310 (fixed https URL)
            data = resp.read()
        tmp = dest.with_name(dest.name + ".part")
        tmp.write_bytes(data)
        tmp.replace(dest)
        retrieved = utc_now()
    return {"url": url, "path": str(dest), "sha256": sha256_file(dest),
            "retrieved_utc": retrieved}


# Variables that would make git ignore "-C <path>" and act on another repository.
GIT_BINDINGS = ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE",
                "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES", "GIT_NAMESPACE")


def git_env() -> dict[str, str]:
    """Environment for git subprocesses, without the variables in GIT_BINDINGS."""
    return {k: v for k, v in os.environ.items() if k.upper() not in GIT_BINDINGS}


def git_head(path: Path) -> str | None:
    try:
        out = subprocess.run(["git", "-C", str(path), "rev-parse", "HEAD"],
                             capture_output=True, text=True, timeout=30, env=git_env())
        return out.stdout.strip() or None
    except (OSError, subprocess.SubprocessError):
        return None


def git_blob(repo: Path, commit: str, path: str, fetch: bool = True) -> bytes | None:
    """Bytes of path at commit in the clone repo, or None if the object is not available.

    With fetch=True a missing commit is fetched from origin once; with
    fetch=False (offline runs) no network access is attempted.
    """
    def show():
        return subprocess.run(["git", "-C", str(repo), "show", f"{commit}:{path}"],
                              capture_output=True, timeout=300, env=git_env())
    try:
        out = show()
        if out.returncode != 0 and fetch:
            subprocess.run(["git", "-C", str(repo), "fetch", "-q", "origin", commit],
                           capture_output=True, timeout=600, env=git_env())
            out = show()
        return out.stdout if out.returncode == 0 else None
    except (OSError, subprocess.SubprocessError):
        return None


def repo_file(args: argparse.Namespace, rel: str) -> Path:
    """Path of a repository file at the selected revision.

    candidate: the file in the checkout (args.repo_root). pinned: the blob at
    code.commit, written to a private temporary directory per invocation with the
    same relative path, so scripts that load files beside them still work.
    """
    if getattr(args, "revision", "candidate") != "pinned":
        return Path(args.repo_root) / rel
    blob = git_blob(Path(args.repo_root), args.pinned_commit, rel,
                    fetch=not getattr(args, "offline", False))
    if blob is None:
        raise FileNotFoundError(f"{rel} at {args.pinned_commit[:12]} is not readable from {args.repo_root}")
    if not hasattr(args, "_pinned_root"):
        args._pinned_root = Path(tempfile.mkdtemp(prefix="report-evals-pinned-"))
    dest = args._pinned_root / rel
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_bytes(blob)
    return dest


def guard(ids: list[str], values: dict, notes: dict, fn: Callable[[], None]) -> None:
    """Run one independent check; a failure marks only its own result ids.

    A source that cannot be reached (OSError, which includes URLError and
    timeouts) leaves its ids NOT_EVALUATED with a note; any other exception
    marks them ERROR. Ids already filled before the failure keep their values.
    """
    try:
        fn()
    except OSError as exc:
        for rid in ids:
            if rid not in values:
                notes[rid] = f"source unavailable: {type(exc).__name__}: {exc}"[:200]
    except Exception as exc:  # noqa: BLE001 (record, do not crash the run)
        for rid in ids:
            if rid not in values:
                values[rid] = exc


def _is_number(v: Any) -> bool:
    return isinstance(v, (int, float)) and not isinstance(v, bool)


_JSON_TYPES = {"object": dict, "array": list, "string": str, "boolean": bool}


def _type_ok(value: Any, name: str) -> bool:
    if name == "number":
        return _is_number(value)
    if name == "integer":
        return _is_number(value) and float(value).is_integer()
    if name == "null":
        return value is None
    return isinstance(value, _JSON_TYPES[name])


def schema_errors(instance: Any, schema: dict, root: dict | None = None, path: str = "$") -> list[str]:
    """Validate against the JSON Schema keywords used by results.schema.json.

    Standard library only. Covers $ref (local), const, enum, type, oneOf,
    required, properties, additionalProperties (false), items, minItems,
    pattern and minimum; annotation keywords (title, description, format,
    default) are not checked. The repository tests cross-check with jsonschema
    where it is installed.
    """
    import re
    root = root or schema
    if "$ref" in schema:
        node: Any = root
        for part in schema["$ref"].lstrip("#/").split("/"):
            node = node[part]
        return schema_errors(instance, node, root, path)
    errs: list[str] = []
    if "const" in schema and instance != schema["const"]:
        errs.append(f"{path}: must be {schema['const']!r}")
    if "enum" in schema and instance not in schema["enum"]:
        errs.append(f"{path}: must be one of {schema['enum']}")
    if "type" in schema:
        names = schema["type"] if isinstance(schema["type"], list) else [schema["type"]]
        if not any(_type_ok(instance, n) for n in names):
            return errs + [f"{path}: must be of type {'/'.join(names)}"]
    if "oneOf" in schema:
        passing = sum(not schema_errors(instance, sub, root, path) for sub in schema["oneOf"])
        if passing != 1:
            errs.append(f"{path}: must match exactly one allowed form")
    if isinstance(instance, dict):
        errs += [f"{path}: missing {k}" for k in schema.get("required", []) if k not in instance]
        props = schema.get("properties", {})
        for k, v in instance.items():
            if k in props:
                errs += schema_errors(v, props[k], root, f"{path}.{k}")
            elif schema.get("additionalProperties") is False:
                errs.append(f"{path}: unexpected field {k}")
    if isinstance(instance, list):
        if len(instance) < schema.get("minItems", 0):
            errs.append(f"{path}: needs at least {schema['minItems']} items")
        if "items" in schema:
            for i, v in enumerate(instance):
                errs += schema_errors(v, schema["items"], root, f"{path}[{i}]")
    if isinstance(instance, str) and "pattern" in schema and not re.search(schema["pattern"], instance):
        errs.append(f"{path}: does not match {schema['pattern']}")
    if _is_number(instance) and "minimum" in schema and instance < schema["minimum"]:
        errs.append(f"{path}: must be >= {schema['minimum']}")
    return errs


def validate(doc: dict[str, Any], schema: dict | None = None) -> list[str]:
    """Check results.json: the full schema (when given) plus the grading rules.

    Standard library only; no network and no code from the report is run.
    """
    errors = schema_errors(doc, schema) if schema is not None else []
    errors += [f"missing top-level field: {k}" for k in REQUIRED_TOP if k not in doc]
    if doc.get("schema_version") != SCHEMA_VERSION:
        errors.append(f"schema_version is {doc.get('schema_version')!r}, expected {SCHEMA_VERSION}")
    errors += [f"report missing {k}" for k in REQUIRED_REPORT if k not in doc.get("report", {})]
    errors += [f"code missing {k}" for k in REQUIRED_CODE if k not in doc.get("code", {})]
    results = doc.get("results")
    if not isinstance(results, list) or not results:
        errors.append("results must be a non-empty list")
        results = []
    seen = set()
    for i, r in enumerate(results):
        if not isinstance(r, dict):
            errors.append(f"results[{i}] must be an object")
            continue
        errors += [f"results[{i}] missing {k}" for k in REQUIRED_RESULT if k not in r]
        rid = r.get("id")
        if rid in seen:
            errors.append(f"duplicate result id {rid}")
        seen.add(rid)
        tol = r.get("tolerance", {})
        if not (isinstance(tol, dict) and len(tol) == 1 and set(tol) <= {"abs", "rel_pct"}):
            errors.append(f"results[{i}] tolerance must be one of {{abs}} or {{rel_pct}}")
        else:
            t = next(iter(tol.values()))
            if not _is_number(t) or not math.isfinite(t) or t < 0:
                errors.append(f"results[{i}] tolerance must be a finite number >= 0")
        value = r.get("value")
        if _is_number(value) and not math.isfinite(value):
            errors.append(f"results[{i}] value must be finite")
        if not isinstance(value, (int, float, str, bool)):
            errors.append(f"results[{i}] value must be a number, string or boolean")
        if r.get("basis") not in ("fixed", "live"):
            errors.append(f"results[{i}] basis must be fixed or live")
        if r.get("scope", "both") not in ("both", "rebuild", "pinned"):
            errors.append(f"results[{i}] scope must be both, rebuild or pinned")
        if not r.get("limits"):
            errors.append(f"results[{i}] limits must list at least one limit")
        commit = str(r.get("commit", ""))
        if len(commit) != 40 or any(c not in "0123456789abcdef" for c in commit):
            errors.append(f"results[{i}] commit must be a full 40-character SHA")
    return errors


def compare_one(expected: dict[str, Any], got: Any) -> tuple[str, str]:
    """Return (status, difference text) for one result (see module docstring)."""
    if got is None:
        return "NOT_EVALUATED", ""
    want = expected["value"]
    if isinstance(want, bool):
        if not isinstance(got, bool):
            return "MISMATCH", f"expected a boolean, got {type(got).__name__}"
        ok, text = got == want, ""
    elif _is_number(want):
        if not _is_number(got):
            return "MISMATCH", f"expected a number, got {type(got).__name__}"
        try:
            if not math.isfinite(got):
                return "MISMATCH", f"non-finite {got!r}"
            diff = float(got) - float(want)
        except OverflowError:  # an integer beyond the float range (for example 401 digits)
            return "MISMATCH", "number outside the comparable range"
        tol = expected["tolerance"]
        both_int = isinstance(want, int) and isinstance(got, int)
        if both_int:
            bound = (Fraction(str(tol["abs"])) if "abs" in tol else
                     Fraction(str(tol["rel_pct"])) / 100 * abs(want))
            ok = abs(got - want) <= bound
        else:
            bound = (float(tol["abs"]) if "abs" in tol else
                     float(tol["rel_pct"]) / 100.0 * abs(float(want)))
            allowance = 1e-9 * abs(float(want))  # float representation only
            ok = abs(diff) <= bound + allowance
        text = f"{diff:+.6g} ({diff / abs(float(want)) * 100:+.3g} %)" if want else f"{diff:+.6g}"
    else:
        if not isinstance(got, str):
            return "MISMATCH", f"expected a string, got {type(got).__name__}"
        ok, text = got == want, ""
    if ok:
        return "MATCH", text
    return ("DRIFT" if expected["basis"] == "live" else "MISMATCH"), text or f"got {got!r}"


def grade(rows: list[dict[str, Any]], structural_errors: list[str] | None = None) -> str:
    statuses = {r["status"] for r in rows}
    if structural_errors or statuses & {"MISMATCH", "ERROR"}:
        return "FAIL"
    if statuses & {"DRIFT", "NOT_EVALUATED"} or not rows:
        return "PARTIAL"
    return "PASS"


def evaluate(results: list[dict[str, Any]], values: dict[str, Any],
             notes: dict[str, str]) -> list[dict[str, Any]]:
    rows = []
    for r in results:
        rid = r["id"]
        got = values.get(rid)
        if isinstance(got, Exception):
            status, diff, got = "ERROR", f"{type(got).__name__}: {got}"[:300], None
        else:
            status, diff = compare_one(r, got)
        rows.append({"id": rid, "expected": r["value"], "got": got, "unit": r["unit"],
                     "basis": r["basis"], "status": status, "difference": diff,
                     "note": notes.get(rid, "")})
    return rows


def print_table(report_id: str, rows: list[dict[str, Any]], verdict: str,
                errors: list[str]) -> None:
    print(f"report: {report_id}")
    width = max((len(r["id"]) for r in rows), default=10)
    for r in rows:
        got = "-" if r["got"] is None else r["got"]
        line = f"  {r['status']:<13} {r['id']:<{width}}  expected {r['expected']} {r['unit']}  got {got}"
        if r["difference"]:
            line += f"  diff {r['difference']}"
        if r["note"]:
            line += f"  [{r['note']}]"
        print(line)
    for e in errors:
        print(f"  ERROR         {e}")
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


def reader_values(path: Path, known: set[str]) -> tuple[dict[str, Any], list[str]]:
    """Read a reader's results file; return values and structural errors."""
    try:
        data = json.loads(Path(path).read_text(encoding="utf-8"))
    except (OSError, ValueError) as exc:
        return {}, [f"reader file unreadable: {exc}"]
    items = data.get("results") if isinstance(data, dict) else None
    if not isinstance(items, list):
        return {}, ['reader file must be {"results": [{"id": ..., "value": ...}, ...]}']
    values, errors = {}, []
    for i, item in enumerate(items):
        if not isinstance(item, dict) or "id" not in item or "value" not in item:
            errors.append(f"reader results[{i}] needs id and value")
            continue
        rid = item["id"]
        if not isinstance(rid, str):
            errors.append(f"reader results[{i}] id must be a string")
            continue
        if rid in values:
            errors.append(f"reader file repeats id {rid}")
        elif rid not in known:
            errors.append(f"reader file has unknown id {rid}")
        values[rid] = item["value"]
    return values, errors


def artifacts(folder: Path, names: tuple[str, ...]) -> dict[str, str | None]:
    """LF-normalized SHA-256 of each named file; None for a file that does not exist."""
    return {name: sha256_text(folder / name) if (folder / name).is_file() else None for name in names}


def check_record(folder: Path, record_path: Path) -> list[str]:
    """Problems that make a run record stale for the files it graded."""
    try:
        record = json.loads(Path(record_path).read_text(encoding="utf-8"))
    except (OSError, ValueError) as exc:
        return [f"run record unreadable: {exc}"]
    names = PROMPT_ARTIFACTS if record.get("mode", "").startswith("prompt") else REBUILD_ARTIFACTS
    bound = record.get("artifacts", {})
    problems = [f"{n} not bound in the record" for n in names if not bound.get(n)]
    for name, digest in artifacts(folder, names).items():
        if digest is None:
            problems.append(f"{name} is missing")
        elif bound.get(name) and bound[name] != digest:
            problems.append(f"{name} changed since the run")
    return problems


def main(results_path: Path,
         compute: Callable[[argparse.Namespace, dict, dict, dict], None],
         add_args: Callable[[argparse.ArgumentParser], None] | None = None,
         argv: list[str] | None = None) -> int:
    folder = Path(results_path).resolve().parent
    doc = json.loads(Path(results_path).read_text(encoding="utf-8"))
    ap = argparse.ArgumentParser(description=f"Eval for {doc['report']['title']}")
    ap.add_argument("--out", type=Path,
                    help="write the run record (JSON) here; default: a new file under "
                         "<temp>/report-evals-runs/ named by report, mode and time")
    ap.add_argument("--revision", choices=("candidate", "pinned"), default="candidate",
                    help="candidate: code and repository inputs of this checkout; "
                         "pinned: code and repository inputs read from code.commit")
    ap.add_argument("--compare", type=Path,
                    help="grade a reader's results file instead of rebuilding")
    ap.add_argument("--validate-only", action="store_true",
                    help="check results.json against the contract and stop (no network, no code run)")
    ap.add_argument("--check-record", type=Path, metavar="RUN_JSON",
                    help="check that a run record is bound to the current files and stop")
    if add_args:
        add_args(ap)
    args = ap.parse_args(argv)
    args.pinned_commit = doc["code"]["commit"]
    try:
        sys.stdout.reconfigure(encoding="utf-8")
    except AttributeError:
        pass
    schema_path = folder.parent / "results.schema.json"
    schema = json.loads(schema_path.read_text(encoding="utf-8")) if schema_path.exists() else None
    errors = validate(doc, schema)
    if schema is None:
        errors.append(f"schema not found: {schema_path}")
    if errors:
        print("results.json does not meet the contract:")
        for e in errors:
            print("  " + e)
        return EXIT["FAIL"]
    if args.validate_only:
        print(f"results.json valid against results.schema.json ({len(doc['results'])} results)")
        return EXIT["PASS"]
    if args.check_record:
        problems = check_record(folder, args.check_record)
        print("run record is current" if not problems else "run record is stale:\n  " + "\n  ".join(problems))
        return EXIT["FAIL"] if problems else EXIT["PASS"]

    started = utc_now()
    values: dict[str, Any] = {}
    notes: dict[str, str] = {}
    context: dict[str, Any] = {}
    structural: list[str] = []
    if args.compare:
        mode = "prompt-level"
        graded = [r for r in doc["results"] if r.get("scope", "both") == "both"]
        values, structural = reader_values(args.compare, {r["id"] for r in graded})
        try:
            context["reader_file_sha256"] = sha256_file(args.compare) if args.compare.is_file() else None
        except OSError:
            context["reader_file_sha256"] = None
        names = PROMPT_ARTIFACTS
    else:
        mode = "rebuild"
        # scope pinned: graded only in a pinned rebuild (for example code identity with the pin)
        graded = [r for r in doc["results"]
                  if r.get("scope", "both") != "pinned" or args.revision == "pinned"]
        names = REBUILD_ARTIFACTS
        context["revision"] = args.revision
        try:
            compute(args, values, notes, context)
        except Exception as exc:  # noqa: BLE001 (record the failure, keep finished values)
            message = "".join(traceback.format_exception_only(type(exc), exc)).strip()
            context["compute_error"] = message
            # An unexpected failure fails the run even if every value was already filled.
            structural.append(f"compute() raised: {message}"[:300])
            for r in graded:
                if r["id"] not in values and r["id"] not in notes:
                    values[r["id"]] = exc
    if "repo_root" in context:  # keep the folder name only, not the local path
        context["repo_root"] = Path(context["repo_root"]).name
    context["artifacts_repo_head"] = git_head(folder)
    try:
        status = subprocess.run(["git", "-C", str(folder), "status", "--porcelain", "--", *names],
                                capture_output=True, text=True, timeout=30, env=git_env())
        context["artifacts_dirty"] = bool(status.stdout.strip()) if status.returncode == 0 else None
    except (OSError, subprocess.SubprocessError):
        context["artifacts_dirty"] = None
    head = context.get("repo_head")
    if mode == "rebuild" and "code_mode" not in context:
        # historical: code and repository inputs at the pinned commit; candidate: another revision.
        context["code_mode"] = ("historical" if args.revision == "pinned" or head == doc["code"]["commit"]
                                else "candidate")
    rows = evaluate(graded, values, notes)
    verdict = grade(rows, structural)
    print_table(doc["report"]["id"], rows, verdict, structural)
    record = {
        "schema_version": SCHEMA_VERSION,
        "report_id": doc["report"]["id"],
        "mode": mode,
        "started_utc": started,
        "finished_utc": utc_now(),
        "grade": verdict,
        "pinned_commit": doc["code"]["commit"],
        "artifacts": artifacts(folder, names),
        "python": platform.python_version(),
        "platform": platform.system(),
        "context": scrub(context),
        "errors": structural,
        "results": rows,
    }
    text = json.dumps(record, indent=1, default=str) + "\n"
    if args.out:  # an explicit destination is replaced on purpose
        out = args.out
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_text(text, encoding="utf-8")
    else:  # a new, uniquely named file for every run, created exclusively
        runs = Path(tempfile.gettempdir()) / "report-evals-runs"
        runs.mkdir(parents=True, exist_ok=True)
        tag = f"{doc['report']['id']}-{mode}-{context.get('revision', 'reader')}-{started.replace(':', '')}-"
        handle, name = tempfile.mkstemp(prefix=tag, suffix=".json", dir=runs)
        with os.fdopen(handle, "w", encoding="utf-8") as fh:
            fh.write(text)
        out = Path(name)
    print(f"run record: {scrub(str(out))}")
    return EXIT[verdict]
