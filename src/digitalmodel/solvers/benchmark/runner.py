"""Run the baseline pack: case x variant x repeat, aggregated into one receipt.

A receipt is only comparable to another of the same ``PACK_VERSION``; bump it
whenever any case definition in ``cases.py`` or ``solvers.py`` changes.
"""

from __future__ import annotations

import contextlib
import datetime as _dt
import math
import os
import platform
import re
import shutil
import statistics
import traceback
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable

from digitalmodel.solvers.benchmark import env

PACK_VERSION = "1"

BUSY_AVG_PCT = 10.0
BUSY_CORE_PCT = 80.0

# Only these per-repeat keys reach the receipt; everything else a case returns
# (stdout, environment values, paths) is dropped before serialisation.
_REPEAT_KEYS = ("ok", "wall_s", "solve_s", "phases", "fingerprint",
                "threads_observed", "input_sha256", "note", "error")


@dataclass
class Case:
    name: str
    solver: str
    variants: list
    run: Callable[[Path, int], dict]
    version: Callable[[], str | None]
    rel_tol: float = 1e-6
    abs_tol: float = 0.0
    variant_label: str = "threads"
    extra: dict = field(default_factory=dict)


def sanitise(text: str) -> str:
    """Strip hostnames, user home paths and licence-server addresses."""
    text = str(text)
    node = platform.node()
    if node:
        text = re.sub(re.escape(node), "<host>", text, flags=re.I)
    text = re.sub(r"\d+@[\w.\-<>]+", "<licence-server>", text)
    text = re.sub(r"[A-Za-z]:\\Users\\[^\\\s]+", r"<home>", text)
    text = re.sub(r"/(home|Users)/[^/\s]+", "<home>", text)
    return text[:500]


def _finite_fingerprint(fp) -> str | None:
    """Reason the fingerprint is unusable, or None when it is valid."""
    if not isinstance(fp, dict) or not fp:
        return "fingerprint missing"
    for key, value in fp.items():
        values = value if isinstance(value, list) else [value]
        for v in values:
            if not isinstance(v, (int, float)) or not math.isfinite(v):
                return f"fingerprint {key} is not finite: {v!r}"
    return None


def _clean(outcome: dict) -> dict:
    kept = {k: outcome[k] for k in _REPEAT_KEYS if k in outcome}
    if "error" in kept:
        kept["error"] = sanitise(kept["error"])
    if kept.get("ok"):
        reason = _finite_fingerprint(kept.get("fingerprint"))
        if reason:
            kept.update(ok=False, error=reason)
    return kept


def _run_once(case: Case, work_dir: Path, variant: int) -> dict:
    work_dir.mkdir(parents=True, exist_ok=True)
    try:
        return _clean(case.run(work_dir, variant))
    except Exception as exc:
        return {"ok": False, "error": sanitise(f"{type(exc).__name__}: {exc}")}


def _stats(values) -> dict | None:
    if not values:
        return None
    return {"median": statistics.median(values), "min": min(values), "max": max(values)}


def _same(a: dict, b: dict, rel: float, ab: float) -> bool:
    if a.keys() != b.keys():
        return False
    for key in a:
        xs = a[key] if isinstance(a[key], list) else [a[key]]
        ys = b[key] if isinstance(b[key], list) else [b[key]]
        if len(xs) != len(ys):
            return False
        if not all(math.isclose(x, y, rel_tol=rel, abs_tol=ab) for x, y in zip(xs, ys)):
            return False
    return True


def _busy(load: dict | None) -> str | None:
    if load is None:
        return None
    if load["avg_pct"] > BUSY_AVG_PCT or load["max_core_pct"] > BUSY_CORE_PCT:
        return (f"host busy: CPU avg {load['avg_pct']} % (limit {BUSY_AVG_PCT}), "
                f"busiest core {load['max_core_pct']} % (limit {BUSY_CORE_PCT})")
    return None


@contextlib.contextmanager
def pack_lock(path: Path):
    """Refuse to start while another benchmark holds the machine-wide lock."""
    path = Path(path)
    try:
        fd = os.open(path, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
    except FileExistsError:
        raise RuntimeError(f"another benchmark holds {path}; remove it if stale")
    try:
        os.write(fd, str(os.getpid()).encode())
        os.close(fd)
        yield
    finally:
        with contextlib.suppress(FileNotFoundError):
            path.unlink()


def _run_variant(case, variant, root, repeats, warmup, load_sampler, allow_busy,
                 keep):
    entry = {"case": case.name, "solver": case.solver, "variant": variant,
             "variant_label": case.variant_label, "warmup": warmup,
             "solver_version": case.version(), "rel_tol": case.rel_tol,
             "abs_tol": case.abs_tol, "load_before": [], "busy_allowed": False}
    repeats_out, errors = [], []
    for i in range(warmup + repeats):
        load = load_sampler()
        entry["load_before"].append(load)
        reason = _busy(load)
        if reason and not allow_busy:
            entry["skipped"] = reason
            break
        if reason:
            entry["busy_allowed"] = True
        work = root / f"{case.name}-{variant}-{i}"
        outcome = _run_once(case, work, variant)
        if not keep:
            shutil.rmtree(work, ignore_errors=True)
        if i < warmup:
            continue
        repeats_out.append(outcome)
        if not outcome.get("ok"):
            errors.append(outcome.get("error", "failed without an error message"))

    ok = [r for r in repeats_out if r.get("ok")]
    entry["n_ok"] = len(ok)
    entry["repeats"] = repeats_out
    entry["errors"] = errors
    entry["wall_s"] = _stats([r["wall_s"] for r in ok if "wall_s" in r])
    entry["solve_s"] = _stats([r["solve_s"] for r in ok if "solve_s" in r])
    entry["threads_observed"] = sorted({r["threads_observed"] for r in ok
                                        if r.get("threads_observed") is not None})
    entry["fingerprint"] = ok[0]["fingerprint"] if ok else None
    entry["input_sha256"] = ok[0].get("input_sha256") if ok else None
    entry["fingerprint_consistent"] = bool(ok) and all(
        _same(ok[0]["fingerprint"], r["fingerprint"], case.rel_tol, case.abs_tol)
        for r in ok[1:])
    entry["complete"] = (len(ok) == repeats and not errors
                         and "skipped" not in entry)
    entry["baseline_eligible"] = (entry["complete"] and entry["fingerprint_consistent"]
                                  and not entry["busy_allowed"])
    return entry


def run_pack(cases, work_root: Path, *, machine_label: str, repeats: int = 3,
             warmup: int = 1, load_sampler=env.sample_cpu_load,
             allow_busy: bool = False, keep: bool = False,
             cores: int | None = None) -> dict:
    from digitalmodel.solvers.benchmark.cases import resolve_variants

    work_root = Path(work_root)
    work_root.mkdir(parents=True, exist_ok=True)
    cores = cores or os.cpu_count() or 1
    started = _dt.datetime.now(_dt.timezone.utc)
    results = []
    for case in cases:
        for variant in resolve_variants(case.variants, cores):
            results.append(_run_variant(case, variant, work_root, repeats, warmup,
                                        load_sampler, allow_busy, keep))
    return {
        "pack_version": PACK_VERSION,
        "machine_label": machine_label,
        "started_utc": started.isoformat(timespec="seconds"),
        "finished_utc": _dt.datetime.now(_dt.timezone.utc).isoformat(timespec="seconds"),
        "environment": env.machine_environment(),
        "repeats": repeats,
        "results": results,
        "ok": bool(results) and all(r["complete"] and r["fingerprint_consistent"]
                                    for r in results),
    }
