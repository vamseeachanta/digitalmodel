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

from . import env

PACK_VERSION = "1"

BUSY_AVG_PCT = 10.0
BUSY_CORE_PCT = 80.0

# Only these per-repeat keys reach the receipt; everything else a case returns
# (stdout, environment values, paths) is dropped before serialisation.
_REPEAT_KEYS = ("ok", "wall_s", "solve_s", "phases", "fingerprint",
                "threads_observed", "input_sha256", "timing_basis", "note", "error")

# Environment variables whose values name licence servers.
_LICENCE_ENV = ("ANSYSLMD_LICENSE_FILE", "ANSYSLI_SERVERS", "ORCINA_LICENSE_FILE",
                "LM_LICENSE_FILE")

_ABS_PATH = re.compile(
    r"(?:\\\\[^\s\\]+\\|[A-Za-z]:[\\/]|/(?=[^\s/]+/))"  # UNC, drive, posix dir
    r"(?:[^\s\\/]+[\\/])*"                              # directories
    r"(?P<base>[^\s\\/]*)"                              # keep the basename
)


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
    """Strip hostnames, licence servers and directories (keeping basenames)."""
    text = str(text)
    hosts = {platform.node()}
    for key in _LICENCE_ENV:
        for item in re.split(r"[;,]", os.environ.get(key, "")):
            host = item.split("@")[-1].strip()
            if host:
                hosts.add(host.split(":")[0])
    for host in sorted(filter(None, hosts), key=len, reverse=True):
        text = re.sub(re.escape(host), "<host>", text, flags=re.I)
    text = re.sub(r"\d+@[\w.\-<>]+", "<licence-server>", text)
    text = _ABS_PATH.sub(lambda m: m.group("base"), text)
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


def _positive(value) -> bool:
    return isinstance(value, (int, float)) and math.isfinite(value) and value > 0


def _clean(outcome: dict, variant) -> dict:
    kept = {k: outcome[k] for k in _REPEAT_KEYS if k in outcome}
    for key in ("error", "note"):
        if key in kept:
            kept[key] = sanitise(kept[key])
    if kept.get("ok"):
        reason = _finite_fingerprint(kept.get("fingerprint"))
        if reason is None and not _positive(kept.get("solve_s")):
            reason = f"solve time missing or invalid: {kept.get('solve_s')!r}"
        threads = kept.get("threads_observed")
        if reason is None and isinstance(variant, int) and threads != variant:
            reason = f"thread/rank count observed {threads!r} != requested {variant}"
        if reason:
            kept.update(ok=False, error=reason)
    return kept


def _run_once(case: Case, work_dir: Path, variant: int) -> dict:
    work_dir.mkdir(parents=True, exist_ok=True)
    try:
        return _clean(case.run(work_dir, variant), variant)
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


def default_lock_path() -> Path:
    """A lock path shared by every user on the machine (not the per-user temp)."""
    if os.name == "nt":
        return Path(os.environ.get("ProgramData", r"C:\ProgramData")) / "solver_benchmark.lock"
    return Path("/tmp/solver_benchmark.lock")


def _pid_alive(pid: int) -> bool:
    if pid <= 0:
        return False
    if os.name == "nt":
        import ctypes

        kernel32 = ctypes.windll.kernel32
        handle = kernel32.OpenProcess(0x1000, False, pid)  # QUERY_LIMITED_INFORMATION
        if not handle:
            return False
        code = ctypes.c_ulong()
        kernel32.GetExitCodeProcess(handle, ctypes.byref(code))
        kernel32.CloseHandle(handle)
        return code.value == 259  # STILL_ACTIVE
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


@contextlib.contextmanager
def pack_lock(path: Path):
    """Refuse to start while a live benchmark process holds the machine-wide lock.

    A lock left by a process that no longer exists is taken over; on exit the
    lock is removed only if it still names this process.
    """
    path = Path(path)
    me = str(os.getpid())
    for _ in range(2):
        try:
            fd = os.open(path, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
            break
        except FileExistsError:
            try:
                owner = int(path.read_text().strip() or 0)
            except (OSError, ValueError):
                owner = 0
            if _pid_alive(owner):
                raise RuntimeError(f"another benchmark (pid {owner}) holds {path}")
            with contextlib.suppress(FileNotFoundError):
                path.unlink()
    else:
        raise RuntimeError(f"another benchmark holds {path}")
    try:
        os.write(fd, me.encode())
        os.close(fd)
        yield
    finally:
        with contextlib.suppress(OSError, ValueError):
            if path.read_text().strip() == me:
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
    entry["timing_basis"] = ok[0].get("timing_basis", "solver") if ok else None
    if len({r.get("input_sha256") for r in ok}) > 1:
        errors.append("input digest differs between repeats")
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
    from .cases import resolve_variants

    if repeats < 1 or warmup < 0:
        raise ValueError(f"repeats must be >= 1 and warmup >= 0 "
                         f"(got repeats={repeats}, warmup={warmup})")
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
