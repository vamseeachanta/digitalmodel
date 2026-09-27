"""Native parallel OrcaFlex runner: N worker processes, one licence and one solver thread each.

No queue service. The caller passes a list of cases and an *adapter* (``"module:attribute"``) that
knows how to build a text-YAML model for a case and what to extract from the result. Per case, a
worker:

1. builds the model (``adapter.build(case, model_dir) -> master.yml``) and hashes the input
   (every model file plus the canonical case JSON);
2. loads it with one solver thread, applies ``adapter.prepare(model, case)`` if present, and solves
   statics (``adapter.statics(model, case)`` if present, else ``CalculateStatics``) and, for
   ``analysis == "dynamics"``, the simulation;
3. saves the result (``.sim``), **reopens it** and verifies its state (static state, or a completed
   simulation that reached its stop time) - plan Section 8 "reopen-and-verify";
4. extracts the required channels from the *reopened* result (``adapter.extract``) to a compact
   JSON results file with its SHA-256;
5. records timings per step, CPU time, the OrcaFlex DLL version and the digests.

The ``.sim`` file is kept locally (referenced by its digest) or deleted after verification.

Retry policy (owner decision W306): at most ``max_attempts`` per case, and only for infrastructure
faults (licence, file I/O, a result that will not reopen); never for statics divergence, a failed or
unstable simulation, a verification or extraction failure, or a model-build error. A batch stops
(pending cases are cancelled, running ones finish) on solver version drift, two consecutive licence
faults, or when more than 5 % of the batch has failed.

Every attempt is appended to ``<out_dir>/ledger.jsonl``.
"""

from __future__ import annotations

import datetime as dt
import hashlib
import importlib
import json
import math
import os
import re
import shutil
import time
from concurrent.futures import FIRST_COMPLETED, Future, ProcessPoolExecutor, wait
from pathlib import Path
from typing import Any, Iterable

DEFAULT_MAX_WORKERS = 57  # owner decision W325: 90 % of the 64 cores of the licensed host
ANALYSES = ("statics", "dynamics")
CASE_ID = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$")
RETRYABLE = {"licence_fault", "infra_fault"}

# --------------------------------------------------------------------------- policy


def fault_class(phase: str, *, licensing: bool, io_error: bool) -> tuple[str, bool]:
    """(status, retry) for a failure in ``phase``. Only infrastructure faults are retried (W306)."""
    if licensing:
        return "licence_fault", True
    if io_error or phase == "reopen":
        return "infra_fault", True
    return {
        "build": "build_failed",
        "load": "load_failed",
        "prepare": "build_failed",
        "statics": "statics_diverged",
        "dynamics": "dynamics_failed",
        "save": "infra_fault",
        "verify": "verify_failed",
        "extract": "extraction_failed",
    }.get(phase, "failed"), False


class BatchGuard:
    """Batch stop rules: solver version drift, two consecutive licence faults, > 5 % of the batch failed."""

    def __init__(self, batch_size: int, fail_fraction: float = 0.05):
        self.limit = max(0, math.floor(fail_fraction * batch_size))
        self.failed = 0
        self.licence_run = 0
        self.stop_reason: str | None = None

    def record(self, attempt: dict, *, final: bool) -> None:
        s = attempt["status"]
        if s == "version_drift":
            self.stop_reason = self.stop_reason or "solver version drift from the pin"
        self.licence_run = self.licence_run + 1 if s == "licence_fault" else 0
        if self.licence_run >= 2:
            self.stop_reason = self.stop_reason or "two consecutive licence faults"
        if final and s != "ok":
            self.failed += 1
            if self.failed > self.limit:
                self.stop_reason = self.stop_reason or f"more than 5 % of the batch failed ({self.failed} cases)"


def check_workers(n: int) -> int:
    ceiling = os.cpu_count() or 1
    if not 1 <= int(n) <= ceiling:
        raise ValueError(f"max_workers must be 1..{ceiling} (cores on this host), got {n}")
    return int(n)


def check_cases(cases: Iterable[dict]) -> list[dict]:
    cases = list(cases)
    seen = set()
    for c in cases:
        cid = c.get("case_id", "")
        if not CASE_ID.match(cid):
            raise ValueError(f"case_id {cid!r}: letters, digits, '.', '_' and '-' only")
        if cid in seen:
            raise ValueError(f"duplicate case_id {cid!r}")
        seen.add(cid)
        if c.get("analysis") not in ANALYSES:
            raise ValueError(f"{cid}: analysis {c.get('analysis')!r} not in {ANALYSES}")
        json.dumps(c)  # must be JSON-serialisable (it is hashed and sent to a worker)
    return cases


# --------------------------------------------------------------------------- digests


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def input_digest(master: Path, case: dict) -> str:
    """SHA-256 over every file of the text model (master.yml and all files below its folder, sorted by
    relative path) and the canonical JSON of the case."""
    master = Path(master)
    root = master.parent
    h = hashlib.sha256()
    for p in sorted((q for q in root.rglob("*") if q.is_file() and q.suffix in (".yml", ".yaml")),
                    key=lambda q: q.relative_to(root).as_posix()):
        h.update(f"{p.relative_to(root).as_posix()}:{sha256_file(p)}\n".encode())
    h.update(json.dumps(case, sort_keys=True, separators=(",", ":")).encode())
    return h.hexdigest()


# --------------------------------------------------------------------------- worker

_WORKER: dict[str, Any] = {}


def _utc() -> str:
    return dt.datetime.now(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def _adapter(path: str):
    mod, _, attr = path.partition(":")
    obj = importlib.import_module(mod)
    for part in attr.split("."):
        obj = getattr(obj, part)
    return obj


def _api(pin: str, expected_dll: str):
    from digitalmodel.solvers.orcaflex import orcaflex_api

    if not _WORKER.get("configured"):
        orcaflex_api.configure(pin)
        _WORKER["configured"] = True
    ofx = orcaflex_api.api()
    return ofx, ofx.DLLVersion()


def _is_licensing(ofx, exc: BaseException) -> bool:
    status = getattr(exc, "status", None)
    code = getattr(ofx, "stLicensingError", object())
    return status == code or "licen" in str(exc).lower()


def _is_io(ofx, exc: BaseException) -> bool:
    status = getattr(exc, "status", None)
    return isinstance(exc, OSError) or status in (getattr(ofx, "stFileReadError", object()),
                                                  getattr(ofx, "stFileWriteError", object()))


def _verify(ofx, model, analysis: str) -> None:
    state = model.state
    if analysis == "statics":
        if state != ofx.ModelState.InStaticState:
            raise RuntimeError(f"reopened result is in state {state!r}, expected the static state")
        return
    if state != ofx.ModelState.SimulationStopped:
        raise RuntimeError(f"reopened result is in state {state!r}, expected a completed simulation")
    ts = model.simulationTimeStatus
    if ts.CurrentTime < ts.StopTime - 1e-6:
        raise RuntimeError(f"simulation ended at {ts.CurrentTime} s before its stop time {ts.StopTime} s")


def run_one(case: dict, *, adapter: str, out_dir: str, attempt: int = 1, pin: str = "11.6",
            expected_dll: str = "11.6c", threads: int = 1, keep_sim: bool = False) -> dict:
    """Run one case in this process; never raises - failures are returned as a ledger record."""
    out = Path(out_dir)
    cid = case["case_id"]
    work = out / "work" / cid
    rec: dict[str, Any] = {"case_id": cid, "attempt": attempt, "analysis": case["analysis"],
                           "started_utc": _utc(), "worker_pid": os.getpid(), "threads": threads,
                           "input_sha256": None, "orcaflex_dll": None, "sim_sha256": None, "sim_bytes": None,
                           "sim_kept": None, "results_path": None, "results_sha256": None, "message": ""}
    timings: dict[str, float] = {}
    t_all, cpu0 = time.perf_counter(), time.process_time()
    phase = "load"
    ofx = None
    try:
        ofx, dll = _api(pin, expected_dll)
        rec["orcaflex_dll"] = dll
        if dll != expected_dll:
            rec.update(status="version_drift", retry=False, message=f"DLL {dll} != pinned {expected_dll}")
            return rec
        ad = _adapter(adapter)
        if work.exists():
            shutil.rmtree(work)
        phase = "build"
        t = time.perf_counter()
        master = Path(ad.build(case, work / "model"))
        rec["input_sha256"] = input_digest(master, case)
        timings["build"] = time.perf_counter() - t

        phase = "load"
        t = time.perf_counter()
        model = ofx.Model(str(master))
        model.threadCount = threads
        timings["load"] = time.perf_counter() - t
        phase = "prepare"
        if hasattr(ad, "prepare"):
            ad.prepare(model, case)

        phase = "statics"
        t = time.perf_counter()
        info = ad.statics(model, case) if hasattr(ad, "statics") else None
        if not hasattr(ad, "statics"):
            model.CalculateStatics()
        timings["statics"] = time.perf_counter() - t

        if case["analysis"] == "dynamics":
            phase = "dynamics"
            t = time.perf_counter()
            model.RunSimulation()
            timings["dynamics"] = time.perf_counter() - t
            if model.state == ofx.ModelState.SimulationStoppedUnstable:
                raise RuntimeError("simulation stopped unstable")

        phase = "save"
        t = time.perf_counter()
        sim = work / f"{cid}.sim"
        model.SaveSimulation(str(sim))
        del model
        timings["save"] = time.perf_counter() - t
        rec["sim_sha256"], rec["sim_bytes"] = sha256_file(sim), sim.stat().st_size

        phase = "reopen"
        t = time.perf_counter()
        reopened = ofx.Model(str(sim))
        timings["reopen"] = time.perf_counter() - t
        phase = "verify"
        _verify(ofx, reopened, case["analysis"])

        phase = "extract"
        t = time.perf_counter()
        channels = ad.extract(reopened, case)
        timings["extract"] = time.perf_counter() - t
        res = out / "results" / f"{cid}.json"
        res.parent.mkdir(parents=True, exist_ok=True)
        res.write_text(json.dumps({"case_id": cid, "input_sha256": rec["input_sha256"], "sim_sha256": rec["sim_sha256"],
                                   "statics_info": info, "channels": channels}, indent=1, sort_keys=True),
                       encoding="utf-8")
        rec["results_path"] = res.relative_to(out).as_posix()
        rec["results_sha256"] = sha256_file(res)
        del reopened
        if keep_sim:
            rec["sim_kept"] = sim.relative_to(out).as_posix()
        else:
            sim.unlink()
        rec.update(status="ok", retry=False)
    except Exception as exc:  # noqa: BLE001 - every failure becomes a ledger record
        lic = ofx is not None and _is_licensing(ofx, exc)
        io = ofx is not None and _is_io(ofx, exc)
        status, retry = fault_class(phase, licensing=lic, io_error=io)
        rec.update(status=status, retry=retry, phase=phase, message=f"{type(exc).__name__}: {exc}"[:2000])
    finally:
        timings["total"] = time.perf_counter() - t_all
        rec["timings_s"] = {k: round(v, 3) for k, v in timings.items()}
        rec["cpu_s"] = round(time.process_time() - cpu0, 3)
        rec["finished_utc"] = _utc()
        if not keep_sim or rec.get("status") != "ok":
            shutil.rmtree(work / "model", ignore_errors=True)
    return rec


# --------------------------------------------------------------------------- parent


def run_cases(cases: Iterable[dict], *, adapter: str, out_dir: Path | str, max_workers: int = DEFAULT_MAX_WORKERS,
              max_attempts: int = 3, keep_sim: bool = False, pin: str = "11.6", expected_dll: str = "11.6c",
              threads: int = 1, on_record=None) -> dict:
    """Run ``cases`` on ``max_workers`` processes; returns the final record per case and the stop reason."""
    cases = check_cases(cases)
    workers = check_workers(min(max_workers, max(1, len(cases))))
    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)
    ledger = out / "ledger.jsonl"
    guard = BatchGuard(len(cases))
    final: dict[str, dict] = {}
    kw = dict(adapter=adapter, out_dir=str(out), pin=pin, expected_dll=expected_dll, threads=threads,
              keep_sim=keep_sim)
    t0 = time.perf_counter()
    with ProcessPoolExecutor(max_workers=workers) as pool:
        pending: dict[Future, tuple[dict, int]] = {pool.submit(run_one, c, attempt=1, **kw): (c, 1) for c in cases}
        while pending:
            done, _ = wait(pending, return_when=FIRST_COMPLETED)
            for fut in done:
                case, attempt = pending.pop(fut)
                if fut.cancelled():
                    continue
                try:
                    rec = fut.result()
                except Exception as exc:  # a worker process died
                    rec = {"case_id": case["case_id"], "attempt": attempt, "status": "infra_fault", "retry": True,
                           "message": f"worker lost: {type(exc).__name__}: {exc}", "finished_utc": _utc()}
                again = rec.get("retry") and attempt < max_attempts and guard.stop_reason is None
                guard.record(rec, final=not again)
                with ledger.open("a", encoding="utf-8") as fh:
                    fh.write(json.dumps(rec, sort_keys=True) + "\n")
                if on_record is not None:
                    on_record(rec)
                if again and guard.stop_reason is None:
                    pending[pool.submit(run_one, case, attempt=attempt + 1, **kw)] = (case, attempt + 1)
                else:
                    final[case["case_id"]] = rec
            if guard.stop_reason:
                for f in list(pending):
                    if f.cancel():
                        c, a = pending.pop(f)
                        final[c["case_id"]] = {"case_id": c["case_id"], "attempt": a - 1, "status": "not_run",
                                               "message": f"batch stopped: {guard.stop_reason}"}
    results = [final[c["case_id"]] for c in cases if c["case_id"] in final]
    return {"results": results, "stop_reason": guard.stop_reason, "workers": workers,
            "wall_s": round(time.perf_counter() - t0, 3)}
