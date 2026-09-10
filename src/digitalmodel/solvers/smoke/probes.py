"""End-to-end solver licence probes shared by the CLI script and the engine arm.

Each probe *solves something small* rather than just checking that a binding
imports — licence checkout is the failure mode that matters on a licensed host,
and only an actual solve exercises it.

Kept dependency-light at module import: the solver imports happen inside the
probe functions so this module is importable on hosts with neither solver.
"""

from __future__ import annotations

import math
import os
import platform
import sys
import time
import traceback
from pathlib import Path

# .../src/digitalmodel/solvers/smoke/probes.py -> repo root
REPO_ROOT = Path(__file__).resolve().parents[4]

# The AQWA probe reuses the repo-managed acceptance fixture (5-panel unit box,
# 3 frequencies x 3 headings) so the deck stays small and self-contained.
AQWA_SPEC = (
    REPO_ROOT
    / "tests"
    / "hydrodynamics"
    / "diffraction"
    / "fixtures"
    / "acceptance_610"
    / "spec.yml"
)


def _result(solver: str, ok: bool, **detail) -> dict:
    return {"solver": solver, "ok": ok, **detail}


def check_orcaflex(work_dir: Path) -> dict:
    """Import OrcFxAPI, solve statics, run a short dynamic sim, round-trip files."""
    detail: dict = {"thread_count_requested": 1}
    start = time.monotonic()
    try:
        import OrcFxAPI
    except Exception as exc:
        return _result(
            "orcaflex", False, stage="import", error=f"{type(exc).__name__}: {exc}"
        )

    detail["module"] = getattr(OrcFxAPI, "__file__", None)
    try:
        detail["dll_version"] = OrcFxAPI.DLLVersion()
    except Exception as exc:  # pragma: no cover - version call is advisory
        detail["dll_version"] = f"unavailable ({exc})"

    try:
        _solve_orcaflex(OrcFxAPI, work_dir, detail)
    except Exception as exc:
        detail.update(
            error=f"{type(exc).__name__}: {exc}",
            traceback=traceback.format_exc(limit=5),
        )
        return _result("orcaflex", False, **detail)

    detail.pop("stage", None)
    detail["elapsed_s"] = round(time.monotonic() - start, 1)
    return _result("orcaflex", True, **detail)


def _finite(value) -> float:
    value = float(value)
    if not math.isfinite(value):
        raise ValueError("nonfinite numerical result")
    return value


def _thread_proof(model, role: str, detail: dict) -> None:
    count = model.threadCount
    if count != 1:
        raise ValueError(f"{role} thread count must equal 1, got {count}")
    detail.setdefault("thread_counts_observed", {})[role] = int(count)


def _completed(model, api, detail: dict, prefix: str = "") -> None:
    detail[prefix + "simulation_state"] = str(model.state)
    complete = bool(model.simulationComplete)
    detail[prefix + "simulation_complete"] = complete
    if not complete or model.state != api.ModelState.SimulationStopped:
        raise ValueError("dynamics must be complete and in SimulationStopped state")


def _history_proof(line, api, detail: dict, prefix: str = "") -> int:
    history = line.TimeHistory("Effective Tension", api.Period(1), api.oeEndA)
    if len(history) < 2:
        raise ValueError("dynamic history requires at least two samples")
    values = [_finite(value) for value in history]
    detail[prefix + "dynamic_samples"] = len(values)
    detail[prefix + "dynamic_tension_kN"] = [
        round(min(values), 3), round(max(values), 3)
    ]
    detail[prefix + "dynamic_finite"] = True
    return len(values)


def _solve_orcaflex(api, work_dir: Path, detail: dict) -> None:
    model = line = None
    try:
        detail["stage"] = "construct"
        model = api.Model(threadCount=1)
        line = model.CreateObject(api.otLine, "SmokeTestLine")
        line.EndAX, line.EndAY, line.EndAZ = 0.0, 0.0, 0.0
        line.EndBX, line.EndBY, line.EndBZ = 50.0, 0.0, -30.0
        line.Length[0] = 70.0
        line.TargetSegmentLength[0] = 5.0
        detail["stage"] = "statics"
        model.CalculateStatics()
        detail["statics_state"] = str(model.state)
        tension = _finite(line.StaticResult("Effective Tension", api.oeEndA))
        detail.update(static_tension_kN=round(tension, 3), static_finite=True)
        detail["stage"] = "dynamics"
        model.general.StageDuration = [4.0, 8.0]
        model.environment.WaveType = "Airy"
        model.environment.WaveHeight = 2.0
        model.environment.WavePeriod = 8.0
        model.RunSimulation()
        _thread_proof(model, "solve", detail)
        _completed(model, api, detail)
        _history_proof(line, api, detail)
        _saved_proof(model, api, work_dir, detail)
    finally:
        line = None
        model = None


def _saved_proof(model, api, work_dir: Path, detail: dict) -> None:
    reader = line = None
    try:
        detail["stage"] = "save_data"
        work_dir.mkdir(parents=True, exist_ok=True)
        dat, sim = work_dir / "smoke.dat", work_dir / "smoke.sim"
        model.SaveData(dat)
        detail["stage"] = "load_data"
        reader = api.Model(dat, threadCount=1)
        _thread_proof(reader, "data_reader", detail)
        reader = None
        detail["stage"] = "save_simulation"
        model.SaveSimulation(sim)
        detail["sim_bytes"] = sim.stat().st_size
        if detail["sim_bytes"] <= 0:
            raise ValueError("saved simulation must be nonempty")
        detail["stage"] = "load_simulation"
        reader = api.Model(threadCount=1)
        reader.LoadSimulation(sim)
        detail["saved_sim_reloaded"] = True
        _thread_proof(reader, "simulation_reader", detail)
        line = reader["SmokeTestLine"]
        _completed(reader, api, detail, "reloaded_")
        count = _history_proof(line, api, detail, "reloaded_")
        if count != detail["dynamic_samples"]:
            raise ValueError("reloaded history sample count differs from solve")
    finally:
        line = None
        reader = None


def check_aqwa(work_dir: Path) -> dict:
    """Run the acceptance diffraction deck through AQWARunner and require rc=0."""
    detail: dict = {}
    start = time.monotonic()
    try:
        from digitalmodel.hydrodynamics.diffraction.aqwa_runner import (
            AQWARunConfig,
            AQWARunner,
            WINDOWS_AQWA_CANDIDATES,
        )
        from digitalmodel.hydrodynamics.diffraction.input_schemas import (
            DiffractionSpec,
        )
    except Exception as exc:
        return _result(
            "aqwa", False, stage="import", error=f"{type(exc).__name__}: {exc}"
        )

    executable = next((p for p in WINDOWS_AQWA_CANDIDATES if Path(p).exists()), None)
    detail["executable"] = executable
    detail["licence_env"] = {
        key: os.environ.get(key)
        for key in ("ANSYSLMD_LICENSE_FILE", "ANSYSLI_SERVERS")
    }
    if executable is None:
        return _result(
            "aqwa",
            False,
            stage="detect",
            error="no Aqwa.exe found on any known install path",
            **detail,
        )

    if not AQWA_SPEC.exists():
        return _result(
            "aqwa",
            False,
            stage="fixture",
            error=f"acceptance spec missing: {AQWA_SPEC}",
            **detail,
        )

    work_dir.mkdir(parents=True, exist_ok=True)
    try:
        spec = DiffractionSpec.from_yaml(AQWA_SPEC)
        runner = AQWARunner(
            AQWARunConfig(output_dir=work_dir, dry_run=False, timeout_seconds=900)
        )
        run = runner.run(spec, spec_path=AQWA_SPEC)
    except Exception as exc:
        detail.update(
            stage="solve",
            error=f"{type(exc).__name__}: {exc}",
            traceback=traceback.format_exc(limit=5),
        )
        return _result("aqwa", False, **detail)

    status = str(getattr(run.status, "name", run.status))
    detail["status"] = status
    detail["return_code"] = getattr(run, "return_code", None)
    detail["error_message"] = run.error_message
    detail["elapsed_s"] = round(time.monotonic() - start, 1)

    # A DRY_RUN status means the runner silently fell back to not solving - that
    # is a failure here even though the call itself succeeded.
    listing = [p.name for p in work_dir.rglob("*") if p.is_file()]
    detail["produced_lis"] = any(n.upper().endswith(".LIS") for n in listing)
    ok = status == "COMPLETED" and detail["produced_lis"]
    return _result("aqwa", ok, **detail)


CHECKS = {"orcaflex": check_orcaflex, "aqwa": check_aqwa}


def run_probes(solvers, work_root: Path, include_host: bool = False) -> dict:
    """Run the named probes under ``work_root`` and return the report dict."""
    results = []
    for name in solvers:
        results.append(CHECKS[name](Path(work_root) / name))
    report = {
        "python": sys.executable,
        "results": results,
        "ok": all(r["ok"] for r in results),
    }
    # Hostnames are private deployment data in this fleet - opt in explicitly.
    if include_host:
        report["host"] = platform.node()
    return report
