"""Solver legs of the baseline pack (#2300).

Each leg builds its fixed case from ``cases.py``, times the end-to-end run
(``wall_s``) and the solver-only phase (``solve_s``), proves the thread/rank
count the solver actually used and returns a numerical fingerprint sampled at
fixed points. Licence checkout happens before the solve timer starts.

Solver imports and executables are resolved inside each leg, so this module
imports on hosts that have none of them.
"""

from __future__ import annotations

import glob
import math
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

from digitalmodel.solvers.benchmark import cases
from digitalmodel.solvers.benchmark.runner import Case

_now = time.perf_counter


def _round(value: float, digits: int = 9) -> float:
    value = float(value)
    if not math.isfinite(value):
        raise ValueError(f"nonfinite result {value!r}")
    return float(f"{value:.{digits}g}")


# ------------------------------------------------------------------ OrcaFlex


def orcaflex_version() -> str | None:
    try:
        import OrcFxAPI

        return OrcFxAPI.DLLVersion()
    except Exception:
        return None


def run_orcaflex(work_dir: Path, threads: int) -> dict:
    import OrcFxAPI as api

    p = cases.ORCAFLEX
    start = _now()
    model = api.Model(threadCount=threads)  # licence checkout: outside solve_s
    env = model.environment
    env.WaterDepth = p["water_depth_m"]
    env.WaveType = p["wave"]["type"]
    env.WaveJONSWAPParameters = "Partially Specified"
    env.WaveHs = p["wave"]["hs_m"]
    env.WaveTp = p["wave"]["tp_s"]
    env.UserSpecifiedRandomWaveSeeds = "Yes"
    env.WaveSeed = p["wave"]["seed"]
    env.WaveNumberOfComponents = p["wave"]["components"]
    model.general.StageDuration = list(p["stage_durations_s"])
    model.general.ImplicitConstantTimeStep = p["time_step_s"]
    lines = []
    for k in range(p["risers"]):
        angle = 2 * math.pi * k / p["risers"]
        c, s = math.cos(angle), math.sin(angle)
        line = model.CreateObject(api.otLine, f"Riser{k + 1}")
        line.EndAConnection = "Fixed"
        line.EndBConnection = "Fixed"
        line.EndAX, line.EndAY, line.EndAZ = (p["end_a_radius_m"] * c,
                                              p["end_a_radius_m"] * s, 0.0)
        line.EndBX, line.EndBY, line.EndBZ = (p["end_b_radius_m"] * c,
                                              p["end_b_radius_m"] * s, p["end_b_z_m"])
        line.Length[0] = p["riser_length_m"]
        line.TargetSegmentLength[0] = p["segment_length_m"]
        lines.append(line)

    if model.threadCount != threads:
        raise ValueError(f"OrcaFlex thread count {model.threadCount} != {threads}")
    t0 = _now()
    model.CalculateStatics()
    t1 = _now()
    model.RunSimulation()
    t2 = _now()
    if not model.simulationComplete:
        raise ValueError("OrcaFlex simulation did not complete")

    riser = lines[0]
    static = riser.StaticResult("Effective Tension", api.oeEndA)
    samples = []
    for t in p["sample_times_s"]:
        value = riser.TimeHistory("Effective Tension", api.SpecifiedPeriod(t, t),
                                  api.oeEndA)
        samples.append(_round(value[0], 7))
    history = riser.TimeHistory("Effective Tension", api.Period(1), api.oeEndA)
    return {
        "ok": True,
        "wall_s": round(_now() - start, 3),
        "solve_s": round(t2 - t0, 3),
        "phases": {"statics_s": round(t1 - t0, 3), "dynamics_s": round(t2 - t1, 3)},
        "threads_observed": int(model.threadCount),
        "input_sha256": cases.digest(p),
        "fingerprint": {
            "static_tension_end_a_kN": _round(static, 7),
            "tension_end_a_at_samples_kN": samples,
            "tension_end_a_max_kN": _round(max(history), 7),
            "tension_end_a_min_kN": _round(min(history), 7),
        },
    }


# ---------------------------------------------------------- OrcaWave / AQWA


def _write_diffraction_inputs(work_dir: Path):
    import yaml

    from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec

    gdf = cases.write_barge_gdf(work_dir / "barge.gdf")
    spec_path = work_dir / "spec.yml"
    spec_path.write_text(yaml.safe_dump(cases.diffraction_spec("barge.gdf"),
                                        sort_keys=False))
    sha = cases.digest({"gdf": cases.digest(gdf),
                        "spec": cases.diffraction_spec("barge.gdf")})
    return DiffractionSpec.from_yaml(spec_path), spec_path, sha


def _sampled(values, indices):
    return [_round(values[i], 7) for i in indices]


def run_orcawave(work_dir: Path, threads: int) -> dict:
    import numpy as np
    import OrcFxAPI as api

    from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
        OrcaWaveRunner,
        RunConfig,
    )

    start = _now()
    spec, spec_path, sha = _write_diffraction_inputs(work_dir)
    runner = OrcaWaveRunner(RunConfig(output_dir=work_dir / "out", dry_run=True,
                                      use_api=True, thread_count=threads,
                                      validate_outputs=False))
    prepared = runner.prepare(spec, spec_path=spec_path)
    diffraction = api.Diffraction(str(prepared.input_file), threadCount=threads)
    if diffraction.threadCount != threads:
        raise ValueError(f"OrcaWave thread count {diffraction.threadCount} != {threads}")
    t0 = _now()
    diffraction.Calculate()
    solve = _now() - t0

    # OrcaWave frequencies are Hz, descending; the pack's are rad/s, ascending.
    order = np.argsort(np.asarray(diffraction.frequencies, dtype=float))
    added = np.asarray(diffraction.addedMass, dtype=float)[order, 2, 2]
    raos = np.asarray(diffraction.displacementRAOs)
    head0 = list(np.asarray(diffraction.headings, dtype=float)).index(0.0)
    heave = np.abs(raos[head0, order, 2])
    idx = cases.DIFFRACTION_SAMPLE_INDICES
    return {
        "ok": True,
        "wall_s": round(_now() - start, 3),
        "solve_s": round(solve, 3),
        "threads_observed": int(diffraction.threadCount),
        "input_sha256": sha,
        "fingerprint": {
            "frequencies": len(added),
            "a33_te_at_samples": _sampled(added, idx),
            "heave_rao_0deg_at_samples": _sampled(heave, idx),
        },
    }


def orcawave_version() -> str | None:
    return orcaflex_version()


def aqwa_executable() -> str | None:
    try:
        from digitalmodel.hydrodynamics.diffraction.aqwa_runner import (
            WINDOWS_AQWA_CANDIDATES,
        )
    except Exception:
        return None
    return next((str(p) for p in WINDOWS_AQWA_CANDIDATES if Path(p).exists()), None)


def aqwa_version() -> str | None:
    exe = aqwa_executable()
    if not exe:
        return None
    # ...\ANSYS Inc\v261\aqwa\bin\winx64\Aqwa.exe -> v261
    return next((part for part in Path(exe).parts if part.lower().startswith("v")
                 and part[1:].isdigit()), "unknown")


def run_aqwa(work_dir: Path, variant: int) -> dict:
    from digitalmodel.hydrodynamics.diffraction.aqwa_lis_parser import (
        extract_aqwa_added_mass,
        extract_aqwa_raos,
    )
    from digitalmodel.hydrodynamics.diffraction.aqwa_runner import (
        AQWARunConfig,
        AQWARunner,
    )

    start = _now()
    spec, spec_path, sha = _write_diffraction_inputs(work_dir)
    out = work_dir / "out"
    run = AQWARunner(AQWARunConfig(output_dir=out, dry_run=False,
                                   timeout_seconds=3600)).run(spec, spec_path=spec_path)
    wall = _now() - start
    status = str(getattr(run.status, "name", run.status))
    if status != "COMPLETED":
        return {"ok": False, "error": f"AQWA status {status}: {run.error_message}"}
    lis = sorted(out.rglob("*.LIS")) + sorted(out.rglob("*.lis"))
    if not lis:
        return {"ok": False, "error": "AQWA completed without a .LIS file"}

    added = extract_aqwa_added_mass(lis[0])
    freqs = sorted(added)
    a33 = [added[f][2][2] for f in freqs]
    raos = extract_aqwa_raos(lis[0])
    heave = []
    for f in freqs:
        match = next((v for (freq, head), v in raos.items()
                      if math.isclose(freq, f, rel_tol=1e-3) and abs(head) < 1e-6), None)
        heave.append(match["heave"][0] if match else math.nan)
    idx = cases.DIFFRACTION_SAMPLE_INDICES
    return {
        "ok": True,
        "wall_s": round(wall, 3),
        "solve_s": round(float(run.duration_seconds or wall), 3),
        "note": "AQWA core count is the solver default (not controlled by the pack)",
        "input_sha256": sha,
        "fingerprint": {
            "frequencies": len(freqs),
            "a33_kg_at_samples": _sampled(a33, idx),
            "heave_rao_0deg_at_samples": _sampled(heave, idx),
        },
    }


# --------------------------------------------------------------------- MAPDL


def mapdl_executable() -> str | None:
    if sys.platform == "win32":
        found = sorted(glob.glob(r"C:\Program Files\ANSYS Inc\v*\ansys\bin\winx64\MAPDL.exe"))
    else:
        found = sorted(glob.glob("/ansys_inc/v*/ansys/bin/mapdl")
                       + glob.glob("/usr/ansys_inc/v*/ansys/bin/mapdl"))
    return found[-1] if found else None


def mapdl_version() -> str | None:
    exe = mapdl_executable()
    if not exe:
        return None
    return next((part for part in Path(exe).parts if part.lower().startswith("v")
                 and part[1:].isdigit()), "unknown")


def run_mapdl(work_dir: Path, cores: int) -> dict:
    exe = mapdl_executable()
    if exe is None:
        return {"ok": False, "error": "MAPDL executable not found"}
    deck = work_dir / "block.inp"
    deck.write_text(cases.apdl_deck())
    command = [exe, "-b", "-smp", "-np", str(cores), "-j", "bench",
               "-dir", str(work_dir), "-i", str(deck), "-o", str(work_dir / "bench.out")]
    start = _now()
    proc = subprocess.run(command, cwd=work_dir, capture_output=True, text=True,
                          timeout=7200)
    wall = _now() - start
    out_file = work_dir / "bench.out"
    text = out_file.read_text(errors="replace") if out_file.exists() else ""
    fp_file = work_dir / "fingerprint.txt"
    if proc.returncode not in (0, 8) or not fp_file.exists():
        tail = " | ".join(text.strip().splitlines()[-3:]) or proc.stderr[-300:]
        return {"ok": False,
                "error": f"MAPDL rc={proc.returncode}, fingerprint missing: {tail}"}
    elapsed = cases.parse_mapdl_elapsed(text)
    threads = None
    for pattern in ("Total number of cores requested", "number of cores"):
        for line in text.splitlines():
            if pattern.lower() in line.lower():
                digits = [int(t) for t in line.replace("=", " ").split() if t.isdigit()]
                if digits:
                    threads = digits[-1]
                    break
        if threads is not None:
            break
    return {
        "ok": True,
        "wall_s": round(wall, 3),
        "solve_s": round(elapsed.get("solution_s", wall), 3),
        "phases": elapsed,
        "threads_observed": threads,
        "input_sha256": cases.digest(cases.apdl_deck()),
        "fingerprint": cases.parse_mapdl_fingerprint(fp_file),
    }


# ------------------------------------------------------------------ OpenFOAM


def _foam_root() -> Path | None:
    root = Path("/usr/lib/openfoam") / cases.OPENFOAM_VERSION
    return root if root.exists() else None


def openfoam_version() -> str | None:
    return cases.OPENFOAM_VERSION if _foam_root() else None


def _foam(case_dir: Path, command: str, log: str, timeout: int = 7200) -> str:
    bashrc = _foam_root() / "etc" / "bashrc"
    script = f"source {bashrc} >/dev/null 2>&1; cd '{case_dir}' && {command}"
    proc = subprocess.run(["bash", "-c", script], capture_output=True, text=True,
                          timeout=timeout)
    text = proc.stdout + proc.stderr
    (case_dir / f"log.{log}").write_text(text)
    if proc.returncode != 0:
        tail = " | ".join(text.strip().splitlines()[-3:])
        raise RuntimeError(f"{log} failed rc={proc.returncode}: {tail}")
    return text


def run_openfoam(work_dir: Path, n_procs: int) -> dict:
    root = _foam_root()
    if root is None:
        return {"ok": False, "error": f"{cases.OPENFOAM_VERSION} not installed"}
    case = work_dir / "motorBike"
    shutil.copytree(root / "tutorials" / cases.OPENFOAM_TUTORIAL, case)
    cases.prepare_openfoam_case(case, n_procs)
    tri = case / "constant" / "triSurface"
    tri.mkdir(parents=True, exist_ok=True)
    shutil.copy(root / "tutorials" / "resources" / "geometry" / "motorBike.obj.gz", tri)

    mesh_np = cases.OPENFOAM_MESH_PROCS
    mesh_dict = "-decomposeParDict system/decomposeParDict.mesh"
    start = _now()
    _foam(case, "surfaceFeatureExtract", "surfaceFeatureExtract")
    _foam(case, "blockMesh", "blockMesh")
    _foam(case, f"decomposePar {mesh_dict}", "decomposePar.mesh")
    _foam(case, f"mpirun -np {mesh_np} snappyHexMesh {mesh_dict} -parallel -overwrite",
          "snappyHexMesh")
    _foam(case, "reconstructParMesh -constant", "reconstructParMesh")
    for proc_dir in case.glob("processor*"):
        shutil.rmtree(proc_dir)
    shutil.copytree(case / "0.orig", case / "0")
    check = _foam(case, "checkMesh -constant", "checkMesh")
    mesh_s = _now() - start

    t0 = _now()
    _foam(case, "decomposePar", "decomposePar")
    _foam(case, f"mpirun -np {n_procs} potentialFoam -parallel -writephi",
          "potentialFoam")
    t1 = _now()
    log = _foam(case, f"mpirun -np {n_procs} simpleFoam -parallel", "simpleFoam")
    t2 = _now()

    procs = cases.parse_openfoam_procs(log)
    last = cases.parse_openfoam_last_time(log)
    if procs != n_procs:
        raise ValueError(f"simpleFoam ran on {procs} ranks, expected {n_procs}")
    if last != cases.OPENFOAM_ITERATIONS:
        raise ValueError(f"simpleFoam stopped at {last}, expected "
                         f"{cases.OPENFOAM_ITERATIONS} iterations")
    coeff = sorted(case.glob("postProcessing/forceCoeffs*/0/coefficient.dat"))
    if not coeff:
        raise ValueError("forceCoeffs output missing")
    forces = cases.parse_force_coeffs(coeff[0])
    return {
        "ok": True,
        "wall_s": round(_now() - start, 3),
        "solve_s": round(t2 - t1, 3),
        "phases": {"mesh_s": round(mesh_s, 3), "decompose_init_s": round(t1 - t0, 3),
                   "simpleFoam_s": round(t2 - t1, 3)},
        "threads_observed": procs,
        "input_sha256": cases.digest({"tutorial": cases.OPENFOAM_TUTORIAL,
                                      "version": cases.OPENFOAM_VERSION,
                                      "iterations": cases.OPENFOAM_ITERATIONS,
                                      "mesh_procs": mesh_np}),
        "fingerprint": {"cells": cases.parse_cell_count(check),
                        "cd": _round(forces["cd"], 7),
                        "iterations": forces["iterations"]},
    }


# --------------------------------------------------------------------- pack

CASES = {
    "orcaflex": lambda: Case("orcaflex_risers", "orcaflex", [1, "all"], run_orcaflex,
                             orcaflex_version, rel_tol=1e-6, abs_tol=1e-6),
    "orcawave": lambda: Case("orcawave_barge", "orcawave", [1, "all"], run_orcawave,
                             orcawave_version, rel_tol=1e-6, abs_tol=1e-9),
    "aqwa": lambda: Case("aqwa_barge", "aqwa", ["default"], run_aqwa, aqwa_version,
                         rel_tol=1e-4, abs_tol=1e-6, variant_label="cores"),
    "mapdl": lambda: Case("mapdl_block", "mapdl", [1, 4], run_mapdl, mapdl_version,
                          rel_tol=1e-6, abs_tol=1e-12, variant_label="smp_cores"),
    "openfoam": lambda: Case("openfoam_motorbike", "openfoam", [8, "all"],
                             run_openfoam, openfoam_version, rel_tol=1e-3,
                             abs_tol=1e-6, variant_label="mpi_ranks"),
}
