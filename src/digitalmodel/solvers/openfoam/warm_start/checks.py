"""Checkpoint rules R1--R6 and automatic cold fallback."""
from __future__ import annotations

import os
import re
import shutil
import subprocess
import time
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path

import numpy as np

from ..force_cycle_average import analyse, load_force


@dataclass(frozen=True)
class CheckResult:
    verdict: str
    reason: str | None
    iteration: int
    details: tuple[str, ...] = ()


def force_file(case: Path) -> Path:
    candidates = list((case / "postProcessing" / "forces_hull").glob("*/force.dat"))
    candidates += list((case / "postProcessing" / "forces").glob("*/force.dat"))
    if not candidates:
        raise FileNotFoundError(f"no force.dat under {case}")
    return max(candidates, key=lambda p: float(p.parent.name))


def power_gate(total: np.ndarray, pressure: np.ndarray, window=400) -> bool:
    if len(total) < 2 * window:
        return False
    a, b = total[-2*window:-window].mean(), total[-window:].mean()
    drift = abs(a-b) / max(abs(b), 1e-12)
    wobble = pressure[-window:].max() - pressure[-window:].min()
    return drift < .01 and wobble < .02 * max(abs(b), 1e-12)


def evaluate_checkpoint(case: Path, reference: dict, *, n_cold: int,
                        n_abort: int, checkpoint=400, hop="speed") -> CheckResult:
    path = force_file(case)
    _, t, total, pressure, viscous = load_force(path)
    current = int(t[-1])
    details = []
    amplitude = float(reference["first_cycle_amplitude_pressure"])
    excursion = float(pressure.max() - pressure.min())
    details.append(f"R1 pressure excursion={excursion:g}, limit={.5*amplitude:g}")
    if current >= checkpoint and excursion >= .5 * amplitude:
        return CheckResult("ABORT", "pressure_excursion", current, tuple(details))
    log_text = "\n".join(p.read_text(errors="ignore") for p in case.glob("log*"))
    if "FOAM FATAL" in log_text:
        return CheckResult("ABORT", "fatal", current, tuple(details))
    bounds = [float(v) for v in re.findall(r"bounding alpha\.water[^\n]*?([-+]\d+(?:\.\d+)?(?:[eE][-+]?\d+)?)", log_text)]
    if bounds and any(v < -.001 or v > 1.001 for v in bounds):
        return CheckResult("ABORT", "bounding", current, tuple(details))
    volumes = [float(v) for v in re.findall(r"Phase-1 volume fraction\s*=\s*([-+0-9.eE]+)", log_text)]
    if len(volumes) > 1 and abs(volumes[-1]-volumes[0])/max(abs(volumes[0]), 1e-12) > .005:
        return CheckResult("ABORT", "mass_drift", current, tuple(details))
    visc_ref = reference.get("settled_viscous")
    if isinstance(visc_ref, dict):
        speed = str(reference.get("target_speed", "")); visc_ref = visc_ref.get(speed)
    if visc_ref is not None and current >= checkpoint:
        error = abs(viscous[-100:].mean()-float(visc_ref))/max(abs(float(visc_ref)), 1e-12)
        details.append(f"R2 viscous error={error:.4%}")
        if error > .03:
            return CheckResult("ABORT", "viscous_off", current, tuple(details))
    if power_gate(total, pressure):
        if current <= .75 * n_cold:
            return CheckResult("OK", None, current, tuple(details))
    if current >= n_cold:
        return CheckResult("ABORT", "cap", current, tuple(details))
    if current >= n_abort:
        try:
            fit = analyse(path, start=max(0, checkpoint))
            cycle = fit.get("cycles", [{}])[0].get("total")
            valid = cycle is not None and all(k in fit for k in ("aitken_total", "fit_total"))
            valid &= all(abs(fit[k]-cycle)/max(abs(cycle), 1e-12) < .02 for k in ("aitken_total", "fit_total"))
        except Exception:
            valid = False
        if not valid:
            return CheckResult("ABORT", "no_asymptote", current, tuple(details))
    return CheckResult("CONTINUE", None, current, tuple(details))


def stop_and_fallback(case: Path, reason: str, relaunch: str | None = None,
                      pid: int | None = None) -> Path:
    subprocess.run(["foamDictionary", "system/controlDict", "-entry", "stopAt", "-set", "writeNow"],
                   cwd=case, check=True)
    if pid:
        while Path(f"/proc/{pid}/cwd").exists():
            time.sleep(1)
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    archive = case / f"warm_attempt_{stamp}"; archive.mkdir()
    for log in case.glob("log*"):
        shutil.move(str(log), archive / log.name)
    if (case / "postProcessing").exists():
        shutil.copytree(case / "postProcessing", archive / "postProcessing")
    if (case / "0").exists():
        shutil.rmtree(case / "0")
    shutil.copytree(case / "0.cold", case / "0")
    (case / "WARM_ABORTED").write_text(reason + "\n")
    (case / "COLD_FALLBACK").write_text(reason + "\n")
    command = relaunch or str(case / "solve_chain.sh")
    subprocess.Popen(command, cwd=case, shell=True, start_new_session=True,
                     stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    return archive
