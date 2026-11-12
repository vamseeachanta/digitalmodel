"""Conservation and measured clearance gate for a zero-flow rolling hold.

Method: docs/domains/openfoam/resonant_vent_hold.md, Conservation and geometry (2026).
This module does not predict resonant amplification from a static tilt angle.
Acceptance tolerance, observation duration and sampling limit are inputs.
"""

from __future__ import annotations

import math
from collections.abc import Iterable, Mapping, Sequence
from typing import Any

from .sloshing_sweep import SweepCase


def worst_fill_resonant_cases(cases: Iterable[SweepCase]) -> tuple[SweepCase, ...]:
    """Keep every highest-fill near-resonant row, ordered by stable case ID.

    The near-resonance flag comes from the sweep specification, not a new band.
    Highest fill screens freeboard only; it does not bound dynamic run-up.
    """
    candidates = [case for case in cases if case.near_resonance]
    if not candidates:
        return ()
    fill = max(case.fill_fraction for case in candidates)
    return tuple(sorted(
        (case for case in candidates if case.fill_fraction == fill),
        key=lambda case: case.case_id,
    ))


def _finite(name: str, value: float) -> None:
    if not isinstance(value, (int, float)) or isinstance(value, bool) or not math.isfinite(value):
        raise ValueError(f"{name} must be finite")


def assess_vent_hold(
    series: Sequence[tuple[float, float]],
    *,
    case_id: str,
    initial_volume_m3: float,
    tank_height_m: float,
    fill_fraction: float,
    roll_period_s: float,
    cycles: int,
    tolerance_pct: float,
    max_sample_gap_s: float,
    runup_evidence: Mapping[str, Any] | None = None,
) -> dict[str, Any]:
    """Assess one prescribed rolling hold; absent evidence stays NOT_EVALUATED.

    V0 is the initialized inventory, not the first written sample. Both net
    drift and the largest absolute excursion from V0 are reported. No imposed
    exchange is allowed by this contract. The caller must establish roll motion
    and a successful solver exit; these cannot be inferred from a volume trace.

    Run-up evidence must describe the spatial maximum of the free surface in
    tank coordinates across the entire requested interval, for this case and
    the top-vent variant. Sparse probes and global-frame heights do not qualify.
    Its extraction method and numerical uncertainty belong in ``source``.
    """
    for name, value in (
        ("initial_volume_m3", initial_volume_m3), ("tank_height_m", tank_height_m),
        ("fill_fraction", fill_fraction), ("roll_period_s", roll_period_s),
        ("cycles", cycles), ("tolerance_pct", tolerance_pct),
        ("max_sample_gap_s", max_sample_gap_s),
    ):
        _finite(name, value)
    if min(initial_volume_m3, tank_height_m, roll_period_s, max_sample_gap_s) <= 0:
        raise ValueError("volume, height, period and sample gap must be positive")
    if not 0 < fill_fraction < 1 or cycles <= 0 or int(cycles) != cycles:
        raise ValueError("fill must be between zero and one; cycles a positive integer")
    if tolerance_pct < 0:
        raise ValueError("tolerance_pct must be non-negative")

    duration = cycles * roll_period_s
    _finite("duration", duration)
    previous_time = None
    for time, volume in series:
        _finite("sample time", time)
        _finite("sample volume", volume)
        if time < 0 or volume < 0:
            raise ValueError("sample times and volumes must be non-negative")
        if previous_time is not None and time <= previous_time:
            raise ValueError("sample times must be strictly increasing")
        previous_time = time

    checks = dict(volume="NOT_EVALUATED", coverage="NOT_EVALUATED",
                  headroom="NOT_EVALUATED")
    final_drift = max_deviation = max_loss = None
    if series:
        # Conservation definition: Method (2026), Conservation and geometry.
        # The factor 100 converts a dimensionless fraction to percent.
        deviations = [100.0 * (v - initial_volume_m3) / initial_volume_m3
                      for _, v in series]
        final_drift = deviations[-1]
        max_deviation = max(abs(d) for d in deviations)
        max_loss = max(0.0, -min(deviations))
        checks["volume"] = "PASS" if max_deviation <= tolerance_pct else "FAIL"
        gaps = [series[0][0]] + [b[0] - a[0] for a, b in zip(series, series[1:])]
        if (len(series) >= 2 and series[-1][0] >= duration
                and max(gaps) <= max_sample_gap_s):
            checks["coverage"] = "PASS"

    # Rectangular-tank geometry: Method (2026), Conservation and geometry.
    nominal_depth = fill_fraction * tank_height_m
    peak_runup = remaining = None
    if runup_evidence is not None:
        if not isinstance(runup_evidence, Mapping):
            raise ValueError("run-up evidence must be an object")
        if (runup_evidence.get("case_id") != case_id
                or runup_evidence.get("variant") != "vent_top"
                or runup_evidence.get("frame") != "tank"
                or not isinstance(runup_evidence.get("source"), str)
                or not runup_evidence["source"].strip()):
            raise ValueError("run-up evidence needs matching case, top vent, tank frame and source")
        for key in ("peak_elevation_m", "start_time_s", "end_time_s"):
            if key not in runup_evidence:
                raise ValueError(f"run-up evidence missing {key}")
            _finite(key, runup_evidence[key])
        if runup_evidence["start_time_s"] != 0 or runup_evidence["end_time_s"] < duration:
            raise ValueError("run-up evidence must cover the entire requested interval")
        peak = runup_evidence["peak_elevation_m"]
        if peak < nominal_depth:
            raise ValueError("spatial maximum including the initial surface is below nominal depth")
        peak_runup = peak - nominal_depth
        remaining = tank_height_m - peak
        # Strictly positive geometric clearance; no empirical safety factor.
        checks["headroom"] = "PASS" if remaining > 0 else "FAIL"

    status = ("FAIL" if "FAIL" in checks.values() else
              "PASS" if all(v == "PASS" for v in checks.values()) else "NOT_EVALUATED")
    return {
        "case_status": status,
        "checks": checks,
        "required_duration_s": duration,
        "max_sample_gap_s": max_sample_gap_s,
        "initial_volume_m3": initial_volume_m3,
        "tolerance_pct": tolerance_pct,
        "final_drift_pct": final_drift,
        "max_abs_deviation_pct": max_deviation,
        "max_loss_pct": max_loss,
        "nominal_freeboard_m": tank_height_m - nominal_depth,
        "peak_runup_m": peak_runup,
        "remaining_headroom_m": remaining,
        "runup_source": None if runup_evidence is None else runup_evidence["source"],
        "remedy": "raise_domain_and_repeat" if checks["headroom"] == "FAIL" else None,
        "matrix_status": "NOT_EVALUATED",
    }
