# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Offline fatigue assessment of named stress histories.

No extraction or solver binding is used. The caller supplies already resolved
scalar stresses and a curve whose ``cycles(ranges)`` returns allowable counts.
History samples are scalar stresses; curve inputs are counted stress RANGES
in the same declared unit. No amplitude conversion, mean-stress, thickness or
environmental correction is applied. CurveBasis records the caller's chosen
basis, not its qualification.

Counting and damage use the existing rainflow_count and miner_damage primitives.
Residual pairs retain their half-cycle weights exactly once. Exact ranges are
used without binning. For each location, design-life damage equals exposure
damage times design_life / exposure_duration: the exposure is assumed to repeat
throughout that life. Both durations use the caller's single time_unit, avoiding
an implicit hours-to-years convention. Locations are assessed separately, never
summed; the greatest design-life damage governs, with lexical name order for ties.
"""

from __future__ import annotations

from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from numbers import Real
from typing import Protocol

import numpy as np

from .damage import miner_damage
from .rainflow import rainflow_count


class SNCurve(Protocol):
    """Caller-supplied range-based curve compatible with miner_damage."""

    def cycles(self, stress_ranges: np.ndarray) -> np.ndarray:
        """Return positive allowable counts; positive infinity means no damage."""
        ...


@dataclass(frozen=True)
class CurveBasis:
    """Explicit curve provenance, assumptions and range stress unit."""

    description: str
    stress_unit: str


@dataclass(frozen=True)
class StressHistory:
    """Ordered real scalar stress samples and their explicit stress unit."""

    samples: Sequence[float] | np.ndarray
    stress_unit: str


@dataclass(frozen=True)
class CycleDamage:
    """Unbinned exposure cycle with the primitive's count and Miner contribution."""

    stress_range: float
    mean_stress: float
    cycles: float
    allowable_cycles: float
    damage: float


@dataclass(frozen=True)
class LocationDamage:
    """Damage from one measured exposure and its design-life extrapolation."""

    exposure_damage: float
    design_life_damage: float
    cycles: tuple[CycleDamage, ...]


@dataclass(frozen=True)
class FatigueReport:
    """Offline results and the explicit basis needed to interpret them."""

    locations: Mapping[str, LocationDamage]
    governing_location: str
    exposure_duration: float
    design_life: float
    time_unit: str
    exposure_scale: float
    curve_basis: CurveBasis
    half_cycle_policy: str


def _text(value: str, field: str) -> None:
    """Require meaningful basis metadata without changing the supplied label."""
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{field} must be a nonempty string")


def _duration(value: float, field: str) -> float:
    """Require a positive finite duration in the declared common time unit."""
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{field} must be a positive finite number")
    try:
        value = float(value)
    except OverflowError as exc:
        raise ValueError(f"{field} must be a positive finite number") from exc
    if not np.isfinite(value) or value <= 0:
        raise ValueError(f"{field} must be a positive finite number")
    return value


def _samples(history: StressHistory, basis: CurveBasis) -> np.ndarray:
    """Validate each history before invoking any curve or counting operation."""
    if not isinstance(history, StressHistory):
        raise ValueError("each history must be a StressHistory with explicit units")
    _text(history.stress_unit, "history stress_unit")
    if history.stress_unit != basis.stress_unit:
        raise ValueError(
            "history stress_unit must equal curve stress_unit; convert explicitly"
        )
    try:
        if np.iscomplexobj(history.samples):
            raise ValueError("history must contain real stress samples")
        signal = np.asarray(history.samples, dtype=float)
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError("history must contain real numeric samples") from exc
    # A stress excursion needs two samples; singleton histories cannot define it.
    if signal.ndim != 1 or signal.size < 2 or not np.isfinite(signal).all():
        raise ValueError(
            "history must be finite, one-dimensional and contain at least two samples"
        )
    return signal


class _CheckedCurve:
    """Validate curve output before miner_damage divides by its endurance."""

    def __init__(self, curve: SNCurve):
        self.curve = curve

    def cycles(self, ranges: np.ndarray) -> np.ndarray:
        """Return validated counts for the primitive's exact range vector."""
        allowable = np.asarray(self.curve.cycles(ranges), dtype=float)
        if (
            allowable.shape != ranges.shape
            or np.isnan(allowable).any()
            or (allowable <= 0).any()
        ):
            raise ValueError(
                "allowable_cycles must match ranges and be positive, without NaN"
            )
        return allowable


def fatigue_assessment(
    histories: Mapping[str, StressHistory],
    sn_curve: SNCurve,
    design_life: float,
    *,
    exposure_duration: float,
    time_unit: str,
    curve_basis: CurveBasis,
) -> FatigueReport:
    """Assess in-memory histories using explicit curve, units and durations.

    Args:
        histories: Nonempty mapping of unique location names to stress histories.
            All histories cover the same exposure_duration.
        sn_curve: Caller's curve with cycles(stress_ranges), in curve_basis units.
        design_life: Positive finite life in time_unit.
        exposure_duration: Positive finite sampled exposure in time_unit.
        time_unit: Common duration unit label, retained verbatim; no conversion.
        curve_basis: Caller description of curve provenance and corrections,
            with a stress unit exactly matching each history.

    Returns:
        Per-location exposure and design-life damage, counted cycles and basis.

    Raises:
        ValueError: Invalid histories, units, durations, curve counts or overflow.

    Constant histories have no cycles and zero damage. Positive infinite curve
    endurance is permitted and contributes zero damage. No curve or environment
    is selected automatically. This function does not qualify an engineering
    design or select acceptance factors.
    """
    exposure = _duration(exposure_duration, "exposure_duration")
    life = _duration(design_life, "design_life")
    _text(time_unit, "time_unit")
    if not isinstance(curve_basis, CurveBasis):
        raise ValueError("curve_basis must be a CurveBasis")
    _text(curve_basis.description, "curve_basis description")
    _text(curve_basis.stress_unit, "curve_basis stress_unit")
    if not callable(getattr(sn_curve, "cycles", None)):
        raise ValueError("sn_curve must provide cycles(stress_ranges)")
    if not isinstance(histories, Mapping) or not histories:
        raise ValueError("histories must be a nonempty mapping")
    for name in histories:
        _text(name, "history location name")
    signals = {
        name: _samples(histories[name], curve_basis) for name in sorted(histories)
    }
    scale = life / exposure
    if not np.isfinite(scale) or scale <= 0:
        raise ValueError("exposure_scale must be positive and finite")

    locations = {}
    for name, signal in signals.items():
        try:
            with np.errstate(over="raise", invalid="raise", divide="raise"):
                counted = rainflow_count(signal)
                if not np.isfinite(counted["stress_range"].to_numpy(dtype=float)).all():
                    raise ValueError("computed stress ranges must be finite")
                # A flat residual pair has no excursion and no fatigue damage.
                counted = counted.loc[counted["stress_range"] > 0]
                if counted.empty:
                    locations[name] = LocationDamage(0.0, 0.0, ())
                    continue
                # Primitive contract: one closed cycle or one residual half-cycle.
                # Weights 1 and 0.5 are passed unchanged to the Miner sum(n/N).
                if not counted["cycles"].isin([0.5, 1.0]).all():
                    raise ValueError("unexpected rainflow cycle weights")
                result = miner_damage(counted, _CheckedCurve(sn_curve))
                damage = result.attrs["total_damage"]
                scaled = damage * scale
        except (FloatingPointError, OverflowError) as exc:
            raise ValueError(
                "computed fatigue damage or stress range overflowed"
            ) from exc
        if not np.isfinite(damage) or not np.isfinite(scaled):
            raise ValueError("computed fatigue damage must be finite")
        cycles = tuple(CycleDamage(
            stress_range=float(row.stress_range), mean_stress=float(row.mean_stress),
            cycles=float(row.cycles), allowable_cycles=float(row.allowable_cycles),
            damage=float(row.damage),
        ) for row in result.itertuples(index=False))
        locations[name] = LocationDamage(damage, scaled, cycles)

    # Locations were sorted before counting; max keeps the first exact tie.
    governing = max(locations, key=lambda name: locations[name].design_life_damage)
    return FatigueReport(
        locations=locations, governing_location=governing,
        exposure_duration=exposure, design_life=life, time_unit=time_unit,
        exposure_scale=scale, curve_basis=curve_basis, half_cycle_policy="retain",
    )
