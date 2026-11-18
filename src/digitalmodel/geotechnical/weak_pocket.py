# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic cohesive-layer sensitivity; no spatial or dynamic soil model.

Local reference pressure is assumed to be q = 6.34 su at every depth, using
the uniform-clay factor documented in spudcan.py. Linear strength interpolation
and abrupt pocket boundaries are assumptions. This does not extend the Hu
sand-over-clay calibration or implement a general layered bearing solution.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, fields

from digitalmodel.geotechnical.spudcan import PenetrationPoint, finite


@dataclass(frozen=True)
class CohesiveLayer:
    """Synthetic strength varies linearly between the supplied mudline depths."""

    name: str
    top_m: float
    bottom_m: float
    top_su_kpa: float
    bottom_su_kpa: float


@dataclass(frozen=True)
class WeakPocket:
    """Factors retain strength: 0.6 means a 40 percent strength reduction.

    Supply factors in decreasing order, from mild to severe. Survey distance
    records screening rationale only; it never changes strength or probability.
    None marks missing information and produces an unresolved sensitivity.
    """

    layer: str | None = None
    strength_factors: tuple[float, ...] | None = None
    top_m: float | None = None
    bottom_m: float | None = None
    nearest_survey_distance_m: float | None = None


@dataclass(frozen=True)
class DropIndication:
    """Static peak-to-recovery distance and an assumed constant-rate time scale."""

    peak_depth_m: float
    recovery_depth_m: float
    jump_m: float
    assumed_rate_m_s: float
    duration_s: float
    duration_order_s: float
    model: str = 'assumed_constant_settlement_rate'


@dataclass(frozen=True)
class PocketReaction:
    """Separate potential capacity loss from a load reaching a descending peak."""

    name: str
    load_kn: float
    status: str
    baseline_penetration_m: float | None
    reduced_penetration_m: float | None
    penetration_change_m: float | None
    load_reaches_drop: bool
    drop: DropIndication | None


@dataclass(frozen=True)
class PocketCase:
    """One retained-strength factor, its full curve and both leg reactions."""

    strength_factor: float
    punch_through_flag: bool
    curve: tuple[PenetrationPoint, ...]
    reactions: tuple[PocketReaction, ...]


@dataclass(frozen=True)
class PocketSensitivity:
    """Unresolved inputs or recovery are explicit; no factor is interpolated."""

    status: str
    missing_inputs: tuple[str, ...]
    pocket: WeakPocket | None
    baseline_curve: tuple[PenetrationPoint, ...] = ()
    cases: tuple[PocketCase, ...] = ()
    first_flag_factor: float | None = None


def _validate(layers, area, pocket, preload, storm, rate):
    """Require a contiguous profile and an interval contained in one layer."""
    for name, value in (('area', area), ('preload', preload), ('storm', storm), ('rate', rate)):
        finite(name, value, strict=True)
    if not layers or len({layer.name for layer in layers}) != len(layers):
        raise ValueError('layers must be nonempty with distinct names')
    previous_bottom = 0
    for layer in layers:
        if not isinstance(layer.name, str) or not layer.name.strip():
            raise ValueError('layer name must not be empty')
        for name in ('top_m', 'bottom_m', 'top_su_kpa', 'bottom_su_kpa'):
            finite(name, getattr(layer, name))
        if layer.top_m != previous_bottom or layer.bottom_m <= layer.top_m:
            raise ValueError('layers must be contiguous from zero with positive thickness')
        previous_bottom = layer.bottom_m
    matches = [layer for layer in layers if layer.name == pocket.layer]
    if not matches:
        raise ValueError('pocket layer is absent from the supplied profile')
    layer = matches[0]
    finite('pocket top', pocket.top_m)
    finite('pocket bottom', pocket.bottom_m, minimum=pocket.top_m, strict=True)
    finite('survey distance', pocket.nearest_survey_distance_m)
    if pocket.top_m < layer.top_m or pocket.bottom_m > layer.bottom_m:
        raise ValueError('pocket extent must lie within the selected layer')
    if not pocket.strength_factors:
        raise ValueError('strength factor sweep must not be empty')
    for factor in pocket.strength_factors:
        finite('strength factor', factor, strict=True)
        if factor > 1:
            raise ValueError('retained-strength factor must not exceed one')
    if any(b >= a for a, b in zip(pocket.strength_factors, pocket.strength_factors[1:])):
        raise ValueError('strength factors must be strictly decreasing from mild to severe')


def _curve(layers, area, pocket, factor):
    """Retain both sides of abrupt boundaries without numerical depth offsets."""
    points = []
    for layer in layers:
        depths = {layer.top_m, layer.bottom_m}
        if layer.name == pocket.layer:
            depths.update((pocket.top_m, pocket.bottom_m))
        depths = sorted(depths)
        for top, bottom in zip(depths, depths[1:]):
            reduced = (layer.name == pocket.layer
                       and pocket.top_m <= top and bottom <= pocket.bottom_m)
            multiplier = factor if reduced else 1.0
            for depth in (top, bottom):
                strength = layer.top_su_kpa + (layer.bottom_su_kpa - layer.top_su_kpa) * (
                    depth - layer.top_m) / (layer.bottom_m - layer.top_m)
                pressure = 6.34 * strength * multiplier
                load = pressure * area
                finite('synthetic resistance', load)
                point = PenetrationPoint(depth, pressure, load, 'synthetic_cohesive_reference')
                if not points or point != points[-1]:
                    points.append(point)
    return tuple(points)


def _equilibrium(curve, load, start=0):
    """First upward intersection, following a descending peak at exact equality.

    A vertical strength recovery crosses the load at its boundary depth. None
    means no support within the supplied profile; extrapolation is prohibited.
    """
    if start == 0 and load < curve[0].load_kn:
        return curve[0].depth_m

    def descends_after(index):
        while index + 1 < len(curve) and curve[index + 1].load_kn == load:
            index += 1
        return index + 1 < len(curve) and curve[index + 1].load_kn < load

    for index in range(start, len(curve) - 1):
        a, b = curve[index:index + 2]
        if b.load_kn > a.load_kn and a.load_kn <= load <= b.load_kn:
            if load == b.load_kn and descends_after(index + 1):
                continue
            return a.depth_m + (b.depth_m - a.depth_m) * (load - a.load_kn) / (b.load_kn - a.load_kn)
        if load == a.load_kn == b.load_kn and not descends_after(index + 1):
            return a.depth_m
    return None


def _reaction(name, load, baseline, curve, drops, rate):
    """Compare static intersections and indicate the first reachable drop."""
    original = _equilibrium(baseline, load)
    reduced = _equilibrium(curve, load)
    reachable = [i for i in drops if load >= curve[i].load_kn
                 and (reduced is None or reduced >= curve[i].depth_m)]
    indication = None
    if reachable:
        index = reachable[0]
        recovery = _equilibrium(curve, load, start=index + 1)
        if recovery is not None:
            peak_depth = curve[index].depth_m
            jump = recovery - peak_depth
            if jump > 0:
                duration = jump / rate
                finite('drop duration', duration, strict=True)
                indication = DropIndication(
                    peak_depth, recovery, jump, rate, duration,
                    10.0 ** math.floor(math.log10(duration)))
    resolved = original is not None and reduced is not None
    return PocketReaction(
        name, load, 'resolved' if resolved else 'unresolved', original, reduced,
        reduced - original if resolved else None, bool(reachable), indication,
    )


def weak_pocket_sensitivity(
    *, layers: tuple[CohesiveLayer, ...] | None = None, area_m2: float | None = None,
    pocket: WeakPocket | None = None, preload_kn: float | None = None,
    storm_kn: float | None = None, assumed_settlement_rate_m_s: float | None = None,
) -> PocketSensitivity:
    """Sweep a local synthetic strength loss and compare preload/storm depths.

    Every descending segment warns independently of the applied load, retaining
    the preload screen's warning semantics. Drop distance is reported only when
    the load reaches a descending peak and recovery exists in the supplied depth
    interval. Duration equals distance divided by an explicitly assumed rate;
    duration_order_s is the lower power of ten of the containing decade
    (10 for a time in tens of seconds). Neither is a dynamic bound.
    The first flag is the first sampled factor, not a critical-factor solve.
    """
    supplied = dict(layers=layers, area_m2=area_m2, pocket=pocket,
                    preload_kn=preload_kn, storm_kn=storm_kn,
                    assumed_settlement_rate_m_s=assumed_settlement_rate_m_s)
    missing = [name for name, value in supplied.items() if value is None]
    if pocket is not None:
        missing.extend(f'pocket.{item.name}' for item in fields(pocket)
                       if getattr(pocket, item.name) is None)
    if layers is not None:
        missing.extend(f'layers[{index}].{item.name}'
                       for index, layer in enumerate(layers) for item in fields(layer)
                       if getattr(layer, item.name) is None)
    if missing:
        return PocketSensitivity('unresolved', tuple(missing), pocket)
    _validate(layers, area_m2, pocket, preload_kn, storm_kn, assumed_settlement_rate_m_s)
    baseline = _curve(layers, area_m2, pocket, 1.0)
    cases = []
    for factor in pocket.strength_factors:
        curve = _curve(layers, area_m2, pocket, factor)
        drops = [i for i, (a, b) in enumerate(zip(curve, curve[1:])) if b.load_kn < a.load_kn]
        reactions = tuple(_reaction(name, load, baseline, curve, drops, assumed_settlement_rate_m_s)
                          for name, load in (('preload', preload_kn), ('storm', storm_kn)))
        cases.append(PocketCase(factor, bool(drops), curve, reactions))
    status = 'resolved' if all(r.status == 'resolved' for c in cases for r in c.reactions) else 'unresolved'
    first = next((case.strength_factor for case in cases if case.punch_through_flag), None)
    return PocketSensitivity(status, (), pocket, baseline, tuple(cases), first)
