# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Prepare parallel passing-ship forces, without a coupled response solver.

Reuses the existing force calculator and its SI/sign conventions. Finite-depth
corrections and sectional-area assumptions inherit that calculator's limits;
the prepared cases are not qualified against measured passing-ship forces.
Wind loads, when requested, must be supplied as explicit SI force/moment inputs;
no aerodynamic coefficients are inferred. Requested tensions, excursions and
fender loads remain explicitly uncomputed. Launch is always refused until a
qualified adapter and an approved manifest digest can be enforced together.
"""

from __future__ import annotations

from itertools import product
import math
from typing import Any

from .calculator import PassingShipCalculator
from .configuration import EnvironmentalConfig, VesselConfig
from .input_schemas import AnalysisCase, PassingShipSpec


def _finite(values: Any) -> None:
    """Reject non-finite numerical inputs before entering the integrator."""
    if isinstance(values, dict):
        for value in values.values():
            _finite(value)
    elif isinstance(values, (list, tuple)):
        for value in values:
            _finite(value)
    elif isinstance(values, (float, int)) and not math.isfinite(values):
        raise ValueError("all numerical inputs must be finite")


def validate_model(spec: PassingShipSpec) -> None:
    """Validate preparation geometry; this is not solver model qualification."""
    PassingShipSpec.model_validate(spec.model_dump())
    _finite(spec.model_dump())
    track = spec.passing_ship.track
    if track.angle_deg != 0:
        raise ValueError("only parallel tracks are implemented")
    if spec.environment.current_speed:
        raise ValueError("nonzero current needs a qualified directional load model")
    draft = max(spec.moored_vessel.effective_draft(), spec.passing_ship.dimensions.draft)
    if not spec.environment.is_infinite_depth and spec.environment.water_depth <= draft:
        raise ValueError("water depth must exceed both drafts")
    lines = spec.moored_vessel.mooring_lines
    if not lines or len({line.line_id for line in lines}) != len(lines):
        raise ValueError("mooring lines must be populated with unique identifiers")
    for line in lines:
        distance = math.hypot(line.anchor_x - line.fairlead_x, line.anchor_y - line.fairlead_y)
        if distance <= 0 or distance > line.length:
            raise ValueError("line length must span its positive horizontal reach")
    names = [case.name for case in spec.analysis_cases]
    if not names or len(set(names)) != len(names):
        raise ValueError("analysis cases must have unique populated names")
    half_beams = (spec.moored_vessel.hull.beam + spec.passing_ship.dimensions.beam) / 2
    separations = [track.separation_m] + [c.separation_m for c in spec.analysis_cases if c.separation_m is not None]
    if min(separations) <= half_beams:
        raise ValueError("centreline separation must exceed combined half beams")


def prepare_campaign(spec: PassingShipSpec, matrix: dict) -> list[PassingShipSpec]:
    """Expand speed, separation and depth ratio as a serial Cartesian matrix."""
    validate_model(spec)
    axes = [matrix[k] for k in ("speed_knots", "separation_m", "depth_to_draft")]
    _finite(axes)
    if any(not axis or len(set(axis)) != len(axis) for axis in axes):
        raise ValueError("matrix axes must be nonempty and unique")
    if any(value <= 0 for axis in axes for value in axis):
        raise ValueError("matrix values must be positive")
    prepared = []
    for index, (speed, separation, ratio) in enumerate(product(*axes), start=1):
        case = spec.model_copy(deep=True)
        case.name = f"matrix-{index:03d}"
        case.passing_ship.track.speed_knots = speed
        case.passing_ship.track.separation_m = separation
        case.environment.water_depth = ratio * spec.moored_vessel.effective_draft()
        case.analysis_cases = [AnalysisCase(name=case.name)]
        # Revalidate assignments as well as geometry, since these schema models
        # do not enable assignment validation.
        case = PassingShipSpec.model_validate(case.model_dump())
        validate_model(case)
        prepared.append(case)
    return prepared


def _calculator(spec: PassingShipSpec) -> PassingShipCalculator:
    def vessel(geometry: Any, draft: float) -> VesselConfig:
        return VesselConfig(length=geometry.loa, beam=geometry.beam, draft=draft,
                            block_coefficient=geometry.block_coefficient)
    return PassingShipCalculator(
        vessel(spec.moored_vessel.hull, spec.moored_vessel.effective_draft()),
        vessel(spec.passing_ship.dimensions, spec.passing_ship.dimensions.draft),
        EnvironmentalConfig(water_depth=None if spec.environment.is_infinite_depth else spec.environment.water_depth,
                            water_density=spec.environment.water_density),
    )


def execute_cases(spec: PassingShipSpec, *, staggers_m: list[float],
                  wind_loads: dict[str, list[float]] | None = None) -> list[dict]:
    """Evaluate force samples for every declared case; never compute response.

    Samples use the module's signed longitudinal stagger, not a time integration.
    Supplied wind vectors are [surge N, sway N, yaw N m] in the same axes and
    remain constant across the samples. Wind physics is outside this executor.
    """
    validate_model(spec)
    if not staggers_m:
        raise ValueError("stagger samples must not be empty")
    _finite(staggers_m)
    calculator = _calculator(spec)
    track = spec.passing_ship.track
    records = []
    for case in spec.analysis_cases:
        wind = (wind_loads or {}).get(case.name) if case.include_wind else [0, 0, 0]
        if wind is None or len(wind) != 3:
            raise ValueError("requested wind requires an explicit three-component SI load vector")
        _finite(wind)
        speed = track.speed_ms if case.passing_speed_knots is None else case.passing_speed_knots * (track.speed_ms / track.speed_knots)
        separation = track.separation_m if case.separation_m is None else case.separation_m
        values = []
        for stagger in staggers_m:
            force = calculator.calculate_forces(separation, stagger, speed, use_cache=False) if case.include_passing_ship else dict.fromkeys(("surge", "sway", "yaw"), 0.0)
            values.append([force[key] + wind[i] for i, key in enumerate(("surge", "sway", "yaw"))])
        _finite(values)
        records.append({"name": case.name, "speed_knots": speed / (track.speed_ms / track.speed_knots),
                        "separation_m": separation, "stagger_m": list(staggers_m),
                        **{key: [v[i] for v in values] for i, key in enumerate(("surge_n", "sway_n", "yaw_nm"))},
                        "requested_outputs": spec.outputs.model_dump(),
                        "response_status": "not computed; response adapter required",
                        "wind_basis": "supplied load vector" if case.include_wind else "excluded"})
    return records


def compare_benchmark(record: dict, computed: list[float]) -> dict:
    """Compare explicit numerical references using predeclared mixed tolerances."""
    reference = record.get("reference")
    if reference is None:
        raise ValueError("tabulated reference forces and moments are required")
    relative = record["relative_tolerance"]
    absolute = record["absolute_tolerance"]
    _finite([reference, computed, relative, absolute])
    if any(len(v) != 3 for v in (reference, computed, absolute)) or relative < 0 or min(absolute) < 0:
        raise ValueError("three components and nonnegative tolerances are required")
    errors = [abs(a - b) for a, b in zip(reference, computed)]
    limits = [max(a, relative * abs(r)) for a, r in zip(absolute, reference)]
    return {"absolute_errors": errors, "limits": limits,
            "passed": all(error <= limit for error, limit in zip(errors, limits))}


def launch_solver(manifest_digest: str) -> None:
    """Refuse every launch; a digest alone cannot authorise execution."""
    raise RuntimeError("launch refused: qualified execution adapter and approved manifest digest required")
