# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Execute declared passing-ship cases using sampled quasi-static equilibrium.

Only parallel passage is supported by the existing force formulation. Wind is
componentwise quadratic drag with supplied reference areas and coefficients,
not a tabulated ship wind model. Direction is meteorological 'from', measured
clockwise from the moored bow; global bearings must first be transformed by the
caller. Wind moment is r cross F about midship. No current or wave model is
applied. See mooring_response for the taut-line, small-motion model limits.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from .calculator import PassingShipCalculator
from .configuration import EnvironmentalConfig, VesselConfig
from .force_time_history import ForceTimeHistory
from .input_schemas import AnalysisCase, PassingShipSpec
from .mooring_response import solve_response


@dataclass
class AnalysisResult:
    """One named case; unrequested outputs are explicitly None."""

    name: str
    time: np.ndarray
    forces: ForceTimeHistory | None
    motions: np.ndarray | None
    line_tensions_n: dict[str, np.ndarray] | None
    fender_loads_n: dict[str, np.ndarray] | None
    surge_natural_period_s: float | None = None
    passage_duration_s: float | None = None
    applicability_basis: dict[str, float] | None = None


def wind_force(spec: PassingShipSpec) -> np.ndarray:
    """Return [Fx N, Fy N, Mz Nm] from explicit body-axis wind inputs.

    NASA Glenn, 'Drag Equation' (2021, updated 2025), D=Cd*rho*V**2*A/2:
    https://www1.grc.nasa.gov/beginners-guide-to-aeronautics/drag-equation/
    The componentwise projection is an explicit model assumption. Moment
    balance follows OpenStax University Physics Volume 1 (2016), section 12.1.
    """
    env, wind = spec.environment, spec.moored_vessel.wind_load
    if wind is None or env.wind_speed is None or env.wind_direction_deg is None:
        raise ValueError(
            "wind requires speed, body-relative direction and wind_load inputs"
        )
    values = list(wind.model_dump().values()) + [env.wind_speed, env.wind_direction_deg]
    if not np.isfinite(values).all():
        raise ValueError("wind inputs must be finite")
    angle = np.deg2rad(env.wind_direction_deg)
    velocity = -env.wind_speed * np.array([np.cos(angle), np.sin(angle)])
    area = np.array([wind.frontal_area_m2, wind.lateral_area_m2])
    coefficient = np.array([wind.surge_coefficient, wind.sway_coefficient])
    # The factor one half is the NASA drag equation, not a fitted coefficient.
    fx, fy = (
        wind.air_density_kg_m3 / 2 * area * coefficient * velocity * np.abs(velocity)
    )
    moment = wind.centre_of_pressure_x_m * fy - wind.centre_of_pressure_y_m * fx
    return np.array([fx, fy, moment])


def _calculator(spec: PassingShipSpec) -> PassingShipCalculator:
    """Adapt canonical dimensions and effective draft to the existing calculator."""

    def geometry(hull, draft):
        return VesselConfig(
            length=hull.loa,
            beam=hull.beam,
            draft=draft,
            block_coefficient=hull.block_coefficient,
        )

    return PassingShipCalculator(
        geometry(spec.moored_vessel.hull, spec.moored_vessel.effective_draft()),
        geometry(spec.passing_ship.dimensions, spec.passing_ship.dimensions.draft),
        EnvironmentalConfig(
            water_depth=None
            if spec.environment.is_infinite_depth
            else spec.environment.water_depth,
            water_density=spec.environment.water_density,
        ),
    )


def _finite(value):
    if isinstance(value, dict):
        return all(_finite(v) for v in value.values())
    if isinstance(value, list):
        return all(_finite(v) for v in value)
    return not isinstance(value, (int, float)) or np.isfinite(value)


def run_analysis_cases(
    spec: PassingShipSpec,
    *,
    time_s: list[float] | np.ndarray,
    start_stagger_m: float,
) -> list[AnalysisResult]:
    """Run every named case in order, without mutating the specification.

    Times must start at zero. Stagger=start_stagger_m+track offset+U*time.
    The caller supplies the sample grid and event extent; no duration or time
    step is inferred. An empty case list runs a single default 'base' passage
    without wind. All requested quantities must be evaluable or the call raises.
    Force histories contain the combined wind and passing loads.
    """
    if not _finite(spec.model_dump()):
        raise ValueError("all analysis inputs must be finite")
    spec = PassingShipSpec.model_validate(spec.model_dump())
    time = np.asarray(time_s, dtype=float)
    if (
        time.ndim != 1
        or not len(time)
        or not np.isfinite(time).all()
        or time[0] != 0
        or np.any(np.diff(time) <= 0)
    ):
        raise ValueError("time must be finite, start at zero and strictly increase")
    if not np.isfinite(start_stagger_m):
        raise ValueError("start stagger must be finite")
    cases = spec.analysis_cases or [AnalysisCase(name="base")]
    if len({c.name for c in cases}) != len(cases) or any(
        not c.name.strip() for c in cases
    ):
        raise ValueError("case names must be populated and unique")
    env = spec.environment
    if env.current_speed:
        raise ValueError("nonzero current has no implemented load model")
    draft = max(
        spec.moored_vessel.effective_draft(), spec.passing_ship.dimensions.draft
    )
    if not env.is_infinite_depth and env.water_depth <= draft:
        raise ValueError("water depth must exceed both drafts")
    outputs = spec.outputs
    need_response = (
        outputs.line_tensions or outputs.vessel_motions or outputs.fender_loads
    )
    if need_response and spec.response_settings is None:
        raise ValueError(
            "response_settings are required for requested response outputs"
        )
    if outputs.fender_loads and not spec.moored_vessel.fenders:
        raise ValueError("fender loads requested without fender definitions")
    results = []
    for case in cases:
        track = spec.passing_ship.track.model_copy(deep=True)
        if case.passing_speed_knots is not None:
            track.speed_knots = case.passing_speed_knots
        if case.separation_m is not None:
            track.separation_m = case.separation_m
        stagger = (
            start_stagger_m + track.closest_approach_offset_m + track.speed_ms * time
        )
        loads = np.zeros((len(time), 3))
        if case.include_passing_ship:
            if track.angle_deg != 0:
                raise ValueError("only parallel passing tracks are implemented")
            half_beams = (
                spec.moored_vessel.hull.beam + spec.passing_ship.dimensions.beam
            ) / 2
            if track.separation_m <= half_beams:
                raise ValueError("separation must exceed combined half beams")
            calc = _calculator(spec)
            for i, x in enumerate(stagger):
                force = calc.calculate_forces(
                    stagger=float(x),
                    separation=track.separation_m,
                    velocity=track.speed_ms,
                )
                loads[i] = [force["surge"], force["sway"], force["yaw"]]
        if case.include_wind:
            loads += wind_force(spec)
        history = ForceTimeHistory(
            time=time.copy(),
            surge=loads[:, 0],
            sway=loads[:, 1],
            yaw=loads[:, 2],
            stagger=stagger,
            separation=track.separation_m,
            speed=track.speed_ms,
        )
        response = (
            solve_response(spec.moored_vessel, history, spec.response_settings)
            if need_response
            else None
        )
        results.append(
            AnalysisResult(
                name=case.name,
                time=time.copy(),
                forces=history if outputs.force_time_histories else None,
                motions=response.motions if outputs.vessel_motions else None,
                line_tensions_n=response.line_tensions_n
                if outputs.line_tensions
                else None,
                fender_loads_n=response.fender_loads_n
                if outputs.fender_loads
                else None,
                surge_natural_period_s=response.surge_natural_period_s
                if response is not None else None,
                # Kinematic duration: distance swept by both hull lengths / U.
                passage_duration_s=(
                    spec.moored_vessel.hull.loa + spec.passing_ship.dimensions.loa
                ) / track.speed_ms if case.include_passing_ship else None,
                applicability_basis={
                    "mass_kg": spec.moored_vessel.hull.displacement_tonnes * 1000,
                    "surge_stiffness_n_m": response.surge_stiffness_n_m,
                    "moored_length_m": spec.moored_vessel.hull.loa,
                    "passing_length_m": spec.passing_ship.dimensions.loa,
                    "speed_m_s": track.speed_ms,
                } if response is not None else None,
            )
        )
    return results
