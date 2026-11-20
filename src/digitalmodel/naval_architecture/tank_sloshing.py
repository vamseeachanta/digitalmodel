"""First-mode rectangular-tank relation from a legacy engineering workbook.

SI lengths, seconds, Hz and caller-supplied gravity. Linear inviscid gravity
waves, horizontal free surface, uniform depth, rigid rectangular walls and
small amplitude are assumed. No damping, baffles, forcing amplitude, roof
impact, coupled vessel response or stability acceptance is calculated.
"""
from __future__ import annotations

from dataclasses import dataclass

from digitalmodel.hydrodynamics.sloshing import rectangular_tank_periods
from .tank_gauging.calibration import finite, positive


@dataclass(frozen=True)
class TankGeometryInput:
    overall_height_m: float
    bottom_height_m: float


@dataclass(frozen=True)
class TankDepthInput:
    geometry: TankGeometryInput
    fill_fraction_dimensionless: float


@dataclass(frozen=True)
class SloshingInput:
    breadth_m: float
    liquid_depth_m: float
    gravity_m_s2: float


@dataclass(frozen=True)
class FrequencyInput:
    frequency_hz: float


@dataclass(frozen=True)
class PeriodRatioInput:
    ship_period_s: float
    tank_period_s: float


@dataclass(frozen=True)
class TankCaseInput:
    fill_fraction_dimensionless: float
    breadth_m: float


@dataclass(frozen=True)
class TankModelInput:
    ship_period_s: float
    gravity_m_s2: float
    geometry: TankGeometryInput
    cases: tuple[TankCaseInput, ...]

    def __post_init__(self) -> None:
        object.__setattr__(self, "cases", tuple(self.cases))


@dataclass(frozen=True)
class TankResponse:
    liquid_depth_m: float
    frequency_hz: float
    period_s: float
    ship_to_tank_period_ratio_dimensionless: float


def available_depth_m(inputs: TankGeometryInput) -> float:
    """Identity H = overall height - bottom height, common datum, in m.

    Bottom height must be nonnegative; require a positive available depth.
    """
    overall = finite(inputs.overall_height_m, "overall_height_m")
    bottom = finite(inputs.bottom_height_m, "bottom_height_m")
    if bottom < 0:
        raise ValueError("bottom_height_m must be nonnegative")
    return positive(overall - bottom, "available_depth_m")


def liquid_depth_m(inputs: TankDepthInput) -> float:
    """Identity h = fill fraction * available depth for a constant-area tank.

    Require 0 < fill <= 1; zero fill has no finite sloshing period. A fill of
    one is a geometric limit with an exposed free surface, not pressed-full.
    """
    fill = finite(inputs.fill_fraction_dimensionless, "fill_fraction_dimensionless")
    if not 0 < fill <= 1:
        raise ValueError("fill fraction must lie in (0, 1]")
    return positive(fill * available_depth_m(inputs.geometry), "liquid_depth_m")


def tank_natural_frequency_hz(inputs: SloshingInput) -> float:
    """Linear-wave relation f = sqrt(g*k*tanh(k*h))/(2*pi), k=pi/breadth.

    Fundamental transverse standing wave only; lengths in m and g in m/s².
    Gravity is required; no geographic or conventional value is assumed.
    """
    breadth = positive(inputs.breadth_m, "breadth_m")
    depth = positive(inputs.liquid_depth_m, "liquid_depth_m")
    gravity = positive(inputs.gravity_m_s2, "gravity_m_s2")
    period = positive(rectangular_tank_periods(breadth, depth, n_modes=1, g=gravity)[0],
                      "period_s")
    return positive(1 / period, "frequency_hz")


def natural_period_s(inputs: FrequencyInput) -> float:
    """Identity T = 1/f, seconds for a strictly positive frequency in Hz."""
    return positive(1 / positive(inputs.frequency_hz, "frequency_hz"), "period_s")


def period_ratio(inputs: PeriodRatioInput) -> float:
    """Identity R = ship period / tank period, dimensionless; both in s.

    The ship input is a period, not a frequency; no resonance or load verdict
    follows from this ratio alone.
    """
    ship = positive(inputs.ship_period_s, "ship_period_s")
    tank = positive(inputs.tank_period_s, "tank_period_s")
    return positive(ship / tank, "period ratio")


def calculate_tank_response(inputs: TankModelInput) -> tuple[TankResponse, ...]:
    """Evaluate independent breadth/fill cases with explicit geometry and period."""
    positive(inputs.ship_period_s, "ship_period_s")
    positive(inputs.gravity_m_s2, "gravity_m_s2")
    available_depth_m(inputs.geometry)
    if not inputs.cases:
        raise ValueError("at least one tank case is required")
    responses = []
    for case in inputs.cases:
        depth = liquid_depth_m(TankDepthInput(inputs.geometry, case.fill_fraction_dimensionless))
        frequency = tank_natural_frequency_hz(SloshingInput(case.breadth_m, depth,
                                                          inputs.gravity_m_s2))
        period = natural_period_s(FrequencyInput(frequency))
        ratio = period_ratio(PeriodRatioInput(inputs.ship_period_s, period))
        responses.append(TankResponse(depth, frequency, period, ratio))
    return tuple(responses)
