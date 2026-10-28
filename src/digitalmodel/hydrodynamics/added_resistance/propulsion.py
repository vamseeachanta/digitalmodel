# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Required-curve, constant-shaft-power speed reduction for screening."""

from dataclasses import dataclass
from typing import Callable

import numpy as np

from .models import Status, finite, vector


def efficiency_check(value: float) -> None:
    finite(value, "propulsive efficiency", positive=True)
    # Efficiency is defined as useful power / shaft power, bounded by unity.
    if value > 1:
        raise ValueError("propulsive efficiency cannot exceed unity")


def added_shaft_power(
    resistance_n: float, speed_m_s: float, *, propulsive_efficiency: float
) -> float:
    """Added shaft power R_added*U/eta, with REQUIRED overall propulsive efficiency.

    The power identity P_effective=R*U follows the definition of mechanical
    power. The constant-efficiency reduction of the power balance is consistent
    with ITTC 7.5-02-07-02.8 (2024, revision 02), section 2.5.4 and Fig. 3:
    https://ittc.info/media/11900/75-02-07-028.pdf. It is not the full ITTC
    propeller open-water analysis. No efficiency or hull curve is assumed.
    """
    finite(resistance_n, "added resistance")
    finite(speed_m_s, "speed")
    efficiency_check(propulsive_efficiency)
    power = resistance_n * speed_m_s / propulsive_efficiency
    finite(power, "shaft power")
    return power


@dataclass(frozen=True)
class CalmWaterCurve:
    """Supplied monotone R0(U) samples in SI units; linear interpolation only.

    A zero-speed, zero-force endpoint is required to bracket the steady-state
    power solution without extrapolating any hull law. The caller controls
    table resolution; a tabulated curve need not preserve an analytic law
    between knots. Resistance may have flat intervals.
    """

    speed_m_s: tuple[float, ...]
    resistance_n: tuple[float, ...]

    def __post_init__(self):
        speeds = vector(self.speed_m_s, "curve speeds")
        forces = vector(self.resistance_n, "curve resistances")
        if len(speeds) < 2 or len(speeds) != len(forces):
            raise ValueError("a calm-water curve needs at least two paired samples")
        if speeds[0] != 0 or forces[0] != 0:
            raise ValueError("calm-water curve must start at zero speed and force")
        if np.any(np.diff(speeds) <= 0):
            raise ValueError("calm-water speeds must be strictly increasing")
        if np.any(np.diff(forces) < 0):
            raise ValueError("calm-water resistance must be nondecreasing")
        object.__setattr__(self, "speed_m_s", speeds)
        object.__setattr__(self, "resistance_n", forces)

    def resistance(self, speed_m_s: float) -> float:
        finite(speed_m_s, "curve query speed")
        if speed_m_s > self.speed_m_s[-1]:
            raise ValueError("calm-water curve coverage does not include this speed")
        return float(np.interp(speed_m_s, self.speed_m_s, self.resistance_n))


@dataclass(frozen=True)
class SpeedLossResult:
    status: Status
    calm_speed_m_s: float
    speed_m_s: float | None
    speed_loss_m_s: float | None
    available_shaft_power_w: float
    required_shaft_power_w: float | None
    iterations: int
    reason: str


def speed_loss(
    calm_water_curve: CalmWaterCurve,
    calm_speed_m_s: float,
    added_resistance_at_speed: Callable[[float], float],
    *,
    propulsive_efficiency: float,
    speed_tolerance_m_s: float,
    max_iterations: int,
) -> SpeedLossResult:
    """Bisect [0,U0] for (R0(U)+R_added(U))*U/eta = R0(U0)*U0/eta.

    This follows the constant-output construction in ITTC 7.5-02-07-02.8
    (2024, rev. 02), section 2.5.4, Fig. 3, with a supplied constant efficiency.
    A speed-dependent added-resistance callback is REQUIRED and is reevaluated
    during iteration. A deliberately frozen force can be supplied as a constant
    callback, but is only a screening approximation. The caller must provide
    a continuous total power curve with a unique crossing over [0,U0], and must
    recheck method validity, spectrum and motion evidence across those speeds.
    Iteration controls are supplied numerical accuracy choices, not engineering
    acceptance limits. Nonconvergence returns NOT_EVALUATED with no speed.
    """
    efficiency_check(propulsive_efficiency)
    finite(calm_speed_m_s, "calm-water speed", positive=True)
    finite(speed_tolerance_m_s, "speed tolerance", positive=True)
    if (
        not isinstance(max_iterations, int)
        or isinstance(max_iterations, bool)
        or max_iterations < 1
    ):
        raise ValueError("maximum iterations must be a positive integer")
    if not callable(added_resistance_at_speed):
        raise ValueError("a speed-dependent added-resistance callback is required")
    calm_force = calm_water_curve.resistance(calm_speed_m_s)
    available = added_shaft_power(
        calm_force, calm_speed_m_s, propulsive_efficiency=propulsive_efficiency
    )
    finite(available, "available shaft power", positive=True)

    def power(speed):
        added = added_resistance_at_speed(speed)
        finite(added, "added-resistance curve result")
        return added_shaft_power(
            calm_water_curve.resistance(speed) + added,
            speed,
            propulsive_efficiency=propulsive_efficiency,
        )

    upper_power = power(calm_speed_m_s)
    if upper_power == available:
        return SpeedLossResult(
            Status.EVALUATED,
            calm_speed_m_s,
            calm_speed_m_s,
            0,
            available,
            upper_power,
            0,
            "No speed loss at the supplied shaft power.",
        )
    low, high = 0.0, calm_speed_m_s
    power(low)  # Validate the lower endpoint rather than ignoring invalid data.
    for iteration in range(1, max_iterations + 1):
        trial = (low + high) / 2
        required = power(trial)
        if required == available or (high - low) / 2 <= speed_tolerance_m_s:
            return SpeedLossResult(
                Status.EVALUATED,
                calm_speed_m_s,
                trial,
                calm_speed_m_s - trial,
                available,
                required,
                iteration,
                "Constant shaft power and supplied constant efficiency; accuracy bounded by the speed bracket.",
            )
        if required > available:
            high = trial
        else:
            low = trial
    return SpeedLossResult(
        Status.NOT_EVALUATED,
        calm_speed_m_s,
        None,
        None,
        available,
        None,
        max_iterations,
        "Speed iteration did not converge within the supplied controls.",
    )
