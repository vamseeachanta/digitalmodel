"""Derived operability quantities from a MotionForecast (digitalmodel #1359).

Turns the 6-DOF ``MotionForecast`` (#1358) into the time-domain quantities that
marine operations actually gate on, at a structure-interface point of interest.

All governing series are **magnitudes** (``|velocity|``, ``hypot(roll,pitch)``,
``|excursion|``) so a symmetric limit is applied correctly and a large *downward*
velocity swing cannot slip past a below-limit check.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Dict, Tuple

import numpy as np

from .models import MotionForecast
from .reconstruct import time_derivative, vertical_motion_at

Offset = Tuple[float, float, float]


@dataclass
class GoverningSeries:
    """A governing quantity over time (magnitude), with its unit."""

    name: str
    t: np.ndarray
    values: np.ndarray  # magnitude, >= 0
    unit: str


# ----- individual quantities -------------------------------------------------

def vertical_velocity_magnitude(motion: MotionForecast, offset: Offset) -> np.ndarray:
    """|d/dt| of vertical motion at a rigid-body point (m/s)."""
    z = vertical_motion_at(motion, offset)
    return np.abs(time_derivative(motion.t, z))


def vertical_excursion_magnitude(motion: MotionForecast, offset: Offset) -> np.ndarray:
    """|vertical displacement| at a rigid-body point (m)."""
    return np.abs(vertical_motion_at(motion, offset))


def heave_velocity_magnitude(motion: MotionForecast) -> np.ndarray:
    """|d/dt heave| at the motion origin (m/s)."""
    return np.abs(time_derivative(motion.t, motion.dof["heave"]))


def inclination_deg(motion: MotionForecast) -> np.ndarray:
    """Resultant deck inclination hypot(roll, pitch) (deg, >= 0)."""
    return np.hypot(motion.dof["roll"], motion.dof["pitch"])


# ----- governing-quantity dispatch ------------------------------------------
# Each entry maps a governing key -> (callable(motion, offset) -> series, unit).
# offset is ignored by quantities defined at the motion origin.

_GOVERNING: Dict[str, Tuple[Callable[[MotionForecast, Offset], np.ndarray], str]] = {
    "crane_tip_vertical_velocity": (vertical_velocity_magnitude, "m/s"),
    "gangway_tip_vertical_velocity": (vertical_velocity_magnitude, "m/s"),
    "gangway_tip_excursion": (vertical_excursion_magnitude, "m"),
    "top_connection_vertical_velocity": (vertical_velocity_magnitude, "m/s"),
    "helideck_inclination": (lambda m, off: inclination_deg(m), "deg"),
    "helideck_heave_velocity": (lambda m, off: heave_velocity_magnitude(m), "m/s"),
}


def available_governing() -> Tuple[str, ...]:
    return tuple(_GOVERNING)


def compute_governing(
    motion: MotionForecast, governing: str, offset: Offset = (0.0, 0.0, 0.0)
) -> GoverningSeries:
    """Compute a named governing quantity (magnitude series) from a motion."""
    if governing not in _GOVERNING:
        raise ValueError(
            f"unknown governing quantity {governing!r}; "
            f"available: {', '.join(available_governing())}"
        )
    fn, unit = _GOVERNING[governing]
    return GoverningSeries(name=governing, t=motion.t, values=fn(motion, offset), unit=unit)
