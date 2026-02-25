"""Named unit-conversion helpers for the diffraction pipeline.

Replaces ad-hoc magic-number conversions (``/ 1000.0``, ``2π``,
``np.degrees``) with self-documenting function calls.  All functions
accept and return raw floats or numpy arrays for backward compatibility.
"""

from __future__ import annotations

import math
from typing import Union

import numpy as np

ArrayLike = Union[float, np.ndarray]

# ---------------------------------------------------------------------------
# Mass  (kg <-> tonnes)
# ---------------------------------------------------------------------------

def kg_to_tonnes(v: ArrayLike) -> ArrayLike:
    """Convert mass from kilograms to metric tonnes."""
    return v / 1000.0


def tonnes_to_kg(v: ArrayLike) -> ArrayLike:
    """Convert mass from metric tonnes to kilograms."""
    return v * 1000.0


# ---------------------------------------------------------------------------
# Density  (kg/m³ <-> t/m³)
# ---------------------------------------------------------------------------

def density_kg_m3_to_t_m3(v: ArrayLike) -> ArrayLike:
    """Convert density from kg/m³ to tonnes/m³."""
    return v / 1000.0


def density_t_m3_to_kg_m3(v: ArrayLike) -> ArrayLike:
    """Convert density from tonnes/m³ to kg/m³."""
    return v * 1000.0


# ---------------------------------------------------------------------------
# Inertia  (kg·m² <-> t·m²)
# ---------------------------------------------------------------------------

def inertia_kg_m2_to_t_m2(v: ArrayLike) -> ArrayLike:
    """Convert moment of inertia from kg·m² to t·m²."""
    return v / 1000.0


def inertia_t_m2_to_kg_m2(v: ArrayLike) -> ArrayLike:
    """Convert moment of inertia from t·m² to kg·m²."""
    return v * 1000.0


# ---------------------------------------------------------------------------
# Frequency  (Hz <-> rad/s <-> period)
# ---------------------------------------------------------------------------

_TWO_PI = 2.0 * math.pi


def hz_to_rad_per_s(v: ArrayLike) -> ArrayLike:
    """Convert frequency from Hz to rad/s."""
    return v * _TWO_PI


def rad_per_s_to_hz(v: ArrayLike) -> ArrayLike:
    """Convert frequency from rad/s to Hz."""
    return v / _TWO_PI


def rad_per_s_to_period_s(v: ArrayLike) -> ArrayLike:
    """Convert angular frequency (rad/s) to period (seconds)."""
    return _TWO_PI / v


def period_s_to_rad_per_s(v: ArrayLike) -> ArrayLike:
    """Convert period (seconds) to angular frequency (rad/s)."""
    return _TWO_PI / v


# ---------------------------------------------------------------------------
# Angle  (radians <-> degrees) + complex-phase helper
# ---------------------------------------------------------------------------

def radians_to_degrees(v: ArrayLike) -> ArrayLike:
    """Convert angle from radians to degrees (numpy-compatible)."""
    return np.degrees(v)


def degrees_to_radians(v: ArrayLike) -> ArrayLike:
    """Convert angle from degrees to radians (numpy-compatible)."""
    return np.radians(v)


def complex_phase_degrees(v: ArrayLike) -> ArrayLike:
    """Return the phase angle(s) of complex value(s) in degrees.

    Equivalent to ``np.degrees(np.angle(v))`` but with a self-documenting
    name that makes the intent clear at call sites.
    """
    return np.degrees(np.angle(v))
