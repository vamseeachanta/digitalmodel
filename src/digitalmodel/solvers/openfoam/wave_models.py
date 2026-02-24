#!/usr/bin/env python3
"""
ABOUTME: Wave and current boundary condition generators for OpenFOAM marine
simulations. Implements regular waves (Stokes), JONSWAP irregular waves, and
current profiles (uniform, power-law, log-law).
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Dict, List, Optional

import numpy as np


# ============================================================================
# Enumerations
# ============================================================================


class WaveTheory(Enum):
    """Supported wave theories for regular wave generation."""

    LINEAR = "linear"
    STOKES_2ND = "stokes2nd"
    STOKES_5TH = "stokes5th"
    CNOIDAL = "cnoidal"


class CurrentProfileType(Enum):
    """Current velocity profile types."""

    UNIFORM = "uniform"
    POWER_LAW = "powerLaw"
    LOG_LAW = "logLaw"


# ============================================================================
# RegularWaveBC
# ============================================================================

_GRAVITY = 9.81  # m/s^2


@dataclass
class RegularWaveBC:
    """Regular (monochromatic) wave boundary condition.

    Uses linear dispersion relation to compute wave length from period
    and water depth via Newton-Raphson iteration.

    Attributes:
        wave_height: Wave height H (m).
        wave_period: Wave period T (s).
        water_depth: Water depth d (m).
        theory: Wave theory to apply.
        direction: Wave propagation direction angle from x-axis (degrees).
        phase: Initial phase offset (radians).
    """

    wave_height: float
    wave_period: float
    water_depth: float
    theory: WaveTheory = WaveTheory.STOKES_2ND
    direction: float = 0.0
    phase: float = 0.0

    @property
    def amplitude(self) -> float:
        """Wave amplitude a = H/2."""
        return self.wave_height / 2.0

    @property
    def angular_frequency(self) -> float:
        """Angular frequency omega = 2*pi/T."""
        return 2.0 * math.pi / self.wave_period

    @property
    def wave_number(self) -> float:
        """Wave number k from linear dispersion omega^2 = g*k*tanh(k*d)."""
        return _solve_dispersion(self.angular_frequency, self.water_depth)

    @property
    def wave_length(self) -> float:
        """Wave length L = 2*pi/k."""
        return 2.0 * math.pi / self.wave_number

    @property
    def phase_speed(self) -> float:
        """Phase speed c = L/T."""
        return self.wave_length / self.wave_period

    def to_dict(self) -> Dict[str, Any]:
        """Serialise to an OpenFOAM-compatible wave BC parameter dict."""
        return {
            "waveHeight": self.wave_height,
            "wavePeriod": self.wave_period,
            "waterDepth": self.water_depth,
            "waveNumber": self.wave_number,
            "waveLength": self.wave_length,
            "direction": self.direction,
            "phase": self.phase,
            "theory": self.theory.value,
        }


# ============================================================================
# IrregularWaveBC
# ============================================================================


@dataclass
class IrregularWaveBC:
    """JONSWAP irregular wave boundary condition.

    Attributes:
        significant_wave_height: Hs (m).
        peak_period: Tp (s).
        water_depth: Water depth d (m).
        gamma: JONSWAP peak enhancement factor (default 3.3).
        direction: Mean wave propagation direction from x-axis (degrees).
        spreading: Directional spreading exponent.
        seed: Random seed for phase generation.
    """

    significant_wave_height: float
    peak_period: float
    water_depth: float
    gamma: float = 3.3
    direction: float = 0.0
    spreading: float = 10.0
    seed: int = 42

    @property
    def peak_frequency(self) -> float:
        """Peak angular frequency fp = 1/Tp."""
        return 1.0 / self.peak_period

    def to_dict(self) -> Dict[str, Any]:
        """Serialise to OpenFOAM-compatible irregular wave BC parameters."""
        return {
            "Hs": self.significant_wave_height,
            "Tp": self.peak_period,
            "waterDepth": self.water_depth,
            "gamma": self.gamma,
            "direction": self.direction,
            "spreading": self.spreading,
            "seed": self.seed,
            "spectrum": "JONSWAP",
        }


# ============================================================================
# CurrentProfile
# ============================================================================


@dataclass
class CurrentProfile:
    """Current velocity profile model.

    Provides depth-varying current speed for boundary condition generation.

    Attributes:
        surface_speed: Current speed at the free surface (m/s).
        profile_type: Velocity profile shape.
        reference_depth: Reference depth for scaling (m).
        exponent: Power-law exponent (default 1/7 for marine).
        roughness_length: Roughness height z0 for log-law (m).
        direction: Current direction from x-axis (degrees).
    """

    surface_speed: float
    profile_type: CurrentProfileType = CurrentProfileType.UNIFORM
    reference_depth: float = 30.0
    exponent: float = 1.0 / 7.0
    roughness_length: float = 0.05
    direction: float = 0.0

    def speed_at_depth(self, depth_below_surface: float) -> float:
        """Return current speed at a given depth below the free surface.

        Args:
            depth_below_surface: Depth (m), 0 = surface, positive downward.

        Returns:
            Current speed (m/s) at that depth.
        """
        d = max(0.0, depth_below_surface)

        if self.profile_type == CurrentProfileType.UNIFORM:
            return self.surface_speed

        if self.profile_type == CurrentProfileType.POWER_LAW:
            # u(z) = u_ref * ((d_ref - z) / d_ref)^n
            if self.reference_depth <= 0.0:
                return self.surface_speed
            z_norm = 1.0 - d / self.reference_depth
            z_norm = max(0.0, z_norm)
            return self.surface_speed * (z_norm ** self.exponent)

        if self.profile_type == CurrentProfileType.LOG_LAW:
            # u(z) = (u* / kappa) * ln((z_ref - z + z0) / z0)
            # Scaled so that u(0) = surface_speed
            z_ref = self.reference_depth
            z0 = self.roughness_length
            kappa = 0.41
            # Reference friction velocity from surface condition
            if z_ref + z0 <= z0:
                return self.surface_speed
            u_star = (
                self.surface_speed
                * kappa
                / math.log((z_ref + z0) / z0)
            )
            z_val = max(z_ref - d + z0, z0 * 1.001)
            return (u_star / kappa) * math.log(z_val / z0)

        return self.surface_speed

    def to_dict(self) -> Dict[str, Any]:
        """Serialise to a profile parameter dict."""
        return {
            "type": self.profile_type.value,
            "surfaceSpeed": self.surface_speed,
            "referenceDepth": self.reference_depth,
            "exponent": self.exponent,
            "roughnessLength": self.roughness_length,
            "direction": self.direction,
        }


# ============================================================================
# Dispersion relation solver
# ============================================================================


def _solve_dispersion(
    omega: float,
    water_depth: float,
    max_iter: int = 100,
    tol: float = 1e-8,
) -> float:
    """Solve the linear dispersion relation omega^2 = g*k*tanh(k*d).

    Uses Newton-Raphson iteration starting from the deep-water estimate.

    Args:
        omega: Angular frequency (rad/s).
        water_depth: Water depth d (m).
        max_iter: Maximum iterations.
        tol: Convergence tolerance.

    Returns:
        Wave number k (rad/m).
    """
    g = _GRAVITY
    d = water_depth

    # Deep-water initial estimate: k0 = omega^2/g
    k = omega ** 2 / g

    for _ in range(max_iter):
        kd = k * d
        tanh_kd = math.tanh(kd)
        sech2_kd = 1.0 - tanh_kd ** 2  # sech^2(kd)

        f = g * k * tanh_kd - omega ** 2
        df = g * (tanh_kd + k * d * sech2_kd)

        if abs(df) < 1e-15:
            break

        k_new = k - f / df

        if abs(k_new - k) < tol:
            k = k_new
            break

        k = k_new

    return max(k, 1e-10)
