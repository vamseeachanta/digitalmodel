# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Sectional work diagnostic; radiated-energy resistance remains unevaluated.

The 1972 force-conversion equation and method validity limits were not available
from a readable public source. No damping correction, force conversion or
oblique-sea factor is inferred. The diagnostic below is harmonic mechanical
work, not an implementation or validation of that resistance method.
"""

from dataclasses import dataclass
from math import cos, isfinite, pi, radians, sqrt
from typing import Sequence

import numpy as np

from .models import OperatingPoint, RegularWaves, ResistanceRow, Ship, Status, finite

_trapezoid = getattr(np, "trapezoid", None) or np.trapz


def signed_sections(values: Sequence[float]) -> np.ndarray:
    x = np.asarray(values, dtype=float)
    if (
        x.ndim != 1
        or len(x) < 2
        or not np.all(np.isfinite(x))
        or np.any(np.diff(x) <= 0)
    ):
        raise ValueError(
            "section positions must be finite, strictly increasing, and have at least two points"
        )
    return x


def complex_vector(values: Sequence[complex], count: int, name: str) -> np.ndarray:
    result = np.asarray(values, dtype=complex)
    if result.shape != (count,) or not np.all(np.isfinite(result)):
        raise ValueError(f"{name} must be a finite complex vector of length {count}")
    return result


@dataclass(frozen=True)
class SectionalEnergy:
    mean_power_w: float
    energy_per_cycle_j: float


def sectional_energy(
    section_x_m: Sequence[float],
    damping_n_s_m2: Sequence[float],
    relative_velocity_m_s: Sequence[complex],
    encounter_rad_s: float,
) -> SectionalEnergy:
    """Integrate harmonic sectional work with trapezoidal spatial quadrature.

    Identity derived directly from v(t)=Re(v_hat*exp(i*omega_e*t)):
    <v(t)^2>=|v_hat|^2/2, P=integral b*|v_hat|^2/2 dx,
    E_cycle=P*(2*pi/omega_e), hence E_cycle*omega_e/(2*pi)=P.
    The factor 1/2 is the analytic integral of cos² over one cycle, not an
    empirical coefficient. Damping is per unit length; there is no density
    multiplier or default damping. End sections should bound the wetted hull.
    """
    x = signed_sections(section_x_m)
    finite(encounter_rad_s, "encounter frequency", positive=True)
    b = np.asarray(damping_n_s_m2, dtype=float)
    v = complex_vector(relative_velocity_m_s, len(x), "relative velocity")
    if b.shape != x.shape or not np.all(np.isfinite(b)) or np.any(b < 0):
        raise ValueError(
            "sectional damping must be finite, nonnegative and match sections"
        )
    power = float(_trapezoid(b * np.abs(v) ** 2, x) / 2)
    if not isfinite(power):
        raise ValueError("sectional power overflowed")
    return SectionalEnergy(power, power * 2 * pi / encounter_rad_s)


def relative_vertical_velocity(
    section_x_m: Sequence[float],
    amplitude_m: float,
    encounter_rad_s: float,
    heave_rao: complex,
    pitch_rao_rad_per_m: complex,
    incident_vertical_velocity_rao: Sequence[complex],
) -> np.ndarray:
    """Rigid-body velocity minus supplied local wave velocity, retaining phase.

    x is forward from the common RAO origin; positive pitch raises the bow.
    RAOs use exp(i*omega_e*t), heave in m/m and pitch in rad/m. Thus the
    derivative of (h+x*p)*a is i*omega_e*(h+x*p)*a. Incident velocities are
    complex m/s per metre of wave amplitude, at the same sections and phase
    origin. The caller supplies attenuation and forward-speed conventions.
    No effective-wave or strip-theory correction is assumed.
    """
    x = signed_sections(section_x_m)
    finite(amplitude_m, "wave amplitude", positive=True)
    finite(encounter_rad_s, "encounter frequency", positive=True)
    h, p = complex_vector([heave_rao, pitch_rao_rad_per_m], 2, "motion RAOs")
    wave = complex_vector(
        incident_vertical_velocity_rao, len(x), "incident velocity RAO"
    )
    return amplitude_m * (1j * encounter_rad_s * (h + x * p) - wave)


def wave_frequencies(
    ship: Ship, point: OperatingPoint, wavelength_m: float
) -> tuple[float, float]:
    """Deep-water dispersion and signed Doppler shift; no encounter spectrum.

    Source for omega²=g*k: repository capytaine/screening.py,
    finite_depth_wave_number docstring, MIT Marine Hydrodynamics (2005),
    Lecture 20, section 6.2.4, equation (6), deep-water limit.
    k=2*pi/lambda is the definition of radian wavenumber. With waves coming
    from heading beta, differentiating phase along the ship path adds
    k*U*cos(beta); head seas here are zero, unlike propagation-angle readers.
    """
    k = 2 * pi / wavelength_m
    omega = sqrt(ship.gravity_m_s2 * k)
    return omega, omega + k * point.speed_m_s * cos(radians(point.heading_deg))


def radiated_energy_table(
    ship: Ship,
    point: OperatingPoint,
    waves: RegularWaves,
    *,
    heave_rao: Sequence[complex],
    pitch_rao_rad_per_m: Sequence[complex],
) -> list[ResistanceRow]:
    """Reserve the force method with required RAOs and an explicit source gap.

    Reference sought: International Shipbuilding Progress 19 (1972), 217,
    'Analysis of the resistance increase in waves of a fast cargo ship',
    pages 285–293, as listed in ITTC 7.5-02-07-02.8 (2024), bibliography.
    Public equation access was unavailable.
    Supplied RAOs are checked but not used to claim a resistance prediction.
    Sectional energy diagnostics can be calculated separately.
    """
    count = len(waves.wavelength_m)
    complex_vector(heave_rao, count, "heave RAO")
    complex_vector(pitch_rao_rad_per_m, count, "pitch RAO")
    rows = []
    for wavelength, amplitude in zip(waves.wavelength_m, waves.amplitude_m):
        omega, encounter = wave_frequencies(ship, point, wavelength)
        rows.append(
            ResistanceRow(
                wavelength,
                amplitude,
                omega,
                encounter,
                Status.NOT_EVALUATED,
                None,
                None,
                "Public source equation and heading validity unavailable; sectional work alone is not added resistance.",
                "radiated-energy source gap",
            )
        )
    return rows
