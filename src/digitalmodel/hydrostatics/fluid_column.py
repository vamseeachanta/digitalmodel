"""
Fluid-column hydrostatics — seabed / hydrostatic pressure.
==========================================================

Closed-form helpers for the static pressure of a fluid column and the
pressure-unit conversions that go with offshore well / riser pressure
calculations.

Core relation (gauge / hydrostatic pressure of a column)::

    Δp = rho * g * h            [Pa]   (rho in kg/m^3, h in m)

and the seabed (absolute) pressure of a fluid column below a surface
pressure::

    p_seabed = p_surface + rho * g * h

Mud / fluid density is frequently quoted in pounds-per-gallon (ppg) and
depths in feet, so unit converters are included.  Pressures convert
between Pa, bar and psi.

References
----------
- Generic hydrostatics (Pascal's law); standard offshore drilling /
  riser pressure practice.

Issue: corpus calculators epic #767 / #779.
"""

from __future__ import annotations

from typing import Union

__all__ = [
    "G_STANDARD",
    "PPG_TO_KG_M3",
    "FT_TO_M",
    "PSI_TO_PA",
    "PA_TO_PSI",
    "BAR_TO_PSI",
    "PSI_TO_BAR",
    "ppg_to_kg_m3",
    "ft_to_m",
    "psi_to_pa",
    "pa_to_psi",
    "bar_to_psi",
    "psi_to_bar",
    "hydrostatic_pressure",
    "seabed_pressure",
]

ArrayLike = Union[float, "object"]  # float or numpy ndarray (numpy not required)

# Standard gravity (m/s^2)
G_STANDARD = 9.81

# Unit conversion constants (match the corpus design-sheet factors)
PPG_TO_KG_M3 = 119.826426627972   # 1 lb/US-gal -> kg/m^3
FT_TO_M = 0.3048                  # 1 ft -> m
PSI_TO_PA = 6894.74482549401      # 1 psi -> Pa
PA_TO_PSI = 1.0 / PSI_TO_PA
# 1 bar = 1e5 Pa  -> psi
BAR_TO_PSI = 1.0e5 * PA_TO_PSI
PSI_TO_BAR = 1.0 / BAR_TO_PSI


def ppg_to_kg_m3(density_ppg: ArrayLike) -> ArrayLike:
    """Convert fluid density from pounds-per-gallon (ppg) to kg/m^3."""
    return density_ppg * PPG_TO_KG_M3


def ft_to_m(depth_ft: ArrayLike) -> ArrayLike:
    """Convert a depth/length from feet to metres."""
    return depth_ft * FT_TO_M


def psi_to_pa(pressure_psi: ArrayLike) -> ArrayLike:
    """Convert pressure from psi to pascals."""
    return pressure_psi * PSI_TO_PA


def pa_to_psi(pressure_pa: ArrayLike) -> ArrayLike:
    """Convert pressure from pascals to psi."""
    return pressure_pa * PA_TO_PSI


def bar_to_psi(pressure_bar: ArrayLike) -> ArrayLike:
    """Convert pressure from bar to psi."""
    return pressure_bar * BAR_TO_PSI


def psi_to_bar(pressure_psi: ArrayLike) -> ArrayLike:
    """Convert pressure from psi to bar."""
    return pressure_psi * PSI_TO_BAR


def hydrostatic_pressure(
    density: ArrayLike,
    height: ArrayLike,
    gravity: float = G_STANDARD,
) -> ArrayLike:
    """Hydrostatic pressure of a fluid column, ``Δp = rho * g * h``.

    Parameters
    ----------
    density : float or array-like
        Fluid density in kg/m^3.
    height : float or array-like
        Column height (depth) in metres.
    gravity : float
        Gravitational acceleration in m/s^2 (default 9.81).

    Returns
    -------
    float or np.ndarray
        Pressure in pascals (Pa).
    """
    return density * gravity * height


def seabed_pressure(
    density: ArrayLike,
    height: ArrayLike,
    surface_pressure: ArrayLike = 0.0,
    gravity: float = G_STANDARD,
) -> ArrayLike:
    """Absolute pressure at the bottom of a fluid column.

    ``p_seabed = p_surface + rho * g * h``

    Parameters
    ----------
    density : float or array-like
        Fluid density in kg/m^3.
    height : float or array-like
        Column height (depth) in metres.
    surface_pressure : float or array-like
        Pressure at the top of the column in Pa (default 0 = gauge).
    gravity : float
        Gravitational acceleration in m/s^2 (default 9.81).

    Returns
    -------
    float or np.ndarray
        Seabed (bottom) pressure in pascals (Pa).
    """
    return surface_pressure + hydrostatic_pressure(density, height, gravity)
