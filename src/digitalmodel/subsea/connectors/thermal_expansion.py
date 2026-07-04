"""Thermal-expansion driven loads on rigid subsea jumpers (API 17R).

A rigid M-/U-shaped jumper bridges a tree hub and a manifold/PLET hub.
When the flowline/jumper heats up from ambient seabed temperature to
operating temperature it tries to grow by ``delta_L = alpha * L * dT``.
The jumper geometry (the legs of the M/U) accommodates this growth
elastically; the resulting end reactions (axial thrust + bending) are
what the connectors must react.

This module provides the classic restrained / partially-restrained
thermal-load relations used for first-pass sizing.

Equations
---------
Free thermal strain:           eps_th = alpha * dT                  (1)
Fully restrained axial stress: sigma_th = E * alpha * dT            (2)
Fully restrained axial force:  F_th = sigma_th * A = E A alpha dT   (3)
Free thermal growth:           delta_L = alpha * L * dT             (4)

The fully-restrained force (3) is the *upper bound* on the axial thrust a
connector sees; a compliant jumper geometry reacts only a fraction of it,
captured by ``restraint_factor`` in [0, 1] (0 = free, 1 = fully fixed).

References
----------
API 17R / ISO 13628-1 jumper thermal design; restrained-bar thermal stress
is standard mechanics of materials (Roark, Ch. 1).
"""

from __future__ import annotations

from dataclasses import dataclass

from .section_properties import PipeSection


@dataclass
class ThermalResult:
    """Thermal-expansion load result."""

    thermal_strain: float       # eps_th [-]
    free_growth: float          # delta_L [m]
    restrained_stress: float    # sigma_th fully restrained [Pa]
    axial_force: float          # F_th reacted at connector [N]


def thermal_expansion_load(
    section: PipeSection,
    length: float,
    delta_temperature: float,
    thermal_expansion_coeff: float = 1.17e-5,
    youngs_modulus: float = 207e9,
    restraint_factor: float = 1.0,
) -> ThermalResult:
    """Thermal-expansion load on a jumper leg/connector.

    Parameters
    ----------
    section : PipeSection
        Jumper cross section.
    length : float
        Heated length L [m] contributing to growth.
    delta_temperature : float
        Temperature rise dT above installation/ambient [degC or K].
    thermal_expansion_coeff : float
        alpha [1/K]; default 1.17e-5 for carbon steel.
    youngs_modulus : float
        E [Pa]; default 207e9 for steel.
    restraint_factor : float
        Fraction of the fully-restrained force actually reacted at the
        connector, in [0, 1] (1 = fully fixed, 0 = free growth).

    Returns
    -------
    ThermalResult
    """
    if not 0.0 <= restraint_factor <= 1.0:
        raise ValueError("restraint_factor must be in [0, 1]")

    eps_th = thermal_expansion_coeff * delta_temperature
    free_growth = eps_th * length
    sigma_full = youngs_modulus * eps_th
    f_full = sigma_full * section.area
    return ThermalResult(
        thermal_strain=eps_th,
        free_growth=free_growth,
        restrained_stress=sigma_full,
        axial_force=f_full * restraint_factor,
    )
