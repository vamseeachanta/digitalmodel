# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Shared nominal stresses for a centred, axial, single-plate lifting lug.

Source: MIT, Stress-Strain Curves (2001), Eq. (1), sigma=P/A:
https://web.mit.edu/course/3/3.11/www/modules/ss.pdf
Source: MIT, Shear and Torsion (2000), p. 2, direct shear tau=P/A:
https://web.mit.edu/course/3/3.11/www/modules/torsion.pdf

Areas below follow the explicitly stated rectangular load-path idealisation.
They are not ABS resistance equations and include no empirical coefficients.
"""

from __future__ import annotations

import math
from dataclasses import dataclass


@dataclass(frozen=True)
class PlateDemands:
    """Nominal bearing, net tension and two-plane tear-out stresses, in MPa."""

    pin_bearing: float
    net_tension: float
    shear_tearout: float


def plate_demands_mpa(
    load_kn: float,
    thickness_mm: float,
    pin_diameter_mm: float,
    hole_diameter_mm: float,
    width_mm: float,
    centre_to_loaded_edge_mm: float,
) -> PlateDemands:
    """Return P/A demands for a centred hole and an in-plane axial load.

    Width is measured across the hole perpendicular to load; the edge distance
    is from the hole centre along load to the free edge. Cheek plates, pin
    bending, contact peaks, hoop tension and interacting failure are excluded.
    """
    dimensions = (
        thickness_mm, pin_diameter_mm, hole_diameter_mm, width_mm,
        centre_to_loaded_edge_mm,
    )
    if not all(math.isfinite(value) and value > 0 for value in dimensions):
        raise ValueError("plate dimensions must be finite and positive")
    if not math.isfinite(load_kn) or load_kn < 0:
        raise ValueError("load_kn must be finite and nonnegative")
    if pin_diameter_mm > hole_diameter_mm:
        raise ValueError("pin diameter must not exceed hole diameter")
    if width_mm <= hole_diameter_mm:
        raise ValueError("width must exceed hole diameter")
    # Two is the number of shear planes; radius=diameter/2 by geometry.
    ligament = centre_to_loaded_edge_mm - hole_diameter_mm / 2
    if ligament <= 0:
        raise ValueError("loaded edge must leave a positive clear ligament")
    areas = (
        pin_diameter_mm * thickness_mm,
        (width_mm - hole_diameter_mm) * thickness_mm,
        2 * ligament * thickness_mm,
    )
    force_n = load_kn * 1000  # SI unit conversion: kN to N, N/mm² = MPa.
    if not math.isfinite(force_n) or not all(
        math.isfinite(area) and area > 0 for area in areas
    ):
        raise ValueError("derived force and areas must be finite and representable")
    stresses = tuple(force_n / area for area in areas)
    if not all(math.isfinite(stress) for stress in stresses):
        raise ValueError("derived stresses must be finite")
    return PlateDemands(*stresses)
