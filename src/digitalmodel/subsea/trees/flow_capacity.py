# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Bore-area capacity screen and separate production/annulus port handoff.

API RP 14E is the intended erosional-screen reference and API 17D / API 6A
identify the equipment scope. Verified editions and clause locators are not
available here; no standard compliance or rating qualification is asserted.
No standard coefficient is included. The caller supplies a coefficient in SI
units, C [m/s * sqrt(kg/m3)], for v_limit=C/sqrt(rho); Q_limit=pi*d^2*v_limit/4.
An oilfield-unit C value cannot be passed unchanged. Selection of C requires
an independently qualified design basis. This velocity-only screen excludes
sand erosion, corrosion, multiphase slip and pressure-loss prediction.

The handoff carries the supplied operating rates and pressure, flags the rate
criterion, and never silently clips flow. Production and annulus remain
separate streams. Downstream network composition remains outside this module.
"""

from __future__ import annotations

import math
from dataclasses import dataclass


def _nonnegative(*values: float) -> None:
    if any(not math.isfinite(value) or value < 0 for value in values):
        raise ValueError("values must be finite and nonnegative")


@dataclass(frozen=True)
class BoreCapacity:
    """Velocity ceiling in m/s and volumetric rate ceiling in m3/s."""

    velocity_m_s: float
    rate_m3_s: float

    def __post_init__(self) -> None:
        """Require finite positive ceilings even for directly built results."""
        _nonnegative(self.velocity_m_s, self.rate_m3_s)
        if not self.velocity_m_s or not self.rate_m3_s:
            raise ValueError("capacity ceilings must be positive")


def bore_capacity(diameter_m: float, density_kg_m3: float,
                  erosional_coefficient_si: float) -> BoreCapacity:
    """Calculate one port ceiling using the explicit, caller-qualified C."""
    _nonnegative(diameter_m, density_kg_m3, erosional_coefficient_si)
    if not all((diameter_m, density_kg_m3, erosional_coefficient_si)):
        raise ValueError("diameter, density and coefficient must be positive")
    velocity = erosional_coefficient_si / math.sqrt(density_kg_m3)
    return BoreCapacity(velocity, math.pi * diameter_m**2 * velocity / 4)


@dataclass(frozen=True)
class FlowHandoff:
    """Separate port rates, downstream pressure and velocity-screen outcome."""

    production_rate_m3_s: float
    annulus_rate_m3_s: float
    downstream_pressure_pa: float
    within_capacity: bool


def flow_handoff(production_rate_m3_s: float, annulus_rate_m3_s: float,
                 production_capacity: BoreCapacity, annulus_capacity: BoreCapacity | None,
                 upstream_pressure_pa: float, pressure_drop_pa: float) -> FlowHandoff:
    """Carry actual rates without mixing ports; losses are caller-supplied."""
    _nonnegative(production_rate_m3_s, annulus_rate_m3_s,
                 upstream_pressure_pa, pressure_drop_pa)
    if pressure_drop_pa > upstream_pressure_pa:
        raise ValueError("pressure drop exceeds available pressure")
    if annulus_capacity is None and annulus_rate_m3_s:
        raise ValueError("annulus flow requires an annulus capacity")
    annulus_limit = annulus_capacity.rate_m3_s if annulus_capacity else 0.0
    within = (production_rate_m3_s <= production_capacity.rate_m3_s
              and annulus_rate_m3_s <= annulus_limit)
    return FlowHandoff(production_rate_m3_s, annulus_rate_m3_s,
                       upstream_pressure_pa - pressure_drop_pa, within)
