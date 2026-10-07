"""Burst and collapse pressure capacity of the riser main tube (owner decision W503).

Reference: API STD 2RD, 2nd Ed (2013), 5.3.2 (burst) and 5.3.3 (collapse). Wall thickness ``t`` is the minimum wall
(nominal less the minus tolerance). Units: diameters and wall in m, strengths and pressures in MPa.
"""

from __future__ import annotations

import math

REFERENCE = "API STD 2RD 2nd Ed (2013) 5.3.2 burst, 5.3.3 collapse"
E_STEEL_MPA = 207000.0
POISSON = 0.3


def _check(od: float, t: float) -> None:
    if od <= 0.0 or t <= 0.0 or 2.0 * t >= od:
        raise ValueError(f"invalid pipe geometry: OD {od}, wall {t}")


def burst_pressure_mpa(od: float, t: float, smys: float, smts: float) -> float:
    """p_b = 0.45 (S + U) ln(D / D_i), D_i = D - 2 t."""
    _check(od, t)
    return 0.45 * (smys + smts) * math.log(od / (od - 2.0 * t))


def collapse_pressure_mpa(od: float, t: float, smys: float, e: float = E_STEEL_MPA, nu: float = POISSON) -> float:
    """p_c = p_y p_el / sqrt(p_y^2 + p_el^2); p_y = 2 S t / D; p_el = 2 E (t / D)^3 / (1 - nu^2)."""
    _check(od, t)
    py = 2.0 * smys * t / od
    pel = 2.0 * e * (t / od) ** 3 / (1.0 - nu ** 2)
    return py * pel / math.sqrt(py ** 2 + pel ** 2)


__all__ = ["REFERENCE", "burst_pressure_mpa", "collapse_pressure_mpa"]
