# ABOUTME: Shallow-foundation (mudmat) bearing-capacity screening calculator.
# ABOUTME: Brinch Hansen / DNV-RP-C212 general bearing capacity + sliding check.
"""Mudmat (shallow foundation) bearing-capacity screening.

Pure calculator for the general bearing-capacity equation with shape, depth and
load-eccentricity (effective-area) corrections, plus a base-sliding check. Used
for offshore mudmat installation screening (subsea structure foundations).

q_ult = c*Nc*sc*dc + p0'*Nq*sq*dq + 0.5*gamma'*B_eff*Ngamma*sgamma*dgamma

Bearing-capacity factors (Brinch Hansen 1970 / DNV-RP-C212):
    Nq     = exp(pi*tan(phi)) * tan(45 + phi/2)**2
    Nc     = (Nq - 1) / tan(phi)          (phi -> 0: Nc = 2 + pi = 5.14)
    Ngamma = 1.5 * (Nq - 1) * tan(phi)

References: DNV-RP-C212 (Offshore soil mechanics and geotechnical engineering);
Brinch Hansen J. (1970) "A revised and extended formula for bearing capacity".
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field


@dataclass
class BearingCapacityResult:
    standard: str
    condition: str
    Nc: float
    Nq: float
    Ngamma: float
    shape_factors: dict[str, float]
    depth_factors: dict[str, float]
    effective_width_m: float
    effective_length_m: float
    effective_area_m2: float
    q_ult_kpa: float
    vertical_capacity_kn: float
    sliding_capacity_kn: float
    notes: list[str] = field(default_factory=list)


def bearing_capacity_factors(phi_deg: float) -> tuple[float, float, float]:
    """Brinch Hansen bearing-capacity factors (Nc, Nq, Ngamma)."""
    if phi_deg < 0.0:
        raise ValueError("friction angle must be non-negative")
    phi = math.radians(phi_deg)
    if phi_deg == 0.0:
        return 2.0 + math.pi, 1.0, 0.0
    nq = math.exp(math.pi * math.tan(phi)) * math.tan(math.pi / 4 + phi / 2) ** 2
    nc = (nq - 1.0) / math.tan(phi)
    ngamma = 1.5 * (nq - 1.0) * math.tan(phi)
    return nc, nq, ngamma


def _shape_factors(
    phi_deg: float, b_eff: float, l_eff: float, nc: float, nq: float
) -> dict[str, float]:
    ratio = b_eff / l_eff
    if phi_deg == 0.0:
        return {"sc": 1.0 + 0.2 * ratio, "sq": 1.0, "sgamma": 1.0}
    phi = math.radians(phi_deg)
    return {
        "sc": 1.0 + ratio * (nq / nc),
        "sq": 1.0 + ratio * math.tan(phi),
        "sgamma": 1.0 - 0.4 * ratio,
    }


def _depth_factors(
    phi_deg: float, depth: float, b_eff: float, nq: float
) -> dict[str, float]:
    # Brinch Hansen depth factors, valid for D/B <= 1.
    k = depth / b_eff
    k = min(k, 1.0)
    if phi_deg == 0.0:
        return {"dc": 1.0 + 0.4 * k, "dq": 1.0, "dgamma": 1.0}
    phi = math.radians(phi_deg)
    dq = 1.0 + 2.0 * math.tan(phi) * (1.0 - math.sin(phi)) ** 2 * k
    dc = dq - (1.0 - dq) / (nq * math.tan(phi))
    return {"dc": dc, "dq": dq, "dgamma": 1.0}


def mudmat_bearing_capacity(
    width_b_m: float,
    length_l_m: float,
    embedment_depth_m: float,
    condition: str,
    submerged_unit_weight_kn_m3: float,
    vertical_load_kn: float,
    moment_knm: float = 0.0,
    undrained_shear_strength_kpa: float | None = None,
    friction_angle_deg: float | None = None,
    effective_cohesion_kpa: float = 0.0,
    interface_friction_angle_deg: float | None = None,
) -> BearingCapacityResult:
    """General bearing capacity + sliding for a rectangular mudmat.

    ``condition`` is ``"undrained"`` (phi = 0, total-stress, c = su) or
    ``"drained"`` (effective-stress, phi and c'). Eccentricity from
    ``moment_knm`` reduces the effective width via Meyerhof's effective area
    (B_eff = B - 2*M/V) applied along the width.
    """
    if width_b_m <= 0.0 or length_l_m <= 0.0:
        raise ValueError("foundation width and length must be positive")
    if width_b_m > length_l_m:
        raise ValueError("width_b_m must be the shorter plan dimension (B <= L)")
    if embedment_depth_m < 0.0:
        raise ValueError("embedment_depth_m must be non-negative")
    if vertical_load_kn <= 0.0:
        raise ValueError("vertical_load_kn must be positive")

    notes: list[str] = []
    condition = condition.strip().lower()
    if condition == "undrained":
        if undrained_shear_strength_kpa is None:
            raise ValueError(
                "undrained condition requires undrained_shear_strength_kpa"
            )
        cohesion = float(undrained_shear_strength_kpa)
        phi_deg = 0.0
    elif condition == "drained":
        if friction_angle_deg is None:
            raise ValueError("drained condition requires friction_angle_deg")
        cohesion = float(effective_cohesion_kpa)
        phi_deg = float(friction_angle_deg)
    else:
        raise ValueError("condition must be 'undrained' or 'drained'")

    # Effective dimensions (Meyerhof effective area) for moment eccentricity.
    eccentricity = abs(moment_knm) / vertical_load_kn if moment_knm else 0.0
    b_eff = width_b_m - 2.0 * eccentricity
    if b_eff <= 0.0:
        raise ValueError(
            "load eccentricity falls outside the base (B - 2e <= 0): foundation overturns"
        )
    if eccentricity > 0.0:
        notes.append(f"effective width reduced for eccentricity e={eccentricity:.3f} m")
    l_eff = length_l_m
    area_eff = b_eff * l_eff

    nc, nq, ngamma = bearing_capacity_factors(phi_deg)
    shape = _shape_factors(phi_deg, b_eff, l_eff, nc, nq)
    depth = _depth_factors(phi_deg, embedment_depth_m, b_eff, nq)

    # Effective overburden at foundation base.
    p0 = submerged_unit_weight_kn_m3 * embedment_depth_m

    q_c = cohesion * nc * shape["sc"] * depth["dc"]
    q_q = p0 * nq * shape["sq"] * depth["dq"]
    q_g = (
        0.5
        * submerged_unit_weight_kn_m3
        * b_eff
        * ngamma
        * shape["sgamma"]
        * depth["dgamma"]
    )
    q_ult = q_c + q_q + q_g
    vertical_capacity = q_ult * area_eff

    # Base sliding resistance.
    if condition == "undrained":
        sliding_capacity = cohesion * area_eff
    else:
        delta_deg = (
            float(interface_friction_angle_deg)
            if interface_friction_angle_deg is not None
            else phi_deg
        )
        sliding_capacity = (
            vertical_load_kn * math.tan(math.radians(delta_deg)) + cohesion * area_eff
        )

    return BearingCapacityResult(
        standard="DNV-RP-C212 / Brinch Hansen (1970)",
        condition=condition,
        Nc=nc,
        Nq=nq,
        Ngamma=ngamma,
        shape_factors=shape,
        depth_factors=depth,
        effective_width_m=b_eff,
        effective_length_m=l_eff,
        effective_area_m2=area_eff,
        q_ult_kpa=q_ult,
        vertical_capacity_kn=vertical_capacity,
        sliding_capacity_kn=sliding_capacity,
        notes=notes,
    )
