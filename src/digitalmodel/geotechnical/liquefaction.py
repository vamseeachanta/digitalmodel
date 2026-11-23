# ABOUTME: Seed-Idriss simplified liquefaction-triggering screening.
# ABOUTME: CSR vs CRR (SPT (N1)60cs) with rd, MSF; per-layer factor of safety.
"""Liquefaction triggering — Seed-Idriss simplified procedure (NCEER / Youd 2001).

Per soil layer, compares the earthquake-induced cyclic stress ratio (CSR) to the
soil's cyclic resistance ratio (CRR) to get a factor of safety against
liquefaction triggering:

    CSR    = 0.65 * (a_max/g) * (sigma_v / sigma_v') * rd
    CRR7.5 = 1/(34 - N) + N/135 + 50/(10 N + 45)^2 - 1/200      [N = (N1)60cs]
    FS     = (CRR7.5 / CSR) * MSF * K_sigma

with the depth stress-reduction factor rd (Liao & Whitman 1986), the magnitude
scaling factor MSF (Idriss) = 174 / Mw^2.56, and the overburden factor K_sigma
(taken 1.0 for screening). Clean sand with (N1)60cs >= 30 is treated as
non-liquefiable. The layer liquefies when FS < the required factor of safety.

References: Youd et al. (2001) "Liquefaction Resistance of Soils" (NCEER/NSF
workshops), J. Geotech. Geoenviron. Eng.; Seed & Idriss (1971).
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field

# (N1)60cs at/above which clean sand is treated as non-liquefiable.
_N_NON_LIQUEFIABLE = 30.0
# CRR assigned to non-liquefiable layers (high, dimensionless cap).
_CRR_CAP = 2.0


@dataclass
class LayerResult:
    depth_m: float
    rd: float
    csr: float
    crr75: float
    crr: float
    msf: float
    factor_of_safety: float
    liquefies: bool


@dataclass
class LiquefactionResult:
    standard: str
    pga_g: float
    magnitude: float
    msf: float
    required_factor_of_safety: float
    governing_depth_m: float
    governing_factor_of_safety: float
    screening_status: str
    layers: list[LayerResult] = field(default_factory=list)


def stress_reduction_factor(depth_m: float) -> float:
    """Depth stress-reduction factor rd (Liao & Whitman 1986)."""
    if depth_m < 0.0:
        raise ValueError("depth_m must be non-negative")
    if depth_m <= 9.15:
        return 1.0 - 0.00765 * depth_m
    if depth_m <= 23.0:
        return 1.174 - 0.0267 * depth_m
    if depth_m <= 30.0:
        return 0.744 - 0.008 * depth_m
    return 0.5


def magnitude_scaling_factor(magnitude: float) -> float:
    """Magnitude scaling factor MSF = 174 / Mw^2.56 (Idriss; ~1.0 at Mw 7.5)."""
    if magnitude <= 0.0:
        raise ValueError("magnitude must be positive")
    return 174.0 / magnitude**2.56


def cyclic_stress_ratio(
    pga_g: float, sigma_v_kpa: float, sigma_v_eff_kpa: float, rd: float
) -> float:
    """Seed-Idriss cyclic stress ratio CSR = 0.65 (a_max/g)(sv/sv') rd."""
    if sigma_v_eff_kpa <= 0.0:
        raise ValueError("sigma_v_eff_kpa must be positive")
    return 0.65 * pga_g * (sigma_v_kpa / sigma_v_eff_kpa) * rd


def cyclic_resistance_ratio_75(n1_60cs: float) -> float:
    """Clean-sand CRR at Mw 7.5 from (N1)60cs (Youd et al. 2001)."""
    if n1_60cs < 0.0:
        raise ValueError("n1_60cs must be non-negative")
    if n1_60cs >= _N_NON_LIQUEFIABLE:
        return _CRR_CAP
    return (
        1.0 / (34.0 - n1_60cs)
        + n1_60cs / 135.0
        + 50.0 / (10.0 * n1_60cs + 45.0) ** 2
        - 1.0 / 200.0
    )


def assess_liquefaction(
    pga_g: float,
    magnitude: float,
    layers: list[dict],
    required_factor_of_safety: float = 1.2,
    k_sigma: float = 1.0,
) -> LiquefactionResult:
    """Per-layer factor of safety against liquefaction triggering."""
    if pga_g <= 0.0:
        raise ValueError("pga_g must be positive")
    if not layers:
        raise ValueError("layers must be a non-empty list")
    if required_factor_of_safety <= 0.0:
        raise ValueError("required_factor_of_safety must be positive")

    msf = magnitude_scaling_factor(magnitude)
    results: list[LayerResult] = []
    for layer in layers:
        depth = float(layer["depth_m"])
        sigma_v = float(layer["sigma_v_kpa"])
        sigma_v_eff = float(layer["sigma_v_eff_kpa"])
        n1_60cs = float(layer["N1_60cs"])

        rd = stress_reduction_factor(depth)
        csr = cyclic_stress_ratio(pga_g, sigma_v, sigma_v_eff, rd)
        crr75 = cyclic_resistance_ratio_75(n1_60cs)
        crr = crr75  # CRR at the design magnitude is handled via MSF below
        if csr <= 0.0:
            raise ValueError(f"layer at {depth} m produced non-positive CSR")
        fos = (crr / csr) * msf * k_sigma
        results.append(
            LayerResult(
                depth_m=depth,
                rd=rd,
                csr=csr,
                crr75=crr75,
                crr=crr,
                msf=msf,
                factor_of_safety=fos,
                liquefies=fos < required_factor_of_safety,
            )
        )

    governing = min(results, key=lambda r: r.factor_of_safety)
    passes = not any(r.liquefies for r in results)
    return LiquefactionResult(
        standard="Seed-Idriss simplified (Youd et al. 2001 / NCEER)",
        pga_g=pga_g,
        magnitude=magnitude,
        msf=msf,
        required_factor_of_safety=required_factor_of_safety,
        governing_depth_m=governing.depth_m,
        governing_factor_of_safety=governing.factor_of_safety,
        screening_status="pass" if passes else "fail",
        layers=results,
    )
