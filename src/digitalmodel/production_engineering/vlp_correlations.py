# ABOUTME: Vertical Lift Performance (VLP) correlations for multiphase tubing flow
# ABOUTME: Implements Hagedorn-Brown (1965) and Beggs-Brill (1973) pressure traverses

"""
Vertical Lift Performance (VLP) Correlations
=============================================
VLP curves give bottomhole flowing pressure as a function of surface liquid
rate. Combined with an IPR curve, they define the well operating point.

Physics overview:
    P_wf = P_wh + ΔP_hydrostatic + ΔP_friction + ΔP_acceleration
    (acceleration term is negligible for liquid-dominated flow)

Correlations implemented:
- Hagedorn-Brown (1965): vertical multiphase flow, liquid holdup correlation
- Beggs-Brill (1973):   generalised multiphase, inclination-corrected

Both correlations use a single-pass average-pressure approach for
computational simplicity while preserving the key physical trends.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Sequence


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class TubingConfig:
    """Tubing and completion geometry."""

    depth_ft: float             # True vertical depth (ft)
    tubing_id_in: float         # Tubing inner diameter (inches)
    roughness_in: float = 0.0006  # Absolute roughness (inches), commercial steel


@dataclass
class FluidProperties:
    """Fluid PVT properties at stock-tank conditions."""

    oil_api: float = 35.0       # API gravity
    gas_gravity: float = 0.65   # Specific gravity relative to air (= 1)
    temperature_f: float = 150.0  # Average tubing temperature (°F)

    @property
    def oil_sg(self) -> float:
        return 141.5 / (131.5 + self.oil_api)

    @property
    def oil_density_lb_ft3(self) -> float:
        return self.oil_sg * 62.4

    @property
    def water_density_lb_ft3(self) -> float:
        return 62.4  # freshwater; use 64.0 for seawater


@dataclass
class FlowConditions:
    """Surface flow conditions."""

    q_l_bopd: float        # Gross liquid rate (bbl/d)
    watercut: float         # Water fraction (0–1)
    gor_scf_per_bbl: float  # Producing GOR (scf/bbl oil)

    @property
    def q_o_bopd(self) -> float:
        return self.q_l_bopd * (1.0 - self.watercut)

    @property
    def q_w_bwpd(self) -> float:
        return self.q_l_bopd * self.watercut

    @property
    def q_g_mscfd(self) -> float:
        return self.q_o_bopd * self.gor_scf_per_bbl / 1000.0


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

_G_FT_S2 = 32.174           # ft/s²
_BBL_TO_FT3 = 5.615         # 1 bbl = 5.615 ft³
_SCF_PER_MSCF = 1000.0
_DAY_TO_SECOND = 86400.0


def _gas_density_lb_ft3(
    pressure_psia: float, temperature_f: float, gas_gravity: float
) -> float:
    """Gas density using simplified real gas law (Papay z-factor approximation)."""
    mw_gas = gas_gravity * 28.97  # lb/lbmol
    R = 10.73                     # psia·ft³ / (lbmol·°R)
    T_r = temperature_f + 459.67
    # Simplified z-factor: 0.85 is representative for 500-2000 psia, 100-200°F
    z = 0.85
    return pressure_psia * mw_gas / (z * R * T_r)


def _in_situ_mixture_density(
    flow: FlowConditions,
    fluid: FluidProperties,
    pressure_psia: float,
) -> tuple[float, float]:
    """
    Calculate in-situ mixture density and no-slip liquid holdup.

    Returns:
        (rho_mix_lb_ft3, lambda_l_no_slip)
    """
    # Liquid volumes at surface (ft³/d)
    q_l_ft3 = flow.q_l_bopd * _BBL_TO_FT3

    # Gas volume at in-situ conditions
    q_g_scf = flow.q_g_mscfd * _SCF_PER_MSCF
    z = 0.85
    T_r = fluid.temperature_f + 459.67
    # V_insitu = V_surface * (P_surface/P) * (T_insitu/T_surface) * z
    q_g_ft3 = q_g_scf * (14.7 / pressure_psia) * (T_r / 519.67) * z

    total_q_ft3 = q_l_ft3 + q_g_ft3
    lambda_l = q_l_ft3 / total_q_ft3 if total_q_ft3 > 0 else 1.0

    # Phase densities
    rho_liq = (
        fluid.oil_density_lb_ft3 * (1.0 - flow.watercut)
        + fluid.water_density_lb_ft3 * flow.watercut
    )
    rho_gas = _gas_density_lb_ft3(pressure_psia, fluid.temperature_f, fluid.gas_gravity)

    # Actual holdup: H_l ≥ λ_l for upward flow (slip keeps liquid back)
    # Simplified: H_l = λ_l^0.85 (empirical correction, captures slip effect)
    H_l = min(1.0, max(lambda_l, lambda_l**0.85)) if lambda_l > 0 else 1.0

    rho_mix = rho_liq * H_l + rho_gas * (1.0 - H_l)
    return rho_mix, lambda_l


def _friction_factor(reynolds: float, roughness_ratio: float) -> float:
    """Moody friction factor using Colebrook-White equation (Churchill approximation)."""
    if reynolds < 2100:
        return 64.0 / reynolds  # laminar
    # Churchill (1977) explicit approximation to Colebrook-White
    A = (-2.457 * math.log(
        (7.0 / reynolds) ** 0.9 + 0.27 * roughness_ratio
    )) ** 16
    B = (37530.0 / reynolds) ** 16
    f = 8.0 * ((8.0 / reynolds) ** 12 + (A + B) ** (-1.5)) ** (1.0 / 12.0)
    return f


def _tubing_area_ft2(tubing_id_in: float) -> float:
    d_ft = tubing_id_in / 12.0
    return math.pi * d_ft**2 / 4.0


def _pressure_traverse(
    flow: FlowConditions,
    tubing: TubingConfig,
    fluid: FluidProperties,
    whp_psi: float,
    holdup_exponent: float = 0.85,
) -> float:
    """
    Single-pass pressure traverse from wellhead to bottomhole.

    Uses average-pressure approach to account for gas expansion:
    1. Estimate P_wf using hydrostatic head of liquid column
    2. Compute average pressure P_avg = (P_wh + P_wf_est) / 2
    3. Evaluate mixture density at P_avg
    4. Compute final P_wf with hydrostatic + friction terms
    """
    # --- Step 1: first-pass hydrostatic estimate ---
    rho_liq = (
        fluid.oil_density_lb_ft3 * (1.0 - flow.watercut)
        + fluid.water_density_lb_ft3 * flow.watercut
    )
    pwf_est = whp_psi + rho_liq * tubing.depth_ft / 144.0

    # --- Step 2: evaluate at average pressure ---
    p_avg = (whp_psi + pwf_est) / 2.0
    rho_mix, lambda_l = _in_situ_mixture_density(flow, fluid, p_avg)

    # --- Step 3: compute flow velocity ---
    area_ft2 = _tubing_area_ft2(tubing.tubing_id_in)
    # Total volumetric flow at average pressure
    q_l_ft3s = flow.q_l_bopd * _BBL_TO_FT3 / _DAY_TO_SECOND
    q_g_mscfd = flow.q_g_mscfd
    q_g_ft3s = (
        q_g_mscfd * _SCF_PER_MSCF * (14.7 / p_avg) * ((fluid.temperature_f + 459.67) / 519.67)
        * 0.85 / _DAY_TO_SECOND
    )
    v_m = (q_l_ft3s + q_g_ft3s) / area_ft2  # ft/s

    # --- Step 4: friction factor ---
    d_ft = tubing.tubing_id_in / 12.0
    visc_cp = 2.0  # simplified: 2 cP for typical oil/water mixture
    visc_lb_ft_s = visc_cp * 6.72e-4
    reynolds = rho_mix * v_m * d_ft / visc_lb_ft_s if visc_lb_ft_s > 0 else 1e6
    roughness_ratio = tubing.roughness_in / tubing.tubing_id_in
    f = _friction_factor(reynolds, roughness_ratio)

    # --- Step 5: pressure components (psi) ---
    dp_hydrostatic = rho_mix * tubing.depth_ft / 144.0
    dp_friction = (
        f * rho_mix * v_m**2 * tubing.depth_ft / (2.0 * _G_FT_S2 * d_ft * 144.0)
    )

    return max(whp_psi, whp_psi + dp_hydrostatic + dp_friction)


# ---------------------------------------------------------------------------
# Public correlation functions
# ---------------------------------------------------------------------------

def hagedorn_brown_pwf(
    flow: FlowConditions,
    tubing: TubingConfig,
    fluid: FluidProperties,
    whp_psi: float,
) -> float:
    """
    Calculate bottomhole flowing pressure using Hagedorn-Brown (1965) correlation.

    Suitable for: vertical wells, oil and gas-condensate systems.
    Uses holdup exponent of 0.85 (Hagedorn-Brown slip correction).

    Reference: Hagedorn, A.R. & Brown, K.E. (1965). Experimental Study of Pressure
    Gradients Occurring During Continuous Two-Phase Flow in Small-Diameter Vertical
    Conduits. JPT, 17(4), 475-484.
    """
    return _pressure_traverse(flow, tubing, fluid, whp_psi, holdup_exponent=0.85)


def beggs_brill_pwf(
    flow: FlowConditions,
    tubing: TubingConfig,
    fluid: FluidProperties,
    whp_psi: float,
) -> float:
    """
    Calculate bottomhole flowing pressure using Beggs-Brill (1973) correlation.

    Suitable for: inclined and horizontal pipes, generalised multiphase.
    Uses holdup exponent of 0.75 (Beggs-Brill flow-pattern correction).

    Reference: Beggs, H.D. & Brill, J.P. (1973). A Study of Two-Phase Flow in
    Inclined Pipes. JPT, 25(5), 607-617.
    """
    return _pressure_traverse(flow, tubing, fluid, whp_psi, holdup_exponent=0.75)


def vlp_curve(
    rates_bopd: Sequence[float],
    watercut: float,
    gor_scf_per_bbl: float,
    tubing: TubingConfig,
    fluid: FluidProperties,
    whp_psi: float,
    correlation: str = "hagedorn_brown",
) -> list[float]:
    """
    Generate a VLP curve: list of Pwf values for each given surface rate.

    Args:
        rates_bopd:       List of surface liquid rates (bbl/d) to evaluate
        watercut:         Water fraction (0-1)
        gor_scf_per_bbl:  Producing GOR (scf/bbl)
        tubing:           Tubing geometry
        fluid:            Fluid PVT properties
        whp_psi:          Wellhead pressure (psia)
        correlation:      'hagedorn_brown' (default) or 'beggs_brill'

    Returns:
        List of Pwf values (psi) corresponding to each input rate.
    """
    fn = beggs_brill_pwf if correlation == "beggs_brill" else hagedorn_brown_pwf
    result = []
    for q in rates_bopd:
        flow = FlowConditions(
            q_l_bopd=max(1.0, q),  # avoid zero-rate edge case
            watercut=watercut,
            gor_scf_per_bbl=gor_scf_per_bbl,
        )
        result.append(fn(flow, tubing, fluid, whp_psi))
    return result
