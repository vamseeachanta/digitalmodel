# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Compatibility surfaces for on-bottom stability arithmetic.

Lateral and upward checks use clause 3.6.2 Eqs. (3.38)-(3.39) of the
supplied F109 clause-map basis, with explicit factors selected by the caller
under clause 3.6.3 Tables 3-5 through 3-8. Inputs are extreme-cycle peak
magnitudes in SI or kN/m as named. The lateral wrapper uses no passive
resistance and reports the greater of lateral and uplift utilisation.
Instantaneous load helpers do not establish clause 3.6.4 peaks. Generalised
displacement, dynamic analysis and site-specific qualification remain OPEN.
"""
from __future__ import annotations

import math
from dataclasses import dataclass

from digitalmodel.subsea.on_bottom_stability.dnv_rp_f109 import (
    absolute_stability_check,
    absolute_stability_margin,
    hydrodynamic_force_per_meter as _horizontal,
    lift_force_per_meter as _lift,
    submerged_weight_per_meter as _weight,
)


STANDARD: str = "DNV-RP-F109"
G: float = 9.81  # m/s^2, clause 1.5
DEFAULT_CL: float = 0.9  # Legacy instantaneous export; unqualified.


@dataclass
class SubmergedWeightResult:
    """Submerged weight calculation result per unit length."""

    ws_n_per_m: float
    buoyancy_n_per_m: float
    dry_weight_n_per_m: float


@dataclass
class HydrodynamicLoadResult:
    """Hydrodynamic load calculation result per unit length."""

    drag_n_per_m: float
    inertia_n_per_m: float
    total_horizontal_n_per_m: float
    lift_n_per_m: float


@dataclass
class StabilityResult:
    """Lateral stability check result (simplified surface).

    Both ``utilization`` (US spelling, primary field) and ``utilisation``
    (UK alias, property) are exposed for cross-API compatibility with the
    canonical :class:`digitalmodel.subsea.on_bottom_stability.dnv_rp_f109.StabilityResult`
    NamedTuple.
    """

    is_stable: bool
    utilization: float
    required_weight_n_per_m: float = 0.0
    standard: str = STANDARD

    @property
    def utilisation(self) -> float:
        """UK-spelling alias for :attr:`utilization`."""
        return self.utilization


@dataclass
class VerticalStabilityResult:
    """Vertical stability check result."""

    is_stable: bool
    utilization: float
    standard: str = STANDARD

    @property
    def utilisation(self) -> float:
        """UK-spelling alias for :attr:`utilization`."""
        return self.utilization


def submerged_weight(
    od_steel_m: float,
    wt_steel_m: float,
    coating_thickness_m: float,
    rho_steel: float,
    rho_coating: float,
    rho_contents: float,
    rho_seawater: float,
) -> SubmergedWeightResult:
    """Submerged weight of coated pipe per unit length.

    Calculates cross-sectional areas of steel wall, coating annulus,
    and internal contents, then subtracts displaced seawater buoyancy.
    Uses gravity under clause 1.5; phase inputs follow clause 2.3.

    Args:
        od_steel_m: Steel outer diameter (m).
        wt_steel_m: Steel wall thickness (m).
        coating_thickness_m: External coating thickness (m).
        rho_steel: Steel density (kg/m^3).
        rho_coating: Coating density (kg/m^3).
        rho_contents: Internal fluid density (kg/m^3).
        rho_seawater: Seawater density (kg/m^3).

    Returns:
        SubmergedWeightResult with weights per unit length (N/m).
    """
    ws = _weight(od_steel_m, wt_steel_m, rho_steel, rho_coating,
                 coating_thickness_m, rho_contents, rho_seawater)
    id_steel_m = od_steel_m - 2.0 * wt_steel_m
    od_total_m = od_steel_m + 2.0 * coating_thickness_m

    a_steel = math.pi / 4.0 * (od_steel_m**2 - id_steel_m**2)
    a_coating = math.pi / 4.0 * (od_total_m**2 - od_steel_m**2)
    a_contents = math.pi / 4.0 * id_steel_m**2
    a_displaced = math.pi / 4.0 * od_total_m**2

    w_steel = a_steel * rho_steel * G
    w_coating = a_coating * rho_coating * G
    w_contents = a_contents * rho_contents * G
    dry_weight = w_steel + w_coating + w_contents

    buoyancy = a_displaced * rho_seawater * G

    return SubmergedWeightResult(
        ws_n_per_m=ws,
        buoyancy_n_per_m=buoyancy,
        dry_weight_n_per_m=dry_weight,
    )


def hydrodynamic_loads(
    od_total_m: float,
    water_velocity_m_s: float,
    water_acceleration_m_s2: float,
    rho_seawater: float,
    cd: float,
    cm: float,
    cl: float,
) -> HydrodynamicLoadResult:
    """Hydrodynamic forces on a seabed pipe per Morison equation.

    Drag:    ``F_d = 0.5 * rho * Cd * D * U * |U|``
    Inertia: ``F_i = rho * Cm * pi/4 * D^2 * a``
    Lift:    ``F_l = 0.5 * rho * Cl * D * U^2``

    Args:
        od_total_m: Total outer diameter including coating (m).
        water_velocity_m_s: Combined wave + current velocity (m/s).
        water_acceleration_m_s2: Water particle acceleration (m/s^2).
        rho_seawater: Seawater density (kg/m^3).
        cd: Drag coefficient.
        cm: Inertia coefficient.
        cl: Explicit instantaneous lift coefficient; no peak-table lookup.

    Returns:
        HydrodynamicLoadResult with forces per unit length (N/m).
    """
    u = water_velocity_m_s
    a = water_acceleration_m_s2
    d = od_total_m

    drag = _horizontal(rho_seawater, d, u, 0, cd, 0)
    inertia = _horizontal(rho_seawater, d, 0, a, 0, cm)
    lift = _lift(rho_seawater, d, u, cl)

    return HydrodynamicLoadResult(
        drag_n_per_m=drag,
        inertia_n_per_m=inertia,
        total_horizontal_n_per_m=drag + inertia,
        lift_n_per_m=lift,
    )


def drag_force_per_meter(
    current_velocity_ms: float,
    pipe_od_m: float,
    water_density_kg_m3: float,
    drag_coeff: float,
) -> float:
    """Drag force per unit length: ``F_D = 0.5 * rho * C_D * D * U^2``.

    Args:
        current_velocity_ms: Flow velocity (m/s).
        pipe_od_m: Total outer diameter (m).
        water_density_kg_m3: Water density (kg/m^3).
        drag_coeff: Drag coefficient C_D.

    Returns:
        Drag force per metre (N/m).
    """
    return abs(_horizontal(water_density_kg_m3, pipe_od_m,
                           current_velocity_ms, 0, drag_coeff, 0))


def lift_force_per_meter(
    current_velocity_ms: float,
    pipe_od_m: float,
    water_density_kg_m3: float,
    lift_coeff: float,
) -> float:
    """Lift force per unit length: ``F_L = 0.5 * rho * C_L * D * U^2``.

    Note:
        This is the simplified-surface signature with positional order
        ``(velocity, D, rho, C_L)``, matching the geotechnical shadow.
        The canonical :func:`digitalmodel.subsea.on_bottom_stability.dnv_rp_f109.lift_force_per_meter`
        uses ``(rho_w_kg_m3, D_outer_m, U_m_s, C_L)`` — both are exposed
        but the package ``__init__`` re-exports THIS one to satisfy
        retargeted test imports.

    Args:
        current_velocity_ms: Flow velocity (m/s).
        pipe_od_m: Total outer diameter (m).
        water_density_kg_m3: Water density (kg/m^3).
        lift_coeff: Lift coefficient C_L.

    Returns:
        Lift force per metre (N/m).
    """
    return _lift(water_density_kg_m3, pipe_od_m, current_velocity_ms, lift_coeff)


def inertia_force_per_meter(
    acceleration_ms2: float,
    pipe_od_m: float,
    water_density_kg_m3: float,
    inertia_coeff: float,
) -> float:
    """Inertia force per unit length: ``F_I = rho * C_M * pi/4 * D^2 * a``.

    Args:
        acceleration_ms2: Water particle acceleration (m/s^2).
        pipe_od_m: Total outer diameter (m).
        water_density_kg_m3: Water density (kg/m^3).
        inertia_coeff: Inertia coefficient C_M.

    Returns:
        Inertia force per metre (N/m).
    """
    return _horizontal(water_density_kg_m3, pipe_od_m, 0,
                       acceleration_ms2, 0, inertia_coeff)


def lateral_stability_check(
    submerged_weight_n_per_m: float,
    horizontal_force_n_per_m: float,
    lift_force_n_per_m: float,
    friction_coefficient: float,
    safety_factor: float,
) -> StabilityResult:
    """Clause 3.6.2 Eqs. (3.38)-(3.39), friction-only peak checks in N/m.

    The clause 3.6.1 no-slip criterion requires nonnegative extreme-cycle
    peaks, never signed instantaneous samples. Select the explicit factor
    under clause 3.6.3 Tables 3-5 through 3-8. Returns the governing
    utilisation and required weight; raises for nonpositive supplied weight
    or the canonical check's invalid inputs. No displacement is calculated.
    """
    if submerged_weight_n_per_m <= 0.0:
        raise ValueError(
            "submerged_weight_n_per_m must be positive; "
            "pipe with non-positive submerged weight cannot be assessed"
        )

    canonical = absolute_stability_check(
        W_s_N_m=submerged_weight_n_per_m,
        F_H_N_m=horizontal_force_n_per_m,
        F_L_N_m=lift_force_n_per_m,
        mu_soil=friction_coefficient,
        gamma_SC=safety_factor,
    )

    margin = absolute_stability_margin(
        submerged_weight_n_per_m, horizontal_force_n_per_m,
        lift_force_n_per_m, friction_coefficient, safety_factor,
    )
    required_ws = margin.required_weight_N_m

    return StabilityResult(
        is_stable=canonical.is_stable,
        utilization=canonical.utilisation,
        required_weight_n_per_m=required_ws,
    )


def check_lateral_stability(
    submerged_weight_per_meter_kn: float,
    horizontal_load_per_meter_kn: float,
    lift_load_per_meter_kn: float,
    friction_coeff: float,
    safety_factor: float,
) -> StabilityResult:
    """Clause 3.6.2 Eqs. (3.38)-(3.39) with kN/m peak inputs.

    Convenience wrapper around :func:`lateral_stability_check` that
    accepts forces in kN/m instead of N/m.

    Args:
        submerged_weight_per_meter_kn: Submerged weight W_s (kN/m).
        horizontal_load_per_meter_kn: Total horizontal load (kN/m).
        lift_load_per_meter_kn: Lift load (kN/m).
        friction_coeff: Pipe-soil friction coefficient.
        safety_factor: Explicit clause 3.6.3 factor selected by the caller.

    Returns:
        StabilityResult with utilization ratio and stability flag.
        ``required_weight_n_per_m`` remains in N/m for consistency.

    Raises:
        ValueError: If submerged weight is zero or negative.
    """
    return lateral_stability_check(
        submerged_weight_n_per_m=submerged_weight_per_meter_kn * 1000.0,
        horizontal_force_n_per_m=horizontal_load_per_meter_kn * 1000.0,
        lift_force_n_per_m=lift_load_per_meter_kn * 1000.0,
        friction_coefficient=friction_coeff,
        safety_factor=safety_factor,
    )


def check_vertical_stability(
    submerged_weight_per_meter_kn: float,
    lift_load_per_meter_kn: float,
    safety_factor: float,
) -> VerticalStabilityResult:
    """Clause 3.6.2 Eq. (3.39): upward peak acceptance with kN/m inputs.

    The explicit factor follows clause 3.6.3 Tables 3-5 through 3-8.
    Weight must be positive; lift is a nonnegative peak. This is distinct
    from the flotation check under clause 3.2 Eq. (3.1).
    """
    if submerged_weight_per_meter_kn <= 0:
        raise ValueError("submerged_weight_per_meter_kn must be positive")
    canonical = absolute_stability_check(
        submerged_weight_per_meter_kn * 1000, 0,
        lift_load_per_meter_kn * 1000, 1.0, safety_factor,
    )
    utilization = canonical.details["vertical_utilisation"]
    return VerticalStabilityResult(is_stable=utilization <= 1.0, utilization=utilization)
