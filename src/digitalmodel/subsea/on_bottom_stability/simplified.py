# ABOUTME: DNV-RP-F109 "simplified" lateral/vertical stability surface with kN wrappers.
# ABOUTME: Routes to generalized_stability_check(F_R=0); ported from geotechnical/on_bottom_stability.py.
"""DNV-RP-F109 simplified-method surface (ported from geotechnical shadow).

This module exposes the kN-input, single-call surface that previously lived
at ``digitalmodel.geotechnical.on_bottom_stability`` (workspace-hub#2694
unification). All lateral-stability work routes to
:func:`digitalmodel.subsea.on_bottom_stability.dnv_rp_f109.generalized_stability_check`
with ``F_R_N_m=0`` so the two surfaces never disagree on the same scenario.

Symbols ported forward (8 public + 4 result dataclasses + 3 constants):

- :class:`SubmergedWeightResult`, :class:`HydrodynamicLoadResult`,
  :class:`StabilityResult`, :class:`VerticalStabilityResult` — dataclass
  result containers.
- :data:`STANDARD`, :data:`DEFAULT_CL`, :data:`G` — module constants
  (note: ``G = 9.80665`` here, matching the canonical
  :mod:`digitalmodel.subsea.on_bottom_stability.dnv_rp_f109`; the original
  shadow used ``9.81``, see :ref:`gravity-drift-note` below).
- :func:`submerged_weight` — pipe submerged weight, dataclass return.
- :func:`hydrodynamic_loads` — combined Morison drag + inertia + lift.
- :func:`drag_force_per_meter`, :func:`lift_force_per_meter`,
  :func:`inertia_force_per_meter` — individual components.
- :func:`lateral_stability_check` — N/m lateral stability check,
  raises ``ValueError`` on ``W_s <= 0``.
- :func:`check_lateral_stability` — kN/m wrapper around
  :func:`lateral_stability_check`.
- :func:`check_vertical_stability` — vertical (uplift) stability check
  with kN/m inputs.

.. _gravity-drift-note:

**Gravity constant.** This module uses ``g = 9.80665 m/s²`` (matching the
canonical :mod:`dnv_rp_f109` submodule). The original geotechnical shadow
used ``9.81 m/s²``, producing ~0.034 % drift on submerged weight. The
unification ports forward the higher-precision value; numerical
regressions against shadow-era outputs are expected at the 4th significant
figure on weight calculations only.

**Formula choice.** ``lateral_stability_check`` here routes to
DNV-RP-F109 §4.3.2 Eq 4.5 with passive-soil resistance ``F_R = 0`` —
that is, ``util = γ_SC * F_H / (μ * (W_s − F_L))``. This is the
"simplified" form (no soil-resistance credit, but lift-relief credit
retained). It is **NOT** the §4.3.1 Eq 4.1 "absolute" form
``util = γ_SC * (F_H + μ*F_L) / (μ * W_s)``. The investigation note
:file:`docs/field-development/on-bottom-stability-unification-investigation.md`
documents the verdict-flip hazard if these are confused. Use
:func:`~digitalmodel.subsea.on_bottom_stability.dnv_rp_f109.absolute_stability_check`
directly when the absolute method is required.
"""
from __future__ import annotations

import math
from dataclasses import dataclass

from digitalmodel.subsea.on_bottom_stability.dnv_rp_f109 import (
    GAMMA_SC_NORMAL,
    generalized_stability_check,
)


STANDARD: str = "DNV-RP-F109"
G: float = 9.80665  # m/s^2 (matches dnv_rp_f109.py; shadow used 9.81)
DEFAULT_CL: float = 0.9  # lift coefficient for pipe on seabed (smooth, Table 3-3)


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
    Uses ``g = 9.80665 m/s²`` (canonical value, see module note).

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
    ws = dry_weight - buoyancy

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
    cl: float = DEFAULT_CL,
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
        cl: Lift coefficient (default 0.9, smooth pipe per Table 3-3).

    Returns:
        HydrodynamicLoadResult with forces per unit length (N/m).
    """
    u = water_velocity_m_s
    a = water_acceleration_m_s2
    d = od_total_m

    drag = 0.5 * rho_seawater * cd * d * u * abs(u)
    inertia = rho_seawater * cm * (math.pi / 4.0) * d**2 * a
    lift = 0.5 * rho_seawater * cl * d * u**2

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
    return (
        0.5 * water_density_kg_m3 * drag_coeff
        * pipe_od_m * current_velocity_ms**2
    )


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
    return (
        0.5 * water_density_kg_m3 * lift_coeff
        * pipe_od_m * current_velocity_ms**2
    )


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
    return (
        water_density_kg_m3 * inertia_coeff
        * (math.pi / 4.0) * pipe_od_m**2 * acceleration_ms2
    )


def lateral_stability_check(
    submerged_weight_n_per_m: float,
    horizontal_force_n_per_m: float,
    lift_force_n_per_m: float,
    friction_coefficient: float,
    safety_factor: float = GAMMA_SC_NORMAL,
) -> StabilityResult:
    """Lateral stability check per DNV-RP-F109 §4.3.2 (simplified form, F_R=0).

    Routes to
    :func:`~digitalmodel.subsea.on_bottom_stability.dnv_rp_f109.generalized_stability_check`
    with ``F_R_N_m=0`` so utilisation matches the standard's Eq 4.5 form::

        utilization = γ_SC * F_H / (μ * (W_s − F_L))

    Pipe is stable when ``utilization <= 1.0``.

    This is the "simplified method" surface — caller-supplied F_R is
    zero, retaining lift-relief credit but not passive-soil credit.
    For the §4.3.1 Eq 4.1 "absolute" form (no lift-relief credit), call
    :func:`~digitalmodel.subsea.on_bottom_stability.dnv_rp_f109.absolute_stability_check`
    directly.

    Args:
        submerged_weight_n_per_m: Submerged weight W_s (N/m). Must be positive.
        horizontal_force_n_per_m: Total horizontal force F_H (N/m).
        lift_force_n_per_m: Lift force F_L (N/m).
        friction_coefficient: Pipe-soil friction coefficient μ.
        safety_factor: Safety class factor γ_SC (default 1.1 = normal class, Table 4-1).

    Returns:
        StabilityResult with utilization ratio, stability flag,
        required weight, and standard tag.

    Raises:
        ValueError: If submerged weight is zero or negative. (The canonical
            :func:`generalized_stability_check` returns ``inf`` in that case;
            the simplified surface raises to match the shadow's historical
            contract and surface a defensive ``ValueError`` to callers.)
    """
    if submerged_weight_n_per_m <= 0.0:
        raise ValueError(
            "submerged_weight_n_per_m must be positive; "
            "pipe with non-positive submerged weight cannot be assessed"
        )

    canonical = generalized_stability_check(
        W_s_N_m=submerged_weight_n_per_m,
        F_H_N_m=horizontal_force_n_per_m,
        F_L_N_m=lift_force_n_per_m,
        mu_soil=friction_coefficient,
        F_R_N_m=0.0,
        gamma_SC=safety_factor,
    )

    # Required weight to drive utilisation = 1.0:
    #   γ * F_H / (μ * (W_s - F_L)) = 1  →  W_s = γ * F_H / μ + F_L
    if friction_coefficient > 0.0:
        required_ws = (
            safety_factor * horizontal_force_n_per_m / friction_coefficient
            + lift_force_n_per_m
        )
    else:
        required_ws = float("inf")

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
    safety_factor: float = GAMMA_SC_NORMAL,
) -> StabilityResult:
    """Lateral stability check with kN/m inputs.

    Convenience wrapper around :func:`lateral_stability_check` that
    accepts forces in kN/m instead of N/m.

    Args:
        submerged_weight_per_meter_kn: Submerged weight W_s (kN/m).
        horizontal_load_per_meter_kn: Total horizontal load (kN/m).
        lift_load_per_meter_kn: Lift load (kN/m).
        friction_coeff: Pipe-soil friction coefficient.
        safety_factor: Safety class factor γ_SC (default 1.1).

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
    safety_factor: float = GAMMA_SC_NORMAL,
) -> VerticalStabilityResult:
    """Vertical stability check per DNV-RP-F109 (uplift).

    Pipe is vertically stable when ``W_s >= γ * F_L``.
    Utilisation = ``γ * F_L / W_s``; stable when ``≤ 1.0``.

    Args:
        submerged_weight_per_meter_kn: Submerged weight W_s (kN/m).
        lift_load_per_meter_kn: Lift load F_L (kN/m).
        safety_factor: Safety class factor γ_SC (default 1.1).

    Returns:
        VerticalStabilityResult with stability flag and utilisation.

    Raises:
        ValueError: If submerged weight is zero or negative.
    """
    if submerged_weight_per_meter_kn <= 0.0:
        raise ValueError(
            "submerged_weight_per_meter_kn must be positive"
        )

    utilization = safety_factor * lift_load_per_meter_kn / submerged_weight_per_meter_kn

    return VerticalStabilityResult(
        is_stable=utilization <= 1.0,
        utilization=utilization,
    )
