"""Capping-stack deployment lift feasibility (API 17W, DNV-ST-N001 lifting).

A capping stack (API 17W) is deployed by lowering it from an intervention
vessel onto the flowing wellhead.  This module encodes the *deterministic*
lift-feasibility screen used in emergency-response planning: can the
nominated vessel crane lower the stack to the seabed within its rated
safe-working-load (SWL), accounting for submerged weight and a dynamic
amplification factor, and is the sea state within the deployment limit?

The full lower-line dynamics under current (VIV, slack-line snap, ROV
heavy-lift) are a deferred follow-on; this is the static + DAF envelope.

Equations
---------
Submerged (in-water) weight of the stack, buoyancy from its displaced
volume (DNV-ST-N001 / Archimedes):

    W_sub = (m_air - rho_w * V_disp) * g                            (1)

Dynamic hook load applied through the crane during the splash-zone /
lowering phase, with dynamic amplification factor DAF:

    F_hook = DAF * W_sub                                            (2)

Lift utilisation against the crane safe working load:

    U_lift = F_hook / SWL                                           (3)

Deployment is feasible when the lift utilisation is within unity AND the
significant wave height is at or below the deployment sea-state limit:

    feasible = (U_lift <= 1.0) and (Hs <= Hs_limit)                 (4)

Notes / assumptions
-------------------
* DAF defaults to 2.0, a representative splash-zone value (DNV-ST-N001
  uses a hydrodynamic + dynamic factor that is sea-state dependent; a
  full DAF derivation is a follow-on).  Override per project.
* Displaced volume defaults to the dry envelope volume from ``mass_air``
  and a steel-structure bulk density of 4000 kg/m^3 (open-frame stack,
  ~half solid steel) when ``displaced_volume`` is not supplied.
"""

from __future__ import annotations

from dataclasses import dataclass, field

G = 9.80665
SEAWATER_DENSITY = 1025.0
STACK_BULK_DENSITY = 4000.0   # open-frame steel structure effective bulk density


@dataclass(frozen=True)
class CappingStack:
    """Capping-stack mass / volume properties for a lift check."""

    mass_air: float                     # dry weight in air [kg]
    displaced_volume: float | None = None  # submerged displaced volume [m^3]


@dataclass(frozen=True)
class DeploymentScenario:
    """Vessel / environment inputs for a deployment lift feasibility screen."""

    crane_swl: float                    # crane safe working load [N]
    water_depth: float                  # [m]
    hs: float                           # significant wave height [m]
    hs_limit: float                     # deployment sea-state limit [m]
    daf: float = 2.0                    # dynamic amplification factor [-]


@dataclass
class DeploymentResult:
    """Result of a capping-stack deployment lift feasibility screen."""

    feasible: bool
    submerged_weight: float             # W_sub [N]
    hook_load: float                    # F_hook [N]
    lift_utilisation: float             # F_hook / SWL [-]
    governing_constraint: str           # "lift" | "sea_state" | "none"
    notes: list = field(default_factory=list)


def submerged_weight(
    mass_air: float,
    displaced_volume: float,
    seawater_density: float = SEAWATER_DENSITY,
) -> float:
    """In-water (submerged) weight, Eq. (1) [N]."""
    return (mass_air - seawater_density * displaced_volume) * G


def feasibility(
    stack: CappingStack,
    scenario: DeploymentScenario,
    seawater_density: float = SEAWATER_DENSITY,
) -> DeploymentResult:
    """Screen capping-stack deployment lift feasibility (API 17W / DNV-ST-N001).

    Parameters
    ----------
    stack : CappingStack
        Stack mass / displaced volume.
    scenario : DeploymentScenario
        Crane SWL, water depth, sea state and DAF.
    seawater_density : float
        Seawater density [kg/m^3].

    Returns
    -------
    DeploymentResult
        Feasibility verdict and the governing constraint.
    """
    notes: list = []
    v_disp = stack.displaced_volume
    if v_disp is None:
        v_disp = stack.mass_air / STACK_BULK_DENSITY
        notes.append(
            f"displaced_volume defaulted from bulk density "
            f"{STACK_BULK_DENSITY:.0f} kg/m^3 -> {v_disp:.3f} m^3"
        )

    w_sub = submerged_weight(stack.mass_air, v_disp, seawater_density)
    f_hook = scenario.daf * w_sub
    if scenario.crane_swl <= 0.0:
        raise ValueError("crane_swl must be positive")
    u_lift = f_hook / scenario.crane_swl

    lift_ok = u_lift <= 1.0
    sea_ok = scenario.hs <= scenario.hs_limit

    if not lift_ok:
        governing = "lift"
    elif not sea_ok:
        governing = "sea_state"
    else:
        governing = "none"

    return DeploymentResult(
        feasible=lift_ok and sea_ok,
        submerged_weight=w_sub,
        hook_load=f_hook,
        lift_utilisation=u_lift,
        governing_constraint=governing,
        notes=notes,
    )
