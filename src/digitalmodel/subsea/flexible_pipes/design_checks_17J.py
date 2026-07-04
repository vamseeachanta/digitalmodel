"""API 17J / ISO 13628-2 unbonded flexible pipe design-utilisation checks.

API 17J (ISO 13628-2) specifies a working-stress / utilisation framework for
unbonded flexible pipe layers.  This module encodes the *deterministic*
load-vs-capacity utilisation checks that govern the structural layers of a
dynamic riser, expressed as utilisations ``U = demand / capacity`` that must
stay ``<= 1.0``:

1. **Tensile-armour utilisation** — the effective tension carried by the
   tensile-armour wires generates a wire tensile stress that is checked
   against a factored utilisation of the wire yield strength.

2. **Minimum-bend-radius (MBR) utilisation** — the applied bend radius must
   stay above the storage / operating MBR (curvature utilisation).

3. **Combined tension + curvature utilisation** — API 17J Table-style
   interaction: the tensile-armour wires see both the axial tension stress
   and a bending stress from layer curvature; the combined utilisation is
   checked against the factored yield.

Equations
---------
Mean tensile-armour stress from effective tension ``T`` carried at lay angle
``alpha`` over total armour metallic area ``A_a`` (the axial tension is shared
by the two counter-wound armour packages, each wire inclined at ``alpha`` to
the axis so the wire axial force is ``T / cos(alpha)`` smeared over the area):

    sigma_T = T / (A_a * cos(alpha))                               (1)

Allowable (factored) wire stress for utilisation grouping ``eta``:

    sigma_allow = eta * SMYS                                       (2)

API 17J utilisation factors (Table 6 / Table 7 envelope, indicative):
    normal operation        eta = 0.67
    abnormal / extreme       eta = 0.85
    survival / accidental    eta = 0.91

Tensile-armour utilisation:

    U_tension = sigma_T / sigma_allow                              (3)

Curvature / MBR utilisation against the qualified operating MBR:

    U_curv = MBR_required / R_applied                              (4)

(``U_curv <= 1`` requires the applied radius to exceed the required MBR.)

Bending stress in a tensile-armour wire as the pipe bends to curvature
``kappa = 1/R``.  The wire follows the pipe curvature and bends about its own
neutral axis, so the outer fibre of a flat wire of thickness ``t_w`` sees a
strain ``epsilon_b = (t_w / 2) / R`` (the wire half-thickness is the lever,
NOT the armour radius — using the armour radius would grossly overstate the
local wire stress):

    sigma_b = E * (t_w / 2) / R                                    (5)

Combined tension + bending utilisation (linear superposition, API 17J wire
stress check):

    U_combined = (sigma_T + sigma_b) / sigma_allow                 (6)

A layer / load-case PASSES when every utilisation is ``<= 1.0``.

Assumptions / notes
-------------------
* The utilisation factors in (2) are *indicative* API 17J groupings; the
  caller should override ``utilisation_factor`` per the project-specific
  design basis and load case.
* Tension sharing (1) assumes both armour packages are intact and share the
  axial load equally through the cross-section — the standard
  small-displacement assumption used for screening.
* MBR (4) is a qualified test value supplied by the manufacturer; this module
  checks the applied radius against it, it does not derive MBR from first
  principles.

References
----------
* API 17J / ISO 13628-2, Specification for Unbonded Flexible Pipe.
* API 17B, Recommended Practice for Flexible Pipe.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from enum import Enum
from typing import Optional


class LoadCase(str, Enum):
    """API 17J utilisation groupings (indicative envelope)."""

    NORMAL = "normal"          # eta = 0.67
    EXTREME = "extreme"        # eta = 0.85
    SURVIVAL = "survival"      # eta = 0.91


UTILISATION_FACTORS = {
    LoadCase.NORMAL: 0.67,
    LoadCase.EXTREME: 0.85,
    LoadCase.SURVIVAL: 0.91,
}


@dataclass(frozen=True)
class TensileArmour:
    """Tensile-armour package property bundle (SI units)."""

    metallic_area: float       # total net metallic area, both packages [m^2]
    lay_angle_deg: float       # wire lay angle from pipe axis [deg]
    smys: float                # wire specified minimum yield strength [Pa]
    mean_radius: float         # armour mean radius [m]
    wire_thickness: float = 0.0  # flat-wire radial thickness t_w [m] (eq. 5)
    youngs_modulus: float = 207e9

    def __post_init__(self) -> None:
        if self.metallic_area <= 0:
            raise ValueError("metallic_area must be positive")
        if not 0.0 < self.lay_angle_deg < 90.0:
            raise ValueError("lay_angle_deg must be in (0, 90)")
        if self.smys <= 0:
            raise ValueError("smys must be positive")
        if self.mean_radius <= 0:
            raise ValueError("mean_radius must be positive")
        if self.wire_thickness < 0:
            raise ValueError("wire_thickness must be non-negative")


@dataclass(frozen=True)
class FlexCheckResult:
    """Result of an API 17J utilisation check."""

    armour_stress: float          # sigma_T [Pa] (eq. 1)
    bending_stress: float         # sigma_b [Pa] (eq. 5), 0 if no curvature
    allowable_stress: float       # sigma_allow [Pa] (eq. 2)
    u_tension: float              # eq. 3
    u_curvature: float            # eq. 4 (inf if no MBR supplied)
    u_combined: float             # eq. 6
    governing: str                # name of the highest utilisation
    passes: bool

    @property
    def max_utilisation(self) -> float:
        return max(self.u_tension, self.u_curvature, self.u_combined)


def allowable_wire_stress(
    smys: float,
    load_case: LoadCase = LoadCase.NORMAL,
    utilisation_factor: Optional[float] = None,
) -> float:
    """Factored allowable tensile-armour wire stress [Pa] (eq. 2)."""
    eta = utilisation_factor if utilisation_factor is not None else UTILISATION_FACTORS[load_case]
    if not 0.0 < eta <= 1.0:
        raise ValueError("utilisation_factor must be in (0, 1]")
    return eta * smys


def armour_tension_stress(armour: TensileArmour, effective_tension: float) -> float:
    """Mean tensile-armour stress from effective tension [Pa] (eq. 1)."""
    if effective_tension < 0:
        raise ValueError("effective_tension must be non-negative")
    alpha = math.radians(armour.lay_angle_deg)
    return effective_tension / (armour.metallic_area * math.cos(alpha))


def armour_bending_stress(armour: TensileArmour, applied_radius: float) -> float:
    """Tensile-armour wire bending stress at applied radius [Pa] (eq. 5).

    Returns 0 when no ``wire_thickness`` was supplied (bending lever unknown).
    """
    if applied_radius <= 0:
        raise ValueError("applied_radius must be positive")
    return armour.youngs_modulus * (armour.wire_thickness / 2.0) / applied_radius


def verify_flexible_pipe(
    armour: TensileArmour,
    effective_tension: float,
    applied_radius: Optional[float] = None,
    minimum_bend_radius: Optional[float] = None,
    load_case: LoadCase = LoadCase.NORMAL,
    utilisation_factor: Optional[float] = None,
) -> FlexCheckResult:
    """Run the API 17J tensile-armour utilisation checks for one load case.

    Parameters
    ----------
    armour : TensileArmour
        Tensile-armour property bundle.
    effective_tension : float
        Effective (top/section) tension carried by the armour [N].
    applied_radius : float, optional
        Applied bend radius at the section [m].  If given together with
        ``minimum_bend_radius`` the curvature utilisation (eq. 4) is
        evaluated; it is also used for the combined wire-stress check.
    minimum_bend_radius : float, optional
        Qualified operating MBR [m] (manufacturer value).
    load_case : LoadCase
        Selects the indicative utilisation factor (eq. 2).
    utilisation_factor : float, optional
        Explicit override for ``eta`` (takes precedence over ``load_case``).

    Returns
    -------
    FlexCheckResult
        Utilisations and pass/fail.  Passes when every utilisation <= 1.0.
    """
    sigma_allow = allowable_wire_stress(armour.smys, load_case, utilisation_factor)

    sigma_t = armour_tension_stress(armour, effective_tension)
    u_tension = sigma_t / sigma_allow

    if applied_radius is not None and minimum_bend_radius is not None:
        u_curv = minimum_bend_radius / applied_radius
    else:
        u_curv = 0.0

    if applied_radius is not None:
        sigma_b = armour_bending_stress(armour, applied_radius)
    else:
        sigma_b = 0.0
    u_combined = (sigma_t + sigma_b) / sigma_allow

    utilisations = {
        "tension": u_tension,
        "curvature": u_curv,
        "combined": u_combined,
    }
    governing = max(utilisations, key=utilisations.get)
    passes = all(u <= 1.0 for u in utilisations.values())

    return FlexCheckResult(
        armour_stress=sigma_t,
        bending_stress=sigma_b,
        allowable_stress=sigma_allow,
        u_tension=u_tension,
        u_curvature=u_curv,
        u_combined=u_combined,
        governing=governing,
        passes=passes,
    )
