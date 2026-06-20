"""Subsea connector load / capacity verification (API 17R / ISO 13628-1).

This module encodes the core mechanical checks for a subsea jumper
*connector* (the make-up coupling between a jumper and a tree / manifold /
PLET hub).  Three governing checks are implemented:

1. **End-cap / pressure-thrust load** — internal bore pressure acting on
   the sealed bore generates an axial separating force that the connector
   make-up preload and structural section must resist.

2. **Combined axial + bending capacity (von Mises)** — the connector body
   is checked as a pressure-containing tubular under simultaneous effective
   axial load and bending moment, against a factored material allowable.

3. **Seal integrity / preload margin** — the metal-to-metal seal stays
   energised only while the residual contact (clamp/bolt preload minus the
   pressure end-cap thrust and applied axial tension) keeps a positive
   margin over the required seating load.

Equations
---------
End-cap (pressure-thrust) separating force on a sealed bore of internal
diameter ``D_i`` at internal pressure ``p`` (API 17D / 17R hub thrust):

    F_endcap = p * A_seal = p * (pi/4) * D_s^2                       (1)

where ``D_s`` is the seal mean diameter (defaults to the bore D_i when not
specified).  The total separating (tensile) demand on the connector is

    F_sep = F_endcap + F_axial_applied                              (2)

Combined-stress utilisation of the connector tubular body uses the von
Mises equivalent of axial, bending and hoop stress against the factored
allowable ``f_d = eta * SMYS`` (ISO 13628-1 / API 17R working-stress
design, ``eta`` the usage factor):

    sigma_a = F_eff / A                                             (3a)
    sigma_b = M / Z                                                 (3b)
    sigma_h = p * D_i / (2 t)        (thin-wall hoop)               (3c)
    sigma_vm = sqrt((sigma_a+sigma_b)^2 - (sigma_a+sigma_b) sigma_h
                    + sigma_h^2)                                    (4)
    U_body  = sigma_vm / f_d                                        (5)

Seal residual-contact margin (preload check):

    F_residual = F_preload - F_sep                                  (6)
    U_seal     = F_seat_required / F_residual   (or inf if <= 0)    (7)

A connector PASSES when every utilisation is <= 1.0.  ``governing_load`` is
the check with the highest utilisation.

Notes / assumptions
-------------------
* Thin-wall hoop stress (3c) is used; for D/t < ~20 a thick-wall (Lame)
  expression would be marginally conservative — flagged for the caller.
* Default usage factor ``eta = 0.67`` corresponds to ISO 13628-1 / API 17R
  normal-operating working-stress design (2/3 SMYS).  Override per load case
  (e.g. 0.80 extreme, 0.90 survival).
* When ``seal_mean_diameter`` is not given it defaults to the pipe bore
  ``D_i`` (conservative: full-bore thrust acting on the seal).
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from enum import Enum
from typing import Optional

from .section_properties import PipeSection


class ConnectorType(str, Enum):
    """API 17R connector make-up families."""

    MECHANICAL = "mechanical"   # collet / dog actuated
    CLAMPED = "clamped"         # clamp hub (e.g. AX/VX gasket)
    WELDED = "welded"           # field weld (no make-up preload)


@dataclass(frozen=True)
class ConnectorMaterial:
    """Connector body material (SI: Pa)."""

    name: str
    smys: float                 # specified minimum yield strength [Pa]
    youngs_modulus: float = 207e9


@dataclass
class ConnectorCheckResult:
    """Result of a connector load/capacity verification."""

    utilisation_body: float
    utilisation_seal: float
    f_endcap: float
    f_separation: float
    f_residual: float
    sigma_vm: float
    allowable_stress: float
    governing_load: str
    passed: bool
    notes: list = field(default_factory=list)

    @property
    def utilisation(self) -> float:
        """Maximum (governing) utilisation across all checks."""
        return max(self.utilisation_body, self.utilisation_seal)


def end_cap_force(internal_pressure: float, seal_mean_diameter: float) -> float:
    """Pressure-thrust separating force, Eq. (1).

    Parameters
    ----------
    internal_pressure : float
        Bore pressure p [Pa].
    seal_mean_diameter : float
        Seal mean (or bore) diameter D_s [m].

    Returns
    -------
    float
        Axial separating force F_endcap [N].
    """
    return internal_pressure * math.pi / 4.0 * seal_mean_diameter**2


def verify_connector(
    section: PipeSection,
    material: ConnectorMaterial,
    internal_pressure: float,
    applied_axial: float = 0.0,
    applied_moment: float = 0.0,
    preload: float = 0.0,
    seal_seating_force: float = 0.0,
    seal_mean_diameter: Optional[float] = None,
    usage_factor: float = 0.67,
    connector_type: ConnectorType = ConnectorType.CLAMPED,
) -> ConnectorCheckResult:
    """Verify a subsea jumper connector against API 17R / ISO 13628-1.

    Parameters
    ----------
    section : PipeSection
        Connector body / hub neck cross section.
    material : ConnectorMaterial
        Body material (SMYS in Pa).
    internal_pressure : float
        Bore design pressure p [Pa].
    applied_axial : float
        Externally applied axial load F_axial [N], tension positive.
    applied_moment : float
        Applied bending moment M [N.m].
    preload : float
        Make-up preload F_preload [N] (clamp/bolt or collet seating).  For a
        ``WELDED`` connector this is ignored (seal check skipped).
    seal_seating_force : float
        Minimum residual contact force to keep the seal energised [N].
    seal_mean_diameter : float, optional
        Seal mean diameter D_s [m]; defaults to the pipe bore D_i.
    usage_factor : float
        eta in f_d = eta * SMYS (default 0.67, ISO 13628-1 normal operation).
    connector_type : ConnectorType
        Make-up family; WELDED skips the preload/seal check.

    Returns
    -------
    ConnectorCheckResult
    """
    notes: list = []
    d_s = seal_mean_diameter if seal_mean_diameter is not None else section.inner_diameter

    # --- (1)-(2) end-cap thrust and total separating demand ---
    f_endcap = end_cap_force(internal_pressure, d_s)
    f_separation = f_endcap + max(applied_axial, 0.0)

    # --- (3)-(5) connector body combined von-Mises utilisation ---
    # Effective axial load carried by the body = end-cap thrust + applied axial.
    f_eff = f_endcap + applied_axial
    sigma_a = f_eff / section.area
    sigma_b = applied_moment / section.section_modulus
    sigma_h = internal_pressure * section.inner_diameter / (2.0 * section.wall_thickness)
    sigma_axial_total = sigma_a + sigma_b
    sigma_vm = math.sqrt(
        sigma_axial_total**2 - sigma_axial_total * sigma_h + sigma_h**2
    )
    allowable = usage_factor * material.smys
    u_body = sigma_vm / allowable

    d_over_t = section.outer_diameter / section.wall_thickness
    if d_over_t < 20.0:
        notes.append(
            f"D/t = {d_over_t:.1f} < 20: thin-wall hoop (Eq 3c) is approximate; "
            "consider thick-wall (Lame) for a refined check."
        )

    # --- (6)-(7) seal residual-contact / preload margin ---
    if connector_type is ConnectorType.WELDED:
        f_residual = float("nan")
        u_seal = 0.0
        notes.append("Welded connector: no make-up preload; seal check skipped.")
    else:
        f_residual = preload - f_separation
        if f_residual <= 0.0:
            u_seal = float("inf")
            notes.append("Preload exhausted by separating load: seal de-energised.")
        elif seal_seating_force <= 0.0:
            u_seal = 0.0
        else:
            u_seal = seal_seating_force / f_residual

    # --- governing load + pass/fail ---
    governing = "connector_body" if u_body >= u_seal else "seal_preload"
    passed = (u_body <= 1.0) and (u_seal <= 1.0)

    return ConnectorCheckResult(
        utilisation_body=u_body,
        utilisation_seal=u_seal,
        f_endcap=f_endcap,
        f_separation=f_separation,
        f_residual=f_residual,
        sigma_vm=sigma_vm,
        allowable_stress=allowable,
        governing_load=governing,
        passed=passed,
        notes=notes,
    )
