# ABOUTME: Production tubing design checks — shut-in burst, evacuated collapse,
# ABOUTME: buoyed weight + packer piston tension, composing casing_design.
"""Production tubing design-check layer, composing the casing primitives.

The tubing counterpart of
:mod:`~digitalmodel.well.tubulars.casing_design`: the same
:class:`~digitalmodel.well.tubulars.casing_design.PressureProfile` /
:class:`~digitalmodel.well.tubulars.casing_design.DesignFactors` /
``check_*`` machinery, applied to the production tubing load cases from the
source deck:

* **Burst** — shut-in tubing: SITP at surface plus the produced-gas gradient
  down the string (internal), backed up by the packer-fluid column in the
  tubing-casing annulus (external; no credit taken for annulus surface
  pressure — conservative).
* **Collapse** — evacuated tubing (zero pressure inside, e.g. blown down or
  gas-lifted dry) against the packer-fluid annulus, optionally with a
  surface casing pressure applied on top of the annulus (conservative).
* **Tension** — buoyed string weight in the packer fluid plus the shut-in
  piston force the pressures exert on the tubing at the packer bore
  (``F = (Ap - Ai)*P_tbg - (Ap - Ao)*P_ann``); compression relief from a
  negative piston force is not credited.

``check_production_tubing`` orchestrates the three modes and returns
``dict[str, ModeCheckResult]`` exactly like
:func:`~digitalmodel.well.tubulars.casing_design.check_production_casing`.

Source: B. Hansen (Devon Energy), *Production Casing Design Considerations*,
EPA Hydraulic Fracturing Technical Workshop — Session 2: Well Design, 2011
(the deck covers casing *and* tubing design).  Rating formulas per API 5C3 /
API TR 5C3; piston force per the classic tubing-packer force analysis
(Lubinski et al.).

Worked golden example preserved in the tests: 2-7/8" 6.5# N80 — burst
``0.875 * 2 * 80,000 * 0.217 / 2.875 = 10,567 -> 10,570 psi`` (API rounding),
body yield ``80,000 * 1.812 in^2 = 144,962 -> 145,000 lbf``, collapse
``11,160 psi``.
"""
from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Optional

from digitalmodel.well.tubulars.casing import Casing
from digitalmodel.well.tubulars.casing_design import (
    DEFAULT_GAS_GRADIENT_PSI_FT,
    DesignFactors,
    ModeCheckResult,
    PressureProfile,
    check_burst,
    check_collapse,
    check_tension,
    fluid_column_profile,
    shut_in_tubing_pressure,
)

SOURCE_REFERENCE = (
    "Hansen (Devon Energy), EPA Hydraulic Fracturing Technical Workshop, "
    "Session 2: Well Design, 2011; API 5C3"
)


# ---------------------------------------------------------------------------
# Load-case profile builders
# ---------------------------------------------------------------------------
def shut_in_tubing_profile(
    sitp_psi: float,
    td_ft: float,
    gas_gradient_psi_ft: float = DEFAULT_GAS_GRADIENT_PSI_FT,
) -> PressureProfile:
    """Internal shut-in tubing profile: surface SITP + gas gradient down.

    A shut-in producer holds the shut-in tubing pressure at surface with the
    produced-gas column below, so the internal pressure at depth ``z`` is
    ``SITP + G_gas * z``.  Use
    :func:`~digitalmodel.well.tubulars.casing_design.shut_in_tubing_pressure`
    to derive the SITP from a reservoir pressure.
    """
    if td_ft <= 0.0:
        raise ValueError("td_ft must be positive")
    return PressureProfile(
        (0.0, td_ft),
        (sitp_psi, sitp_psi + gas_gradient_psi_ft * td_ft),
        "shut-in tubing (SITP on gas gradient)",
    )


def packer_fluid_annulus_profile(
    packer_fluid_ppg: float,
    td_ft: float,
    surface_pressure_psi: float = 0.0,
) -> PressureProfile:
    """Packer-fluid column in the tubing-casing annulus.

    Optionally pressurized on top by an applied surface casing pressure
    (``surface_pressure_psi``) — used as the collapse load on the tubing.
    """
    p = fluid_column_profile(packer_fluid_ppg, td_ft,
                             surface_pressure_psi=surface_pressure_psi)
    return PressureProfile(p.depths_ft, p.pressures_psi,
                           "packer-fluid annulus")


# ---------------------------------------------------------------------------
# Packer piston force
# ---------------------------------------------------------------------------
def shut_in_piston_force_lbf(
    packer_bore_in: float,
    tubing_product: Casing,
    tubing_pressure_psi: float,
    annulus_pressure_psi: float = 0.0,
) -> float:
    """Shut-in piston force on the tubing at the packer bore, lbf.

    Classic tubing-packer piston (Lubinski F1) term — the pressure-area
    force at the packer, positive = added tension at surface::

        F = (Ap - Ai) * P_tbg - (Ap - Ao) * P_ann

    where ``Ap = pi/4 * d_bore^2`` is the packer seal-bore area, ``Ai`` /
    ``Ao`` the tubing ID / OD areas, and the pressures are evaluated at the
    packer depth.  With the seal bore equal to the tubing OD this reduces to
    the shut-in ``dP * A`` term on the tubing wall annulus,
    ``(Ao - Ai) * P_tbg``.
    """
    if packer_bore_in <= 0.0:
        raise ValueError("packer_bore_in must be positive")
    ap = math.pi / 4.0 * packer_bore_in ** 2
    ai = math.pi / 4.0 * tubing_product.id_in ** 2
    ao = math.pi / 4.0 * tubing_product.od_in ** 2
    return (ap - ai) * tubing_pressure_psi - (ap - ao) * annulus_pressure_psi


# ---------------------------------------------------------------------------
# Orchestrated design check
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class TubingWell:
    """The well-side inputs for a production tubing design check.

    ``td_ft`` is the tubing setting / packer depth (assumed at the reservoir
    for the gas-column translation).  ``packer_bore_in`` defaults to the
    tubing OD when not given, reducing the piston force to the shut-in
    ``dP * A`` term on the tubing wall.
    """

    td_ft: float
    packer_fluid_ppg: float
    reservoir_pressure_psi: float
    surface_casing_pressure_psi: float = 0.0
    packer_bore_in: Optional[float] = None
    gas_gradient_psi_ft: float = DEFAULT_GAS_GRADIENT_PSI_FT


def check_production_tubing(
    tubing_product: Casing,
    weight_ppf: float,
    well: TubingWell,
    factors: DesignFactors = DesignFactors(),
) -> dict[str, ModeCheckResult]:
    """Run the standard production-tubing design checks for one product.

    Returns the shut-in burst check (SITP + gas gradient inside vs the
    packer-fluid annulus outside; the SITP is also used for the low-SICP
    burst-DF relaxation), the evacuated-tubing collapse check (packer fluid
    plus any surface casing pressure outside, zero inside), and the tension
    check (buoyed weight in packer fluid plus the shut-in packer piston
    force; compression relief is not credited).
    """
    sitp = shut_in_tubing_pressure(well.reservoir_pressure_psi, well.td_ft,
                                   well.gas_gradient_psi_ft)
    internal = shut_in_tubing_profile(sitp, well.td_ft,
                                      well.gas_gradient_psi_ft)
    burst_backup = packer_fluid_annulus_profile(well.packer_fluid_ppg,
                                                well.td_ft)
    collapse_external = packer_fluid_annulus_profile(
        well.packer_fluid_ppg, well.td_ft,
        surface_pressure_psi=well.surface_casing_pressure_psi)

    bore = (well.packer_bore_in if well.packer_bore_in is not None
            else tubing_product.od_in)
    piston = shut_in_piston_force_lbf(
        bore, tubing_product,
        tubing_pressure_psi=float(internal.at(well.td_ft)),
        annulus_pressure_psi=float(collapse_external.at(well.td_ft)))

    return {
        "burst": check_burst(tubing_product, internal, burst_backup,
                             well.td_ft, factors, sicp_psi=sitp),
        "collapse": check_collapse(tubing_product, collapse_external,
                                   well.td_ft, factors=factors),
        "tension": check_tension(tubing_product, weight_ppf, well.td_ft,
                                 well.packer_fluid_ppg,
                                 extra_tension_lbf=max(piston, 0.0),
                                 factors=factors),
    }
