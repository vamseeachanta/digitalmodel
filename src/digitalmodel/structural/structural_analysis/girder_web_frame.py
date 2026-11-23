# ABOUTME: Girder & web-frame buckling — web-panel shear buckling (k_tau) for
# ABOUTME: transverse frames and girder tripping with bracket-spacing effect.
"""Primary supporting member buckling — girders and web frames (DNV-RP-C201).

Two checks for primary members, both built on the validated plate / stiffened-
panel solvers (no buckling physics reimplemented):

* **Web-panel shear buckling** (transverse web frames under shear): the web
  panel between stiffeners/brackets buckles in shear at
  ``tau_cr = JO(k_tau * pi^2 E / (12(1-nu^2)) * (t_w/d)^2)`` with the classical
  shear coefficient ``k_tau`` (a/d branches) and the **same** Johnson-Ostenfeld
  inelastic knockdown as the plate solver
  (:meth:`...buckling.PlateBucklingAnalyzer.johnson_ostenfeld`).

* **Girder tripping** (lateral-torsional buckling of a deep girder): reuses the
  validated DNV-RP-C201 Sec. 7.5.2 tripping solver
  (:meth:`...panel_buckling.StiffenedPanelBucklingAnalyzer.torsional_buckling`)
  with the **tripping-bracket spacing** as the torsional restraint length
  ``L_T`` — closer brackets raise the tripping capacity.

Units: mm (lengths), MPa (stresses), matching the structural-analysis package.

References: DNV-RP-C201 (Buckling Strength of Plated Structures) Sec. 7;
DNV CN 30.1 (buckling strength of bars and frames).
"""
from __future__ import annotations

import math
from dataclasses import dataclass, replace
from typing import Optional

from .buckling import PlateBucklingAnalyzer
from .models import MaterialProperties
from .panel_buckling import (
    StiffenedPanelBucklingAnalyzer,
    StiffenedPanelGeometry,
)

CODE_REFERENCE = "DNV-RP-C201 / DNV CN 30.1"


# ---------------------------------------------------------------------------
# Web-panel shear buckling (transverse web frames)
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class WebPanel:
    """A web panel of a girder / transverse frame (mm)."""

    depth_mm: float          # web depth d (short transverse dimension)
    thickness_mm: float      # web thickness t_w
    panel_length_mm: float   # a, between stiffeners / tripping brackets

    def __post_init__(self) -> None:
        if min(self.depth_mm, self.thickness_mm, self.panel_length_mm) <= 0:
            raise ValueError("web panel dimensions must be > 0")


def web_shear_buckling_coefficient(depth_mm: float, length_mm: float) -> float:
    """Classical plate shear-buckling coefficient ``k_tau``.

    ``k_tau = 5.34 + 4*(d/a)^2`` for ``a/d >= 1`` (long panel),
    ``k_tau = 4.0 + 5.34*(d/a)^2`` for ``a/d < 1`` (short panel),
    with ``d`` the shorter (depth) and ``a`` the panel length.
    """
    d, a = depth_mm, length_mm
    if a / d >= 1.0:
        return 5.34 + 4.0 * (d / a) ** 2
    return 4.0 + 5.34 * (d / a) ** 2


def web_shear_buckling_stress(web: WebPanel,
                              material: MaterialProperties) -> float:
    """Critical shear-buckling stress of a web panel (MPa).

    ``tau_e = k_tau * pi^2 E / (12(1-nu^2)) * (t_w/d)^2``; the inelastic value is
    the plate solver's Johnson-Ostenfeld knockdown of ``tau_e``.
    """
    e = material.youngs_modulus
    nu = material.poissons_ratio
    k_tau = web_shear_buckling_coefficient(web.depth_mm, web.panel_length_mm)
    tau_e = (k_tau * math.pi ** 2 * e / (12.0 * (1.0 - nu ** 2))
             * (web.thickness_mm / web.depth_mm) ** 2)
    # Reuse the validated inelastic (Johnson-Ostenfeld) correction.
    return PlateBucklingAnalyzer(material).johnson_ostenfeld(tau_e)


@dataclass(frozen=True)
class WebShearResult:
    """Web-panel shear-buckling check result."""

    critical_shear_stress_mpa: float
    applied_shear_stress_mpa: float
    k_tau: float
    utilization: float
    passes: bool
    code_reference: str = CODE_REFERENCE


def check_web_frame_shear(
    web: WebPanel, material: MaterialProperties, applied_shear_mpa: float,
    *, gamma_m: float = 1.15,
) -> WebShearResult:
    """Check a transverse-frame web panel against shear buckling."""
    tau_cr = web_shear_buckling_stress(web, material)
    design_cap = tau_cr / gamma_m
    util = applied_shear_mpa / design_cap if design_cap > 0 else float("inf")
    return WebShearResult(
        critical_shear_stress_mpa=tau_cr,
        applied_shear_stress_mpa=applied_shear_mpa,
        k_tau=web_shear_buckling_coefficient(web.depth_mm, web.panel_length_mm),
        utilization=util,
        passes=bool(applied_shear_mpa <= design_cap),
    )


# ---------------------------------------------------------------------------
# Girder tripping — bracket-spacing effect (reuses the validated tripping solver)
# ---------------------------------------------------------------------------
def girder_tripping_capacity(
    girder: StiffenedPanelGeometry, material: MaterialProperties,
    sigma_x: float, *, bracket_spacing_mm: Optional[float] = None,
) -> float:
    """Characteristic tripping (lateral-torsional) capacity ``fT`` (MPa).

    When ``bracket_spacing_mm`` is given it sets the torsional restraint length
    ``L_T`` — closer tripping brackets raise ``fT``.
    """
    if bracket_spacing_mm is not None:
        girder = replace(girder, torsional_restraint_spacing=bracket_spacing_mm)
    analyzer = StiffenedPanelBucklingAnalyzer(material)
    return analyzer.torsional_buckling(girder, sigma_x)["fT"]


@dataclass(frozen=True)
class GirderTrippingResult:
    """Girder tripping check result at a given bracket spacing."""

    tripping_capacity_mpa: float
    applied_stress_mpa: float
    bracket_spacing_mm: float
    utilization: float
    passes: bool
    code_reference: str = CODE_REFERENCE


def check_girder_tripping(
    girder: StiffenedPanelGeometry, material: MaterialProperties,
    sigma_x: float, *, bracket_spacing_mm: Optional[float] = None,
    gamma_m: float = 1.15,
) -> GirderTrippingResult:
    """Check girder tripping at a bracket spacing (DNV-RP-C201 Sec. 7.5.2)."""
    l_t = (bracket_spacing_mm if bracket_spacing_mm is not None
           else (girder.torsional_restraint_spacing or girder.plate_length))
    f_t = girder_tripping_capacity(girder, material, sigma_x,
                                   bracket_spacing_mm=bracket_spacing_mm)
    design_cap = f_t / gamma_m
    util = sigma_x / design_cap if design_cap > 0 else float("inf")
    return GirderTrippingResult(
        tripping_capacity_mpa=f_t,
        applied_stress_mpa=sigma_x,
        bracket_spacing_mm=l_t,
        utilization=util,
        passes=bool(sigma_x <= design_cap),
    )
