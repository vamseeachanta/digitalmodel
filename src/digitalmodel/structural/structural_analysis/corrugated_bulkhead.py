# ABOUTME: Corrugated-bulkhead strength/buckling — trapezoidal-corrugation section
# ABOUTME: properties + local/column/shear buckling (DNV-RP-C201 plate elements / IACS CSR).
"""Corrugated bulkhead strength and buckling (DNV-RP-C201 / IACS CSR).

A corrugated bulkhead is a folded plate forming a trapezoidal wave. The fold
geometry gives the panel a large bending stiffness in the corrugation direction
(it behaves as a deep beam) with failure modes that are distinct from a flat or
stiffened panel:

* **Section properties** of one trapezoidal corrugation are derived in closed
  form below (the same per-corrugation section modulus used by the IACS Common
  Structural Rules for corrugated bulkheads).
* **Local plate buckling** of the *flange* and *web* plate elements reuses the
  validated DNV-RP-C201 plate solver
  (:class:`...buckling.PlateBucklingAnalyzer`) — no plate physics reimplemented.
* **Overall column buckling** of the corrugation over the bulkhead span reuses
  the validated column solver (:class:`...buckling.ColumnBucklingAnalyzer`).
* **Web shear buckling** uses the classical shear coefficient ``k_tau`` with the
  same Johnson-Ostenfeld inelastic knockdown as the plate solver.

Geometry of the repeating *half-pitch* unit (one flange + one web), trapezoidal
corrugation with corrugation angle ``phi`` measured from the bulkhead plane::

        |<-- a -->|
        __________                       a  = flange width
       /          \\                      c  = web (slant) width
      / phi         \\                    d  = corrugation depth = c*sin(phi)
     /__web__________\\__________         s  = half pitch = a + c*cos(phi)
        |<- c*cos(phi) ->|                t  = plate thickness

The neutral axis of the repeating corrugation lies at mid-depth ``d/2`` (the
full-pitch centroid is ``d/2`` when the two flanges share the same thickness).

Closed-form per-half-pitch section properties (one flange + one web):

* Area              ``A = a*t_f + c*t_w``
* Moment of inertia ``I = a*t_f*(d/2)^2 + c*t_w*d^2/12``
* Section modulus    ``Z = I/(d/2) = d*(3*a*t_f + c*t_w)/6``

With equal thickness ``t`` these collapse to ``I = t*d^2*(3a+c)/12`` and
``Z = t*d*(3a+c)/6`` (the IACS CSR corrugated-bulkhead section modulus).

Units: mm (lengths), MPa (stresses), matching the structural-analysis package.

References: DNV-RP-C201 (Buckling Strength of Plated Structures);
IACS Common Structural Rules — corrugated bulkhead section modulus / strength.
"""
from __future__ import annotations

import math
from dataclasses import dataclass

from .buckling import ColumnBucklingAnalyzer, PlateBucklingAnalyzer
from .models import MaterialProperties, PlateGeometry

CODE_REFERENCE = "DNV-RP-C201 / IACS CSR"

# The per-corrugation section properties (Z, I) are rigorously validated closed
# forms (IACS CSR Pt 1 Ch 3). The buckling *screens* reuse the validated
# DNV-RP-C201 plate/column solvers, but no in-repo CSR worked example anchors the
# overall-buckling path, so the combined check is flagged PRELIMINARY.
PRELIMINARY = True


# ---------------------------------------------------------------------------
# Geometry + section properties
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class CorrugatedBulkheadGeometry:
    """One trapezoidal corrugation of a bulkhead (mm, degrees).

    The corrugation depth is *derived* from the web slant width and the
    corrugation angle (``depth = web_width * sin(angle)``) so the inputs cannot
    be mutually inconsistent.  ``web_thickness_mm`` defaults to the flange
    thickness (a constant-thickness corrugation).
    """

    flange_width_mm: float  # a — flat flange (face) breadth
    web_width_mm: float  # c — slant length of the inclined web
    thickness_mm: float  # t_f — flange (and default web) thickness
    corrugation_angle_deg: float  # phi — web inclination from bulkhead plane
    span_mm: float  # bulkhead span / height (column + plate len)
    web_thickness_mm: float = 0.0  # t_w — defaults to flange thickness if <= 0

    def __post_init__(self) -> None:
        if (
            min(
                self.flange_width_mm, self.web_width_mm, self.thickness_mm, self.span_mm
            )
            <= 0
        ):
            raise ValueError("corrugation dimensions must be > 0")
        if not 0.0 < self.corrugation_angle_deg < 90.0:
            raise ValueError("corrugation angle must be in (0, 90) degrees")

    # -- derived geometry ---------------------------------------------------
    @property
    def t_f(self) -> float:
        return self.thickness_mm

    @property
    def t_w(self) -> float:
        return self.web_thickness_mm if self.web_thickness_mm > 0 else self.thickness_mm

    @property
    def corrugation_depth_mm(self) -> float:
        """Perpendicular depth between the two flange planes, ``c*sin(phi)``."""
        return self.web_width_mm * math.sin(math.radians(self.corrugation_angle_deg))

    @property
    def half_pitch_mm(self) -> float:
        """Repeating unit width ``s = a + c*cos(phi)`` (one flange + one web).

        ``c*cos(phi)`` is the *horizontal projection* of the slant web — not the
        slant width ``c`` itself (web material area stays ``t_w*c``).
        """
        return self.flange_width_mm + self.web_width_mm * math.cos(
            math.radians(self.corrugation_angle_deg)
        )

    @property
    def full_pitch_mm(self) -> float:
        """Full corrugation period ``2*s`` (two flanges + two webs)."""
        return 2.0 * self.half_pitch_mm

    @property
    def neutral_axis_mm(self) -> float:
        """Distance of the neutral axis from a flange plane, ``d/2``."""
        return self.corrugation_depth_mm / 2.0

    # -- per-half-pitch (per-corrugation) section properties ---------------
    @property
    def area_mm2(self) -> float:
        """Cross-sectional area of one half-pitch, ``a*t_f + c*t_w``."""
        return self.flange_width_mm * self.t_f + self.web_width_mm * self.t_w

    @property
    def moment_of_inertia_mm4(self) -> float:
        """Second moment of area of one half-pitch about the neutral axis."""
        d = self.corrugation_depth_mm
        flange = self.flange_width_mm * self.t_f * (d / 2.0) ** 2
        web = self.web_width_mm * self.t_w * d**2 / 12.0
        return flange + web

    @property
    def section_modulus_mm3(self) -> float:
        """Section modulus of one half-pitch, ``I/(d/2) = d(3 a t_f + c t_w)/6``."""
        return self.moment_of_inertia_mm4 / self.neutral_axis_mm

    # -- per-unit-width section properties (divide by half pitch) ----------
    @property
    def area_per_unit_width_mm(self) -> float:
        return self.area_mm2 / self.half_pitch_mm

    @property
    def moment_of_inertia_per_unit_width_mm3(self) -> float:
        return self.moment_of_inertia_mm4 / self.half_pitch_mm

    @property
    def section_modulus_per_unit_width_mm2(self) -> float:
        return self.section_modulus_mm3 / self.half_pitch_mm


# ---------------------------------------------------------------------------
# Buckling check
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class CorrugatedBulkheadResult:
    """Governing buckling check of a corrugated bulkhead."""

    governing_mode: str
    utilization: float
    passes: bool
    flange_buckling_util: float
    web_buckling_util: float
    column_buckling_util: float
    shear_buckling_util: float
    section_modulus_mm3: float
    moment_of_inertia_mm4: float
    code_reference: str = CODE_REFERENCE
    preliminary: bool = PRELIMINARY


def _web_shear_buckling_stress(
    geom: CorrugatedBulkheadGeometry, material: MaterialProperties
) -> float:
    """Critical shear-buckling stress of the inclined web plate (MPa).

    ``tau_e = k_tau * pi^2 E / (12(1-nu^2)) * (t_w/c)^2`` with the classical
    coefficient ``k_tau`` (``c`` the web slant width as the short edge, ``span``
    the long edge), reduced by the plate solver's Johnson-Ostenfeld knockdown.
    """
    c = geom.web_width_mm
    a = geom.span_mm
    if a / c >= 1.0:
        k_tau = 5.34 + 4.0 * (c / a) ** 2
    else:
        k_tau = 4.0 + 5.34 * (c / a) ** 2
    e = material.youngs_modulus
    nu = material.poissons_ratio
    tau_e = k_tau * math.pi**2 * e / (12.0 * (1.0 - nu**2)) * (geom.t_w / c) ** 2
    return PlateBucklingAnalyzer(material).johnson_ostenfeld(tau_e)


def check_corrugated_bulkhead(
    geom: CorrugatedBulkheadGeometry,
    material: MaterialProperties,
    *,
    axial_stress_mpa: float,
    shear_stress_mpa: float = 0.0,
    gamma_m: float = 1.15,
) -> CorrugatedBulkheadResult:
    """Check a corrugated bulkhead against its buckling failure modes.

    Args:
        geom: corrugation geometry.
        material: plate material.
        axial_stress_mpa: in-plane compressive stress along the corrugation
            (positive = compression) carried by the flange/web plate elements.
        shear_stress_mpa: in-plane shear carried by the web.
        gamma_m: material resistance factor.

    Returns the governing (highest-utilisation) mode among local flange plate
    buckling, local web plate buckling, overall column buckling and web shear
    buckling.
    """
    plate = PlateBucklingAnalyzer(material)
    column = ColumnBucklingAnalyzer(material)

    # --- local flange plate buckling (flange width = short edge) ----------
    flange = PlateGeometry(
        length=geom.span_mm, width=geom.flange_width_mm, thickness=geom.t_f
    )
    flange_res = plate.check_plate_buckling(
        flange, sigma_x=axial_stress_mpa, gamma_m=gamma_m
    )

    # --- local web plate buckling (web slant width = short edge) ----------
    web = PlateGeometry(
        length=geom.span_mm, width=geom.web_width_mm, thickness=geom.t_w
    )
    web_res = plate.check_plate_buckling(
        web, sigma_x=axial_stress_mpa, tau=shear_stress_mpa, gamma_m=gamma_m
    )

    # --- overall column buckling of the corrugation over the span ---------
    axial_force = axial_stress_mpa * geom.area_mm2
    col_res = column.check_column_buckling(
        axial_force=max(axial_force, 0.0),
        area=geom.area_mm2,
        I_min=geom.moment_of_inertia_mm4,
        L_eff=geom.span_mm,
        gamma_m=gamma_m,
    )

    # --- web shear buckling -----------------------------------------------
    tau_cr = _web_shear_buckling_stress(geom, material)
    tau_cap = tau_cr / gamma_m
    shear_util = shear_stress_mpa / tau_cap if tau_cap > 0 else float("inf")

    modes = {
        "flange_plate_buckling": float(flange_res.utilization),
        "web_plate_buckling": float(web_res.utilization),
        "column_buckling": float(col_res.utilization),
        "web_shear_buckling": float(shear_util),
    }
    governing_mode = max(modes, key=modes.get)
    governing_util = modes[governing_mode]

    return CorrugatedBulkheadResult(
        governing_mode=governing_mode,
        utilization=governing_util,
        passes=bool(governing_util <= 1.0),
        flange_buckling_util=modes["flange_plate_buckling"],
        web_buckling_util=modes["web_plate_buckling"],
        column_buckling_util=modes["column_buckling"],
        shear_buckling_util=modes["web_shear_buckling"],
        section_modulus_mm3=geom.section_modulus_mm3,
        moment_of_inertia_mm4=geom.moment_of_inertia_mm4,
    )
