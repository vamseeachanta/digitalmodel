"""Parametric mudmat/foundation APDL template generator (digitalmodel #952).

Emits a complete ANSYS Mechanical APDL ``.inp`` for a first-pass **screening**
FE of a rectangular foundation mat (mudmat) resting on soil: a flat plate of the
mat thickness supported on an **elastic Winkler foundation**, loaded by a central
vertical load plus an applied (eccentric) overturning moment, solved static-
structural, post-processed to peak plate von Mises stress (UC vs yield/design
factor) AND peak soil bearing pressure (UC vs allowable bearing).

Soil model: the Winkler soil is applied as an **elastic foundation stiffness**
(EFS) real constant on the SHELL181 plate (``R,1,t, , ,EFS``). EFS has units of
pressure-per-unit-out-of-plane-deflection = the subgrade reaction modulus
(N/mm^3). This distributes a vertical pressure ``p = ksub * w`` under the mat,
the textbook linear Winkler relation; max soil bearing pressure is then recovered
post-solve from the peak downward deflection (``p_max = ksub * w_max``). Choosing
EFS over discrete COMBIN14 nodal springs keeps the script geometry-mesh agnostic
(no per-node tributary-area bookkeeping) and is the clean SHELL181 idiom.

The generic ``apdl_generator`` building blocks cover materials/elements/mesh/BC
but NOT a soil-supported plate, so this module writes the mat geometry and the
foundation real constant directly. The generated script runs on a licensed MAPDL
via the fail-closed ANSYS runner (#940); this module needs no license (it only
writes text).

Honesty: a SHELL181 plate on a linear Winkler foundation is a screening
idealisation. It assumes the soil is a bed of linear springs that can take
**tension as well as compression** (no uplift / no contact separation), no soil
nonlinearity, no consolidation/settlement-over-time, and no global
bearing-capacity (Brinch-Hansen) failure surface. Use it to size and rank the
mat plate and to flag bearing-pressure exceedance; verify governing cases with a
no-tension contact foundation and a proper geotechnical bearing-capacity check.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path


@dataclass
class MudmatGeometry:
    """Mudmat plate geometry, load, soil, and material (units: mm, N, MPa).

    Loads are given in kN and kN*m for convenience and converted to N / N*mm
    inside the generator.
    """

    mat_length_mm: float = 4000.0
    mat_width_mm: float = 3000.0
    thickness_mm: float = 60.0
    vertical_load_kn: float = 800.0
    moment_kNm: float = 400.0  # eccentric / overturning moment about the width axis
    subgrade_modulus_n_per_mm3: float = 0.05  # Winkler soil stiffness (N/mm^3)
    youngs_modulus_mpa: float = 205_000.0
    poisson: float = 0.3
    yield_strength_mpa: float = 355.0
    design_factor: float = 1.67  # allowable = yield / design_factor (~0.6 Fy)
    allowable_bearing_mpa: float = 0.25  # allowable soil bearing pressure (MPa)
    element_size_mm: float = 100.0

    def validate(self) -> list[str]:
        issues: list[str] = []
        if min(self.mat_length_mm, self.mat_width_mm, self.thickness_mm) <= 0:
            issues.append("mat dimensions must be positive")
        if self.element_size_mm <= 0:
            issues.append("element size must be positive")
        if self.subgrade_modulus_n_per_mm3 <= 0:
            issues.append("subgrade modulus must be positive")
        if self.vertical_load_kn <= 0:
            issues.append("vertical load must be positive")
        if self.moment_kNm < 0:
            issues.append("moment must be non-negative")
        if self.youngs_modulus_mpa <= 0 or self.poisson <= 0:
            issues.append("material properties must be positive")
        if self.design_factor <= 0 or self.yield_strength_mpa <= 0:
            issues.append("yield strength and design factor must be positive")
        if self.allowable_bearing_mpa <= 0:
            issues.append("allowable bearing pressure must be positive")
        # Effective-area validity (#2094). The Meyerhof idealisation carries the
        # vertical load on L - 2e; at e >= L/2 the effective area vanishes and no
        # bearing solution exists. Checked here rather than left to the solver,
        # which would report a plausible stress for a foundation that has
        # overturned.
        if self.vertical_load_kn > 0 and self.moment_kNm >= 0:
            ecc_mm = (self.moment_kNm * 1.0e6) / (self.vertical_load_kn * 1000.0)
            if ecc_mm * 2.0 >= self.mat_length_mm:
                issues.append(
                    f"eccentricity e={ecc_mm:.1f} mm reaches half the mat length "
                    f"({self.mat_length_mm / 2:.1f} mm): effective area vanishes"
                )
        return issues


def generate_mudmat_apdl(geom: MudmatGeometry) -> str:
    """Return a complete MAPDL ``.inp`` script for the mudmat screening FE."""
    issues = geom.validate()
    if issues:
        raise ValueError("invalid mudmat geometry: " + "; ".join(issues))

    allowable = geom.yield_strength_mpa / geom.design_factor
    v_n = geom.vertical_load_kn * 1000.0  # N, downward
    m_nmm = geom.moment_kNm * 1.0e6  # kN*m -> N*mm
    mat_l = geom.mat_length_mm
    mat_b = geom.mat_width_mm

    # --- soil reaction: Meyerhof effective area (#2094 D1) -------------------
    # The Winkler bed is gone. SHELL181 never consumed the subgrade modulus it
    # was given -- the real constant was accepted and ignored, leaving every UZ
    # degree of freedom unrestrained and the model singular -- and a linear bed
    # carries tension, which a mat under moment does not.
    #
    # Instead the soil reaction is the same idealisation the bearing check uses:
    # the vertical load is carried by a uniform pressure over the effective area
    # B_eff x L, with B_eff = L - 2e reduced for eccentricity. The patch runs
    # flush to the loaded edge, because x_centroid + L_eff/2 = L/2 + e + (L-2e)/2
    # = L identically.
    ecc = m_nmm / v_n
    l_eff = mat_l - 2.0 * ecc
    if l_eff <= 0.0:
        raise ValueError(
            f"eccentricity e={ecc:.1f} mm exceeds half the mat length "
            f"({mat_l / 2:.1f} mm): the effective area vanishes and no bearing "
            "solution exists. Reduce the moment or lengthen the mat."
        )
    a_eff = l_eff * mat_b
    q_soil = v_n / a_eff  # MPa, uniform over the effective patch
    patch_x0 = mat_l - l_eff

    # --- applied field: uniform load plus the overturning moment as a gradient
    # p(x) = V/(L*B) + 12*M*(x - L/2)/(B*L^3). Integrates to V and to M exactly,
    # so the applied field and the soil reaction balance by construction and the
    # plate needs only a statically determinate restraint set. That the reactions
    # then vanish is the conservation check, not an incidental property.
    #
    # This also removes the FCUM defect: the couple is part of one pressure field
    # rather than a second F command overwriting the first. The previous deck
    # never issued FCUM, so the default FCUM,REPL made the edge couple REPLACE
    # the short-edge nodes' share of the uniform load. The applied resultant came
    # to 760,976 N against an intended 800,000 N, and the 4.88 percent deficit was
    # 2/(L/ESIZE + 1) -- a function of mesh density, so refining the mesh changed
    # the answer while converging on nothing.
    p_uniform = v_n / (mat_l * mat_b)
    p_slope = 12.0 * m_nmm / (mat_b * mat_l**3)
    p_at_x0 = p_uniform - p_slope * mat_l / 2.0
    uplift_note = (
        ""
        if p_at_x0 >= 0.0
        else (
            "\n! WARNING: the applied field is negative at x=0 "
            f"({p_at_x0:.6f} MPa), i.e. the structure would lift the near edge.\n"
            "! A linear pressure field cannot represent that; treat this case as\n"
            "! outside the screening model's range."
        )
    )

    return f"""! Mudmat / shallow-foundation screening FE — generated by digitalmodel.ansys.mudmat
! Units: mm, N, MPa. SHELL181 plate; soil reaction as a Meyerhof effective-area
! pressure patch. The plate-bending check is here; the bearing check belongs to
! digitalmodel.geotechnical.mudmat and is NOT computed by this deck (#2094 D1).
!
! Eccentricity e = M/V         = {ecc:.3f} mm
! Effective length L_eff = L-2e = {l_eff:.3f} mm
! Effective area  A_eff        = {a_eff:.1f} mm^2
! Soil pressure   q = V/A_eff  = {q_soil:.6f} MPa (uniform, upward, x >= {patch_x0:.3f}){uplift_note}
FINISH
/CLEAR,NOSTART
/TITLE,Mudmat screening: {geom.mat_length_mm}x{geom.mat_width_mm}mm t={geom.thickness_mm}mm
/UNITS,MPA
/PREP7

! --- material (linear elastic steel) ---
MP,EX,1,{geom.youngs_modulus_mpa}
MP,PRXY,1,{geom.poisson}

! --- element: SHELL181, thickness from the section ---
! No elastic foundation real constant: SHELL181 does not consume one. The prior
! deck set R,1,t,,,ksub; RLIST confirmed the value was stored and the element
! ignored it, so the plate had no vertical stiffness and the solve was singular
! at every UZ degree of freedom (#2094).
ET,1,SHELL181
SECTYPE,1,SHELL
SECDATA,{geom.thickness_mm},1
SECNUM,1

! --- geometry: rectangular mat in the X-Y plane ---
BLC4,0,0,{geom.mat_length_mm},{geom.mat_width_mm}   ! area 1 = mat plate

! --- mesh ---
TYPE,1
MAT,1
ESIZE,{geom.element_size_mm}
AMESH,ALL

! --- boundary conditions: statically determinate, six modes removed ---
! The applied field and the soil reaction balance by construction, so these
! restraints carry no load. Their reactions are the conservation check.
NSEL,S,LOC,X,0
NSEL,R,LOC,Y,0
D,ALL,UX,0
D,ALL,UY,0
D,ALL,UZ,0
ALLSEL,ALL
NSEL,S,LOC,X,{geom.mat_length_mm}
NSEL,R,LOC,Y,0
D,ALL,UY,0
D,ALL,UZ,0
ALLSEL,ALL
NSEL,S,LOC,X,0
NSEL,R,LOC,Y,{geom.mat_width_mm}
D,ALL,UZ,0
ALLSEL,ALL

! --- load 1: applied field, uniform + overturning gradient (downward) ---
! p(x) = {p_uniform:.8f} + {p_slope:.10f}*x   [MPa], integrates to V and to M
ALLSEL,ALL
SFGRAD,PRES,0,X,0,{p_slope:.10f}
SFE,ALL,1,PRES,,{p_at_x0:.8f}
SFGRAD                      ! clear the gradient before the next surface load

! --- load 2: soil reaction over the effective area (upward) ---
! Applied to face 2 so it opposes load 1. If the sign were wrong the reactions
! below would not vanish, which is exactly what that check is for.
ESEL,S,CENT,X,{patch_x0:.6f},{geom.mat_length_mm}
SFE,ALL,2,PRES,,{q_soil:.8f}
ALLSEL,ALL

! --- solve (static structural) ---
/SOLU
ANTYPE,STATIC
SOLVE
FINISH

! --- post: peak plate von Mises + equilibrium ---
/POST1
SET,LAST
NSORT,S,EQV
*GET,smax,SORT,0,MAX
*GET,peak_node,SORT,0,IMAX
allow = {allowable}
stress_uc = smax / allow

! --- equilibrium check (#2094) ---
! Applied load and soil reaction balance by construction, so every reaction
! shall be zero to solver precision. A non-zero FZ means the applied field and
! the patch do not integrate to the same resultant.
NSEL,S,LOC,X,0
NSEL,A,LOC,X,{geom.mat_length_mm}
FSUM
*GET,rfz,FSUM,0,ITEM,FZ
ALLSEL,ALL

/COM,============================================================
/COM,MUDMAT SCREENING RESULT (plate bending only)
*MSG,INFO,smax,allow,stress_uc
peak von Mises = %G MPa ; allowable = %G MPa ; stress UC = %G
*MSG,INFO,rfz
reaction FZ = %G N  (shall be ~0: loads balance by construction)
/COM,Soil bearing is NOT evaluated here. Use
/COM,digitalmodel.geotechnical.mudmat.mudmat_bearing_capacity for the
/COM,bearing check; this deck applies q = V/A_eff from that same idealisation.
/COM,============================================================
! numeric digest for the postprocessor (small, returnable)
*CFOPEN,mudmat_result,csv
*VWRITE,smax,stress_uc,peak_node,rfz,{q_soil:.8f},{ecc:.6f}
('max_seqv_mpa,',F12.4,',stress_uc,',F10.5,',peak_node,',F10.0,',reaction_fz_n,',F14.4,',q_soil_mpa,',F12.6,',eccentricity_mm,',F12.4)
*CFCLOS
FINISH
"""


def write_mudmat_inp(geom: MudmatGeometry, path: Path | str) -> Path:
    """Write the generated APDL script to ``path`` and return it."""
    out = Path(path)
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(generate_mudmat_apdl(geom), encoding="utf-8")
    return out
