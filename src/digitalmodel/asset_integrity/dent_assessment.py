# ABOUTME: Dent and dent-gouge screening for pipelines (API 579-1 Part 12 scope)
# ABOUTME: — depth screen, ASME B31.8 Appendix R strain estimate, conservative routing.
"""Dent and dent-gouge screening assessment for pipelines.

Level 1/2 *screening* of dents in line pipe, in the scope of API 579-1/ASME
FFS-1 Part 12 (dents, gouges and dent-gouge combinations), built from the
well-published closed-form criteria:

**1. Plain-dent depth screen.** A plain (smooth, no metal loss, weld-free)
dent is screened on its depth ratio ``d / OD``.  The default acceptance
threshold is the widely published **7 % OD** plain-dent limit (EPRG
guidelines on the assessment of mechanical damage, Roovers et al.; reproduced
in the Pipeline Defect Assessment Manual).  ASME B31.8 uses a 6 % OD
operational acceptance limit for plain dents — pass
``plain_dent_depth_limit=0.06`` to apply it.  The threshold is a parameter,
not a constant.

**2. Dent strain estimate (ASME B31.8, Nonmandatory Appendix R).**  With the
dent lengths measured in both directions the bending + membrane strains are
estimated from dent geometry (all lengths in consistent units; strains are
dimensionless):

    R0 = OD / 2                     initial pipe surface radius
    R1 = signed transverse (circumferential-plane) radius at the dent apex
    R2 = signed longitudinal-plane radius at the dent apex
         (negative when the dent reverses the surface curvature)

    eps1 = (t / 2) * (1/R0 - 1/R1)      circumferential bending strain
    eps2 = -(t / 2) * (1/R2)            longitudinal bending strain
    eps3 = (1/2) * (d / L)**2           longitudinal membrane strain
    eps_i = sqrt(eps1**2 - eps1*(eps2 + eps3) + (eps2 + eps3)**2)   (inside)

``eps_i`` is the Appendix R inside-surface combined strain as widely
reproduced in the dent-assessment literature.  The outside surface is checked
with the same effective-strain combination with the bending signs reversed
(membrane unchanged):

    eps_o = sqrt(eps1**2 + eps1*(eps3 - eps2) + (eps3 - eps2)**2)

and the maximum of the two governs.  When only depth and lengths are
measured, the apex radii are estimated with the parabolic-profile relation
``R = L**2 / (8 d)`` (pure geometry; slightly smaller than the circular-arc
sagitta radius, hence conservative) and the dent is assumed to *reverse* the
surface curvature in both planes (conservative screening assumption):

    1/R1 = -8 d / Lc**2        R2 = -LL**2 / (8 d)

The default strain acceptance limit is the published **6 %** plain-dent
strain limit (ASME B31.8 practice for strain-based acceptance of plain
dents); parameterized via ``strain_limit``.

**3. Dent-gouge screen (conservative).**  Dents interacting with gouges,
grooves, scratches, arc burns or other metal loss are **not** acceptable by
closed-form screening: dent-gouge failure is fracture-controlled (PDAM,
EPRG), highly sensitive to gouge depth, toughness and dent rerounding, and
the published screening position is rejection / detailed assessment.  Such
combinations always return ``NEEDS_ASSESSMENT`` routing to a Level 3 /
quantitative dent-gouge model (EPRG dent-gouge fracture model is a stated
follow-up, not implemented here).

**4. Restraint and location modifiers.**  Dents on seam or girth welds
tolerate far less strain than the pipe body: the default weld screen accepts
only shallow dents (**2 % OD** default, the widely applied limit for dents
affecting welds, cf. 49 CFR 192.309(b) construction acceptance and ASME
B31.8 practice) and then only as ``MONITOR``; deeper dents on welds are
rejected at screening level (retention would require a Level 3 engineering
critical assessment).  Rock-/soil-restrained dents that would otherwise pass
are capped at ``MONITOR``: restrained dents do not reround under pressure and
carry additional fatigue / restraint-removal uncertainty.

Verdicts (ordered): ``ACCEPT`` < ``MONITOR`` < ``NEEDS_ASSESSMENT`` <
``REJECT``.  Every threshold above is a keyword parameter with the published
value as its documented default — no proprietary table values are embedded.

Units are US customary (inches) for geometry, psi for the optional SMYS
(recorded for downstream pressure-based methods; not used by these
geometric/strain screens).
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional

from digitalmodel.codes import API_579, ASME_B31_8

# --- Published screening defaults (all parameterized in assess_dent) --------
#: Plain-dent depth acceptance, d/OD.  EPRG plain-dent guideline (7 % OD);
#: ASME B31.8 operational acceptance uses 6 % OD (pass 0.06 to apply).
PLAIN_DENT_DEPTH_LIMIT = 0.07
#: Depth acceptance for dents affecting seam/girth welds, d/OD (2 % OD,
#: cf. 49 CFR 192.309(b) for pipe > NPS 12; widely used integrity screen).
WELD_DENT_DEPTH_LIMIT = 0.02
#: Strain acceptance for plain dents (ASME B31.8 strain-based practice, 6 %).
PLAIN_DENT_STRAIN_LIMIT = 0.06

#: Verdict severity ordering (higher = worse).
VERDICT_ORDER = {"ACCEPT": 0, "MONITOR": 1, "NEEDS_ASSESSMENT": 2, "REJECT": 3}

_CITATIONS = [
    f"{API_579.label} Part 12 — Assessment of Dents, Gouges, and "
    "Dent-Gouge Combinations (scope and Level 1/2 screening framework)",
    f"{ASME_B31_8.label} Nonmandatory Appendix R — Estimating Strain in Dents "
    "(bending/membrane strain equations)",
    "EPRG guidelines on the assessment of mechanical damage "
    "(Roovers et al.) — 7 % OD plain-dent acceptance",
    "Pipeline Defect Assessment Manual (Cosham & Hopkins) — dent-gouge "
    "failure is fracture-controlled; screening rejection is the "
    "conservative position",
    "49 CFR 192.309(b) — 2 % OD acceptance for dents affecting welds "
    "(pipe > NPS 12)",
]


@dataclass
class DentStrain:
    """ASME B31.8 Appendix R strain estimate for a dent (see module docstring)."""

    eps_circ_bending: float    # eps1 = (t/2)(1/R0 - 1/R1)
    eps_long_bending: float    # eps2 = -(t/2)(1/R2)
    eps_long_membrane: float   # eps3 = (1/2)(d/L)^2
    eps_inside: float          # combined strain, inside surface
    eps_outside: float         # combined strain, outside surface
    eps_max: float             # max(eps_inside, eps_outside) — governs
    r0_in: float               # OD/2
    r1_in: float               # signed transverse apex radius (negative=reversed)
    r2_in: float               # signed longitudinal apex radius (negative=reversed)


@dataclass
class DentAssessment:
    """Screening result for a pipeline dent (API 579-1 Part 12 scope)."""

    od_in: float
    wt_in: float
    dent_depth_in: float
    depth_ratio: float                    # d / OD
    verdict: str                          # ACCEPT|MONITOR|NEEDS_ASSESSMENT|REJECT
    governing_criterion: str
    strain: Optional[DentStrain] = None   # None when dent lengths not given
    on_weld: bool = False
    restrained: bool = False
    with_gouge_or_metal_loss: bool = False
    thresholds: dict = field(default_factory=dict)
    citations: list = field(default_factory=list)
    validity_notes: list = field(default_factory=list)
    code_reference: str = ""


def _effective_strain(eps_x: float, eps_y: float) -> float:
    """Plane effective-strain combination sqrt(x^2 - x*y + y^2)."""
    return math.sqrt(eps_x * eps_x - eps_x * eps_y + eps_y * eps_y)


def dent_strain_b31_8(
    od_in: float,
    wt_in: float,
    dent_depth_in: float,
    dent_length_axial_in: float,
    dent_length_circ_in: float,
) -> DentStrain:
    """ASME B31.8 Appendix R dent strain estimate from depth and lengths.

    Implements exactly the formulation written out in the module docstring:
    parabolic apex radii ``R = L^2/(8 d)`` with curvature assumed *reversed*
    in both planes (conservative screening assumption), then

        eps1  = (t/2) * (1/R0 - 1/R1)   with  1/R1 = -8 d / Lc^2
        eps2  = -(t/2) * (1/R2)         with  R2   = -LL^2 / (8 d)
        eps3  = (1/2) * (d / LL)^2
        eps_i = sqrt(eps1^2 - eps1*(eps2 + eps3) + (eps2 + eps3)^2)
        eps_o = sqrt(eps1^2 + eps1*(eps3 - eps2) + (eps3 - eps2)^2)

    Args:
        od_in: pipe outside diameter (in).
        wt_in: nominal wall thickness (in).
        dent_depth_in: dent depth d (in), measured from original surface.
        dent_length_axial_in: dent length LL along the pipe axis (in).
        dent_length_circ_in: dent length Lc around the circumference (in).
    """
    _validate_geometry(od_in, wt_in)
    d = dent_depth_in
    if d <= 0:
        raise ValueError(f"dent depth must be > 0 for a strain estimate (got {d}).")
    ll, lc = dent_length_axial_in, dent_length_circ_in
    if ll is None or lc is None or ll <= 0 or lc <= 0:
        raise ValueError("dent lengths (axial and circumferential) must be > 0.")

    t = wt_in
    r0 = od_in / 2.0
    r1 = -(lc * lc) / (8.0 * d)          # signed; negative = reversed curvature
    r2 = -(ll * ll) / (8.0 * d)          # signed; negative = reversed curvature

    eps1 = (t / 2.0) * (1.0 / r0 - 1.0 / r1)
    eps2 = -(t / 2.0) * (1.0 / r2)
    eps3 = 0.5 * (d / ll) ** 2

    eps_i = _effective_strain(eps1, eps2 + eps3)
    eps_o = _effective_strain(-eps1, -eps2 + eps3)
    return DentStrain(
        eps_circ_bending=eps1,
        eps_long_bending=eps2,
        eps_long_membrane=eps3,
        eps_inside=eps_i,
        eps_outside=eps_o,
        eps_max=max(eps_i, eps_o),
        r0_in=r0,
        r1_in=r1,
        r2_in=r2,
    )


def assess_dent(
    od_in: float,
    wt_in: float,
    dent_depth_in: float,
    dent_length_axial_in: Optional[float] = None,
    dent_length_circ_in: Optional[float] = None,
    *,
    on_weld: bool = False,
    restrained: bool = False,
    with_gouge_or_metal_loss: bool = False,
    smys_psi: Optional[float] = None,
    plain_dent_depth_limit: float = PLAIN_DENT_DEPTH_LIMIT,
    weld_dent_depth_limit: float = WELD_DENT_DEPTH_LIMIT,
    strain_limit: float = PLAIN_DENT_STRAIN_LIMIT,
) -> DentAssessment:
    """Screen a pipeline dent (API 579-1 Part 12 scope). See module docstring.

    Args:
        od_in: pipe outside diameter (in).
        wt_in: nominal wall thickness (in).
        dent_depth_in: dent depth (in) from the original surface.  For
            unrestrained dents use the depth measured under pressure /
            after rerounding where available.
        dent_length_axial_in: dent length along the pipe axis (in); with
            ``dent_length_circ_in`` enables the Appendix R strain estimate.
        dent_length_circ_in: dent length around the circumference (in).
        on_weld: dent affects a seam or girth weld -> much stricter screen.
        restrained: dent is held by rock/support (does not reround) -> a
            passing dent is capped at MONITOR.
        with_gouge_or_metal_loss: dent coincident with a gouge/groove/
            scratch/arc burn or corrosion metal loss -> NEEDS_ASSESSMENT
            (fracture-controlled; outside screening scope).
        smys_psi: specified minimum yield strength (psi).  Recorded only;
            the geometric/strain screens implemented here do not use it
            (reserved for pressure-based dent-gouge follow-up models).
        plain_dent_depth_limit: plain-dent d/OD acceptance (default 0.07,
            EPRG; ASME B31.8 operational practice = 0.06).
        weld_dent_depth_limit: d/OD acceptance for dents on welds
            (default 0.02, cf. 49 CFR 192.309(b)).
        strain_limit: dent strain acceptance (default 0.06, ASME B31.8
            plain-dent strain practice).
    """
    _validate_geometry(od_in, wt_in)
    if dent_depth_in < 0:
        raise ValueError(f"dent depth must be >= 0 (got {dent_depth_in}).")
    if dent_depth_in >= od_in:
        raise ValueError(
            f"dent depth {dent_depth_in} must be smaller than OD {od_in}."
        )
    for name, val in (("dent_length_axial_in", dent_length_axial_in),
                      ("dent_length_circ_in", dent_length_circ_in)):
        if val is not None and val <= 0:
            raise ValueError(f"{name} must be > 0 if given (got {val}).")
    if not (0.0 < weld_dent_depth_limit <= plain_dent_depth_limit):
        raise ValueError(
            "weld_dent_depth_limit must be in (0, plain_dent_depth_limit] — "
            "dents on welds are screened more strictly than the pipe body."
        )

    depth_ratio = dent_depth_in / od_in
    notes: list = []

    # Strain estimate (needs both lengths and a non-zero depth).
    strain: Optional[DentStrain] = None
    if dent_length_axial_in is not None and dent_length_circ_in is not None \
            and dent_depth_in > 0:
        strain = dent_strain_b31_8(
            od_in, wt_in, dent_depth_in,
            dent_length_axial_in, dent_length_circ_in)
        for label, length in (("axial", dent_length_axial_in),
                              ("circumferential", dent_length_circ_in)):
            if dent_depth_in / length > 0.5:
                notes.append(
                    f"dent depth exceeds half the {label} length — the "
                    "parabolic apex-radius estimate is unreliable for such "
                    "sharp dents; measure the actual profile radii.")
    elif dent_depth_in > 0:
        notes.append(
            "dent lengths not provided in both directions — no Appendix R "
            "strain estimate; screening is on depth ratio alone. Measure "
            "axial and circumferential dent lengths to enable the strain "
            "screen.")

    # --- Verdict ------------------------------------------------------------
    if with_gouge_or_metal_loss:
        verdict = "NEEDS_ASSESSMENT"
        governing = (
            "dent-gouge / dent + metal-loss interaction: fracture-controlled "
            "failure mode, not acceptable by closed-form screening (PDAM/EPRG "
            "position) — route to Level 3 / quantitative dent-gouge model")
        notes.append(
            "quantitative EPRG dent-gouge fracture model is a stated "
            "follow-up; this screen conservatively rejects all dent-gouge "
            "combinations from screening-level acceptance.")
    elif on_weld:
        if depth_ratio <= weld_dent_depth_limit:
            verdict = "MONITOR"
            governing = (
                f"dent on weld with d/OD = {depth_ratio:.4f} <= "
                f"{weld_dent_depth_limit:.2%} OD weld screen — shallow enough "
                "to retain, but dents affecting welds are monitored, not "
                "screening-accepted")
        else:
            verdict = "REJECT"
            governing = (
                f"dent on weld with d/OD = {depth_ratio:.4f} > "
                f"{weld_dent_depth_limit:.2%} OD weld screen — repair/replace "
                "per screening practice (retention would require a Level 3 "
                "engineering critical assessment of the weld)")
    else:
        strain_ok = strain is not None and strain.eps_max <= strain_limit
        strain_bad = strain is not None and strain.eps_max > strain_limit
        if depth_ratio <= plain_dent_depth_limit:
            if strain_bad:
                verdict = "NEEDS_ASSESSMENT"
                governing = (
                    f"plain-dent depth screen passes (d/OD = "
                    f"{depth_ratio:.4f} <= {plain_dent_depth_limit:.2%}) but "
                    f"estimated dent strain {strain.eps_max:.3f} exceeds the "
                    f"{strain_limit:.0%} strain limit — strain governs; "
                    "detailed assessment required")
            else:
                verdict = "ACCEPT"
                governing = (
                    f"plain dent, d/OD = {depth_ratio:.4f} <= "
                    f"{plain_dent_depth_limit:.2%} OD depth screen"
                    + (f"; strain {strain.eps_max:.3f} <= "
                       f"{strain_limit:.0%}" if strain is not None else ""))
        else:
            if strain_ok:
                verdict = "MONITOR"
                governing = (
                    f"d/OD = {depth_ratio:.4f} exceeds the "
                    f"{plain_dent_depth_limit:.2%} OD depth screen but "
                    f"estimated strain {strain.eps_max:.3f} <= "
                    f"{strain_limit:.0%} (strain-qualified per ASME B31.8 "
                    "Appendix R practice) — monitor; confirm ILI passage and "
                    "operational constraints")
            else:
                verdict = "NEEDS_ASSESSMENT"
                governing = (
                    f"d/OD = {depth_ratio:.4f} exceeds the "
                    f"{plain_dent_depth_limit:.2%} OD plain-dent depth screen"
                    + (f" and estimated strain {strain.eps_max:.3f} exceeds "
                       f"the {strain_limit:.0%} limit"
                       if strain_bad else " with no qualifying strain "
                       "estimate") + " — detailed assessment required")

    if restrained and verdict == "ACCEPT":
        verdict = "MONITOR"
        governing += (
            "; dent is restrained (rock/support) — capped at MONITOR: "
            "restrained dents do not reround and carry fatigue / "
            "restraint-removal uncertainty")
    if restrained:
        notes.append(
            "restrained dent: depth may increase if the restraint is "
            "removed; include in fatigue-susceptibility review.")

    return DentAssessment(
        od_in=od_in,
        wt_in=wt_in,
        dent_depth_in=dent_depth_in,
        depth_ratio=depth_ratio,
        verdict=verdict,
        governing_criterion=governing,
        strain=strain,
        on_weld=on_weld,
        restrained=restrained,
        with_gouge_or_metal_loss=with_gouge_or_metal_loss,
        thresholds={
            "plain_dent_depth_limit": plain_dent_depth_limit,
            "weld_dent_depth_limit": weld_dent_depth_limit,
            "strain_limit": strain_limit,
            "smys_psi": smys_psi,
        },
        citations=list(_CITATIONS),
        validity_notes=notes,
        code_reference=API_579.label,
    )


def _validate_geometry(od_in: float, wt_in: float) -> None:
    if od_in <= 0 or wt_in <= 0 or wt_in >= od_in / 2.0:
        raise ValueError(f"Invalid geometry: OD={od_in}, t={wt_in}.")
