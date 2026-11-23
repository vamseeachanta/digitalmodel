# ABOUTME: Full Smith incremental-iterative hull-girder ultimate-strength method
# ABOUTME: (IACS UR S11A): cross-section elements + load-shortening curves + curvature march.
"""Hull-girder ultimate strength by the **Smith incremental-iterative method**.

This is the rigorous successor to the *simplified* single-step estimate
``M_U = sigma_U * Z`` in
:mod:`digitalmodel.naval_architecture.hull_girder_strength` (issue #1082). Where
the simplified figure assumes the compression flange reaches its buckling-reduced
ultimate while the tension flange yields and ignores neutral-axis migration and
post-buckling redistribution, the Smith march captures all three:

1. The midship cross-section is discretised into a list of structural
   **elements** (:class:`HullGirderElement`) — a stiffener with its attached
   plate, or a hard-corner plate. Each element carries an area, a vertical
   position ``z`` (height above baseline), a yield ``fy`` and a *compressive*
   ultimate stress ``sigma_u`` (buckling-reduced — supplied directly or derived
   from the validated stiffened-panel solver; in tension every element develops
   ``fy``).
2. Each element has a **load-shortening curve** ``sigma(strain)`` (see
   :meth:`HullGirderElement.stress`): tension is elastic to ``+fy`` then a
   plateau; compression is elastic until it reaches its capacity ``sigma_u``,
   then a post-buckling branch decaying toward a residual fraction of
   ``sigma_u``. A non-buckling element (``sigma_u >= fy``) is simply
   elastic-perfectly-plastic.
3. The **Smith march** imposes increasing hull curvatures. At each curvature the
   instantaneous neutral-axis height ``z_NA`` is solved so the sum of element
   axial forces is zero; each element strain is ``curvature * (z - z_NA)``, its
   stress comes from its load-shortening curve, and the bending moment is
   ``sum(force * (z - z_NA))``. The **peak** of the moment-curvature curve is the
   ultimate moment ``M_U`` (computed for both hogging and sagging).

Sign convention: ``strain = curvature * (z - z_NA)``, so a **positive curvature**
puts the deck (high ``z``) in tension and the keel in compression = **hogging**;
a negative curvature is **sagging** (deck in compression — usually governing).

Units: areas in m^2, ``z`` in m, stresses in MPa, curvatures in 1/m, moments in
**kN.m** (matching ``hull_girder_strength`` and ``loading_computer``).

The element compression capacities are **not** reimplemented here: the factory
helpers delegate to the validated DNV-RP-C201 solvers in
``structural_analysis`` (``StiffenedPanelBucklingAnalyzer`` /
``PlateBucklingAnalyzer.johnson_ostenfeld``).

References: IACS UR S11A (hull-girder ultimate strength), CSR Common Structural
Rules Pt 1 Ch 5 Sec 4 (Smith incremental-iterative procedure), Smith (1977)
"Influence of local compressive failure on ultimate longitudinal strength of a
ship's hull".
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import List, Optional

CODE_REFERENCE = "IACS UR S11A / Smith method"

# kN.m per (MPa * m^3): sigma[MPa] = 1e6 Pa; M[N.m] = sigma*A*lever; /1e3 -> kN.m.
_KNM_PER_MPA_M3 = 1.0e3

# Default Young's modulus for classification-society marine steel (MPa).
_DEFAULT_E_MPA = 206000.0

# Default fraction of the compressive capacity retained at large shortening for a
# *buckled* element (Smith-style post-buckling decay). 1.0 -> no softening
# (elastic-perfectly-plastic with a buckling cap).
_DEFAULT_POST_BUCKLING_RESIDUAL = 0.5


# ---------------------------------------------------------------------------
# Structural element + its load-shortening curve
# ---------------------------------------------------------------------------
@dataclass
class HullGirderElement:
    """One structural element of the midship cross-section.

    A stiffener-with-attached-plate or a hard-corner plate, lumped at a single
    height ``z_m`` above the baseline.

    ``sigma_u_mpa`` is the *compressive* ultimate (buckling-reduced) stress
    magnitude. In tension the element develops its yield ``fy_mpa``. A
    hard-corner / non-buckling element has ``sigma_u_mpa >= fy_mpa``.
    """

    area_m2: float
    z_m: float  # height above baseline (m)
    fy_mpa: float  # yield strength (MPa)
    sigma_u_mpa: float  # compressive ultimate (MPa), buckling-reduced
    youngs_modulus_mpa: float = _DEFAULT_E_MPA
    post_buckling_residual: float = _DEFAULT_POST_BUCKLING_RESIDUAL
    name: str = ""

    @property
    def buckles(self) -> bool:
        """True if the compressive capacity is below yield (buckling-governed)."""
        return bool(self.sigma_u_mpa < self.fy_mpa * (1.0 - 1e-9))

    @property
    def compressive_capacity_mpa(self) -> float:
        """Compressive plateau stress = min(sigma_u, fy) (MPa, magnitude)."""
        return min(self.sigma_u_mpa, self.fy_mpa)

    def stress(self, strain: float) -> float:
        """Load-shortening stress ``sigma(strain)`` (MPa).

        Positive = tension, negative = compression.

        * Tension (``strain >= 0``): elastic ``E*strain`` capped at ``+fy``.
        * Compression (``strain < 0``): elastic ``-E*|strain|`` until the
          compressive capacity ``min(sigma_u, fy)`` is reached, then
          - non-buckling element: a ``-capacity`` plateau (perfectly plastic);
          - buckling element: a post-buckling decay
            ``-capacity * (r + (1-r) * eps_u/|strain|)`` toward the residual
            fraction ``r`` of the capacity as shortening grows.
        """
        E = self.youngs_modulus_mpa
        if strain >= 0.0:
            return min(E * strain, self.fy_mpa)

        shortening = -strain
        cap = self.compressive_capacity_mpa
        sigma_elastic = E * shortening
        if sigma_elastic <= cap:
            return -sigma_elastic
        if not self.buckles:
            return -cap
        strain_at_cap = cap / E
        r = self.post_buckling_residual
        return -cap * (r + (1.0 - r) * (strain_at_cap / shortening))


# ---------------------------------------------------------------------------
# Element factories — REUSE the validated buckling solvers for sigma_u
# ---------------------------------------------------------------------------
def stiffened_plate_element(
    panel,
    material,
    z_m: float,
    *,
    area_m2: Optional[float] = None,
    post_buckling_residual: float = _DEFAULT_POST_BUCKLING_RESIDUAL,
    name: str = "",
) -> HullGirderElement:
    """Build a Smith element from a stiffened panel + material.

    The compressive ultimate ``sigma_u`` is the governing-mode
    ``critical_stress`` from the validated DNV-RP-C201 stiffened-panel solver
    (:class:`...panel_buckling.StiffenedPanelBucklingAnalyzer`). The element area
    defaults to the solver's effective combined area (plate + stiffener),
    converted mm^2 -> m^2.
    """
    from digitalmodel.structural.structural_analysis.panel_buckling import (
        StiffenedPanelBucklingAnalyzer,
    )

    analyzer = StiffenedPanelBucklingAnalyzer(material)
    result = analyzer.check_panel(panel, sigma_x=material.yield_strength)
    if area_m2 is None:
        area_mm2 = analyzer.effective_section(panel)["area_total"]
        area_m2 = area_mm2 * 1.0e-6
    return HullGirderElement(
        area_m2=area_m2,
        z_m=z_m,
        fy_mpa=material.yield_strength,
        sigma_u_mpa=result.critical_stress,
        youngs_modulus_mpa=material.youngs_modulus,
        post_buckling_residual=post_buckling_residual,
        name=name,
    )


def plate_element(
    plate,
    material,
    z_m: float,
    *,
    area_m2: Optional[float] = None,
    post_buckling_residual: float = _DEFAULT_POST_BUCKLING_RESIDUAL,
    name: str = "",
) -> HullGirderElement:
    """Build a Smith element from an unstiffened plate field + material.

    ``sigma_u`` is the Johnson-Ostenfeld-corrected critical stress from the
    validated DNV-RP-C201 plate solver
    (:meth:`...buckling.PlateBucklingAnalyzer.johnson_ostenfeld`).
    """
    from digitalmodel.structural.structural_analysis.buckling import (
        PlateBucklingAnalyzer,
    )

    analyzer = PlateBucklingAnalyzer(material)
    sigma_u = analyzer.johnson_ostenfeld(analyzer.elastic_buckling_stress(plate))
    if area_m2 is None:
        area_m2 = plate.width * plate.thickness * 1.0e-6
    return HullGirderElement(
        area_m2=area_m2,
        z_m=z_m,
        fy_mpa=material.yield_strength,
        sigma_u_mpa=min(sigma_u, material.yield_strength),
        youngs_modulus_mpa=material.youngs_modulus,
        post_buckling_residual=post_buckling_residual,
        name=name,
    )


def hard_corner_element(
    area_m2: float,
    z_m: float,
    fy_mpa: float,
    *,
    youngs_modulus_mpa: float = _DEFAULT_E_MPA,
    name: str = "",
) -> HullGirderElement:
    """A hard-corner / non-buckling plate element (``sigma_u = fy``)."""
    return HullGirderElement(
        area_m2=area_m2,
        z_m=z_m,
        fy_mpa=fy_mpa,
        sigma_u_mpa=fy_mpa,
        youngs_modulus_mpa=youngs_modulus_mpa,
        name=name,
    )


# ---------------------------------------------------------------------------
# Section geometry helpers (lumped-element)
# ---------------------------------------------------------------------------
def section_centroid_m(elements: List[HullGirderElement]) -> float:
    """Area-weighted centroid height (m)."""
    area = sum(el.area_m2 for el in elements)
    if area <= 0.0:
        raise ValueError("total section area must be positive")
    return sum(el.area_m2 * el.z_m for el in elements) / area


def section_modulus_m3(elements: List[HullGirderElement]) -> float:
    """Lumped elastic section modulus ``Z = I / c`` (m^3).

    ``I`` is the lumped second moment ``sum(A * (z - z_c)^2)`` (each element's own
    inertia is neglected — consistent with the point-area idealisation) and ``c``
    is the largest distance from the centroid to an element.
    """
    z_c = section_centroid_m(elements)
    inertia = sum(el.area_m2 * (el.z_m - z_c) ** 2 for el in elements)
    c = max(abs(el.z_m - z_c) for el in elements)
    if c <= 0.0:
        raise ValueError("section has zero depth")
    return inertia / c


# ---------------------------------------------------------------------------
# Smith march
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class MomentCurvaturePoint:
    """One point on the moment-curvature response."""

    curvature: float  # 1/m (signed: + hogging, - sagging)
    moment_kn_m: float  # signed bending moment (kN.m)
    neutral_axis_m: float  # instantaneous neutral-axis height (m)


@dataclass(frozen=True)
class SmithResult:
    """Result of a Smith incremental-iterative hull-girder ultimate analysis."""

    ultimate_moment_hogging_kn_m: float  # peak +M (deck tension)
    ultimate_moment_sagging_kn_m: float  # peak |M| (deck compression), magnitude
    curvature_at_ult_hogging: float  # 1/m
    curvature_at_ult_sagging: float  # 1/m (negative)
    hogging_curve: List[MomentCurvaturePoint]
    sagging_curve: List[MomentCurvaturePoint]
    code_reference: str = CODE_REFERENCE


def _net_axial_force(elements, curvature, z_na) -> float:
    """Sum of element axial forces (MPa.m^2) at a trial neutral-axis height."""
    total = 0.0
    for el in elements:
        total += el.stress(curvature * (el.z_m - z_na)) * el.area_m2
    return total


def _solve_neutral_axis(
    elements, curvature, z_lo, z_hi, *, n_scan=200, max_iter=80, tol=1e-10
) -> float:
    """Solve ``z_NA`` so the net axial force vanishes, by sign-change scan +
    bisection (robust to the mild non-monotonicity of post-buckling branches).
    """
    f_lo = _net_axial_force(elements, curvature, z_lo)
    if abs(f_lo) < tol:
        return z_lo
    # Scan for the first sign change across the bracket.
    a, fa = z_lo, f_lo
    found = False
    for i in range(1, n_scan + 1):
        b = z_lo + (z_hi - z_lo) * i / n_scan
        fb = _net_axial_force(elements, curvature, b)
        if fa == 0.0:
            return a
        if (fa < 0.0) != (fb < 0.0):
            found = True
            break
        a, fa = b, fb
    if not found:
        # No sign change detected: fall back to the centroid (degenerate).
        return section_centroid_m(elements)
    # Bisection within [a, b].
    for _ in range(max_iter):
        mid = 0.5 * (a + b)
        fm = _net_axial_force(elements, curvature, mid)
        if abs(fm) < tol or (b - a) < tol:
            return mid
        if (fa < 0.0) != (fm < 0.0):
            b = mid
        else:
            a, fa = mid, fm
    return 0.5 * (a + b)


def _moment_kn_m(elements, curvature, z_na) -> float:
    """Bending moment (kN.m) about the neutral axis at a curvature."""
    total = 0.0
    for el in elements:
        lever = el.z_m - z_na
        total += el.stress(curvature * lever) * el.area_m2 * lever
    return _KNM_PER_MPA_M3 * total


def _first_yield_curvature(elements, z_c) -> float:
    """Curvature (1/m) at which the extreme fibre first reaches yield."""
    best = float("inf")
    for el in elements:
        lever = abs(el.z_m - z_c)
        if lever <= 0.0:
            continue
        k = el.fy_mpa / (el.youngs_modulus_mpa * lever)
        if k < best:
            best = k
    if best == float("inf"):
        raise ValueError("section has no element offset from the centroid")
    return best


def _march(elements, curvatures, z_lo, z_hi) -> List[MomentCurvaturePoint]:
    """Build a moment-curvature branch over a list of (signed) curvatures."""
    points: List[MomentCurvaturePoint] = []
    z_c = section_centroid_m(elements)
    for kappa in curvatures:
        if kappa == 0.0:
            points.append(MomentCurvaturePoint(0.0, 0.0, z_c))
            continue
        z_na = _solve_neutral_axis(elements, kappa, z_lo, z_hi)
        moment = _moment_kn_m(elements, kappa, z_na)
        points.append(MomentCurvaturePoint(kappa, moment, z_na))
    return points


def smith_ultimate(
    elements: List[HullGirderElement],
    *,
    max_curvature: Optional[float] = None,
    num_steps: int = 200,
    curvature_factor: float = 8.0,
) -> SmithResult:
    """Run the Smith incremental-iterative march and return the ultimate moments.

    Parameters
    ----------
    elements
        The midship cross-section discretised into :class:`HullGirderElement`.
    max_curvature
        Peak |curvature| (1/m) to march to, for both hogging and sagging. If
        ``None`` it defaults to ``curvature_factor`` times the first-yield
        curvature of the extreme fibre.
    num_steps
        Number of curvature increments per branch (excludes the zero point).
    curvature_factor
        Multiplier on the first-yield curvature used when ``max_curvature`` is
        not given.

    Returns
    -------
    SmithResult
        Hogging and sagging ultimate moments (the moment-curvature peaks), the
        curvatures at which they occur, and the full moment-curvature and
        neutral-axis-migration data for each branch.
    """
    if len(elements) < 2:
        raise ValueError("need at least two elements to form a section")

    z_c = section_centroid_m(elements)
    z_values = [el.z_m for el in elements]
    depth = max(z_values) - min(z_values)
    pad = depth if depth > 0 else 1.0
    z_lo = min(z_values) - pad
    z_hi = max(z_values) + pad

    if max_curvature is None:
        max_curvature = curvature_factor * _first_yield_curvature(elements, z_c)
    max_curvature = abs(max_curvature)

    step = max_curvature / num_steps
    hog_kappas = [step * i for i in range(0, num_steps + 1)]
    sag_kappas = [-step * i for i in range(0, num_steps + 1)]

    hog_curve = _march(elements, hog_kappas, z_lo, z_hi)
    sag_curve = _march(elements, sag_kappas, z_lo, z_hi)

    hog_peak = max(hog_curve, key=lambda p: p.moment_kn_m)
    sag_peak = min(sag_curve, key=lambda p: p.moment_kn_m)

    return SmithResult(
        ultimate_moment_hogging_kn_m=hog_peak.moment_kn_m,
        ultimate_moment_sagging_kn_m=abs(sag_peak.moment_kn_m),
        curvature_at_ult_hogging=hog_peak.curvature,
        curvature_at_ult_sagging=sag_peak.curvature,
        hogging_curve=hog_curve,
        sagging_curve=sag_curve,
    )
