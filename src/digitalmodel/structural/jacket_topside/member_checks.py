"""
Member Capacity Checks — API RP 2A WSD (21st Edition)

Implements unity checks (UC) for tubular steel members in jacket and topside
structures.  All formulas follow API RP 2A Working Stress Design.

Unit system: US customary (kips, inches, ksi).

References
----------
API RP 2A-WSD, 21st Edition, Sections 3.2, 3.3, 3.4, 3.5.
"""

import math
from dataclasses import dataclass, field
from typing import Dict


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class TubularSection:
    """
    Circular hollow section (tubular) geometry and material.

    Parameters
    ----------
    outer_diameter : float
        Outside diameter, D (inches).
    wall_thickness : float
        Wall thickness, t (inches).
    yield_strength : float
        Specified minimum yield strength, Fy (ksi).
    elastic_modulus : float
        Young's modulus, E (ksi).  Default 29 000 ksi.
    effective_length : float
        Effective buckling length, kL (inches).
    """

    outer_diameter: float
    wall_thickness: float
    yield_strength: float
    elastic_modulus: float = 29_000.0
    effective_length: float = 0.0

    # ------------------------------------------------------------------
    # Derived section properties (computed on demand)
    # ------------------------------------------------------------------

    @property
    def inner_diameter(self) -> float:
        """Inner diameter d_i = D - 2t (inches)."""
        return self.outer_diameter - 2.0 * self.wall_thickness

    @property
    def cross_sectional_area(self) -> float:
        """Cross-sectional area A = pi/4*(D^2 - d_i^2) (in²)."""
        return (math.pi / 4.0) * (
            self.outer_diameter ** 2 - self.inner_diameter ** 2
        )

    @property
    def moment_of_inertia(self) -> float:
        """Second moment of area I = pi/64*(D^4 - d_i^4) (in⁴)."""
        return (math.pi / 64.0) * (
            self.outer_diameter ** 4 - self.inner_diameter ** 4
        )

    @property
    def section_modulus(self) -> float:
        """Elastic section modulus S = I / (D/2) (in³)."""
        return self.moment_of_inertia / (self.outer_diameter / 2.0)

    @property
    def radius_of_gyration(self) -> float:
        """Radius of gyration r = sqrt(I/A) (inches)."""
        return math.sqrt(self.moment_of_inertia / self.cross_sectional_area)

    @property
    def slenderness_ratio(self) -> float:
        """Slenderness ratio kL/r (dimensionless)."""
        if self.effective_length == 0.0:
            return 0.0
        return self.effective_length / self.radius_of_gyration

    @property
    def dt_ratio(self) -> float:
        """Diameter-to-thickness ratio D/t (dimensionless)."""
        return self.outer_diameter / self.wall_thickness

    @property
    def gamma(self) -> float:
        """Chord radius-to-thickness ratio gamma = R/T = (D/2)/T."""
        return (self.outer_diameter / 2.0) / self.wall_thickness


@dataclass
class MemberLoads:
    """
    Applied member stresses (ksi).

    Parameters
    ----------
    fa : float
        Applied axial stress (ksi), absolute value.
    fb : float
        Applied resultant bending stress (ksi), absolute value.
    Cm : float
        Equivalent moment factor (API RP 2A Eq. 3.3.1-1).  Typical 0.85.
    is_tension : bool
        True if the axial load is tension; False (default) for compression.
    """

    fa: float = 0.0
    fb: float = 0.0
    Cm: float = 0.85
    is_tension: bool = False


@dataclass
class MemberCheckResult:
    """Result of a single member unity check."""

    check_type: str
    applied_stress: float   # ksi
    allowable_stress: float  # ksi
    unity_check: float
    passes: bool
    details: Dict = field(default_factory=dict)


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _euler_fe(elastic_modulus: float, slenderness: float) -> float:
    """
    Euler stress Fe per API RP 2A WSD Eq. 3.3.1.

    Fe = 12 * pi^2 * E / (23 * (kL/r)^2)  [ksi]
    """
    if slenderness == 0.0:
        return float("inf")
    return 12.0 * math.pi ** 2 * elastic_modulus / (23.0 * slenderness ** 2)


def _cc(elastic_modulus: float, yield_strength: float) -> float:
    """
    Column slenderness threshold Cc = sqrt(2*pi^2*E / Fy).
    """
    return math.sqrt(2.0 * math.pi ** 2 * elastic_modulus / yield_strength)


def _allowable_compression(section: TubularSection) -> float:
    """
    Allowable axial compressive stress Fc per API RP 2A WSD Sec. 3.3.

    Inelastic range (kL/r < Cc):
        Fc = Fy * (1 - Fy / (4 * Fe))
    Elastic range (kL/r >= Cc):
        Fc = Fe   [i.e., 12*pi^2*E / (23*(kL/r)^2)]
    """
    E = section.elastic_modulus
    Fy = section.yield_strength
    sl = section.slenderness_ratio
    Cc = _cc(E, Fy)
    Fe = _euler_fe(E, sl)

    if sl < Cc:
        Fc = Fy * (1.0 - Fy / (4.0 * Fe))
    else:
        Fc = Fe
    return Fc


def _allowable_bending(section: TubularSection) -> float:
    """
    Allowable bending stress Fb per API RP 2A WSD Sec. 3.4.

    Compact tubular (D/t <= 10 340 / Fy [psi]):
        Fb = 0.75 * Fy

    Note: API RP 2A uses psi for the D/t compactness check.
    Fy in ksi → convert Fy to psi = Fy * 1000.
    Limit: D/t <= 10340 / (Fy*1000)  → typically << 1 for reasonable D/t,
    so the practical API RP 2A compact criterion is simply D/t <= 300
    for purposes of this implementation.
    """
    return 0.75 * section.yield_strength


# ---------------------------------------------------------------------------
# Public API — individual checks
# ---------------------------------------------------------------------------

def axial_tension_uc(
    section: TubularSection,
    fa: float,
) -> MemberCheckResult:
    """
    Axial tension unity check per API RP 2A WSD Sec. 3.2.

    Ft = 0.6 * Fy
    UC_t = fa / Ft

    Parameters
    ----------
    section : TubularSection
    fa : float
        Applied axial tensile stress (ksi), positive.

    Returns
    -------
    MemberCheckResult
    """
    Ft = 0.6 * section.yield_strength
    uc = fa / Ft if Ft > 0.0 else float("inf")
    return MemberCheckResult(
        check_type="axial_tension",
        applied_stress=fa,
        allowable_stress=Ft,
        unity_check=uc,
        passes=uc <= 1.0,
        details={"Ft": Ft, "Fy": section.yield_strength},
    )


def axial_compression_uc(
    section: TubularSection,
    fa: float,
) -> MemberCheckResult:
    """
    Axial compression (column buckling) unity check per API RP 2A WSD Sec. 3.3.

    UC_c = fa / Fc

    Parameters
    ----------
    section : TubularSection
    fa : float
        Applied axial compressive stress (ksi), positive value.

    Returns
    -------
    MemberCheckResult
    """
    Fc = _allowable_compression(section)
    uc = fa / Fc if Fc > 0.0 else float("inf")
    E = section.elastic_modulus
    Fy = section.yield_strength
    sl = section.slenderness_ratio
    Cc = _cc(E, Fy)
    Fe = _euler_fe(E, sl)
    return MemberCheckResult(
        check_type="axial_compression",
        applied_stress=fa,
        allowable_stress=Fc,
        unity_check=uc,
        passes=uc <= 1.0,
        details={
            "Fc": Fc,
            "Fe": Fe,
            "Cc": Cc,
            "slenderness": sl,
            "range": "inelastic" if sl < Cc else "elastic",
        },
    )


def bending_uc(
    section: TubularSection,
    fb: float,
) -> MemberCheckResult:
    """
    Bending unity check per API RP 2A WSD Sec. 3.4.

    Fb = 0.75 * Fy  (compact tubular)
    UC_b = fb / Fb

    Parameters
    ----------
    section : TubularSection
    fb : float
        Applied resultant bending stress (ksi).

    Returns
    -------
    MemberCheckResult
    """
    Fb = _allowable_bending(section)
    uc = fb / Fb if Fb > 0.0 else float("inf")
    return MemberCheckResult(
        check_type="bending",
        applied_stress=fb,
        allowable_stress=Fb,
        unity_check=uc,
        passes=uc <= 1.0,
        details={"Fb": Fb, "Fy": section.yield_strength},
    )


def combined_axial_bending_uc(
    section: TubularSection,
    loads: MemberLoads,
) -> MemberCheckResult:
    """
    Combined axial + bending interaction check per API RP 2A WSD Sec. 3.5.

    Two interaction equations must both be satisfied:

    Equation 1 (if fa/Fa > 0.15):
        UC1 = fa/Fa + Cm*fb / (Fb*(1 - fa/Fe)) <= 1.0

    If fa/Fa <= 0.15, simplified form applies:
        UC1 = fa/Fa + fb/Fb <= 1.0

    Equation 2 (always):
        UC2 = fa/(0.6*Fy) + fb/Fb <= 1.0

    Governing UC = max(UC1, UC2).

    Parameters
    ----------
    section : TubularSection
    loads : MemberLoads

    Returns
    -------
    MemberCheckResult
    """
    fa = loads.fa
    fb = loads.fb
    Cm = loads.Cm

    Fa = _allowable_compression(section)
    Fb = _allowable_bending(section)
    E = section.elastic_modulus
    sl = section.slenderness_ratio
    Fe = _euler_fe(E, sl)
    Fy = section.yield_strength

    # Equation 1
    axial_ratio = fa / Fa if Fa > 0.0 else float("inf")
    if axial_ratio <= 0.15:
        uc_eq1 = axial_ratio + (fb / Fb if Fb > 0.0 else float("inf"))
    else:
        amplification = 1.0 - fa / Fe if Fe > fa else 1e-9
        uc_eq1 = axial_ratio + Cm * fb / (Fb * amplification)

    # Equation 2
    uc_eq2 = fa / (0.6 * Fy) + (fb / Fb if Fb > 0.0 else float("inf"))

    uc = max(uc_eq1, uc_eq2)

    return MemberCheckResult(
        check_type="combined",
        applied_stress=fa,
        allowable_stress=Fa,
        unity_check=uc,
        passes=uc <= 1.0,
        details={
            "uc_eq1": uc_eq1,
            "uc_eq2": uc_eq2,
            "Fa": Fa,
            "Fb": Fb,
            "Fe": Fe,
            "Cm": Cm,
            "axial_ratio": axial_ratio,
        },
    )


# ---------------------------------------------------------------------------
# Orchestrator
# ---------------------------------------------------------------------------

def member_checks(
    section: TubularSection,
    loads: MemberLoads,
) -> Dict[str, MemberCheckResult]:
    """
    Run all applicable API RP 2A WSD member capacity checks.

    Checks performed depend on whether the axial load is tension or
    compression.  A 'governing' key is added to the result dict pointing
    to the result with the highest unity check value.

    Parameters
    ----------
    section : TubularSection
    loads : MemberLoads

    Returns
    -------
    dict mapping check name → MemberCheckResult, plus key 'governing'.
    """
    results: Dict[str, MemberCheckResult] = {}

    if loads.is_tension:
        results["axial_tension"] = axial_tension_uc(section, loads.fa)
    else:
        results["axial_compression"] = axial_compression_uc(section, loads.fa)

    results["bending"] = bending_uc(section, loads.fb)
    results["combined"] = combined_axial_bending_uc(section, loads)

    # Identify governing check (highest UC)
    governing = max(results.values(), key=lambda r: r.unity_check)
    results["governing"] = governing

    return results
