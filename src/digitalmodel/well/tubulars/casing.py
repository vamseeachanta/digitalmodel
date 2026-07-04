# ABOUTME: API 5CT casing/tubing product catalog with API 5C3 performance
# ABOUTME: ratings — collapse (4-regime), internal yield (burst), body yield.
"""API 5CT casing & tubing as a product catalog with API 5C3 ratings.

A casing string is a *product*: a standard OD and nominal weight (ppf) — which
fix the wall thickness — plus an API 5CT grade.  This module crosses the
dimensional catalog with the casing grades and computes the API 5C3 / API TR 5C3
performance ratings:

* **Internal yield (burst)** ``P_iy = 0.875 * 2 * Yp * t / D`` (the 0.875 covers
  the 12.5% wall tolerance).
* **Pipe-body yield (axial)** ``F_y = Yp * A`` over the metal area.
* **Collapse** by the four API 5C3 regimes (yield / plastic / transition /
  elastic) selected on ``D/t`` with the grade-dependent A/B/C/F/G constants.

A :class:`Casing` computes the very ``burst_rating_psi`` / ``collapse_rating_psi``
that :class:`~digitalmodel.well.tubulars.design_envelope.TubularGeometry` (and
the VME / API-ellipse design envelopes) previously required as hand-entered API
table values — see :meth:`Casing.to_tubular_geometry`.

Validated against published API ratings (e.g. 7" 26# P110: collapse 6230 psi,
burst 9960 psi, body yield 830,000 lbf).

Standards: API 5CT (casing/tubing), API 5C3 / API TR 5C3 (performance).
"""
from __future__ import annotations

import math
from dataclasses import dataclass
from typing import TYPE_CHECKING, Optional

if TYPE_CHECKING:
    from digitalmodel.well.tubulars.design_envelope import TubularGeometry

CODE_REFERENCE = "API 5CT / API 5C3"

# Elastic-collapse numerator constant (API 5C3), psi.
_ELASTIC_K = 46.95e6


# ---------------------------------------------------------------------------
# Grades (API 5CT) — yield has a min AND a max; tensile is a minimum.
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class CasingGrade:
    """An API 5CT casing/tubing grade (strengths in psi)."""

    name: str
    min_yield_psi: float
    max_yield_psi: float
    min_tensile_psi: float
    standard: str = "API 5CT"


#: API 5CT grades (min/max yield, min tensile in psi).
API_5CT_GRADES: dict[str, CasingGrade] = {
    "H40": CasingGrade("H40", 40_000, 80_000, 60_000),
    "J55": CasingGrade("J55", 55_000, 80_000, 75_000),
    "K55": CasingGrade("K55", 55_000, 80_000, 95_000),
    "N80": CasingGrade("N80", 80_000, 110_000, 100_000),
    "L80": CasingGrade("L80", 80_000, 95_000, 95_000),
    "C90": CasingGrade("C90", 90_000, 105_000, 100_000),
    "C95": CasingGrade("C95", 95_000, 110_000, 105_000),
    "T95": CasingGrade("T95", 95_000, 110_000, 105_000),
    "P110": CasingGrade("P110", 110_000, 140_000, 125_000),
    "Q125": CasingGrade("Q125", 125_000, 150_000, 135_000),
}


def casing_grade(name: str) -> CasingGrade:
    """Look up an API 5CT grade by name (case-insensitive)."""
    key = name.strip().upper()
    if key not in API_5CT_GRADES:
        raise KeyError(f"Unknown API 5CT grade: {name!r}")
    return API_5CT_GRADES[key]


# ---------------------------------------------------------------------------
# Dimensions: (OD in, nominal weight ppf) -> wall thickness (in), API 5CT.
# Nominal weight includes the coupling, so it differs slightly from the
# plain-end weight; the wall thickness is the governing dimension.
# ---------------------------------------------------------------------------
CASING_SIZES: dict[tuple[float, float], float] = {
    (4.5, 9.50): 0.205, (4.5, 11.60): 0.250, (4.5, 13.50): 0.290,
    (4.5, 15.10): 0.337,
    (5.0, 11.50): 0.220, (5.0, 13.00): 0.253, (5.0, 15.00): 0.296,
    (5.0, 18.00): 0.362, (5.0, 21.40): 0.437, (5.0, 23.20): 0.478,
    (5.5, 14.00): 0.244, (5.5, 15.50): 0.275, (5.5, 17.00): 0.304,
    (5.5, 20.00): 0.361, (5.5, 23.00): 0.415,
    (7.0, 17.00): 0.231, (7.0, 20.00): 0.272, (7.0, 23.00): 0.317,
    (7.0, 26.00): 0.362, (7.0, 29.00): 0.408, (7.0, 32.00): 0.453,
    (7.0, 35.00): 0.498, (7.0, 38.00): 0.540,
    (7.625, 24.00): 0.300, (7.625, 26.40): 0.328, (7.625, 29.70): 0.375,
    (7.625, 33.70): 0.430, (7.625, 39.00): 0.500,
    (9.625, 36.00): 0.352, (9.625, 40.00): 0.395, (9.625, 43.50): 0.435,
    (9.625, 47.00): 0.472, (9.625, 53.50): 0.545,
    (10.75, 40.50): 0.350, (10.75, 45.50): 0.400, (10.75, 51.00): 0.450,
    (10.75, 55.50): 0.495, (10.75, 60.70): 0.545, (10.75, 65.70): 0.595,
    (13.375, 48.00): 0.330, (13.375, 54.50): 0.380, (13.375, 61.00): 0.430,
    (13.375, 68.00): 0.480, (13.375, 72.00): 0.514,
    (16.0, 65.00): 0.375, (16.0, 75.00): 0.438, (16.0, 84.00): 0.495,
    (16.0, 109.00): 0.656,
    (20.0, 94.00): 0.438, (20.0, 106.50): 0.500, (20.0, 133.00): 0.635,
}


# ---------------------------------------------------------------------------
# Tubing dimensions: (OD in, nominal weight ppf) -> wall thickness (in).
# Standard API 5CT tubing sizes (API 5CT tubing dimension tables; walls match
# the published API tubing tables, e.g. 2-3/8" 4.7# -> 0.190" (ID 1.995"),
# 2-7/8" 6.5# -> 0.217" (ID 2.441"), 3-1/2" 9.3# -> 0.254" (ID 2.992"),
# 4" 11.0# -> 0.262" (ID 3.476"), 4-1/2" 12.75# -> 0.271" (ID 3.958")).
# Kept separate from CASING_SIZES: 4-1/2" exists in both catalogs with
# different standard weights.
# ---------------------------------------------------------------------------
TUBING_SIZES: dict[tuple[float, float], float] = {
    (2.375, 4.70): 0.190, (2.375, 5.95): 0.254,
    (2.875, 6.50): 0.217, (2.875, 8.70): 0.308,
    (3.5, 9.30): 0.254, (3.5, 12.95): 0.375,
    (4.0, 11.00): 0.262,
    (4.5, 12.75): 0.271,
}


# ---------------------------------------------------------------------------
# API 5C3 performance formulas
# ---------------------------------------------------------------------------
def internal_yield_pressure(od_in: float, wt_in: float,
                            min_yield_psi: float) -> float:
    """API 5C3 internal yield (burst) pressure, psi (0.875 wall factor)."""
    return 0.875 * 2.0 * min_yield_psi * wt_in / od_in


def pipe_body_yield_strength(od_in: float, wt_in: float,
                             min_yield_psi: float) -> float:
    """API 5C3 pipe-body yield strength in axial tension, lbf."""
    id_in = od_in - 2.0 * wt_in
    area = math.pi / 4.0 * (od_in ** 2 - id_in ** 2)
    return min_yield_psi * area


def _collapse_constants(yp: float) -> tuple[float, float, float, float, float]:
    """API 5C3 empirical collapse constants A, B, C, F, G for yield ``yp``."""
    a = (2.8762 + 0.10679e-5 * yp + 0.21301e-10 * yp ** 2
         - 0.53132e-16 * yp ** 3)
    b = 0.026233 + 0.50609e-6 * yp
    c = (-465.93 + 0.030867 * yp - 0.10483e-7 * yp ** 2
         + 0.36989e-13 * yp ** 3)
    ba = b / a
    f = (_ELASTIC_K * (3.0 * ba / (2.0 + ba)) ** 3
         / (yp * (3.0 * ba / (2.0 + ba) - ba)
            * (1.0 - 3.0 * ba / (2.0 + ba)) ** 2))
    g = f * ba
    return a, b, c, f, g


def collapse_pressure(od_in: float, wt_in: float,
                      min_yield_psi: float) -> tuple[float, str]:
    """API 5C3 collapse pressure (psi) and the governing regime.

    Selects yield / plastic / transition / elastic collapse by D/t.
    """
    dt = od_in / wt_in
    yp = min_yield_psi
    a, b, c, f, g = _collapse_constants(yp)

    # Regime boundaries on D/t.
    bc = b + c / yp
    dt_yp = (((a - 2.0) ** 2 + 8.0 * bc) ** 0.5 + (a - 2.0)) / (2.0 * bc)
    dt_pt = yp * (a - f) / (c + yp * (b - g))
    ba = b / a
    dt_te = (2.0 + ba) / (3.0 * ba)

    if dt <= dt_yp:
        p = 2.0 * yp * ((dt - 1.0) / dt ** 2)
        regime = "yield"
    elif dt <= dt_pt:
        p = yp * (a / dt - b) - c
        regime = "plastic"
    elif dt <= dt_te:
        p = yp * (f / dt - g)
        regime = "transition"
    else:
        p = _ELASTIC_K / (dt * (dt - 1.0) ** 2)
        regime = "elastic"
    return p, regime


# ---------------------------------------------------------------------------
# Casing product object
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class Casing:
    """A specific API 5CT casing/tubing product with API 5C3 ratings."""

    od_in: float
    wt_in: float
    grade: CasingGrade
    weight_ppf: Optional[float] = None
    code_reference: str = CODE_REFERENCE

    def __post_init__(self) -> None:
        if self.od_in <= 0 or self.wt_in <= 0 or self.wt_in >= self.od_in / 2.0:
            raise ValueError(
                f"invalid geometry od={self.od_in}, wt={self.wt_in}")

    @property
    def id_in(self) -> float:
        return self.od_in - 2.0 * self.wt_in

    @property
    def d_over_t(self) -> float:
        return self.od_in / self.wt_in

    @property
    def metal_area_in2(self) -> float:
        return math.pi / 4.0 * (self.od_in ** 2 - self.id_in ** 2)

    @property
    def burst_psi(self) -> float:
        """API 5C3 internal yield (burst) rating, psi."""
        return internal_yield_pressure(self.od_in, self.wt_in,
                                       self.grade.min_yield_psi)

    @property
    def body_yield_lbf(self) -> float:
        """API 5C3 pipe-body yield strength (axial tension), lbf."""
        return pipe_body_yield_strength(self.od_in, self.wt_in,
                                        self.grade.min_yield_psi)

    @property
    def collapse_psi(self) -> float:
        """API 5C3 collapse rating, psi."""
        return collapse_pressure(self.od_in, self.wt_in,
                                 self.grade.min_yield_psi)[0]

    @property
    def collapse_regime(self) -> str:
        """The governing API 5C3 collapse regime."""
        return collapse_pressure(self.od_in, self.wt_in,
                                 self.grade.min_yield_psi)[1]

    @property
    def reference(self) -> str:
        size = f"{self.od_in:g}\""
        wt = f" {self.weight_ppf:g}#" if self.weight_ppf is not None else ""
        return f"API 5CT {self.grade.name} casing, {size}{wt}"

    def to_tubular_geometry(self) -> "TubularGeometry":
        """Build a :class:`TubularGeometry` carrying the computed API ratings.

        Bridges to the triaxial / VME / API-ellipse design envelopes, which
        otherwise require the burst and collapse ratings to be looked up by hand.
        """
        from digitalmodel.well.tubulars.design_envelope import TubularGeometry

        return TubularGeometry(
            od_in=self.od_in,
            id_in=self.id_in,
            smys_psi=self.grade.min_yield_psi,
            burst_rating_psi=self.burst_psi,
            collapse_rating_psi=self.collapse_psi,
        )


# ---------------------------------------------------------------------------
# Catalog query API
# ---------------------------------------------------------------------------
def list_sizes() -> list[tuple[float, float]]:
    """All catalog (OD, weight ppf) pairs, sorted."""
    return sorted(CASING_SIZES)


def wall_for_size(od_in: float, weight_ppf: float) -> float:
    """Wall thickness (in) for a catalog (OD, weight) pair."""
    key = (od_in, weight_ppf)
    if key not in CASING_SIZES:
        raise KeyError(f"No catalog wall for OD {od_in}, weight {weight_ppf}")
    return CASING_SIZES[key]


def casing(
    grade: "str | CasingGrade",
    *,
    od_in: float,
    weight_ppf: Optional[float] = None,
    wt_in: Optional[float] = None,
) -> Casing:
    """Build a :class:`Casing` from a grade plus OD and weight (or wall).

    Args:
        grade: API 5CT grade name or :class:`CasingGrade`.
        od_in: Outside diameter (inches).
        weight_ppf: Nominal weight (lb/ft); looks up the catalog wall.
        wt_in: Explicit wall thickness (inches), instead of ``weight_ppf``.
    """
    g = grade if isinstance(grade, CasingGrade) else casing_grade(grade)
    if weight_ppf is not None and wt_in is not None:
        raise ValueError("Specify either weight_ppf or wt_in, not both")
    if weight_ppf is not None:
        wt = wall_for_size(od_in, weight_ppf)
    elif wt_in is not None:
        wt = wt_in
    else:
        raise ValueError("Provide weight_ppf or wt_in for the wall thickness")
    return Casing(od_in=od_in, wt_in=wt, grade=g, weight_ppf=weight_ppf)


def list_tubing_sizes() -> list[tuple[float, float]]:
    """All tubing-catalog (OD, weight ppf) pairs, sorted."""
    return sorted(TUBING_SIZES)


def tubing_wall_for_size(od_in: float, weight_ppf: float) -> float:
    """Wall thickness (in) for a tubing-catalog (OD, weight) pair."""
    key = (od_in, weight_ppf)
    if key not in TUBING_SIZES:
        raise KeyError(
            f"No tubing catalog wall for OD {od_in}, weight {weight_ppf}")
    return TUBING_SIZES[key]


def tubing(
    grade: "str | CasingGrade",
    *,
    od_in: float,
    weight_ppf: Optional[float] = None,
    wt_in: Optional[float] = None,
) -> Casing:
    """Build a tubing product (a :class:`Casing`) from grade, OD and weight.

    Tubing shares the API 5CT grades and the API 5C3 rating machinery with
    casing — only the dimensional catalog differs — so this returns the same
    :class:`Casing` product class with the wall looked up in
    :data:`TUBING_SIZES`.

    Args:
        grade: API 5CT grade name or :class:`CasingGrade`.
        od_in: Outside diameter (inches).
        weight_ppf: Nominal weight (lb/ft); looks up the tubing-catalog wall.
        wt_in: Explicit wall thickness (inches), instead of ``weight_ppf``.
    """
    g = grade if isinstance(grade, CasingGrade) else casing_grade(grade)
    if weight_ppf is not None and wt_in is not None:
        raise ValueError("Specify either weight_ppf or wt_in, not both")
    if weight_ppf is not None:
        wt = tubing_wall_for_size(od_in, weight_ppf)
    elif wt_in is not None:
        wt = wt_in
    else:
        raise ValueError("Provide weight_ppf or wt_in for the wall thickness")
    return Casing(od_in=od_in, wt_in=wt, grade=g, weight_ppf=weight_ppf)
