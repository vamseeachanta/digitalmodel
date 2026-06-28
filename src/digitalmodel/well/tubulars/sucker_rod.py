# ABOUTME: API 11B sucker-rod product catalog — grades, sizes, and the
# ABOUTME: modified-Goodman allowable-stress fatigue check for rod-string design.
"""API 11B sucker rods as a product catalog with a modified-Goodman check.

A sucker rod is a *product*: a nominal diameter (eighths of an inch) and an
API 11B grade.  The fatigue limit for a rod cycling between a minimum and a peak
polished-rod load is the **modified Goodman** allowable stress (API 11BR):

    SA = (T / 4 + 0.5625 * Smin) * SF

where ``T`` is the rod minimum tensile strength, ``Smin`` the minimum stress in
the cycle, ``SF`` the service factor (1.0 non-corrosive; lower for sour / H2S
service), and ``SA`` the maximum allowable stress.  The rod is acceptable when
the peak stress ``Smax <= SA``.

A :class:`SuckerRod` ties into the dynacard rod-buckling workflow via
:meth:`SuckerRod.to_rod_section` (it carries the correct ``minimum_tensile`` for
the grade, which the generic ``RodSection`` default does not).

Standards: API 11B (sucker rods), API 11BR (recommended practice), modified
Goodman diagram.
"""
from __future__ import annotations

import math
from dataclasses import dataclass

CODE_REFERENCE = "API 11B / modified Goodman"


# ---------------------------------------------------------------------------
# Grades (API 11B) — minimum tensile governs the modified-Goodman limit.
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class RodGrade:
    """An API 11B sucker-rod grade (tensile strengths in psi)."""

    name: str
    min_tensile_psi: float
    max_tensile_psi: float
    description: str = ""


#: API 11B sucker-rod grades.
API_11B_ROD_GRADES: dict[str, RodGrade] = {
    "C": RodGrade("C", 90_000, 115_000, "AISI 1536 carbon; medium loads"),
    "K": RodGrade("K", 85_000, 115_000, "AISI 4623 alloy; corrosive service"),
    "D": RodGrade("D", 115_000, 140_000, "carbon/alloy; high loads"),
}


def rod_grade(name: str) -> RodGrade:
    """Look up an API 11B rod grade by name (case-insensitive)."""
    key = name.strip().upper()
    if key not in API_11B_ROD_GRADES:
        raise KeyError(f"Unknown API 11B rod grade: {name!r}")
    return API_11B_ROD_GRADES[key]


# ---------------------------------------------------------------------------
# Sizes (API 11B) — nominal diameter in inches (eighths).
# ---------------------------------------------------------------------------
#: Standard API 11B rod nominal diameters (inches).
ROD_SIZES_IN: tuple[float, ...] = (0.5, 0.625, 0.75, 0.875, 1.0, 1.125, 1.25)


def rod_area(nominal_in: float) -> float:
    """Cross-sectional metal area of a rod of given nominal diameter (in^2)."""
    return math.pi / 4.0 * nominal_in ** 2


# ---------------------------------------------------------------------------
# Modified-Goodman allowable stress
# ---------------------------------------------------------------------------
def modified_goodman_allowable(
    min_tensile_psi: float, min_stress_psi: float, service_factor: float = 1.0,
) -> float:
    """API 11BR modified-Goodman maximum allowable stress, psi.

    ``SA = (T/4 + 0.5625 * Smin) * SF``.
    """
    return (min_tensile_psi / 4.0 + 0.5625 * min_stress_psi) * service_factor


# ---------------------------------------------------------------------------
# Sucker-rod product object
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class SuckerRod:
    """A specific API 11B sucker rod with a modified-Goodman fatigue check."""

    nominal_in: float
    grade: RodGrade
    service_factor: float = 1.0
    code_reference: str = CODE_REFERENCE

    def __post_init__(self) -> None:
        if self.nominal_in <= 0:
            raise ValueError(f"nominal_in must be > 0, got {self.nominal_in}")
        if not 0.0 < self.service_factor <= 1.0:
            raise ValueError(
                f"service_factor must be in (0, 1], got {self.service_factor}")

    @property
    def area_in2(self) -> float:
        """Cross-sectional metal area (in^2)."""
        return rod_area(self.nominal_in)

    def stress_psi(self, load_lbf: float) -> float:
        """Axial stress (psi) for an axial load (lbf)."""
        return load_lbf / self.area_in2

    def allowable_stress_psi(self, min_stress_psi: float) -> float:
        """Modified-Goodman maximum allowable peak stress (psi)."""
        return modified_goodman_allowable(
            self.grade.min_tensile_psi, min_stress_psi, self.service_factor)

    def goodman_check(self, peak_load_lbf: float, min_load_lbf: float) -> dict:
        """Modified-Goodman check for a load cycle.

        Returns the min/max/allowable stresses, the loading (utilisation =
        Smax / SA) and a ``passes`` flag.
        """
        s_max = self.stress_psi(peak_load_lbf)
        s_min = self.stress_psi(min_load_lbf)
        s_allow = self.allowable_stress_psi(s_min)
        util = s_max / s_allow if s_allow > 0 else float("inf")
        return {
            "max_stress_psi": s_max,
            "min_stress_psi": s_min,
            "allowable_stress_psi": s_allow,
            "loading": util,           # fraction of max allowable
            "passes": bool(s_max <= s_allow),
            "code_reference": self.code_reference,
        }

    def to_rod_section(self, length_ft: float, count: int = 1):
        """Build a dynacard ``RodSection`` carrying this grade's tensile.

        Ties the catalog into the rod-buckling workflow (the generic
        ``RodSection`` default tensile does not reflect the API 11B grade).
        """
        from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
            RodSection,
        )

        return RodSection(
            diameter=self.nominal_in,
            length=length_ft,
            count=count,
            minimum_tensile=self.grade.min_tensile_psi,
            grade=self.grade.name,
        )


def sucker_rod(
    grade: "str | RodGrade", nominal_in: float, *, service_factor: float = 1.0,
) -> SuckerRod:
    """Build a :class:`SuckerRod` from a grade and nominal diameter."""
    g = grade if isinstance(grade, RodGrade) else rod_grade(grade)
    return SuckerRod(nominal_in=nominal_in, grade=g,
                     service_factor=service_factor)


# ---------------------------------------------------------------------------
# Tapered rod string
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class RodTaperSection:
    """One section of a tapered rod string (top-down)."""

    rod: SuckerRod
    length_ft: float


def rod_string_goodman(
    sections: "list[tuple[RodTaperSection, float, float]]",
) -> list[dict]:
    """Modified-Goodman check for each section of a tapered string.

    Args:
        sections: list of ``(RodTaperSection, peak_load_lbf, min_load_lbf)`` —
            the peak/min polished-rod-derived loads carried by that section
            (highest at the top taper, decreasing downward).

    Returns:
        Per-section Goodman result dicts (see :meth:`SuckerRod.goodman_check`),
        each annotated with the section ``nominal_in`` and ``length_ft``.
    """
    results = []
    for section, peak, low in sections:
        res = section.rod.goodman_check(peak, low)
        res["nominal_in"] = section.rod.nominal_in
        res["length_ft"] = section.length_ft
        results.append(res)
    return results
