# ABOUTME: MaterialGrade record + reconciled registry of steel grade strengths
# ABOUTME: with sourced values and derivable back-compat adapters for legacy dicts.
"""Canonical material-grade matrix.

Strengths are stored in **MPa** (SI).  For API 5L the stored ``smts_mpa`` is the
PSL2 minimum tensile strength; ``smts_psl1_mpa`` carries the (slightly lower)
PSL1 / US-customary minimum where it differs — this is the origin of the legacy
"X52 = 455 vs 460 MPa" disagreement (PSL1 vs PSL2).

Sources:
  * API 5L (46th ed.) / ISO 3183 — line-pipe grade strengths (Tables 4/5).
  * IACS UR W11 (Rev.) — Normal & Higher-Strength Hull Structural Steels.
  * EN 10025-2 — hot-rolled structural steel (t <= 16 mm class).
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Optional

# Unit conversion (exact-enough engineering constant).
PSI_PER_MPA = 145.0377
MPA_PER_PSI = 1.0 / PSI_PER_MPA

# Per-standard elastic defaults (MPa), Poisson ratio, density (kg/m^3).
_E_LINE_PIPE = 207_000.0   # API 5L line pipe (207 GPa)
_E_MARINE = 206_000.0      # IACS hull steel (206 GPa)
_E_STRUCTURAL = 210_000.0  # EN 10025 structural (210 GPa)
_NU = 0.30
_RHO = 7850.0


@dataclass(frozen=True)
class MaterialGrade:
    """One reconciled material grade.

    Attributes:
        name: Canonical registry key (e.g. ``"X52"``, ``"AH36"``, ``"S355"``).
        standard: Governing standard (``"API 5L"`` / ``"IACS UR W11"`` /
            ``"EN 10025-2"``).
        smys_mpa: Specified minimum yield strength (MPa).
        smts_mpa: Specified minimum tensile strength (MPa); PSL2 for API 5L.
        E_mpa: Young's modulus (MPa).
        nu: Poisson's ratio.
        rho_kg_m3: Density (kg/m^3).
        designation: Equivalent designation (ISO 3183 L-grade or alias).
        grade_ksi: SMYS in ksi for API 5L (the X-number); ``None`` otherwise.
        toughness_grade: Charpy test-temperature class for hull steel
            (``A`` 20C / ``B`` 0C / ``D`` -20C / ``E`` -40C / ``F`` -60C).
        smts_psl1_mpa: API 5L PSL1 minimum tensile strength (MPa) where it
            differs from PSL2; ``None`` for non-API or PSL2-only grades.
        source: Citation for the strength values.
        notes: Free-form reconciliation notes.
    """

    name: str
    standard: str
    smys_mpa: float
    smts_mpa: float
    E_mpa: float = _E_LINE_PIPE
    nu: float = _NU
    rho_kg_m3: float = _RHO
    designation: Optional[str] = None
    grade_ksi: Optional[float] = None
    toughness_grade: Optional[str] = None
    smts_psl1_mpa: Optional[float] = None
    source: str = ""
    notes: str = ""

    @property
    def smys_psi(self) -> float:
        """Yield strength in psi (MPa -> psi)."""
        return self.smys_mpa * PSI_PER_MPA

    @property
    def smts_psi(self) -> float:
        """Tensile strength in psi (MPa -> psi)."""
        return self.smts_mpa * PSI_PER_MPA


# ---------------------------------------------------------------------------
# Registry construction
# ---------------------------------------------------------------------------
_API_5L_SRC = "API 5L (46th ed.) / ISO 3183, line-pipe grade strengths"
_IACS_SRC = "IACS UR W11, Normal & Higher-Strength Hull Structural Steels"
_EN_SRC = "EN 10025-2, structural steel (t <= 16 mm)"


def _api5l(name, ksi, smys, smts2, smts1, iso, *, note=""):
    return MaterialGrade(
        name=name, standard="API 5L", smys_mpa=smys, smts_mpa=smts2,
        E_mpa=_E_LINE_PIPE, designation=iso, grade_ksi=ksi,
        smts_psl1_mpa=smts1, source=_API_5L_SRC, notes=note)


def _hull(name, smys, smts, tough, *, note=""):
    return MaterialGrade(
        name=name, standard="IACS UR W11", smys_mpa=smys, smts_mpa=smts,
        E_mpa=_E_MARINE, toughness_grade=tough, source=_IACS_SRC, notes=note)


def _en(name, smys, smts):
    return MaterialGrade(
        name=name, standard="EN 10025-2", smys_mpa=smys, smts_mpa=smts,
        E_mpa=_E_STRUCTURAL, source=_EN_SRC)


# API 5L / ISO 3183 — SMYS = ISO L-grade (MPa); SMTS = PSL2 min, PSL1 in 5th col.
# US-customary (ksi) SMYS values differ by ~1-2% on the low grades; the grade
# number (grade_ksi) preserves that designation for psi back-compat.
_API_5L = [
    _api5l("A25", 25, 175.0, 310.0, 310.0, "L175",
           note="PSL1 only; D <= 141.3 mm. US 25 ksi = 172 MPa."),
    _api5l("A", 30, 210.0, 335.0, 335.0, "L210", note="US 30 ksi = 207 MPa."),
    _api5l("B", 35, 245.0, 415.0, 415.0, "L245", note="US 35 ksi = 241 MPa."),
    _api5l("X42", 42, 290.0, 415.0, 414.0, "L290"),
    _api5l("X46", 46, 320.0, 435.0, 434.0, "L320"),
    _api5l("X52", 52, 360.0, 460.0, 455.0, "L360",
           note="Legacy 358/359 MPa = US 52 ksi; PSL1 SMTS 455, PSL2 460."),
    _api5l("X56", 56, 390.0, 490.0, 489.0, "L390"),
    _api5l("X60", 60, 415.0, 520.0, 517.0, "L415"),
    _api5l("X65", 65, 450.0, 535.0, 530.0, "L450"),
    _api5l("X70", 70, 485.0, 570.0, 565.0, "L485"),
    _api5l("X80", 80, 555.0, 625.0, 620.0, "L555"),
    _api5l("X90", 90, 625.0, 695.0, None, "L625", note="PSL2 only."),
    _api5l("X100", 100, 690.0, 760.0, None, "L690", note="PSL2 only."),
    _api5l("X120", 120, 830.0, 915.0, None, "L830", note="PSL2 only."),
]

# IACS UR W11 — normal strength (235 MPa yield, Rm 400-520, min 400).
_HULL_NS = [
    _hull("Grade A", 235.0, 400.0, "A", note="Charpy at +20 C."),
    _hull("Grade B", 235.0, 400.0, "B", note="Charpy at 0 C."),
    _hull("Grade D", 235.0, 400.0, "D", note="Charpy at -20 C."),
    _hull("Grade E", 235.0, 400.0, "E", note="Charpy at -40 C."),
]

# IACS UR W11 — higher strength: 32 -> 315, 36 -> 355, 40 -> 390 MPa yield.
# Tensile minima: 32 -> 440, 36 -> 490, 40 -> 510 MPa.  The leading letter is
# the toughness (test-temperature) class and does not change the strength.
_HULL_HS_LEVELS = [(32, 315.0, 440.0), (36, 355.0, 490.0), (40, 390.0, 510.0)]
_HULL_TOUGHNESS = [("A", "A", "+0 C"), ("D", "D", "-20 C"),
                   ("E", "E", "-40 C"), ("F", "F", "-60 C")]
_HULL_HS = [
    _hull(f"{tletter}H{lvl}", smys, smts, tclass,
          note=f"Higher-strength {lvl}; Charpy at {ttemp}.")
    for (lvl, smys, smts) in _HULL_HS_LEVELS
    for (tletter, tclass, ttemp) in _HULL_TOUGHNESS
]

# EN 10025-2 structural steel (t <= 16 mm class).
_EN_GRADES = [
    _en("S275", 275.0, 430.0),
    _en("S355", 355.0, 510.0),
    _en("S420", 420.0, 520.0),
]

#: The canonical registry, keyed by :attr:`MaterialGrade.name`.
GRADES: dict[str, MaterialGrade] = {
    g.name: g for g in (_API_5L + _HULL_NS + _HULL_HS + _EN_GRADES)
}

# Alias map (ISO L-grade -> canonical name) for forgiving lookup.
_ALIASES: dict[str, str] = {
    g.designation.upper(): g.name
    for g in GRADES.values() if g.designation
}


# ---------------------------------------------------------------------------
# Query API
# ---------------------------------------------------------------------------
def get(name: str) -> MaterialGrade:
    """Look up a grade by canonical name, ISO designation, or alias.

    Case-insensitive; tolerates the ``"Grade "`` prefix for hull steel.

    Raises:
        KeyError: if the grade is unknown.
    """
    if name in GRADES:
        return GRADES[name]
    key = name.strip()
    # Direct, then case-normalised, then alias.
    for candidate in (key, key.upper(), f"Grade {key.upper()[-1]}"):
        if candidate in GRADES:
            return GRADES[candidate]
    upper = key.upper()
    if upper in _ALIASES:
        return GRADES[_ALIASES[upper]]
    # Case-insensitive name match.
    for canonical, grade in GRADES.items():
        if canonical.upper() == upper:
            return grade
    raise KeyError(f"Unknown material grade: {name!r}")


def all_grades() -> list[MaterialGrade]:
    """All registered grades, in registry (standard) order."""
    return list(GRADES.values())


def by_standard(standard: str) -> list[MaterialGrade]:
    """All grades belonging to a standard (case-insensitive substring match)."""
    s = standard.strip().lower()
    return [g for g in GRADES.values() if s in g.standard.lower()]


def smys_mpa(name: str) -> float:
    """Specified minimum yield strength (MPa)."""
    return get(name).smys_mpa


def smts_mpa(name: str, *, psl: str = "PSL2") -> float:
    """Specified minimum tensile strength (MPa).

    For API 5L, ``psl="PSL1"`` returns the PSL1 minimum where it differs.
    """
    g = get(name)
    if psl.upper() == "PSL1" and g.smts_psl1_mpa is not None:
        return g.smts_psl1_mpa
    return g.smts_mpa


def smys_psi(name: str) -> float:
    """Specified minimum yield strength (psi)."""
    return get(name).smys_psi


def smts_psi(name: str, *, psl: str = "PSL2") -> float:
    """Specified minimum tensile strength (psi)."""
    return smts_mpa(name, psl=psl) * PSI_PER_MPA


# ---------------------------------------------------------------------------
# Back-compat adapters — regenerate the legacy FFS dicts from the matrix.
# These are checked against the live legacy dicts by the test-suite, both to
# document the convention and to guard against future drift.  Migration of the
# scattered consumers onto these is tracked separately (issue #1089).
# ---------------------------------------------------------------------------
#: API 5L grades that the legacy asset_integrity dicts covered.
_LEGACY_API_5L = ["X42", "X46", "X52", "X56", "X60", "X65", "X70", "X80"]


def legacy_smys_psi_dict() -> dict[str, float]:
    """Reproduce ``asset_integrity.corroded_pipe.SMYS_PSI``.

    Convention: SMYS in psi = grade number (ksi) x 1000 (X52 -> 52,000 psi).
    """
    return {name: get(name).grade_ksi * 1000.0 for name in _LEGACY_API_5L}


def legacy_smts_psi_dict() -> dict[str, float]:
    """Reproduce ``asset_integrity.dnv_rp_f101.SMTS_PSI``.

    Convention: PSL2 SMTS (MPa) -> psi, rounded to the nearest 100 psi.
    """
    return {
        name: float(round(get(name).smts_mpa * PSI_PER_MPA, -2))
        for name in _LEGACY_API_5L
    }
