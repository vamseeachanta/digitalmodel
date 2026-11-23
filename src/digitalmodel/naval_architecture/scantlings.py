# ABOUTME: Class-rule scantling minimums — minimum plate thickness, plate
# ABOUTME: thickness for lateral pressure, and minimum stiffener section modulus.
"""Prescriptive class-rule scantling checks (DNV-RU-SHIP / IACS CSR style).

These are the *prescriptive* minimums — distinct from, and complementary to, the
buckling utilisation (`structural_analysis.panel_buckling`). They answer "is the
plate thick enough / the stiffener strong enough" per the rule formulas, not "does
it buckle".

Checks (each returns the same dict shape as
:mod:`digitalmodel.naval_architecture.compliance` — ``name / code / pass /
value / required / unit`` — so they aggregate with the other compliance checks):

* **Minimum plate thickness** by location (keel / bottom / side / deck / …):
  ``t_min = (a + b*L) * sqrt(235/ReH)`` (mm).
* **Plate thickness for lateral pressure** (DNV):
  ``t = 15.8 * k_a * s * sqrt(p) / sqrt(sigma) + t_k`` (mm), with the aspect
  factor ``k_a = (1.1 - 0.25 s/l)^2 <= 1.0``.
* **Minimum stiffener section modulus** by span & lateral pressure:
  ``Z = 1000 * p * s * l^2 / (m * sigma)`` (cm^3), bending factor ``m`` (12
  clamped / 8 simply supported).

``s`` = stiffener spacing (m), ``l`` = stiffener span (m), ``p`` = design lateral
pressure (kN/m^2), ``sigma`` = permissible bending stress (N/mm^2).

References: DNV-RU-SHIP Pt.3, ABS Rules for Steel Vessels, IACS CSR.
"""
from __future__ import annotations

import math

CODE_REFERENCE = "DNV-RU-SHIP / IACS CSR"

# Representative DNV-RU-SHIP general minimum-thickness coefficients (a, b) for
# t_min = (a + b*L) * sqrt(235/ReH) (mm).  Location-specific; the exact rule
# table should be consulted for a class submission.
_MIN_THICKNESS_COEFF: dict[str, tuple[float, float]] = {
    "keel": (7.0, 0.05),
    "bottom": (5.0, 0.04),
    "side": (5.0, 0.04),
    "deck": (5.5, 0.02),
    "tank_top": (5.5, 0.04),
    "bulkhead": (5.0, 0.04),
}

# Default bending-moment factor m for the stiffener section modulus.
_M_CLAMPED = 12.0
_M_SIMPLY_SUPPORTED = 8.0


def material_factor_f1(yield_mpa: float) -> float:
    """DNV material factor ``f1 = ReH / 235`` (>= 1 for higher-strength steel)."""
    return yield_mpa / 235.0


# ---------------------------------------------------------------------------
# Required-value formulas
# ---------------------------------------------------------------------------
def minimum_plate_thickness(length_m: float, location: str,
                            yield_mpa: float = 235.0) -> float:
    """DNV general minimum plate thickness for a location (mm).

    ``t_min = (a + b*L) * sqrt(235/ReH)``.  Higher-strength steel reduces the
    minimum via the ``sqrt(235/ReH)`` factor.

    Raises:
        KeyError: if ``location`` is not one of the tabulated locations.
    """
    key = location.strip().lower()
    if key not in _MIN_THICKNESS_COEFF:
        raise KeyError(
            f"Unknown location {location!r}; choose from "
            f"{sorted(_MIN_THICKNESS_COEFF)}")
    a, b = _MIN_THICKNESS_COEFF[key]
    return (a + b * length_m) * math.sqrt(235.0 / yield_mpa)


def plate_aspect_factor(spacing_m: float, span_m: float) -> float:
    """DNV plate aspect factor ``k_a = (1.1 - 0.25 s/l)^2``, capped at 1.0."""
    return min(1.0, (1.1 - 0.25 * spacing_m / span_m) ** 2)


def plate_thickness_for_pressure(
    spacing_m: float, span_m: float, pressure_kn_m2: float,
    permissible_mpa: float, *, corrosion_mm: float = 0.0,
) -> float:
    """DNV plate thickness for lateral pressure (mm).

    ``t = 15.8 * k_a * s * sqrt(p) / sqrt(sigma) + t_k``.
    """
    ka = plate_aspect_factor(spacing_m, span_m)
    return (15.8 * ka * spacing_m * math.sqrt(pressure_kn_m2)
            / math.sqrt(permissible_mpa) + corrosion_mm)


def required_section_modulus(
    spacing_m: float, span_m: float, pressure_kn_m2: float,
    permissible_mpa: float, *, m_factor: float = _M_CLAMPED,
) -> float:
    """Minimum stiffener section modulus for lateral pressure (cm^3).

    ``Z = 1000 * p * s * l^2 / (m * sigma)`` (m = 12 clamped / 8 simply
    supported).
    """
    return (1000.0 * pressure_kn_m2 * spacing_m * span_m ** 2
            / (m_factor * permissible_mpa))


# ---------------------------------------------------------------------------
# Pass/fail checks (compliance-style dicts)
# ---------------------------------------------------------------------------
def check_minimum_plate_thickness(
    actual_t_mm: float, length_m: float, location: str,
    yield_mpa: float = 235.0,
) -> dict:
    """Check actual plate thickness against the location minimum."""
    req = minimum_plate_thickness(length_m, location, yield_mpa)
    return {"name": f"Minimum plate thickness ({location})",
            "code": CODE_REFERENCE, "pass": bool(actual_t_mm >= req),
            "value": round(actual_t_mm, 2), "required": round(req, 2),
            "unit": "mm"}


def check_plate_thickness_pressure(
    actual_t_mm: float, spacing_m: float, span_m: float, pressure_kn_m2: float,
    permissible_mpa: float, *, corrosion_mm: float = 0.0,
) -> dict:
    """Check actual plate thickness against the lateral-pressure requirement."""
    req = plate_thickness_for_pressure(
        spacing_m, span_m, pressure_kn_m2, permissible_mpa,
        corrosion_mm=corrosion_mm)
    return {"name": "Plate thickness (lateral pressure)",
            "code": CODE_REFERENCE, "pass": bool(actual_t_mm >= req),
            "value": round(actual_t_mm, 2), "required": round(req, 2),
            "unit": "mm"}


def check_stiffener_section_modulus(
    actual_z_cm3: float, spacing_m: float, span_m: float, pressure_kn_m2: float,
    permissible_mpa: float, *, m_factor: float = _M_CLAMPED,
) -> dict:
    """Check actual stiffener section modulus against the rule minimum."""
    req = required_section_modulus(
        spacing_m, span_m, pressure_kn_m2, permissible_mpa, m_factor=m_factor)
    return {"name": "Stiffener section modulus", "code": CODE_REFERENCE,
            "pass": bool(actual_z_cm3 >= req),
            "value": round(actual_z_cm3, 1), "required": round(req, 1),
            "unit": "cm³"}
