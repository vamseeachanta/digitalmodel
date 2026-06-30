"""
ABOUTME: Foundational CFD validation subpackage (#1161 Phase 2). Each
validation case ships a *known-answer regression*: a pure analytical reference
(no OpenFOAM dependency) plus a case builder that reuses the existing
``solvers/openfoam`` case-building stack. On solver-capable hosts the case is
solved and compared to the reference within tolerance; on dry-run-only hosts
the analytical + build checks still run green.

Cases:
- Flat plate (Blasius), #1167 — laminar boundary layer; Cf, delta99, plate Cd.
- Cylinder Re=100, #1166 — laminar Karman vortex street; mean Cd, Strouhal St.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Dict

from .cylinder import (
    CYLINDER_RE100_CD,
    CYLINDER_RE100_CD_RANGE,
    CYLINDER_RE100_STROUHAL,
    CYLINDER_TOLERANCE,
    CylinderConfig,
    build_cylinder_case,
    cylinder_reference_cd,
    cylinder_reference_strouhal,
    shedding_frequency,
    strouhal_number,
)
from .flat_plate import (
    BLASIUS_TOLERANCE,
    FlatPlateConfig,
    blasius_cf,
    blasius_delta99,
    blasius_plate_cd,
    build_flat_plate_case,
)


@dataclass
class ValidationCase:
    """A foundational validation case = build + known-answer reference.

    Attributes:
        name: Human-readable case identifier.
        build: Callable that generates the case directory (``(config, parent)``
            -> ``Path``); defaults baked into the underlying builder.
        reference: Mapping of quantity name -> analytical reference value.
        tolerance: Relative tolerance for the known-answer gate (fraction).
        domain: Physical domain tag (e.g. ``"external-aero (boundary layer)"``).
    """

    name: str
    build: Callable[..., Any]
    reference: Dict[str, float]
    tolerance: float
    domain: str
    metadata: Dict[str, Any] = field(default_factory=dict)

    def relative_error(self, quantity: str, measured: float) -> float:
        """Relative error of ``measured`` against the stored reference value."""
        ref = self.reference[quantity]
        if ref == 0.0:
            return abs(measured)
        return abs(measured - ref) / abs(ref)

    def within_tolerance(self, quantity: str, measured: float) -> bool:
        """True iff ``measured`` matches the reference within ``tolerance``."""
        return self.relative_error(quantity, measured) <= self.tolerance


# Registry of the foundational cases (Phase 2).
FLAT_PLATE_CASE = ValidationCase(
    name="flat_plate_blasius",
    build=build_flat_plate_case,
    reference={
        "cf_at_re_1e5": blasius_cf(1.0e5),
        "delta99_at_x1_re_1e5": blasius_delta99(1.0, 1.0e5),
        "plate_cd_re_1e5": blasius_plate_cd(1.0e5),
    },
    tolerance=BLASIUS_TOLERANCE,
    domain="external-aero (boundary layer)",
    metadata={"issue": "#1167", "reference": "Blasius (1908)"},
)

CYLINDER_CASE = ValidationCase(
    name="cylinder_re100",
    build=build_cylinder_case,
    reference={
        "mean_cd": CYLINDER_RE100_CD,
        "strouhal": CYLINDER_RE100_STROUHAL,
    },
    tolerance=CYLINDER_TOLERANCE,
    domain="external-aero (bluff body) / marine VIV",
    metadata={"issue": "#1166", "reference": "Williamson (1996); Henderson (1997)"},
)

FOUNDATIONAL_CASES = (FLAT_PLATE_CASE, CYLINDER_CASE)

__all__ = [
    # Shared
    "ValidationCase",
    "FOUNDATIONAL_CASES",
    "FLAT_PLATE_CASE",
    "CYLINDER_CASE",
    # Flat plate (#1167)
    "BLASIUS_TOLERANCE",
    "FlatPlateConfig",
    "blasius_cf",
    "blasius_delta99",
    "blasius_plate_cd",
    "build_flat_plate_case",
    # Cylinder (#1166)
    "CYLINDER_RE100_CD",
    "CYLINDER_RE100_CD_RANGE",
    "CYLINDER_RE100_STROUHAL",
    "CYLINDER_TOLERANCE",
    "CylinderConfig",
    "build_cylinder_case",
    "cylinder_reference_cd",
    "cylinder_reference_strouhal",
    "shedding_frequency",
    "strouhal_number",
]
