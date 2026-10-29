"""Explicit-basis pressure stage for a pipeline integrity workflow.

Source: 49 CFR §192.105(a), 2025 annual edition, design formula for steel pipe.
https://www.govinfo.gov/content/pkg/CFR-2025-title49-vol3/xml/CFR-2025-title49-vol3-sec192-105.xml

Only the nominal pressure-design equation is evaluated. This is not an FFS
assessment or an implementation of every requirement of Part 192.
"""

from __future__ import annotations

from dataclasses import dataclass
import math
from typing import Literal


EQUATION_REFERENCE = (
    "49 CFR §192.105(a), 2025 annual edition: Design formula for steel pipe"
)
Status = Literal["EVALUATED", "NOT_EVALUATED"]
LIMITATIONS = (
    "Caller-supplied strength, factors and their references are not verified.",
    "External loads, collapse, buckling and combined loading are not evaluated.",
    "Corrosion, defect dimensions and FFS acceptance are not evaluated.",
)


@dataclass(frozen=True)
class PressureDesignInput:
    """Required inputs in mm and MPa, with caller-supplied source references.

    ``formula_applicable`` confirms that the caller has checked the scope of
    §192.105(a), including external-load wall exclusions and the heated,
    cold-expanded pipe limitation of §192.105(b). Set it to False when unknown.
    Nominal wall thickness must not be replaced by a corroded minimum reading.
    Factors must be fractions in (0, 1]. Strength and factors are not selected
    or verified against their source tables by this workflow.
    """

    outer_diameter_mm: float
    nominal_wall_thickness_mm: float
    pressure_mpa: float
    yield_strength_mpa: float
    design_factor: float
    joint_factor: float
    temperature_factor: float
    strength_reference: str
    factors_reference: str
    formula_applicable: bool

    def __post_init__(self) -> None:
        for name in (
            "outer_diameter_mm", "nominal_wall_thickness_mm", "pressure_mpa",
            "yield_strength_mpa", "design_factor", "joint_factor",
            "temperature_factor",
        ):
            value = getattr(self, name)
            if isinstance(value, bool) or not isinstance(value, (int, float)):
                raise ValueError(f"{name} must be a real number")
            if not math.isfinite(value) or value < 0:
                raise ValueError(f"{name} must be finite and non-negative")
            if name != "pressure_mpa" and value == 0:
                raise ValueError(f"{name} must be positive")
        # Plausibility bound, not table selection: 49 CFR §§192.111, 192.113,
        # 192.115 (2025 annual edition) tabulate factors no greater than 1.
        for name in ("design_factor", "joint_factor", "temperature_factor"):
            if getattr(self, name) > 1:
                raise ValueError(f"{name} must not exceed 1")
        # Circular bore geometry: inside diameter = outside diameter - 2*t.
        if self.nominal_wall_thickness_mm >= self.outer_diameter_mm / 2:
            raise ValueError("Nominal wall thickness must leave a positive bore")
        for name in ("strength_reference", "factors_reference"):
            value = getattr(self, name)
            if not isinstance(value, str) or not value.strip():
                raise ValueError(f"{name} must contain a source reference")
        if not isinstance(self.formula_applicable, bool):
            raise ValueError("formula_applicable must be a boolean")


@dataclass(frozen=True)
class PressureDesignResult:
    """Pressure-stage outputs; unevaluated quantities are None, never zero."""

    inputs: PressureDesignInput
    pressure_status: Status
    design_pressure_mpa: float | None
    required_wall_thickness_mm: float | None
    utilisation_ratio: float | None
    meets_pressure_design: bool | None
    ffs_status: Literal["NOT_EVALUATED"] = "NOT_EVALUATED"
    integrity_status: Literal["NOT_EVALUATED"] = "NOT_EVALUATED"
    equation_reference: str = EQUATION_REFERENCE
    limitations: tuple[str, ...] = LIMITATIONS


def assess_pressure_design(inp: PressureDesignInput) -> PressureDesignResult:
    """Evaluate one nominal-wall pressure case with an explicit design basis.

    Uses the §192.105(a) equation in consistent mm/MPa units. Required wall is
    its algebraic rearrangement; utilisation is pressure demand/design pressure.
    A pressure-stage pass does not establish a pipeline integrity verdict.
    """
    if not inp.formula_applicable:
        return PressureDesignResult(
            inputs=inp,
            pressure_status="NOT_EVALUATED",
            design_pressure_mpa=None,
            required_wall_thickness_mm=None,
            utilisation_ratio=None,
            meets_pressure_design=None,
            limitations=(
                "Pressure formula applicability has not been confirmed.",
                *LIMITATIONS,
            ),
        )

    # 49 CFR §192.105(a), 2025 annual edition: P = (2*S*t/D)*F*E*T.
    # The coefficient 2 is part of that equation; no tabulated factors default.
    # Evaluate t/D first: a positive bore and factors <= 1 keep P below S,
    # avoiding intermediate overflow from 2*S when the result is finite.
    design_pressure = (
        (inp.nominal_wall_thickness_mm / inp.outer_diameter_mm)
        * 2 * inp.yield_strength_mpa * inp.design_factor
        * inp.joint_factor * inp.temperature_factor
    )
    if not math.isfinite(design_pressure) or design_pressure <= 0:
        raise ValueError("The calculated design pressure must be finite and positive")
    required_wall = inp.pressure_mpa / design_pressure * inp.nominal_wall_thickness_mm
    utilisation = inp.pressure_mpa / design_pressure
    if not all(math.isfinite(value) for value in (required_wall, utilisation)):
        raise ValueError("The calculated thickness and utilisation must be finite")

    return PressureDesignResult(
        inputs=inp,
        pressure_status="EVALUATED",
        design_pressure_mpa=design_pressure,
        required_wall_thickness_mm=required_wall,
        utilisation_ratio=utilisation,
        # Definitional screen against the §192.105(a) design pressure,
        # not a regulatory operating-pressure acceptance criterion.
        # Compare pressures directly, without rounding at the boundary.
        meets_pressure_design=inp.pressure_mpa <= design_pressure,
    )
