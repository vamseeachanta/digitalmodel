"""Validated SI definitions for individual S-N power-law branches.

The method in a legacy engineering workbook is N = A / S**m, with stress
*range*, not amplitude. Each branch is evaluated independently, including
extrapolated plotting endpoints. The existing PowerLawSNCurve supplies the
evaluation; its default cycle cap is disabled. No standard parameters are
selected or supplied here. Environment determines the caller's choice of A
and m; this module applies no implicit environmental reduction.

Source references: DNV-RP-C203, Oct 2011 (also Aug 2000, Aug 2005, Apr 2008
and Apr 2010); API-RP-2A-LRFD, Second Edition, Apr 1994; BS 7608. For the
power-law and thickness formulas: clause not stated in source. The source
separately names DNV-RP-C203 "section 2.10, page 20" for guidance about
ignoring the endurance limit when any stress range exceeds it; the edition
of that particular statement is not established. No endurance cutoff is
applied by these functions.

Input gaps: standard clauses for the formulas, complete edition attribution,
curve class/environment selection, intercepts, slopes, transition criteria,
reference thicknesses and thickness exponents require caller verification.
An empty source intercept is missing input, not a zero logarithm. No source
tables are reproduced. These functions do not qualify a curve for design.

Limits: stress ranges, cycle counts and thicknesses must be finite and
positive. Slopes must be finite and positive; thickness exponents may be
zero. Nonpositive, nonfinite or unrepresentable calculation results raise
ValueError. Floating-point intermediate overflow is also rejected. Knees
and fatigue limits are caller-owned metadata, not automatic branch switches.

Next revision: explicit piecewise assembly, transition discontinuity checks,
and export formatting. Existing standard catalogs remain the parameter owners.
"""
from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass
import math
from numbers import Real

import numpy as np

from digitalmodel.fatigue.damage import thickness_correction

from .sn_curves import PowerLawSNCurve


def _finite(value: float, name: str, *, positive: bool = False) -> None:
    if isinstance(value, bool) or not isinstance(value, Real) or not math.isfinite(value):
        raise ValueError(f"{name} must be a finite real number")
    if positive and value <= 0:
        raise ValueError(f"{name} must be positive")


def _positive_output(compute: Callable[[], float]) -> float:
    try:
        with np.errstate(all="ignore"):
            result = float(compute())
    except (ArithmeticError, ValueError) as exc:
        raise ValueError("result is outside the floating-point range") from exc
    _finite(result, "result", positive=True)
    return result


@dataclass(frozen=True)
class SNDefinition:
    """One branch; log10(A) uses A in cycles * Pa**m, and m is dimensionless."""

    log10_intercept_cycles_pa_m: float
    slope_dimensionless: float

    def __post_init__(self) -> None:
        _finite(self.log10_intercept_cycles_pa_m, "log10_intercept_cycles_pa_m")
        _finite(self.slope_dimensionless, "slope_dimensionless", positive=True)
        try:
            intercept = 10.0**self.log10_intercept_cycles_pa_m
        except OverflowError as exc:
            raise ValueError("intercept is outside the floating-point range") from exc
        _finite(intercept, "intercept", positive=True)

    @classmethod
    def from_log10_intercept(
        cls, log10_intercept_cycles_unit_m: float, slope_dimensionless: float,
        *, stress_unit_pa: float,
    ) -> SNDefinition:
        """Convert log10(A) by adding m * log10(Pa per input stress unit).

        Supply the exact unit scale at this boundary (MPa uses 10**6 Pa).
        No rounded customary-unit conversion factor is embedded.
        """
        _finite(log10_intercept_cycles_unit_m, "log10_intercept_cycles_unit_m")
        _finite(slope_dimensionless, "slope_dimensionless", positive=True)
        _finite(stress_unit_pa, "stress_unit_pa", positive=True)
        return cls(
            log10_intercept_cycles_unit_m
            + slope_dimensionless * math.log10(stress_unit_pa),
            slope_dimensionless,
        )


def intercept_cycles_pa_m(definition: SNDefinition) -> float:
    """Return A = 10**log10(A), in cycles * Pa**m; source clause not stated."""
    return 10.0**definition.log10_intercept_cycles_pa_m


def _curve(definition: SNDefinition) -> PowerLawSNCurve:
    return PowerLawSNCurve(
        name="Caller-supplied branch", A=intercept_cycles_pa_m(definition),
        m=definition.slope_dimensionless, cutoff_cycles=math.inf,
    )


def cycles_to_failure(definition: SNDefinition, stress_range_pa: float) -> float:
    """Return N = A / S**m. Source formula clause is not stated; see module."""
    _finite(stress_range_pa, "stress_range_pa", positive=True)
    return _positive_output(
        lambda: _curve(definition).get_allowable_cycles(stress_range_pa)
    )


def stress_range_pa(definition: SNDefinition, cycles_count: float) -> float:
    """Return S = (A / N)**(1/m), in Pa; source clause not stated."""
    _finite(cycles_count, "cycles_count", positive=True)
    return _positive_output(lambda: _curve(definition).get_stress_range(cycles_count))


@dataclass(frozen=True)
class ThicknessDefinition:
    """Caller-supplied thickness correction; lengths in m, exponent unitless."""

    actual_thickness_m: float
    reference_thickness_m: float
    exponent_dimensionless: float

    def __post_init__(self) -> None:
        _finite(self.actual_thickness_m, "actual_thickness_m", positive=True)
        _finite(self.reference_thickness_m, "reference_thickness_m", positive=True)
        _finite(self.exponent_dimensionless, "exponent_dimensionless")
        if self.exponent_dimensionless < 0:
            raise ValueError("exponent_dimensionless must be nonnegative")


def thickness_stress_factor(definition: ThicknessDefinition) -> float:
    """Return max(1, (t/t_ref)**k); source formula clause not stated.

    Reuse the existing thickness correction with a unit stress range. Both
    thicknesses use metres, so their ratio is unchanged.
    """
    return _positive_output(lambda: thickness_correction(
        1.0, definition.actual_thickness_m, definition.reference_thickness_m,
        definition.exponent_dimensionless,
    ))


def nominal_stress_range_pa(
    branch_stress_range_pa: float, thickness: ThicknessDefinition,
    *, scf_dimensionless: float,
) -> float:
    """Return S_nominal = S_branch / thickness_factor / SCF, in Pa.

    This converts the ordinate at fixed cycles; it does not change N or m.
    Source formula clause not stated. The SCF is always caller-supplied.
    """
    _finite(branch_stress_range_pa, "branch_stress_range_pa", positive=True)
    _finite(scf_dimensionless, "scf_dimensionless", positive=True)
    return _positive_output(
        lambda: branch_stress_range_pa / thickness_stress_factor(thickness)
        / scf_dimensionless
    )
