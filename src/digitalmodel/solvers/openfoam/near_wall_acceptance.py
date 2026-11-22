# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Explicit measured-wall acceptance for relative-size hull meshes.

The caller supplies engineering limits and their basis. This module does not
infer achieved y+ from mesh settings or treat synthetic values as measurements.
Coverage is wall area carrying the requested layer count / total assessed wall
area, not the fraction of faces carrying any layer. Compare the same patch in
every case; assess each additional force-bearing patch separately.
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass
from fractions import Fraction
from typing import Mapping


@dataclass(frozen=True)
class NearWallCriteria:
    """Caller-selected limits; fractions, not percentages. No default limits."""

    target_y_plus: float
    relative_tolerance: float
    minimum_coverage: float
    maximum_coverage_spread: float
    source: str


@dataclass(frozen=True)
class WallMeasurement:
    """Extrema over the assessed wall and solution window, with area coverage.

    Source must identify the mesh, solution window and measurement artifact.
    These are supplied measurements, not values computed by this module.
    """

    y_plus_min: float
    y_plus_max: float
    covered_area_m2: float
    total_area_m2: float
    patch: str
    source: str


def pending_near_wall() -> dict:
    """Case construction cannot establish achieved wall resolution."""
    return {
        "status": "NOT_EVALUATED",
        "passed": False,
        "reason": "Measured y+ and requested-layer area coverage are required.",
    }


def _finite(value: float, label: str, *, positive: bool = False) -> None:
    if isinstance(value, bool) or not math.isfinite(value):
        raise ValueError(f"{label} must be finite")
    if value < 0 or (positive and value == 0):
        raise ValueError(f"{label} must be {'positive' if positive else 'nonnegative'}")


def _validate_criteria(criteria: NearWallCriteria) -> None:
    _finite(criteria.target_y_plus, "target_y_plus", positive=True)
    for field in ("relative_tolerance", "minimum_coverage", "maximum_coverage_spread"):
        value = getattr(criteria, field)
        _finite(value, field)
        # Fractions have a unit interval by definition, not an empirical limit.
        if value > 1:
            raise ValueError(f"{field} must be a fraction in [0, 1]")
    if not criteria.source.strip():
        raise ValueError("criteria source is required")


def _score(measurement: WallMeasurement, criteria: NearWallCriteria) -> dict:
    for field in ("y_plus_min", "y_plus_max", "covered_area_m2", "total_area_m2"):
        _finite(getattr(measurement, field), field, positive=field == "total_area_m2")
    if measurement.y_plus_min > measurement.y_plus_max:
        raise ValueError("y_plus_min exceeds y_plus_max")
    if measurement.covered_area_m2 > measurement.total_area_m2:
        raise ValueError("covered area exceeds total area")
    if not measurement.patch.strip() or not measurement.source.strip():
        raise ValueError("measurement patch and source are required")
    # Relative error and area fraction are definitions. Limits are caller inputs.
    # Compare the exact decimal inputs as rationals: no engineering slack is
    # added at an inclusive boundary by a floating-point tolerance.
    target = Fraction(str(criteria.target_y_plus))
    tolerance = Fraction(str(criteria.relative_tolerance))
    lower = target * (1 - tolerance)
    upper = target * (1 + tolerance)
    y_pass = (lower <= Fraction(str(measurement.y_plus_min))
              <= Fraction(str(measurement.y_plus_max)) <= upper)
    # All-zero output is not evidence of a calculated turbulent-wall y+:
    # scripts/cfd/yplus_after.sh, v2312 missing-turbulence-model diagnostic.
    y_pass = y_pass and measurement.y_plus_max > 0
    coverage = Fraction(str(measurement.covered_area_m2)) / Fraction(str(measurement.total_area_m2))
    coverage_pass = coverage >= Fraction(str(criteria.minimum_coverage))
    return {
        "measurement": asdict(measurement),
        "y_plus_bounds": [float(lower), float(upper)],
        "y_plus_passed": y_pass,
        "coverage": float(coverage),
        "coverage_passed": coverage_pass,
        "passed": y_pass and coverage_pass,
    }


def assess_near_wall(
    measurements: Mapping[str, WallMeasurement | None],
    criteria: NearWallCriteria | None,
) -> dict:
    """Score every expected case; callers must include absent cases as None.

    Missing inputs are NOT_EVALUATED. Malformed inputs raise ValueError.
    Passing requires every case and the comparison coverage spread to pass.
    A single-case assessment checks absolute limits only.
    """
    if criteria is None:
        return pending_near_wall()
    _validate_criteria(criteria)
    if not measurements or any(value is None for value in measurements.values()):
        report = pending_near_wall()
        report["criteria"] = asdict(criteria)
        report["missing_cases"] = [key for key, value in measurements.items() if value is None]
        return report
    if any(not key.strip() for key in measurements):
        raise ValueError("case identity is required")
    available = {key: value for key, value in measurements.items() if value is not None}
    patches = {value.patch for value in available.values()}
    if len(patches) != 1:
        raise ValueError("comparison requires the same assessed patch")
    rows = {key: _score(value, criteria) for key, value in available.items()}
    coverage = [Fraction(str(value.covered_area_m2)) / Fraction(str(value.total_area_m2))
                for value in available.values()]
    spread = max(coverage) - min(coverage)
    passed = (all(row["passed"] for row in rows.values())
              and spread <= Fraction(str(criteria.maximum_coverage_spread)))
    return {
        "status": "PASS" if passed else "FAIL",
        "passed": passed,
        "criteria": asdict(criteria),
        "cases": rows,
        "coverage_spread": float(spread),
    }
