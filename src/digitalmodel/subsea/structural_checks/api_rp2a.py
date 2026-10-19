# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Elastic tubular demands supporting API RP 2A check development.

This is not a qualified API RP 2A implementation or workbook reconstruction.
No API coefficient, material property, allowable or interaction is inferred.
Normal-stress comparisons require caller-supplied, citation-validated allowables.
Unsupported checks remain NOT_EVALUATED, including for zero applied load.

Units: mm, N, N mm, MPa (= N/mm²). Positive axial force is tension; pressure
is nonnegative external gauge pressure. Axial force is the effective section
force supplied by the caller; pressure end thrust is not added. Bending moment
is the signed resultant about any centroidal axis of the circular section.

Sources opened for this implementation:
MIT, Stress-Strain Curves (2001), equation 1: sigma = N/A.
https://web.mit.edu/course/3/3.11/www/modules/ss.pdf
MIT, Stresses in Beams (2000), equations 4 and 7: I = integral(y² dA), Mc/I.
https://web.mit.edu/course/3/3.11/www/modules/bstress.pdf
"""

from __future__ import annotations

import math
from collections.abc import Mapping
from dataclasses import dataclass
from numbers import Real
from pathlib import Path
from typing import Literal

from digitalmodel.citations.schema import Citation, CitedValue, validate_citation

Status = Literal["PASS", "FAIL", "NOT_EVALUATED"]

DEMAND_SOURCES = (
    "MIT, Stress-Strain Curves (2001), Engineering Stress-Strain Curves, equation 1",
    "MIT, Stresses in Beams (2000), Normal Stresses, equations 4 and 7",
)

# Requested locators are transcribed from the issue, not verified clause text.
# No standard-derived numeric coefficient is supplied by this table.
_REQUESTED_CHECKS = (
    ("axial_tension", "3.2.1"),
    ("axial_compression", "3.2.2"),
    ("bending", "3.2.3"),
    ("shear", "3.2.4"),
    ("hydrostatic_pressure", "3.2.5"),
    ("tension_bending", "3.3.2"),
    ("tension_hydrostatic", "3.3.3"),
    ("compression_hydrostatic", "3.3.4"),
)
_NORMAL_CHECKS = frozenset(("axial_tension", "axial_compression", "bending"))


@dataclass(frozen=True)
class TubularInputs:
    """Original section and load inputs in the documented coherent units.

    Yield strength is explicit metadata, not an inferred API 5L grade or an
    allowable. Slenderness, boundary conditions and local buckling are absent.
    """

    diameter: float
    thickness: float
    yield_strength: float
    pressure: float
    axial_force: float
    bending_moment: float
    shear_force: float


@dataclass(frozen=True)
class CheckResult:
    """A supplied-allowable comparison or a named unevaluated code check.

    Stress and allowable use MPa. None means not established, never zero.
    PASS compares demand with a supplied allowable; it does not qualify that
    allowable's code applicability or the overall member design.
    """

    name: str
    calculated_stress: float | None
    allowable_stress: float | None
    unity_ratio: float | None
    passes: bool | None
    status: Status
    code_reference: str
    reason: str


@dataclass(frozen=True)
class UnityChecks:
    """All requested checks plus geometry and separate provenance sidecars.

    Area is mm², second moment mm⁴ and elastic section modulus mm³.
    max_unity_ratio covers evaluated checks only; use status for completeness.
    """

    inputs: TubularInputs
    area: float
    second_moment: float
    section_modulus: float
    checks: tuple[CheckResult, ...]
    citations: tuple[Citation, ...]
    demand_sources: tuple[str, ...] = DEMAND_SOURCES

    @property
    def status(self) -> Status:
        """Report failure before incompleteness; never turn a gap into a pass."""
        if any(check.status == "FAIL" for check in self.checks):
            return "FAIL"
        if not self.checks or any(check.status == "NOT_EVALUATED" for check in self.checks):
            return "NOT_EVALUATED"
        return "PASS"

    @property
    def passes(self) -> bool | None:
        """Return None when full acceptance cannot be established."""
        return None if self.status == "NOT_EVALUATED" else self.status == "PASS"

    @property
    def max_unity_ratio(self) -> float | None:
        """Return the maximum known ratio without treating missing ones as zero."""
        ratios = [check.unity_ratio for check in self.checks if check.unity_ratio is not None]
        return max(ratios, default=None)


def _finite(name: str, value: float) -> float:
    """Reject booleans, nonnumbers and nonfinite engineering inputs."""
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a finite real number")
    try:
        number = float(value)
    except OverflowError as error:
        raise ValueError(f"{name} must be a finite real number") from error
    if not math.isfinite(number):
        raise ValueError(f"{name} must be a finite real number")
    return number


def _geometry(inputs: TubularInputs) -> tuple[float, float, float]:
    """Integrate an annulus; the factors below are mathematical, not code limits."""
    diameter, thickness = inputs.diameter, inputs.thickness
    if diameter <= 0 or thickness <= 0 or thickness >= diameter / 2:
        raise ValueError("require diameter > 0 and 0 < thickness < diameter/2")
    inner_diameter = diameter - 2 * thickness
    if not 0 < inner_diameter < diameter:
        raise ValueError("geometry is not representable at floating-point precision")
    # Integrating r dr dtheta gives pi*(D²-d²)/4 = pi*t*(D-t).
    # Integrating y² dA (MIT 2000, equation 4) gives pi*(D⁴-d⁴)/64;
    # factoring yields I = A*(D²+d²)/16. Z = I/(D/2), equation 7.
    area = math.pi * thickness * (diameter - thickness)
    inertia = area * (diameter * diameter + inner_diameter * inner_diameter) / 16
    modulus = inertia / (diameter / 2)
    if any(not math.isfinite(value) or value <= 0 for value in (area, inertia, modulus)):
        raise ValueError("section properties are not representable")
    return area, inertia, modulus


def _validate_allowables(
    allowables: Mapping[str, CitedValue], citation_root: Path | None,
) -> None:
    """Validate every supplied allowable, including allowances for zero demands."""
    if not isinstance(allowables, Mapping):
        raise ValueError("allowables must be a mapping of check names to CitedValue")
    for name, allowable in allowables.items():
        if name not in _NORMAL_CHECKS:
            raise ValueError(f"unsupported allowable check: {name}")
        if not isinstance(allowable, CitedValue):
            raise ValueError(f"allowable for {name} must be a CitedValue")
        if not isinstance(allowable.citation, Citation):
            raise ValueError(f"allowable for {name} must carry a Citation")
        if _finite(f"allowable for {name}", allowable.value) <= 0:
            raise ValueError(f"allowable for {name} must be positive")
        if allowable.units != "MPa":
            raise ValueError(f"allowable for {name} must have units MPa")
        validate_citation(allowable.citation, repo_root=citation_root)


def _check(
    name: str, locator: str, stress: float | None, allowable: CitedValue | None,
) -> CheckResult:
    """Keep missing capacity or missing demand explicitly unevaluated."""
    reference = f"API RP 2A §{locator}; requested locator and edition unverified"
    if stress is None or allowable is None:
        reason = (
            "Qualified demand/capacity equation and applicability evidence unavailable"
            if stress is None else "No citation-validated allowable supplied"
        )
        return CheckResult(name, stress, None, None, None, "NOT_EVALUATED", reference, reason)
    ratio = stress / allowable.value
    if not math.isfinite(ratio):
        raise ValueError("unity ratio is not representable")
    # The unity boundary follows the definition demand/allowable, not an
    # assumed API acceptance coefficient: demand <= supplied allowable.
    passes = stress <= allowable.value
    return CheckResult(
        name, stress, allowable.value, ratio, passes, "PASS" if passes else "FAIL",
        allowable.citation.section,
        "Compared with supplied allowable; code applicability is caller responsibility",
    )


def api_rp2a_check(
    diameter: float,
    thickness: float,
    yield_strength: float,
    pressure: float = 0.0,
    axial_force: float = 0.0,
    bending_moment: float = 0.0,
    shear_force: float = 0.0,
    *,
    allowables: Mapping[str, CitedValue] | None = None,
    citation_root: Path | None = None,
) -> UnityChecks:
    """Calculate elastic demands and the eight requested check records.

    This does not select a code edition or derive capacity from yield strength.
    Optional allowables may address axial_tension, axial_compression and bending;
    each requires a positive CitedValue in MPa and a resolvable Citation. The
    citation's source must establish applicability to the case independently.
    Without these inputs, normal stresses are returned without an acceptance
    claim. Shear, hydrostatic and interactions always remain NOT_EVALUATED.
    """
    values = {
        "diameter": diameter, "thickness": thickness, "yield_strength": yield_strength,
        "pressure": pressure, "axial_force": axial_force,
        "bending_moment": bending_moment, "shear_force": shear_force,
    }
    inputs = TubularInputs(**{name: _finite(name, value) for name, value in values.items()})
    if inputs.yield_strength <= 0:
        raise ValueError("yield_strength must be positive")
    if inputs.pressure < 0:
        raise ValueError("pressure must be nonnegative external gauge pressure")
    area, inertia, modulus = _geometry(inputs)
    # MIT 2001 equation 1: engineering axial stress = N/A.
    # MIT 2000 equation 7: peak elastic bending stress = abs(M)*(D/2)/I.
    axial_stress = inputs.axial_force / area
    bending_stress = abs(inputs.bending_moment) / modulus
    if not all(math.isfinite(value) for value in (axial_stress, bending_stress)):
        raise ValueError("calculated stress is not representable")
    provided = {} if allowables is None else allowables
    _validate_allowables(provided, citation_root)
    supplied = dict(provided)
    demands = {
        "axial_tension": max(axial_stress, 0.0),
        "axial_compression": max(-axial_stress, 0.0),
        "bending": bending_stress,
    }
    checks = tuple(
        _check(name, locator, demands.get(name), supplied.get(name))
        for name, locator in _REQUESTED_CHECKS
    )
    return UnityChecks(
        inputs, area, inertia, modulus, checks,
        tuple(value.citation for value in supplied.values()),
    )
