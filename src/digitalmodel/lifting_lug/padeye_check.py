# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Nominal plate and direct weld screening with explicit comparison criteria.

This extends the lifting_lug package without inheriting workflow.py's allowables
or importing a solver. The plate has a centred circular hole, straight loaded
edge and uniform thickness, with load along the symmetry axis in its plane.
Rounded heads, cheek plates, eccentric forces and oblique loads are excluded.

Sources read for this implementation:
* OpenStax, University Physics Volume 1 (2016), section 12.3: stress = force/area.
  https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus
* Stress Analysis Manual (October 1986), section 9.2, equations (9-3a), (9-6a):
  projected bearing and net-section areas; empirical strength factors are NOT
  implemented. https://engineeringlibrary.org/reference/lug-analysis-air-force-stress-manual
* IMO MSC.1/Circ.1663 (2023), annex 3.1: applicable class or accepted standards
  are needed for design. This guideline supplies no plate stress allowables.

ABS Publication 152, Guide for Certification of Lifting Appliances, is an
unresolved design basis: edition and applicable clauses have not been read.
ABS acceptance is always NOT_EVALUATED. Supplied criteria establish only the
named nominal comparisons, never class compliance or a rated lifting capacity.
"""

from __future__ import annotations

from dataclasses import dataclass, fields, replace
from enum import Enum
import math


class Status(str, Enum):
    """Outcome of a supplied nominal comparison, including missing evidence."""

    PASS = "PASS"
    FAIL = "FAIL"
    NOT_EVALUATED = "NOT_EVALUATED"


def _number(name: str, value: float, *, zero_allowed: bool = False) -> None:
    # Zero is the mathematical origin, not an engineering acceptance limit.
    if (
        isinstance(value, bool)
        or not isinstance(value, (int, float))
        or not math.isfinite(value)
        or (value < 0 if zero_allowed else value <= 0)
    ):
        raise ValueError(
            f"{name} must be finite and {'nonnegative' if zero_allowed else 'positive'}"
        )


@dataclass(frozen=True)
class Source:
    """Caller-declared provenance; text presence does not authenticate a source."""

    document: str
    edition: str
    locator: str

    def __post_init__(self) -> None:
        for field in fields(self):
            value = getattr(self, field.name)
            if not isinstance(value, str) or not value.strip():
                raise ValueError(f"source {field.name} must be nonempty")


@dataclass(frozen=True)
class PadeyePlate:
    """Single plate; edge distance is from hole centre to straight loaded edge.

    Width is perpendicular to the applied force. The hole is centred across
    that width. No cheek-plate contribution or reduced corrosion thickness is
    inferred; enter the effective thickness established by the design basis.
    """

    thickness_mm: float
    width_mm: float
    hole_diameter_mm: float
    pin_diameter_mm: float
    loaded_edge_distance_mm: float

    def __post_init__(self) -> None:
        for field in fields(self):
            _number(field.name, getattr(self, field.name))
        if self.pin_diameter_mm > self.hole_diameter_mm:
            raise ValueError("pin_diameter_mm exceeds hole_diameter_mm")
        if self.width_mm <= self.hole_diameter_mm:
            raise ValueError("width_mm must exceed hole_diameter_mm")
        # Radius = diameter / 2: circle geometry, not a code coefficient.
        if self.loaded_edge_distance_mm <= self.hole_diameter_mm / 2:
            raise ValueError("loaded_edge_distance_mm must exceed hole radius")


@dataclass(frozen=True)
class LoadCase:
    """Static tension in this sling leg, with explicit multiplicative factors.

    P = static tension * dynamic amplification * skew factor is the definition
    of these inputs, not a rule for selecting their values. Source must identify
    their basis. No leg sharing, sling angle or minimum factor is inferred.
    """

    static_sling_load_kn: float
    dynamic_amplification: float
    skew_factor: float
    source: Source

    def __post_init__(self) -> None:
        _number("static_sling_load_kn", self.static_sling_load_kn, zero_allowed=True)
        _number("dynamic_amplification", self.dynamic_amplification)
        _number("skew_factor", self.skew_factor)
        if not isinstance(self.source, Source):
            raise ValueError("load source must be a Source")


@dataclass(frozen=True)
class DirectWeld:
    """Balanced weld group under direct force only, with uniform throat stress.

    Effective throat and total effective length are required inputs. The stress
    is the nominal resultant on the throat, not a resolved shear or normal stress.
    An allowable for a different stress definition is inapplicable. Weld lengths,
    terminations, leg-to-throat conversion and connection eccentricity require
    a separate design basis; none is silently inferred here.
    """

    effective_throat_mm: float
    total_effective_length_mm: float

    def __post_init__(self) -> None:
        for field in fields(self):
            _number(field.name, getattr(self, field.name))


@dataclass(frozen=True)
class Criterion:
    """Positive allowable for this nominal stress definition and its provenance."""

    allowable_mpa: float
    source: Source

    def __post_init__(self) -> None:
        _number("allowable_mpa", self.allowable_mpa)
        if not isinstance(self.source, Source):
            raise ValueError("criterion source must be a Source")


@dataclass(frozen=True)
class ScreeningCriteria:
    """Optional comparison limits; absence yields NOT_EVALUATED, never PASS."""

    bearing: Criterion | None = None
    net_tension: Criterion | None = None
    shear_tearout: Criterion | None = None
    weld_resultant: Criterion | None = None

    def __post_init__(self) -> None:
        for field in fields(self):
            value = getattr(self, field.name)
            if value is not None and not isinstance(value, Criterion):
                raise ValueError(f"{field.name} must be a Criterion or None")


@dataclass(frozen=True)
class CheckResult:
    """Nominal stress, criterion and explicit evaluation state for one mode."""

    mode: str
    area_mm2: float | None
    demand_mpa: float | None
    criterion: Criterion | None
    utilization: float | None
    status: Status
    reason: str
    required_throat_mm: float | None = None


@dataclass(frozen=True)
class PadeyeResult:
    """Defined comparisons and explicit exclusions; no class approval is implied."""

    design_load_kn: float
    checks: tuple[CheckResult, ...]
    load_source: Source

    @property
    def screening_status(self) -> Status:
        """Retain any failure, otherwise propagate missing evaluations."""
        if any(check.status is Status.FAIL for check in self.checks):
            return Status.FAIL
        if any(check.status is Status.NOT_EVALUATED for check in self.checks):
            return Status.NOT_EVALUATED
        return Status.PASS

    @property
    def abs_acceptance(self) -> Status:
        """ABS edition, applicable clauses and complete design checks are absent."""
        return Status.NOT_EVALUATED

    @property
    def exclusions(self) -> tuple[str, ...]:
        """Checks that the nominal model cannot establish."""
        return (
            "ABS rule acceptance; edition and applicable clauses unresolved",
            "Pin bending, pin shear, bushings and contact distribution",
            "Bearing/shear-out/hoop interaction and stress concentration",
            "Oblique or out-of-plane load, eccentricity and connection moments",
            "Weld directional interaction, fatigue, support strength and inspection",
            "Rounded heads, cheek plates and as-built geometry",
        )


def _check(
    mode: str, force_n: float, area_mm2: float, criterion: Criterion | None
) -> CheckResult:
    _number(f"{mode} area", area_mm2)
    stress = force_n / area_mm2
    _number(f"{mode} demand", stress, zero_allowed=True)
    if criterion is None:
        return CheckResult(
            mode,
            area_mm2,
            stress,
            None,
            None,
            Status.NOT_EVALUATED,
            "No allowable with document, edition and locator supplied",
        )
    utilization = stress / criterion.allowable_mpa
    _number(f"{mode} utilization", utilization, zero_allowed=True)
    # Unity is the definition of demand / supplied allowable, not a code limit.
    status = Status.PASS if stress <= criterion.allowable_mpa else Status.FAIL
    return CheckResult(
        mode,
        area_mm2,
        stress,
        criterion,
        utilization,
        status,
        "Comparison against caller-supplied nominal stress allowable",
    )


def check_padeye(
    plate: PadeyePlate,
    load: LoadCase,
    criteria: ScreeningCriteria | None = None,
    weld: DirectWeld | None = None,
) -> PadeyeResult:
    """Calculate nominal demands and supplied-limit comparisons in N, mm, MPa.

    Shear tear-out uses two straight planes from the hole tangent to the straight
    loaded edge. It is a nominal area idealisation, not an ultimate lug capacity
    model. The manual's empirical coefficients and interaction are excluded.
    """
    criteria = criteria if criteria is not None else ScreeningCriteria()
    design_load = (
        load.static_sling_load_kn * load.dynamic_amplification * load.skew_factor
    )
    _number("design_load_kn", design_load, zero_allowed=True)
    # SI prefixes: kilo = 1000; N/mm2 = MPa. OpenStax (2016), 12.3, SI stress.
    force = design_load * 1000
    _number("design_load_n", force, zero_allowed=True)
    # Projected pin area: rectangular projection d_pin*t, F/A (OpenStax 12.3).
    bearing_area = plate.pin_diameter_mm * plate.thickness_mm
    # Rectangular net area, consistent with Stress Analysis Manual (1986), 9-6a.
    net_area = (plate.width_mm - plate.hole_diameter_mm) * plate.thickness_mm
    # Two planes, each of length e-D/2: geometric area derivation + F/A,
    # OpenStax (2016), 12.3. The factors 2 are plane count and diameter/radius.
    shear_area = (
        2
        * (plate.loaded_edge_distance_mm - plate.hole_diameter_mm / 2)
        * plate.thickness_mm
    )
    checks = [
        _check("pin_bearing", force, bearing_area, criteria.bearing),
        _check("net_tension", force, net_area, criteria.net_tension),
        _check("shear_tearout", force, shear_area, criteria.shear_tearout),
    ]
    if weld is None:
        checks.append(
            CheckResult(
                "weld_resultant",
                None,
                None,
                criteria.weld_resultant,
                None,
                Status.NOT_EVALUATED,
                "No weld geometry supplied",
            )
        )
    else:
        # Throat rectangle A=a*L; F/A (OpenStax 2016, 12.3), uniform direct load.
        weld_check = _check(
            "weld_resultant",
            force,
            weld.effective_throat_mm * weld.total_effective_length_mm,
            criteria.weld_resultant,
        )
        if criteria.weld_resultant is not None:
            # a_req=P/(L*allowable), the same F/A relation rearranged.
            # Divide sequentially to avoid overflowing L*allowable.
            required = (
                force
                / weld.total_effective_length_mm
                / criteria.weld_resultant.allowable_mpa
            )
            _number("required_throat_mm", required, zero_allowed=True)
            weld_check = replace(weld_check, required_throat_mm=required)
        checks.append(weld_check)
    return PadeyeResult(design_load, tuple(checks), load.source)
