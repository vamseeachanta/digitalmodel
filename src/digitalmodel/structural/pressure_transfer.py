# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline conservation checks for an instantaneous pressure-transfer load set.

This module neither operates a solver nor qualifies pressure mapping or stress.
All inputs use SI units in a common right-handed Cartesian frame. Positive
pressure is compressive and acts opposite an outward oriented area vector.
Caller-supplied frame and case identifiers are declarations, not authenticated
evidence. The caller shall reconcile phase, pressure components and units first.

Numeric inputs are Python int or float scalars (excluding bool); vectors must
be three-component tuples or lists of those scalars. Array-library scalars and
arrays must be converted to these input types by the caller.
"""

from dataclasses import dataclass
from math import fsum, isfinite

Vector3 = tuple[float, float, float]

FORCE_SOURCE = (
    "University of Texas at Austin, Fluid Mechanics (2016), "
    "Aerodynamic Forces, equation 9.70 (outward unit normal): "
    "https://farside.ph.utexas.edu/teaching/336L/Fluidhtml/node128.html"
)
MOMENT_SOURCE = (
    "OpenStax, University Physics Volume 1 (2016), section 10.6, "
    "equations 10.22 and 10.24: "
    "https://openstax.org/books/university-physics-volume-1/pages/10-6-torque"
)


def _number(value):
    if isinstance(value, bool) or not isinstance(value, (float, int)):
        raise ValueError("a finite real number is required")
    try:
        number = float(value)
    except OverflowError as error:
        raise ValueError("a finite real number is required") from error
    if not isfinite(number):
        raise ValueError("a finite real number is required")
    return number


def _vector(value):
    if not isinstance(value, (tuple, list)) or len(value) != 3:
        raise ValueError("a three-component vector is required")
    return tuple(_number(component) for component in value)


def _label(value):
    if not isinstance(value, str) or not value.strip():
        raise ValueError("a nonempty evidence or coordinate label is required")
    return value


@dataclass(frozen=True)
class SurfacePressure:
    """Uniform pressure on a planar patch; area vector = outward unit normal * area.

    Centroid quadrature is exact for a constant pressure on a planar patch.
    Curved faces or varying pressure require caller-qualified quadrature first.
    """

    pressure_pa: float
    area_vector_m2: Vector3
    centroid_m: Vector3

    def __post_init__(self):
        object.__setattr__(self, "pressure_pa", _number(self.pressure_pa))
        object.__setattr__(self, "area_vector_m2", _vector(self.area_vector_m2))
        object.__setattr__(self, "centroid_m", _vector(self.centroid_m))
        if not any(self.area_vector_m2):
            raise ValueError("patch area must be nonzero")


@dataclass(frozen=True)
class Resultant:
    """Force and moment about the declared origin in the declared frame and case."""

    force_n: Vector3
    moment_nm: Vector3
    origin_m: Vector3
    frame: str
    case_id: str

    def __post_init__(self):
        for field in ("force_n", "moment_nm", "origin_m"):
            object.__setattr__(self, field, _vector(getattr(self, field)))
        _label(self.frame)
        _label(self.case_id)


@dataclass(frozen=True)
class TransferCriteria:
    """Absolute component limits with an explicit caller-supplied criterion source.

    No engineering limits are supplied by this module. Each comparison uses
    abs(mapped - source) <= limit; a zero reference needs no special denominator.
    The source identifies the governing analysis criterion, not an equation source.
    """

    force_tolerance_n: Vector3
    moment_tolerance_nm: Vector3
    source: str

    def __post_init__(self):
        for field in ("force_tolerance_n", "moment_tolerance_nm"):
            values = _vector(getattr(self, field))
            if any(value < 0 for value in values):
                raise ValueError("component limits must be nonnegative")
            object.__setattr__(self, field, values)
        _label(self.source)


@dataclass(frozen=True)
class TransferCheck:
    """Disposition of resultant conservation only; no engineering readiness claim."""

    status: str
    force_difference_n: Vector3 | None
    moment_difference_nm: Vector3 | None
    reason: str
    criteria_source: str | None


def pressure_resultant(patches, *, origin_m, frame, case_id):
    """Integrate compressive patch forces and moments about an explicit origin."""
    origin = _vector(origin_m)
    forces, moments = [], []
    for patch in patches:
        # FORCE_SOURCE, Eq. 9.70: F = -integral(p n dS), n outward.
        force = _vector([-patch.pressure_pa * area for area in patch.area_vector_m2])
        x, y, z = _vector([c - o for c, o in zip(patch.centroid_m, origin)])
        fx, fy, fz = force
        # MOMENT_SOURCE, Eq. 10.22: M = (centroid - origin) cross F.
        moment = _vector([y * fz - z * fy, z * fx - x * fz, x * fy - y * fx])
        forces.append(force)
        moments.append(moment)
    if not forces:
        raise ValueError("a nonempty pressure surface is required")
    try:
        # MOMENT_SOURCE, Eq. 10.24: sum the individual moments about one origin.
        force_sum = tuple(fsum(axis) for axis in zip(*forces))
        moment_sum = tuple(fsum(axis) for axis in zip(*moments))
    except OverflowError as error:
        raise ValueError("resultant exceeds finite arithmetic") from error
    return Resultant(force_sum, moment_sum, origin, frame, case_id)


def compare_transfer(source, mapped, criteria):
    """Compare six signed components; missing evidence yields NOT_EVALUATED.

    Matching resultants is necessary but does not establish the pressure field,
    mass reconciliation, structural equilibrium or submodel validity.
    """
    citation = criteria.source if criteria is not None else None
    if source is None or mapped is None:
        return TransferCheck(
            "NOT_EVALUATED",
            None,
            None,
            "source and mapped resultants required",
            citation,
        )
    if (source.origin_m, source.frame, source.case_id) != (
        mapped.origin_m,
        mapped.frame,
        mapped.case_id,
    ):
        raise ValueError("resultants require the same origin, frame and case")
    force_delta = _vector([m - s for m, s in zip(mapped.force_n, source.force_n)])
    moment_delta = _vector([m - s for m, s in zip(mapped.moment_nm, source.moment_nm)])
    if criteria is None:
        return TransferCheck(
            "NOT_EVALUATED",
            force_delta,
            moment_delta,
            "transfer criteria required",
            None,
        )
    differences = force_delta + moment_delta
    limits = criteria.force_tolerance_n + criteria.moment_tolerance_nm
    passed = all(abs(delta) <= limit for delta, limit in zip(differences, limits))
    return TransferCheck(
        "PASS" if passed else "FAIL",
        force_delta,
        moment_delta,
        "six component differences within supplied limits"
        if passed
        else "a component difference exceeds its supplied limit",
        citation,
    )
