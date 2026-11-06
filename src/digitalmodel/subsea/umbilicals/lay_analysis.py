# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Static umbilical lay screening with explicit load and acceptance inputs.

API 17E / ISO 13628-5 scope: installation geometry, bending and tension.
Clause-level mapping and standard qualification are NOT_EVALUATED because
verified clause text was unavailable; no standard acceptance limits are assumed.

Public equation source: Catenary Curve (2019), equations (3), (4), (9), (10),
(32), (34): https://www.mygeodesy.id.au/documents/Catenary%20Curve.pdf
Using submerged weight w [N/m], horizontal effective tension H [N], a=H/w:
z=a*(cosh(x/a)-1), s=a*sinh(x/a), T=H*cosh(x/a), R=a*cosh(x/a)**2.
For touchdown at a horizontal, frictionless seabed: s=sqrt(z*(z+2*a)),
x=a*asinh(s/a), T_top=H+w*z, R_min=a and residual T_touchdown=H.
For a span X with rise z, write u_mid=asinh(z/(2*a*sinh(X/(2*a)))),
u1=u_mid-X/(2*a), u2=u_mid+X/(2*a). Then L=a*(sinh(u2)-sinh(u1)).
These are identities derived from the cited equations, not empirical factors.

Parallel and bundled cases model one lift from touchdown. Bundled weight is
the supplied combined load; components share a centreline. Connector loads,
relative slip, offset curvature and load sharing remain NOT_EVALUATED.
Suspended cases model two supports with specified horizontal span, rise and
lower-support height above the seabed. A seabed intersection fails the assumed
free-span geometry. Available length is a payout capacity, not a fixed-length
boundary condition. H is a required input; it is not solved from payout length.
The line is uniform, inextensible and perfectly flexible. No current, dynamics,
soil contact, bending stiffness, axial stretch or far-end residual tension is
modelled. Radius screening therefore requires a subsequent stiffness/contact
analysis for installation qualification. Only positive net weight is supported.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Literal

from .umbilical_sizing import (
    CheckResult,
    QualifiedLimit,
    _check,
    _number,
    _standard_gap,
    _unevaluated,
)


@dataclass(frozen=True)
class LayCase:
    """SI installation inputs; minimum limits require a recorded basis."""

    configuration: Literal["parallel", "bundled", "suspended"]
    submerged_weight: float
    horizontal_tension: float
    vertical_drop: float
    available_length: float
    minimum_bend_radius: QualifiedLimit | None = None
    minimum_residual_tension: QualifiedLimit | None = None
    companion_weight: float | None = None
    horizontal_span: float | None = None
    lower_support_clearance: float | None = None


@dataclass(frozen=True)
class LayResult:
    """Static geometry [m], forces [N], curvature [1/m] and bounded checks."""

    configuration: str
    effective_weight: float
    suspended_length: float
    horizontal_span: float
    top_tension: float
    minimum_radius: float
    minimum_clearance: float
    touchdown_tension: float | None
    touchdown_curvature: float | None
    bend_check: CheckResult
    residual_check: CheckResult
    length_check: CheckResult
    contact_check: CheckResult
    configuration_check: CheckResult
    standard_qualification: CheckResult


def _validate(case: LayCase) -> float:
    if case.configuration not in ("parallel", "bundled", "suspended"):
        raise ValueError("unsupported lay configuration")
    for name in ("submerged_weight", "horizontal_tension", "available_length"):
        _number(name, getattr(case, name), positive=True)
    _number("vertical_drop", case.vertical_drop)
    weight = case.submerged_weight
    if case.configuration == "bundled":
        if case.companion_weight is None or not math.isfinite(case.companion_weight):
            raise ValueError("bundled configuration requires finite companion_weight")
        weight += case.companion_weight
    elif case.companion_weight is not None:
        raise ValueError("companion_weight only applies to bundled configuration")
    _number("combined submerged weight", weight, positive=True)
    if case.configuration != "suspended" and (
        case.horizontal_span is not None
        or case.lower_support_clearance is not None
        or case.vertical_drop == 0
    ):
        raise ValueError(
            "touchdown case requires positive drop and no free-span inputs"
        )
    return weight


def analyze_lay(case: LayCase) -> LayResult:
    """Calculate static geometry and compare only with explicitly supplied limits."""
    weight = _validate(case)
    a = case.horizontal_tension / weight
    _number("catenary parameter", a, positive=True)
    try:
        if case.configuration == "suspended":
            span = case.horizontal_span
            lower_clearance = case.lower_support_clearance
            if span is None or lower_clearance is None:
                raise ValueError(
                    "suspended case requires span and lower support clearance"
                )
            _number("horizontal_span", span, positive=True)
            _number("lower_support_clearance", lower_clearance)
            half_span = span / (2 * a)
            midpoint = math.asinh(case.vertical_drop / (2 * a * math.sinh(half_span)))
            u1, u2 = midpoint - half_span, midpoint + half_span
            # Catenary Curve (2019), eqs. (9), (10), (32), with y datum at support 1.
            length = a * (math.sinh(u2) - math.sinh(u1))
            nearest = max(u1, min(0, u2))
            minimum_radius = a * math.cosh(nearest) ** 2
            clearance = lower_clearance + a * (math.cosh(nearest) - math.cosh(u1))
            top_tension = case.horizontal_tension * max(math.cosh(u1), math.cosh(u2))
            touchdown_tension = touchdown_curvature = None
            residual = _unevaluated("A fully suspended span has no touchdown")
            contact = CheckResult(
                "PASS" if clearance >= 0 else "FAIL",
                clearance,
                0,
                None,
                "Geometric nonintersection with the horizontal seabed (z >= 0)",
            )
        else:
            # Catenary Curve (2019), eqs. (3), (4), (10), (32), (34).
            length = math.sqrt(case.vertical_drop * (case.vertical_drop + 2 * a))
            span = a * math.asinh(length / a)
            top_tension = case.horizontal_tension + weight * case.vertical_drop
            minimum_radius, clearance = a, 0.0
            touchdown_tension, touchdown_curvature = case.horizontal_tension, 1 / a
            residual = _check(
                touchdown_tension, case.minimum_residual_tension, minimum=True
            )
            contact = CheckResult(
                "PASS",
                0,
                0,
                None,
                "Ideal horizontal tangent at a frictionless seabed; no soil model",
            )
    except (OverflowError, ZeroDivisionError) as exc:
        raise ValueError("lay geometry exceeds the supported numerical range") from exc
    if not all(
        math.isfinite(v) for v in (length, span, top_tension, minimum_radius, clearance)
    ):
        raise ValueError("lay geometry exceeds the supported numerical range")
    configuration = _unevaluated(
        "Bundle slip, load sharing and offset curvature require separate evidence"
        if case.configuration == "bundled"
        else "Route interaction and installation dynamics require separate evidence"
    )
    return LayResult(
        case.configuration,
        weight,
        length,
        span,
        top_tension,
        minimum_radius,
        clearance,
        touchdown_tension,
        touchdown_curvature,
        _check(minimum_radius, case.minimum_bend_radius, minimum=True),
        residual,
        CheckResult(
            "PASS" if length <= case.available_length else "FAIL",
            length,
            case.available_length,
            length / case.available_length,
            "Required suspended length <= supplied payout capacity",
        ),
        contact,
        configuration,
        _standard_gap(),
    )
