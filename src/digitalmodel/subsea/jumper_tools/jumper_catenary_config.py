# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Uniform, inextensible flexible catenary with a horizontal seabed tangent.

Source: Catenary Curve (2019), equations (1), (4), (9), (10), (14).
This is a scalar screening interface, consistent with catenary_riser's E1-E3
basis. It does not optimize a rigid jumper or calculate bending or soil reaction.
"""

import math
from dataclasses import dataclass

from ._validation import finite_value


@dataclass(frozen=True)
class CatenaryConfig:
    """Vertex/touchdown at (grounded_length, 0, 0), elevation upward.

    The first endpoint lies at the origin on a flat seabed. Grounded length
    equals its horizontal distance to touchdown; suspended_span excludes it.
    Angles are in radians. Lengths are metres and tensions are newtons.
    """

    parameter_m: float
    suspended_length_m: float
    suspended_span_m: float
    grounded_length_m: float
    rise_m: float
    horizontal_tension_n: float
    top_vertical_force_n: float
    top_tension_n: float
    hangoff_angle_from_horizontal_rad: float
    hangoff_angle_from_vertical_rad: float


def calculate_catenary_config(
    horizontal_tension_n: float,
    submerged_weight_n_m: float,
    rise_m: float,
    total_length_m: float,
) -> CatenaryConfig:
    """Prescribe H, downward w, rise and total length; return feasible geometry.

    Total length shall cover the suspended arc. The remaining length is laid
    straight on a frictionless flat seabed. Endpoint span is an output, not an
    independently satisfied constraint. No acceptance factor is applied.
    """
    for name, value in (
        ("horizontal_tension_n", horizontal_tension_n),
        ("submerged_weight_n_m", submerged_weight_n_m),
        ("total_length_m", total_length_m),
    ):
        finite_value(name, value, positive=True)
    finite_value("rise_m", rise_m)
    a = horizontal_tension_n / submerged_weight_n_m
    finite_value("catenary parameter", a, positive=True)
    # Catenary Curve (2019), (9)-(10): s²=h*(h+2a). Factoring limits overflow.
    suspended = math.sqrt(rise_m) * math.hypot(
        math.sqrt(rise_m), math.sqrt(a) * math.sqrt(2)
    )
    # (10): x=a*asinh(s/a), retaining nearly flat spans without acosh(1+h/a).
    span = a * math.asinh(suspended / a)
    vertical = submerged_weight_n_m * suspended
    top = horizontal_tension_n + submerged_weight_n_m * rise_m
    for name, value in (
        ("suspended length", suspended),
        ("span", span),
        ("vertical force", vertical),
        ("top tension", top),
    ):
        finite_value(name, value)
    if suspended > total_length_m:
        raise ValueError("total length is shorter than the suspended catenary")
    # Equation (1): tangent from horizontal = atan2(V,H); complementary angle.
    return CatenaryConfig(
        a,
        suspended,
        span,
        total_length_m - suspended,
        rise_m,
        horizontal_tension_n,
        vertical,
        top,
        math.atan2(vertical, horizontal_tension_n),
        math.atan2(horizontal_tension_n, vertical),
    )
