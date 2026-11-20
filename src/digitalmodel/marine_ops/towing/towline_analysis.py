"""Inextensible, freely suspended two-support catenary under uniform weight.

Coordinates are horizontal x and upward z from the first support, in metres.
Weight is prescribed in N/m (in air under the issue's approved model). Contact,
buoyancy, elasticity and distributed horizontal drag are outside this model.
"""
from dataclasses import dataclass
import math


@dataclass(frozen=True)
class CatenaryResult:
    horizontal_tension_n: float
    top_tension_n: float
    sag_m: float  # maximum vertical distance below the support chord
    span_m: float
    scope: float  # arc length / horizontal span
    x_m: tuple[float, ...]
    z_m: tuple[float, ...]


def _positive(**values: float) -> None:
    for name, value in values.items():
        if not math.isfinite(value) or value <= 0:
            raise ValueError(f"{name} must be finite and positive")


def _shape(span: float, length: float, weight: float, elevation: float,
           a: float, n_points: int) -> CatenaryResult:
    if isinstance(n_points, bool) or not isinstance(n_points, int) or n_points < 3:
        raise ValueError("n_points must be an integer >= 3")
    beta = math.asinh(elevation / math.sqrt((length - elevation) * (length + elevation)))
    vertex = span / 2 - a * beta
    base = math.cosh(vertex / a)
    xs = tuple(span * i / (n_points - 1) for i in range(n_points))
    zs = tuple(a * (math.cosh((x - vertex) / a) - base) for x in xs)
    sag_x = min(span, max(0, vertex + a * math.asinh(elevation / span)))
    sag = elevation * sag_x / span - a * (math.cosh((sag_x - vertex) / a) - base)
    top = weight * a * max(base, math.cosh((span - vertex) / a))
    return CatenaryResult(weight * a, top, sag, span, length / span, xs, zs)


def solve_catenary(*, span_m: float, length_m: float, weight_n_m: float,
                   elevation_m: float = 0, n_points: int = 101) -> CatenaryResult:
    """Solve geometry at fixed supports; reject taut/short lines with no finite solution."""
    _positive(span_m=span_m, length_m=length_m, weight_n_m=weight_n_m)
    if not math.isfinite(elevation_m):
        raise ValueError("elevation_m must be finite")
    if length_m <= math.hypot(span_m, elevation_m):
        raise ValueError("length_m must exceed support distance")
    ratio = math.sqrt((length_m - elevation_m) * (length_m + elevation_m)) / span_m
    lower, upper = 0.0, 1.0
    while math.sinh(upper) / upper < ratio:
        upper *= 2
        if upper > 700:
            raise ValueError("catenary exceeds numerical range")
    for _ in range(100):
        middle = (lower + upper) / 2
        if math.sinh(middle) / middle < ratio:
            lower = middle
        else:
            upper = middle
    a = span_m / (lower + upper)
    return _shape(span_m, length_m, weight_n_m, elevation_m, a, n_points)


def catenary_from_pull(*, horizontal_tension_n: float, length_m: float,
                       weight_n_m: float, n_points: int = 101) -> CatenaryResult:
    """Level supports free to separate under a prescribed horizontal tow load.

    Span is an output. Prescribing span, length and horizontal pull independently
    would overconstrain equilibrium. Used by the configuration grid search.
    """
    _positive(horizontal_tension_n=horizontal_tension_n, length_m=length_m,
              weight_n_m=weight_n_m)
    a = horizontal_tension_n / weight_n_m
    span = 2 * a * math.asinh(length_m / (2 * a))
    return _shape(span, length_m, weight_n_m, 0, a, n_points)
