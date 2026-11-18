"""Discrete symmetric p-y springs using the public-source lateral soil basis.

See ``lateral_soil_response.citations.json`` for validated public citations,
engineering choices and the limits of edition and clause verification.
Clay samples the documented static or cyclic backbone at each requested
ratio. Cyclic residuals are independent of the other sample locations.
Sand samples the documented tanh law at fractions of a selected displacement.

Inputs and units: local soil dataclasses use SI; tributary length and tail
displacement use m; knot ratios and tail factor are dimensionless. Forces
use N. Positive forces denote resistance magnitudes, not signed applied loads.

Input gaps: displacement knots, sand target fraction, tail displacement and
tail factor are required engineering choices. The convergence tail is a
numerical extension; it is not part of the qualified physical backbone.
No solver text, plots or edition-specific reference tables are generated.
"""

from dataclasses import dataclass

from .lateral_soil_response import (
    ClayPoint, SandPoint, check_values, clay_characteristic_displacement_m,
    clay_transition_depth_m, clay_ultimate_pressure_pa, sand_displacement_m,
    sand_resistance_n_per_m,
)


@dataclass(frozen=True)
class SpringPoint:
    """A spring displacement in m and corresponding resistance in N."""

    displacement_m: float
    force_n: float

    def __post_init__(self) -> None:
        check_values(positive=(), nonnegative=(), signed=(self.displacement_m, self.force_n))


def clay_resistance_fraction_one(displacement_ratio_one: float) -> float:
    """0.5 r^(1/3), an exact cube root in place of the public rounded exponent.

    This uncapped scalar expression is not a complete p-y law. The curve
    constructor applies loading-specific caps and depth reduction.
    """
    check_values(positive=(), nonnegative=(displacement_ratio_one,))
    return 0.5 * float(displacement_ratio_one) ** (1 / 3)


def _ordered_positive(values: tuple[float, ...]) -> None:
    if not values:
        raise ValueError("at least one positive knot is required")
    check_values(positive=tuple(values), nonnegative=())
    if any(b < a for a, b in zip(values, values[1:])):
        raise ValueError("positive knots must be nondecreasing")


def symmetric_spring_points(
    positive_points: tuple[SpringPoint, ...], *, tail_displacement_m: float,
    tail_factor_one: float,
) -> tuple[SpringPoint, ...]:
    """Append a convergence tail, reflect signs, and insert the origin.

    Duplicate displacements are preserved for legacy knot sets. Decreasing
    forces are permitted for cyclic softening. No standard clause is stated
    in the source for the tail multiplier or its displacement.
    ``digitalmodel.drilling_riser.global_model.foundation.spring_table_kn``
    handles monotonic ``PYCurve`` tables in kN link form; this helper exists
    because cyclic soft-clay knots can soften and repeat displacements.
    """
    _ordered_positive(tuple(p.displacement_m for p in positive_points))
    check_values(positive=(tail_displacement_m, tail_factor_one),
                  nonnegative=tuple(p.force_n for p in positive_points))
    tail_displacement_m, tail_factor_one = float(tail_displacement_m), float(tail_factor_one)
    if tail_displacement_m <= positive_points[-1].displacement_m:
        raise ValueError("tail displacement must exceed the last knot")
    extended = positive_points + (
        SpringPoint(tail_displacement_m, positive_points[-1].force_n * tail_factor_one),
    )
    return (tuple(SpringPoint(-p.displacement_m, -p.force_n) for p in reversed(extended))
            + (SpringPoint(0.0, 0.0),) + extended)


def clay_spring_curve(
    point: ClayPoint, *, positive_displacement_ratios_one: tuple[float, ...],
    cyclic: bool, tributary_length_m: float, tail_displacement_m: float,
    tail_factor_one: float,
) -> tuple[SpringPoint, ...]:
    """Generate soft-clay knot forces F = (p/pu) pu D h.

    Static knots cannot exceed y/yc = 8. Cyclic points follow the capped
    cube root to 3, linear softening to 15, then the depth-reduced plateau.
    The 0.72 cap closes the rounded-source junction at 3 without a force jump.
    """
    ratios = tuple(positive_displacement_ratios_one)
    _ordered_positive(ratios)
    check_values(positive=(tributary_length_m,), nonnegative=())
    if not isinstance(cyclic, bool):
        raise ValueError("cyclic must be a boolean")
    if len(ratios) < 2:
        raise ValueError("at least two clay knots are required")
    ratios = tuple(float(r) for r in ratios)
    tributary_length_m = float(tributary_length_m)
    if not cyclic and ratios[-1] > 8:
        raise ValueError("clay knots exceed the local model range")
    fractions = [clay_resistance_fraction_one(r) for r in ratios]
    if cyclic:
        depth_ratio = min(1.0, point.depth_m / clay_transition_depth_m(point))
        fractions = [
            min(f, 0.72) if r <= 3 else
            0.72 * (1 - (1 - depth_ratio) * min((r - 3) / 12, 1.0))
            for r, f in zip(ratios, fractions)
        ]
    scale_n = clay_ultimate_pressure_pa(point) * point.diameter_m * tributary_length_m
    yc_m = clay_characteristic_displacement_m(point)
    knots = tuple(SpringPoint(yc_m * r, scale_n * f) for r, f in zip(ratios, fractions))
    return symmetric_spring_points(knots, tail_displacement_m=tail_displacement_m,
                                   tail_factor_one=tail_factor_one)


def sand_spring_curve(
    point: SandPoint, *, displacement_fractions_one: tuple[float, ...],
    target_resistance_fraction_one: float, cyclic: bool, tributary_length_m: float,
    tail_displacement_m: float, tail_factor_one: float,
) -> tuple[SpringPoint, ...]:
    """Sample the documented sand law and compute F = p h at each knot.

    Knots are positive fractions, at most one, of the displacement producing
    the supplied target fraction of A pu. Depth must be positive for inversion.
    """
    fractions = tuple(displacement_fractions_one)
    _ordered_positive(fractions)
    check_values(positive=(tributary_length_m, target_resistance_fraction_one), nonnegative=())
    fractions = tuple(float(f) for f in fractions)
    tributary_length_m = float(tributary_length_m)
    if fractions[-1] > 1:
        raise ValueError("displacement fractions cannot exceed one")
    scale_m = sand_displacement_m(
        point, resistance_fraction_one=target_resistance_fraction_one, cyclic=cyclic,
    )
    knots = tuple(
        SpringPoint(scale_m * f, tributary_length_m * sand_resistance_n_per_m(
            point, scale_m * f, cyclic=cyclic,
        )) for f in fractions
    )
    return symmetric_spring_points(knots, tail_displacement_m=tail_displacement_m,
                                   tail_factor_one=tail_factor_one)
