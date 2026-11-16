"""Discrete symmetric p-y springs from a legacy engineering workbook.

Clay response follows API RP 2A-WSD, 21st Ed. (2000), clause 6.8.3,
and API RP 2A-WSD, Errata and Supplement 3 (2007), replacement clause 6.8.3.
Clause, equation and figure numbers are cited as stated in the legacy source;
they are not independently verified against the standard.
The source uses the smooth expression p/pu = 0.5 (y/yc)^(1/3) at its
interior knots, rather than the rounded reference-table ordinates. Its last
cyclic knot uses the previous knot's response times min(1, z/XR).
The returned points preserve that discrete construction; they do not define
a continuous cyclic constitutive law between knots.

Sand uses the 2000 reference, clause 6.8.7. A selected fraction of A pu defines
a displacement scale; caller-selected fractions of that scale define knots.
The source describes this as a fraction of pu, but its equation includes A.

Inputs and units: local soil dataclasses use SI; tributary length and tail
displacement use m; knot ratios and tail factor are dimensionless. Forces
use N. Positive forces denote resistance magnitudes, not signed applied loads.

Input gaps: displacement knots, sand target fraction, tail displacement and
tail factor are required. The source supplies no standard clause for its
sampling or convergence tail, so clause not stated in source for those choices.
No tables or fit coefficients are supplied by this module. No solver text,
plot layout, metadata, or reference-edition knot selection is generated.
"""

from dataclasses import dataclass

from .lateral_soil_response import (
    ClayPoint, SandPoint, _check_values, clay_characteristic_displacement_m,
    clay_transition_depth_m, clay_ultimate_pressure_pa, sand_displacement_m,
    sand_resistance_n_per_m,
)


@dataclass(frozen=True)
class SpringPoint:
    """A spring displacement in m and corresponding resistance in N."""

    displacement_m: float
    force_n: float

    def __post_init__(self) -> None:
        _check_values(positive=(), nonnegative=(), signed=(self.displacement_m, self.force_n))


def clay_resistance_fraction_one(displacement_ratio_one: float) -> float:
    """0.5 r^(1/3), the source interpolation of clause 6.8.3's clay knots.

    This uncapped scalar expression is not a complete p-y law. The curve
    constructor limits its interior knots and treats the terminal knot.
    """
    _check_values(positive=(), nonnegative=(displacement_ratio_one,))
    return 0.5 * displacement_ratio_one ** (1 / 3)


def _ordered_positive(values: tuple[float, ...]) -> None:
    if not values:
        raise ValueError("at least one positive knot is required")
    _check_values(positive=tuple(values), nonnegative=())
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
    _check_values(positive=(tail_displacement_m, tail_factor_one),
                  nonnegative=tuple(p.force_n for p in positive_points))
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
    """Generate soft-clay knot forces F = (p/pu) pu D h; clause 6.8.3.

    The last cyclic knot uses the preceding knot's cube-root ordinate and
    depth reduction. Static knots cannot exceed y/yc = 8. Interior cyclic
    knots cannot exceed y/yc = 3. Edition-specific knot sets are caller inputs.
    """
    ratios = tuple(positive_displacement_ratios_one)
    _ordered_positive(ratios)
    _check_values(positive=(tributary_length_m,), nonnegative=())
    if not isinstance(cyclic, bool):
        raise ValueError("cyclic must be a boolean")
    if len(ratios) < 2:
        raise ValueError("at least two clay knots are required")
    if (not cyclic and ratios[-1] > 8) or (cyclic and ratios[-2] > 3):
        raise ValueError("clay knots exceed the local model range")
    fractions = [clay_resistance_fraction_one(r) for r in ratios]
    if cyclic:
        fractions[-1] = fractions[-2] * min(1.0, point.depth_m / clay_transition_depth_m(point))
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
    """Sample equation 6.8.7-1 and compute F = p h at each supplied knot.

    Knots are positive fractions, at most one, of the displacement producing
    the supplied target fraction of A pu. Depth must be positive for inversion.
    """
    fractions = tuple(displacement_fractions_one)
    _ordered_positive(fractions)
    _check_values(positive=(tributary_length_m, target_resistance_fraction_one), nonnegative=())
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
