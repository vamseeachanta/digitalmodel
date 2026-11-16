"""Rectangular component mass, first mass moments and centre of gravity.

Method
------
Areal mass is thickness times density. Component mass is length times width
times areal mass times quantity. First mass moments are mass times supplied
coordinates. An aggregate is the sum of masses and first moments, each scaled
by the same supplied factor; its centre is first moment divided by mass.
These are mass moments [kg m], not force moments or moments of inertia.

All physical inputs/results use SI: metres, kilograms, kg/m3, kg/m2 and kg m.
Quantity and mass factors are dimensionless; no gravitational acceleration is applied.

Source context: a legacy engineering workbook names ISO 19901-5 (2016-02-15),
Part 5, Weight control during engineering and construction; clause not stated
in source. This reference identifies the source context, not an independently
verified standard requirement or a claim of compliance.

Input gaps and limits
---------------------
The standard clause and normative basis of category allowances are absent.
Density and mass factors are required arguments without defaults. Callers
supply dimensions, integral quantity, category selection, component centres
and a common Cartesian datum. Coordinates are signed; no frame transform or
component centroid is inferred. Each category may be aggregated separately;
an unfactored net total uses all items with an explicitly supplied unit factor.
Different category factors require separate calls; no factored grand total is
computed here. Uniform factoring preserves a group's centre of gravity.

Empty/zero-mass groups have zero moments and undefined centres. Invalid inputs
and nonfinite arithmetic raise ValueError. Ordinary floating-point arithmetic
is intended for engineering-scale values, not extreme dynamic ranges. Source
summary-range defects, static tables, other revisions, strength/fatigue checks,
uncertainty envelopes and qualification of input geometry remain outside scope.
"""

from __future__ import annotations

from collections.abc import Iterable
from dataclasses import dataclass, field
from math import fsum, isfinite
from numbers import Real


def _finite(value: float, field: str) -> float:
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{field} must be a finite real number")
    try:
        if not isfinite(value):
            raise ValueError(f"{field} must be finite")
        return float(value)
    except OverflowError as exc:
        raise ValueError(f"{field} must be finite") from exc


@dataclass(frozen=True)
class RectangularPlate:
    """Uniform rectangular components; dimensions [m], density [kg/m3].

    The ``_1`` suffix marks a dimensionless count in ``quantity_1``.
    """

    length_m: float
    width_m: float
    thickness_m: float
    quantity_1: int
    density_kg_m3: float

    def __post_init__(self) -> None:
        for name in ("length_m", "width_m", "thickness_m", "density_kg_m3"):
            if _finite(getattr(self, name), name) <= 0:
                raise ValueError(f"{name} must be positive")
        if type(self.quantity_1) is not int or self.quantity_1 < 0:
            raise ValueError("quantity_1 must be a nonnegative integer")


@dataclass(frozen=True)
class MassItem:
    """Nonnegative mass [kg] at supplied signed coordinates [m]."""

    mass_kg: float
    x_m: float
    y_m: float
    z_m: float

    def __post_init__(self) -> None:
        if _finite(self.mass_kg, "mass_kg") < 0:
            raise ValueError("mass_kg must be nonnegative")
        for name in ("x_m", "y_m", "z_m"):
            _finite(getattr(self, name), name)


@dataclass(frozen=True)
class MassTotals:
    """Aggregate mass [kg] and first mass moments [kg m] about one datum.

    Aggregation retains constituent items for delegation of the centre to
    ``floating_wind.fowt_flexible_body.combine_mass_properties``. Explicitly
    constructed totals are adapted to an equivalent point mass instead.
    """

    mass_kg: float
    first_moment_x_kg_m: float
    first_moment_y_kg_m: float
    first_moment_z_kg_m: float
    _items: tuple[MassItem, ...] | None = field(
        default=None, init=False, repr=False, compare=False
    )

    def __post_init__(self) -> None:
        if _finite(self.mass_kg, "mass_kg") < 0:
            raise ValueError("mass_kg must be nonnegative")
        for name in (
            "first_moment_x_kg_m", "first_moment_y_kg_m", "first_moment_z_kg_m"
        ):
            moment = _finite(getattr(self, name), name)
            if self.mass_kg == 0 and moment != 0:
                raise ValueError("zero mass must have zero first moments")


def plate_areal_mass_kg_m2(plate: RectangularPlate) -> float:
    """Return thickness * density [kg/m2].

    Source context: ISO 19901-5; clause not stated in source.
    """
    return _finite(plate.thickness_m * plate.density_kg_m3, "areal_mass_kg_m2")


def plate_mass_kg(plate: RectangularPlate) -> float:
    """Return length * width * areal_mass * quantity [kg].

    Source context: ISO 19901-5; clause not stated in source.
    """
    if plate.quantity_1 == 0:
        return 0.0
    mass = plate.length_m * plate.width_m * plate_areal_mass_kg_m2(plate) * plate.quantity_1
    return _finite(mass, "mass_kg")


def first_mass_moments_kg_m(item: MassItem) -> tuple[float, float, float]:
    """Return (mass * x, mass * y, mass * z) [kg m].

    Source context: ISO 19901-5; clause not stated in source.
    """
    return (
        _finite(item.mass_kg * item.x_m, "first_moment_x_kg_m"),
        _finite(item.mass_kg * item.y_m, "first_moment_y_kg_m"),
        _finite(item.mass_kg * item.z_m, "first_moment_z_kg_m"),
    )


def combine_mass_items(items: Iterable[MassItem], *, mass_factor_1: float) -> MassTotals:
    """Return factor * sum(mass) and factor * sum(mass * coordinate).

    ``items`` must share a datum; select a category before calling if required.
    A uniform factor scales both mass and moments. Empty input yields zeros.
    ``floating_wind.fowt_flexible_body.combine_mass_properties`` is the
    canonical centre aggregation; a uniform factor leaves that centre unchanged.
    Source context: ISO 19901-5; clause not stated in source.
    """
    factor = _finite(mass_factor_1, "mass_factor_1")
    if factor <= 0:
        raise ValueError("mass_factor_1 must be positive")
    items = tuple(items)
    records = [(p.mass_kg, *first_mass_moments_kg_m(p)) for p in items]
    try:
        mass = fsum(p[0] for p in records) * factor
        mx = fsum(p[1] for p in records) * factor
        my = fsum(p[2] for p in records) * factor
        mz = fsum(p[3] for p in records) * factor
    except OverflowError as exc:
        raise ValueError("aggregate mass and moments must be finite") from exc
    totals = MassTotals(mass, mx, my, mz)
    object.__setattr__(totals, "_items", items)
    return totals


def centre_of_gravity_m(totals: MassTotals) -> tuple[float, float, float]:
    """Delegate the centre [m] to ``combine_mass_properties``; reject zero mass.

    Constituent items are passed through when available. Explicit moment totals
    are converted to one equivalent point mass at moment / mass [m]. Imports
    remain lazy so importing this module does not load floating wind or scipy.
    Source context: ISO 19901-5; clause not stated in source.
    """
    if totals.mass_kg <= 0:
        raise ValueError("centre of gravity requires positive mass")
    import numpy as np

    from digitalmodel.floating_wind.fowt_flexible_body import combine_mass_properties

    items = totals._items
    if items is None:
        items = (MassItem(
            totals.mass_kg,
            totals.first_moment_x_kg_m / totals.mass_kg,
            totals.first_moment_y_kg_m / totals.mass_kg,
            totals.first_moment_z_kg_m / totals.mass_kg,
        ),)
    _, centre, _ = combine_mass_properties([
        (item.mass_kg, (item.x_m, item.y_m, item.z_m), np.zeros((3, 3)))
        for item in items
    ])
    return tuple(_finite(value, f"centre_{axis}_m") for axis, value in zip("xyz", centre))
