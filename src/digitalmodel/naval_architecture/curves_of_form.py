# ABOUTME: Curves of form — displacement, KM, TPI, MT1 vs draft interpolation
# ABOUTME: Requires ship-specific hydrostatic data tables (DDG51, FFG-7, etc.)
"""
Curves of form reader and interpolator.

Provides displacement, KM, TPI, MT1" as functions of draft
for specific vessel types. Requires ship data tables.
"""

from collections.abc import Sequence
from dataclasses import dataclass
from math import isfinite
from numbers import Real

from .integration import simpsons_first_rule


def displacement_at_draft(vessel: str, draft_ft: float) -> float:
    """Interpolate displacement from curves of form at given draft.

    For vessels with tabular curves of form data (e.g. DDG-51), performs
    linear interpolation. For registered fleet vessels without tabular data,
    estimates using the block coefficient method.
    """
    from digitalmodel.naval_architecture.ship_data import (
        get_curves_of_form, get_ship, estimate_vessel_hydrostatics,
    )

    cof = get_curves_of_form(vessel)
    if cof is not None:
        drafts = cof["drafts_ft"]
        disps = cof["displacement_lt"]
        return _interpolate(drafts, disps, draft_ft)

    ship = get_ship(vessel)
    if ship is not None:
        hydro = estimate_vessel_hydrostatics(ship)
        if hydro is not None:
            cb = hydro["cb"]
            loa = float(ship.get("loa_ft", 0))
            beam = float(ship.get("beam_ft", 0))
            if loa > 0 and beam > 0 and draft_ft > 0:
                vol = cb * loa * beam * draft_ft
                return 64.0 * vol / 2240.0

    raise NotImplementedError(
        f"Curves of form data for {vessel} not yet available. "
        "Need hydrostatic tables from ship design data."
    )


def _interpolate(xs, ys, x):
    """Linear interpolation with clamping at boundaries."""
    if x <= xs[0]:
        return float(ys[0])
    if x >= xs[-1]:
        return float(ys[-1])
    for i in range(1, len(xs)):
        if xs[i] >= x:
            t = (x - xs[i - 1]) / (xs[i] - xs[i - 1])
            return ys[i - 1] + t * (ys[i] - ys[i - 1])
    return float(ys[-1])


@dataclass(frozen=True)
class OffsetDisplacement:
    """Single-draft offset integrals in metres, cubic metres, kg and square metres."""

    draft_m: float
    submerged_volume_m3: float
    displacement_mass_kg: float
    waterplane_area_m2: float


def offset_displacement(
    half_breadths_m: Sequence[Sequence[float]],
    *,
    station_spacing_m: float,
    waterline_spacing_m: float,
    density_kg_m3: float,
) -> OffsetDisplacement:
    """Integrate uniform half-breadth offsets at their top waterline, at even keel.

    Symmetry about y=0 is assumed, not verified. Coordinates are x forward
    from the aft end, y to port and z upward from the keel. Rows start at z=0;
    columns start at x=0. Each count must be odd and at least three. The sole
    draft is (row count - 1) * waterline_spacing_m; no interpolation is applied.

    Args:
        half_breadths_m: Rows of waterlines, columns of nonnegative half-breadths
            in metres. Inputs are copied; real-hull sampling adequacy remains
            the caller's responsibility. Analytic accuracy does not qualify
            under-resolved hull geometry.
        station_spacing_m: Finite positive station spacing in metres.
        waterline_spacing_m: Finite positive waterline spacing in metres.
        density_kg_m3: Finite positive fluid mass density in kg per cubic metre.

    Returns:
        Immutable draft, submerged volume, displacement mass and waterplane area.
        An all-zero grid gives zero volume, mass and area at the declared draft.

    Raises:
        ValueError: Invalid geometry, numeric input or nonfinite dimensional result.
    """
    dx = _offset_number(station_spacing_m, "station_spacing_m")
    dz = _offset_number(waterline_spacing_m, "waterline_spacing_m")
    density = _offset_number(density_kg_m3, "density_kg_m3")
    grid = _offset_grid(half_breadths_m)
    _offset_number((len(grid[0]) - 1) * dx, "station extent")
    draft = _offset_number((len(grid) - 1) * dz, "draft")
    areas = [
        _offset_number(
            simpsons_first_rule([2.0 * b for b in row], dx),
            "waterplane area", allow_zero=True,
        )
        for row in grid
    ]
    volume = _offset_number(
        simpsons_first_rule(areas, dz), "submerged volume", allow_zero=True
    )
    mass = _offset_number(volume * density, "displacement mass", allow_zero=True)
    return OffsetDisplacement(draft, volume, mass, areas[-1])


def _offset_number(value: float, name: str, *, allow_zero: bool = False) -> float:
    """Validate a real SI scalar without accepting booleans or numeric text."""
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a finite real number")
    try:
        number = float(value)
    except OverflowError as error:
        raise ValueError(f"{name} is outside the finite numeric range") from error
    if not isfinite(number) or number < 0 or (number == 0 and not allow_zero):
        raise ValueError(f"{name} must be finite and {'nonnegative' if allow_zero else 'positive'}")
    return number


def _offset_grid(half_breadths_m: Sequence[Sequence[float]]) -> list[list[float]]:
    """Copy and validate a rectangular odd-by-odd grid of nonnegative offsets."""
    try:
        grid = [list(row) for row in half_breadths_m]
    except TypeError as error:
        raise ValueError("half_breadths_m must contain rows of real numbers") from error
    if len(grid) < 3 or len(grid) % 2 == 0:
        raise ValueError("Need an odd number of at least three waterlines")
    count = len(grid[0])
    if count < 3 or count % 2 == 0 or any(len(row) != count for row in grid):
        raise ValueError("Need a rectangular grid with an odd count of at least three stations")
    return [
        [_offset_number(b, "half-breadth", allow_zero=True) for b in row]
        for row in grid
    ]
