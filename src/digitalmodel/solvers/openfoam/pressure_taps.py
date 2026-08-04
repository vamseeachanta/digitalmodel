#!/usr/bin/env python3
"""
ABOUTME: Named wall pressure taps for OpenFOAM sloshing/impact cases (dm#661).
Given a list of named taps - a point location, a named wall patch/surface, or a
point snapped onto a named patch - this module renders the corresponding
OpenFOAM ``functions{}`` entries (``probes`` for point pressure, ``patchProbes``
for a point on a wall, and ``surfaceFieldValue`` for a whole wall patch),
writing the pressure field ``p`` (and ``p_rgh`` when relevant) time histories.

This generalises the proven Kleefsman impact-sensor pattern
(``validation/kleefsman.py``) so callers can name their own taps. For a plain
rectangular tank, :func:`rectangular_tank_wall_taps` lays out a deterministic
rake of side-wall taps from caller-supplied dimensions; it encodes no particular
tank.

The tap data model lives in ``pressure_tap_models`` and the post-processing in
``pressure_tap_analysis``; both are re-exported here so existing import sites
keep working.
"""

from __future__ import annotations

import math
from typing import List, Optional, Sequence, Tuple

from .pressure_tap_analysis import (
    PressureTapStatistics,
    compute_tap_statistics,
    read_tap_statistics,
)
from .pressure_tap_models import DEFAULT_MULTIPHASE_FIELDS, PressureTap, _fmt

__all__ = [
    "DEFAULT_MULTIPHASE_FIELDS",
    "PressureTap",
    "PressureTapStatistics",
    "compute_tap_statistics",
    "point_tap_names",
    "read_tap_statistics",
    "rectangular_tank_wall_taps",
    "render_patch_probes_entry",
    "render_pressure_tap_functions",
    "render_probes_entry",
    "render_surface_entry",
]


# ============================================================================
# Function-object rendering
# ============================================================================


def _union_fields(taps: Sequence[PressureTap]) -> Tuple[str, ...]:
    """Ordered union of the fields requested across ``taps`` (p first)."""
    ordered: List[str] = []
    for tap in taps:
        for fld in tap.fields:
            if fld not in ordered:
                ordered.append(fld)
    # Keep 'p' leading for readability if present.
    if "p" in ordered:
        ordered = ["p"] + [f for f in ordered if f != "p"]
    return tuple(ordered)


def render_probes_entry(
    taps: Sequence[PressureTap],
    *,
    object_name: str = "pressureTaps",
    write_control: str = "timeStep",
    write_interval: int = 1,
) -> str:
    """Render a single ``probes`` function object for point taps.

    Columns in the output file follow ``taps`` order; the tap names are written
    as a comment so the mapping back to named taps is auditable.

    Args:
        taps: Point taps (``location`` set, ``patch`` unset).
        object_name: Function-object key in ``functions{}``.
        write_control: OpenFOAM ``writeControl`` (default ``"timeStep"``).
        write_interval: ``writeInterval`` in the chosen control units.

    Returns:
        The dictionary block text (indented for embedding in ``functions{}``).
    """
    if not taps:
        raise ValueError("render_probes_entry requires at least one tap.")
    fields = " ".join(_union_fields(taps))
    loc_lines = []
    for tap in taps:
        assert tap.location is not None  # guaranteed by caller
        x, y, z = tap.location
        loc_lines.append(
            f"            ({_fmt(x)} {_fmt(y)} {_fmt(z)})   // {tap.name}"
        )
    locs = "\n".join(loc_lines)
    names = ", ".join(t.name for t in taps)
    return f"""\
    {object_name}
    {{
        type            probes;
        libs            (sampling);
        writeControl    {write_control};
        writeInterval   {write_interval};
        fields          ({fields});
        // named taps (column order): {names}
        probeLocations
        (
{locs}
        );
    }}
"""


def render_patch_probes_entry(
    taps: Sequence[PressureTap],
    patch: str,
    *,
    object_name: Optional[str] = None,
    write_control: str = "timeStep",
    write_interval: int = 1,
) -> str:
    """Render a ``patchProbes`` function object for point-on-wall taps.

    ``patchProbes`` snaps each point to the nearest face of ``patch`` so the
    sample sits exactly on the wall - the correct choice for a wall pressure
    tap on a named boundary.

    Args:
        taps: Patch-point taps that all reference ``patch``.
        patch: The wall patch the points are snapped onto.
        object_name: Function-object key (default ``"pressureTaps_<patch>"``).
        write_control: OpenFOAM ``writeControl``.
        write_interval: ``writeInterval``.
    """
    if not taps:
        raise ValueError("render_patch_probes_entry requires at least one tap.")
    object_name = object_name or f"pressureTaps_{patch}"
    fields = " ".join(_union_fields(taps))
    loc_lines = []
    for tap in taps:
        assert tap.location is not None
        x, y, z = tap.location
        loc_lines.append(
            f"            ({_fmt(x)} {_fmt(y)} {_fmt(z)})   // {tap.name}"
        )
    locs = "\n".join(loc_lines)
    names = ", ".join(t.name for t in taps)
    return f"""\
    {object_name}
    {{
        type            patchProbes;
        libs            (sampling);
        patch           {patch};
        writeControl    {write_control};
        writeInterval   {write_interval};
        fields          ({fields});
        // named taps (column order): {names}
        probeLocations
        (
{locs}
        );
    }}
"""


def render_surface_entry(
    tap: PressureTap,
    *,
    write_control: str = "timeStep",
    write_interval: int = 1,
) -> str:
    """Render a ``surfaceFieldValue`` function object for a whole-patch tap.

    Reduces ``tap.fields`` over the named wall patch with ``tap.operation``
    (e.g. area-average or max pressure across the patch).
    """
    if tap.kind != "surface":
        raise ValueError(
            f"render_surface_entry expects a surface tap; {tap.name!r} is "
            f"a {tap.kind} tap."
        )
    fields = " ".join(tap.fields)
    return f"""\
    {tap.name}
    {{
        type            surfaceFieldValue;
        libs            (fieldFunctionObjects);
        regionType      patch;
        name            {tap.patch};
        operation       {tap.operation};
        fields          ({fields});
        writeControl    {write_control};
        writeInterval   {write_interval};
        writeFields     false;
        log             false;
    }}
"""


def render_pressure_tap_functions(
    taps: Sequence[PressureTap],
    *,
    write_control: str = "timeStep",
    write_interval: int = 1,
) -> str:
    """Render the full ``functions{}`` block for a list of named taps.

    Point taps are grouped into one ``probes`` object; patch-point taps are
    grouped into one ``patchProbes`` object per patch; each surface tap becomes
    its own ``surfaceFieldValue`` object. Returns ``""`` for an empty tap list
    so the caller can stay additive (no taps -> no output).

    Args:
        taps: The named taps.
        write_control: OpenFOAM ``writeControl`` for every emitted object.
        write_interval: ``writeInterval`` for every emitted object.

    Returns:
        A ``functions\\n{ ... }\\n`` block, or ``""`` when ``taps`` is empty.
    """
    taps = list(taps)
    if not taps:
        return ""

    _validate_unique_names(taps)

    point_taps = [t for t in taps if t.kind == "point"]
    patch_point_taps = [t for t in taps if t.kind == "patch_point"]
    surface_taps = [t for t in taps if t.kind == "surface"]

    entries: List[str] = []
    if point_taps:
        entries.append(
            render_probes_entry(
                point_taps,
                write_control=write_control,
                write_interval=write_interval,
            )
        )

    # Group patch-point taps by their patch (deterministic order of first use).
    by_patch: Dict[str, List[PressureTap]] = {}
    for tap in patch_point_taps:
        by_patch.setdefault(tap.patch, []).append(tap)  # type: ignore[arg-type]
    for patch, group in by_patch.items():
        entries.append(
            render_patch_probes_entry(
                group,
                patch,
                write_control=write_control,
                write_interval=write_interval,
            )
        )

    for tap in surface_taps:
        entries.append(
            render_surface_entry(
                tap,
                write_control=write_control,
                write_interval=write_interval,
            )
        )

    body = "".join(entries)
    return f"functions\n{{\n    // Named wall pressure taps (dm#661)\n{body}}}\n"


def _validate_unique_names(taps: Sequence[PressureTap]) -> None:
    seen: set[str] = set()
    for tap in taps:
        if tap.name in seen:
            raise ValueError(f"Duplicate pressure-tap name: {tap.name!r}")
        seen.add(tap.name)


def point_tap_names(taps: Sequence[PressureTap]) -> List[str]:
    """Names of the point taps in the order their probe columns are written.

    Use this to align a parsed ``probes`` output file (columns follow this
    order) with the tap names when computing statistics.
    """
    return [t.name for t in taps if t.kind == "point"]



# ============================================================================
# Neutral rectangular-tank wall taps
# ============================================================================


def _require_positive_dimension(name: str, value: float) -> float:
    """Return ``value`` if it is a finite, strictly positive length."""
    if not math.isfinite(value) or value <= 0.0:
        raise ValueError(f"{name} must be a finite positive length, got {value!r}.")
    return float(value)


def rectangular_tank_wall_taps(
    *,
    tank_length_m: float,
    tank_width_m: float,
    tap_elevations_m: Sequence[float],
    fields: Tuple[str, ...] = DEFAULT_MULTIPHASE_FIELDS,
) -> Tuple[PressureTap, ...]:
    """Deterministic side-wall pressure taps for a rectangular tank.

    Lays out a rake of point taps at mid-length on each of the two side walls,
    one pair per requested elevation. The layout is derived entirely from the
    caller's dimensions and carries no built-in geometry.

    Taps are emitted in a fixed order - for each elevation in the order given,
    the ``y = 0`` wall first and the ``y = tank_width_m`` wall second - and are
    named ``wall_1``, ``wall_2``, ... in that emission order.

    Coordinates assume a tank occupying ``[0, tank_length_m] x
    [0, tank_width_m]`` in plan, with ``z`` measured up from the tank floor at
    ``z = 0``.

    Args:
        tank_length_m: Tank length along x (m). Finite and positive.
        tank_width_m: Tank width/beam along y (m). Finite and positive.
        tap_elevations_m: Elevations above the tank floor (m). Non-empty, each
            finite and strictly above the floor, and all distinct.
        fields: Fields to sample at each tap.

    Returns:
        A tuple of named :class:`PressureTap`, two per elevation.

    Raises:
        ValueError: If any dimension is not a finite positive length, if no
            elevation is given, if an elevation is not finite or not strictly
            above the floor, or if elevations repeat.
    """
    length = _require_positive_dimension("tank_length_m", tank_length_m)
    width = _require_positive_dimension("tank_width_m", tank_width_m)

    elevations = tuple(tap_elevations_m)
    if not elevations:
        raise ValueError("tap_elevations_m must contain at least one elevation.")
    for elevation in elevations:
        if not math.isfinite(elevation):
            raise ValueError(
                f"tap elevations must be finite, got {elevation!r}."
            )
        if elevation <= 0.0:
            raise ValueError(
                "tap elevations must be strictly above the tank floor at 0.0, "
                f"got {elevation!r}."
            )
    if len(set(elevations)) != len(elevations):
        raise ValueError("tap elevations must be distinct.")

    mid_length = length / 2.0
    taps: List[PressureTap] = []
    for elevation in elevations:
        for wall_y in (0.0, width):
            taps.append(
                PressureTap(
                    name=f"wall_{len(taps) + 1}",
                    location=(mid_length, wall_y, float(elevation)),
                    fields=fields,
                )
            )
    return tuple(taps)
