"""Top tensioner schema for top-tensioned and drilling risers (#3976).

Kept apart from ``riser.py`` so that module does not grow further; ``riser.py``
re-exports :class:`RiserTensioner` and calls :func:`validate_tensioners` from
the ``Riser`` model validator.
"""

from __future__ import annotations

from collections.abc import Iterable, Sequence
from typing import Any

from pydantic import BaseModel, ConfigDict, Field


class RiserTensioner(BaseModel):
    """Top tensioner system for a top-tensioned or drilling riser (#3976).

    ``count`` linear spring/damper links run from points on the vessel to the
    free End A node (``arc_length`` must be zero). Interior attachments are
    unsupported because OrcaFlex snaps them to reference-arc-length nodes.
    The vessel points lie on a circle of
    ``radius`` about the riser top in plan, at height ``vessel_z``, in vessel
    axes. The generator sets each unstretched length so that, in the initial
    (generated) geometry, the vertical components of the link tensions sum
    to ``total_tension``:

        T_i = (total_tension / count) * L / dz,   L0 = L - T_i / stiffness

    where L is the link length and dz its vertical extent. The tension is a
    target for the initial geometry; static equilibrium changes it by
    stiffness x (top displacement along the link).

    Attributes:
        name: Prefix for the link names (``<name>_1`` ... ``<name>_<count>``).
        line: Riser line whose End A is supported. That end must be ``free``.
        count: Number of tensioner units.
        total_tension: Target total vertical link tension in the generated geometry (kN).
        stiffness: Stiffness per unit (kN/m).
        damping: Linear damping per unit (kN.s/m). 0 means no damper.
        radius: Plan radius of the vessel points about the riser top (m).
        vessel_z: Height of the vessel points in vessel axes (m).
        start_angle: Plan angle of the first unit in vessel axes (deg).
        arc_length: Attachment arc length from End A (m); only zero is supported.

    Every number must be finite: NaN or infinite inputs would otherwise emit
    NaN connection coordinates and unstretched lengths.
    """

    model_config = ConfigDict(allow_inf_nan=False)

    name: str = Field(..., min_length=1, description="Link name prefix")
    line: str = Field(..., min_length=1, description="Supported riser line")
    count: int = Field(..., ge=1, le=12, description="Number of units")
    total_tension: float = Field(
        ...,
        gt=0,
        description=(
            "Target total vertical link tension in the generated geometry (kN); "
            "the static value is lower by vertical tensioner stiffness x top rise "
            "(linearised)."
        ),
    )
    stiffness: float = Field(..., gt=0, description="Stiffness per unit (kN/m)")
    damping: float = Field(default=0, ge=0, description="Damping per unit (kN.s/m)")
    radius: float = Field(..., gt=0, description="Plan radius (m)")
    vessel_z: float = Field(..., description="Vessel point height, vessel axes (m)")
    start_angle: float = Field(default=0, description="First unit angle (deg)")
    arc_length: float = Field(default=0, ge=0, description="Attachment arc length (m), End A only")


def validate_tensioners(
    tensioners: Sequence[RiserTensioner], lines: Iterable[Any], links: Iterable[Any]
) -> None:
    """Tensioners support the free End A of a defined line; names unique.

    ``lines`` are ``RiserLine`` and ``links`` are ``RiserLink`` objects. Emitted
    names ``<name>_<i>`` and explicit link names are compared case-folded.
    """
    by_name = {line.name: line for line in lines}
    names = [
        f"{t.name}_{i}"
        for t in tensioners
        for i in range(1, t.count + 1)
    ] + [link.name for link in links]
    for tensioner in tensioners:
        line = by_name.get(tensioner.line)
        if line is None:
            raise ValueError(
                f"Tensioner '{tensioner.name}' references unknown line '{tensioner.line}'"
            )
        # ConnectionType is a str enum; comparing with its value avoids a
        # circular import of riser.py.
        if line.end_a.type != "free":
            raise ValueError(
                f"Tensioner '{tensioner.name}': End A of line '{line.name}' must be "
                f"'free' (got '{line.end_a.type.value}')"
            )
        if tensioner.arc_length > line.get_total_length():
            raise ValueError(
                f"Tensioner '{tensioner.name}' arc_length beyond line '{line.name}'"
            )
        if tensioner.arc_length != 0:
            raise ValueError(
                f"Tensioner '{tensioner.name}' supports End A only "
                "(arc_length must be zero); interior attachment snapping is unsupported"
            )
    if len(names) != len({name.casefold() for name in names}):
        raise ValueError(f"Duplicate link or tensioner names: {names}")
