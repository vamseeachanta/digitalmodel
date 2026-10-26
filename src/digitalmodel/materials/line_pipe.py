# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
# ABOUTME: Line-pipe geometry from validated caller-supplied dimensions.
# ABOUTME: Section properties are computed; no dimensional tables are bundled.
"""Caller-supplied line pipe with computed section properties.

ID = OD - 2t; metal area = pi*(OD**2 - ID**2)/4;
bore area = pi*ID**2/4; mass per length = metal area*density.
Density comes from the supplied material row, including for explicit geometry.
"""
from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Optional

from .grades import MaterialGrade, get as _get_grade

# Unit conversions.
_IN_TO_M = 0.0254
_M_TO_IN = 1.0 / _IN_TO_M
_KG_M_TO_LB_FT = 0.671968994
_M3_PER_M_TO_BBL_PER_FT = 6.28981 * 0.3048  # m^3/m -> bbl/ft (1 m^3 = 6.28981 bbl)

from .catalog import CatalogView, Source, current_catalog, positive, schedule_key


NPS_OD_IN = CatalogView(lambda c: {d.nps: d.od_in for d in c.dimensions})
SCHEDULE_WT_IN = CatalogView(lambda c: {
    nps: {d.schedule: d.wt_in for d in c.dimensions if d.nps == nps}
    for nps in {d.nps for d in c.dimensions}
})


@dataclass(frozen=True)
class LinePipe:
    """A caller-supplied line-pipe product (geometry, grade and source).

    Geometry is stored in inches; SI and weight properties are derived.

    Attributes:
        od_in: Outside diameter (inches).
        wt_in: Wall thickness (inches).
        grade: The caller-supplied :class:`MaterialGrade` for this product.
        nps: Nominal Pipe Size (inches) if from the catalog, else ``None``.
        schedule: Schedule label (e.g. ``"SCH 80"``) if specified, else ``None``.
    """

    od_in: float
    wt_in: float
    grade: MaterialGrade
    nps: Optional[float] = None
    schedule: Optional[str] = None
    geometry_source: Optional[Source] = None

    def __post_init__(self) -> None:
        positive(self.od_in, "od_in")
        positive(self.wt_in, "wt_in")
        if not isinstance(self.grade, MaterialGrade):
            raise ValueError("grade must be a validated MaterialGrade")
        if not isinstance(self.geometry_source, Source):
            raise ValueError("geometry_source must be a validated Source")
        if self.nps is not None:
            positive(self.nps, "nps")
        if self.wt_in >= self.od_in / 2.0:
            raise ValueError(
                f"wt_in ({self.wt_in}) must be < od_in/2 ({self.od_in / 2.0})")

    # --- geometry -------------------------------------------------------
    @property
    def id_in(self) -> float:
        """Inside diameter (inches)."""
        return self.od_in - 2.0 * self.wt_in

    @property
    def d_over_t(self) -> float:
        """Outside-diameter to wall-thickness ratio."""
        return self.od_in / self.wt_in

    @property
    def metal_area_in2(self) -> float:
        """Cross-sectional metal area (in^2)."""
        return math.pi / 4.0 * (self.od_in ** 2 - self.id_in ** 2)

    @property
    def bore_area_in2(self) -> float:
        """Internal (bore) cross-sectional area (in^2)."""
        return math.pi / 4.0 * self.id_in ** 2

    @property
    def od_mm(self) -> float:
        return self.od_in * 25.4

    @property
    def wt_mm(self) -> float:
        return self.wt_in * 25.4

    @property
    def id_mm(self) -> float:
        return self.id_in * 25.4

    # --- weight & volume ------------------------------------------------
    @property
    def weight_kg_m(self) -> float:
        """Plain-end weight (kg/m) = metal area x density."""
        area_m2 = self.metal_area_in2 * _IN_TO_M ** 2
        return area_m2 * self.grade.rho_kg_m3

    @property
    def weight_lb_ft(self) -> float:
        """Plain-end weight (lb/ft)."""
        return self.weight_kg_m * _KG_M_TO_LB_FT

    @property
    def internal_volume_m3_per_m(self) -> float:
        """Internal (bore) volume per unit length (m^3 per m) = bore area."""
        return self.bore_area_in2 * _IN_TO_M ** 2

    @property
    def internal_volume_bbl_per_ft(self) -> float:
        """Internal (bore) volume per unit length (barrels per foot)."""
        return self.internal_volume_m3_per_m * _M3_PER_M_TO_BBL_PER_FT

    @property
    def api_5l_reference(self) -> str:
        """Human-readable product reference."""
        size = (f"NPS {self.nps:g}" if self.nps is not None
                else f"OD {self.od_in:g} in")
        sched = f" {self.schedule}" if self.schedule else ""
        return f"{self.grade.standard} {self.grade.name} line pipe, {size}{sched}"


# ---------------------------------------------------------------------------
# Catalog query API
# ---------------------------------------------------------------------------
def list_nps() -> list[float]:
    """All catalog Nominal Pipe Sizes (inches), ascending."""
    return sorted(NPS_OD_IN)


def od_for_nps(nps: float) -> float:
    """Outside diameter (inches) for a Nominal Pipe Size.

    Raises:
        KeyError: if the NPS is not in the catalog.
    """
    positive(nps, "nps")
    if nps not in NPS_OD_IN:
        raise KeyError(f"NPS {nps} not in caller-supplied catalogue")
    return NPS_OD_IN[nps]


def available_schedules(nps: float) -> list[str]:
    """Schedule labels tabulated for a Nominal Pipe Size."""
    positive(nps, "nps")
    return list(SCHEDULE_WT_IN.get(nps, {}))


def wt_for_schedule(nps: float, schedule: str) -> float:
    """Wall thickness (inches) for an NPS + schedule.

    Raises:
        KeyError: if the NPS/schedule combination is not tabulated.
    """
    positive(nps, "nps")
    table = SCHEDULE_WT_IN.get(nps)
    if not table:
        raise KeyError(f"No schedule table for NPS {nps}")
    key = schedule_key(schedule.strip())
    # Normalise "SCH40"/"sch 40"/"40" -> "SCH 40"; pass STD/XS/XXS through.
    if key not in table:
        if key.isdigit():
            key = f"SCH {key}"
        elif key.startswith("SCH") and key != "SCH" and " " not in key:
            key = f"SCH {key[3:].strip()}"
    if key not in table:
        raise KeyError(
            f"Schedule {schedule!r} not tabulated for NPS {nps}; "
            f"available: {sorted(table)}")
    return table[key]


def line_pipe(
    grade: str | MaterialGrade,
    *,
    nps: Optional[float] = None,
    schedule: Optional[str] = None,
    od_in: Optional[float] = None,
    wt_in: Optional[float] = None,
    geometry_source: Optional[Source] = None,
) -> LinePipe:
    """Build a :class:`LinePipe` from a grade plus size + wall specification.

    Provide either ``nps`` (catalog OD) or ``od_in`` for the diameter, and
    either ``schedule`` (catalog wall) or ``wt_in`` for the wall thickness.

    Args:
        grade: API 5L grade name or :class:`MaterialGrade`.
        nps: Nominal Pipe Size (inches); looks up the catalog OD.
        schedule: caller-supplied schedule label (with ``nps``).
        od_in: Explicit outside diameter (inches), instead of ``nps``.
        wt_in: Explicit wall thickness (inches), instead of ``schedule``.

    Raises:
        ValueError: if the diameter or wall specification is missing/ambiguous.
        KeyError: if the NPS/schedule combination is not in the catalog.
    """
    g = grade if isinstance(grade, MaterialGrade) else _get_grade(grade)

    # Diameter.
    if nps is not None and od_in is not None:
        raise ValueError("Specify either nps or od_in, not both")
    if nps is not None:
        od = od_for_nps(nps)
    elif od_in is not None:
        od = od_in
    else:
        raise ValueError("Provide nps or od_in for the diameter")

    # Wall thickness.
    if schedule is not None and wt_in is not None:
        raise ValueError("Specify either schedule or wt_in, not both")
    if schedule is not None:
        if nps is None:
            raise ValueError("schedule requires nps (catalog wall lookup)")
        wt = wt_for_schedule(nps, schedule)
        geometry_source = next(
            row.source for row in current_catalog().dimensions
            if row.nps == nps and row.schedule == schedule_key(schedule.strip())
        )
    elif wt_in is not None:
        wt = wt_in
    else:
        raise ValueError("Provide schedule or wt_in for the wall thickness")

    return LinePipe(od_in=od, wt_in=wt, grade=g, nps=nps, schedule=schedule,
                    geometry_source=geometry_source)
