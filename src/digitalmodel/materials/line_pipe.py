# ABOUTME: API 5L line-pipe product catalog — NPS x schedule (ASME B36.10M) x
# ABOUTME: grade -> LinePipe object carrying geometry, material, and API 5L ref.
"""API 5L line pipe as a usable product catalog.

A line pipe is a *product*: a standard outside diameter (ASME B36.10M / API 5L),
a wall thickness (a schedule or an explicit value), and an API 5L grade.  This
module crosses the dimensional catalog with the canonical material matrix
(:mod:`digitalmodel.materials.grades`) and returns a :class:`LinePipe` carrying
the geometry, the :class:`~digitalmodel.materials.grades.MaterialGrade`, and the
derived section properties (ID, D/t, metal area, plain-end weight, bore volume).

Dimensions are ASME B36.10M (Welded and Seamless Wrought Steel Pipe); the
governing product standard is API 5L / ISO 3183 (line pipe).

Plain-end weight is the section metal area times density (7850 kg/m^3), which
reproduces the ASME B36.10M weight column; the closed form is
``w = 10.68 * t * (OD - t)`` lb/ft (OD, t in inches).
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

# ---------------------------------------------------------------------------
# ASME B36.10M dimensions
# ---------------------------------------------------------------------------
#: Nominal Pipe Size -> outside diameter (inches).  For NPS >= 14, OD == NPS.
NPS_OD_IN: dict[float, float] = {
    0.125: 0.405, 0.25: 0.540, 0.375: 0.675, 0.5: 0.840, 0.75: 1.050,
    1.0: 1.315, 1.25: 1.660, 1.5: 1.900, 2.0: 2.375, 2.5: 2.875,
    3.0: 3.500, 3.5: 4.000, 4.0: 4.500, 5.0: 5.563, 6.0: 6.625,
    8.0: 8.625, 10.0: 10.750, 12.0: 12.750,
    14.0: 14.0, 16.0: 16.0, 18.0: 18.0, 20.0: 20.0, 22.0: 22.0, 24.0: 24.0,
    26.0: 26.0, 28.0: 28.0, 30.0: 30.0, 32.0: 32.0, 34.0: 34.0, 36.0: 36.0,
    38.0: 38.0, 40.0: 40.0, 42.0: 42.0, 44.0: 44.0, 46.0: 46.0, 48.0: 48.0,
    52.0: 52.0, 56.0: 56.0, 60.0: 60.0, 64.0: 64.0, 68.0: 68.0, 72.0: 72.0,
    76.0: 76.0, 80.0: 80.0,
}

#: NPS -> {schedule: wall thickness (inches)} per ASME B36.10M.  Not every
#: NPS/schedule combination exists; use the explicit-WT path for others.
SCHEDULE_WT_IN: dict[float, dict[str, float]] = {
    0.125: {"SCH 40": 0.068, "STD": 0.068, "SCH 80": 0.095, "XS": 0.095},
    0.25: {"SCH 40": 0.088, "STD": 0.088, "SCH 80": 0.119, "XS": 0.119},
    0.375: {"SCH 40": 0.091, "STD": 0.091, "SCH 80": 0.126, "XS": 0.126},
    0.5: {"SCH 40": 0.109, "STD": 0.109, "SCH 80": 0.147, "XS": 0.147,
          "SCH 160": 0.188, "XXS": 0.294},
    0.75: {"SCH 40": 0.113, "STD": 0.113, "SCH 80": 0.154, "XS": 0.154,
           "SCH 160": 0.219, "XXS": 0.308},
    1.0: {"SCH 40": 0.133, "STD": 0.133, "SCH 80": 0.179, "XS": 0.179,
          "SCH 160": 0.250, "XXS": 0.358},
    1.25: {"SCH 40": 0.140, "STD": 0.140, "SCH 80": 0.191, "XS": 0.191,
           "SCH 160": 0.250, "XXS": 0.382},
    1.5: {"SCH 40": 0.145, "STD": 0.145, "SCH 80": 0.200, "XS": 0.200,
          "SCH 160": 0.281, "XXS": 0.400},
    2.0: {"SCH 40": 0.154, "STD": 0.154, "SCH 80": 0.218, "XS": 0.218,
          "SCH 160": 0.344, "XXS": 0.436},
    2.5: {"SCH 40": 0.203, "STD": 0.203, "SCH 80": 0.276, "XS": 0.276,
          "SCH 160": 0.375, "XXS": 0.552},
    3.0: {"SCH 40": 0.216, "STD": 0.216, "SCH 80": 0.300, "XS": 0.300,
          "SCH 160": 0.438, "XXS": 0.600},
    3.5: {"SCH 40": 0.226, "STD": 0.226, "SCH 80": 0.318, "XS": 0.318,
          "XXS": 0.636},
    4.0: {"SCH 40": 0.237, "STD": 0.237, "SCH 80": 0.337, "XS": 0.337,
          "SCH 120": 0.438, "SCH 160": 0.531, "XXS": 0.674},
    5.0: {"SCH 40": 0.258, "STD": 0.258, "SCH 80": 0.375, "XS": 0.375,
          "SCH 120": 0.500, "SCH 160": 0.625, "XXS": 0.750},
    6.0: {"SCH 40": 0.280, "STD": 0.280, "SCH 80": 0.432, "XS": 0.432,
          "SCH 120": 0.562, "SCH 160": 0.719, "XXS": 0.864},
    8.0: {"SCH 20": 0.250, "SCH 30": 0.277, "SCH 40": 0.322, "STD": 0.322,
          "SCH 60": 0.406, "SCH 80": 0.500, "XS": 0.500, "SCH 100": 0.594,
          "SCH 120": 0.719, "SCH 140": 0.812, "XXS": 0.875, "SCH 160": 0.906},
    10.0: {"SCH 20": 0.250, "SCH 30": 0.307, "SCH 40": 0.365, "STD": 0.365,
           "SCH 60": 0.500, "XS": 0.500, "SCH 80": 0.594, "SCH 100": 0.719,
           "SCH 120": 0.844, "SCH 140": 1.000, "XXS": 1.000, "SCH 160": 1.125},
    12.0: {"SCH 20": 0.250, "SCH 30": 0.330, "STD": 0.375, "SCH 40": 0.406,
           "XS": 0.500, "SCH 60": 0.562, "SCH 80": 0.688, "SCH 100": 0.844,
           "SCH 120": 1.000, "XXS": 1.000, "SCH 140": 1.125, "SCH 160": 1.312},
    14.0: {"SCH 10": 0.250, "SCH 20": 0.312, "SCH 30": 0.375, "STD": 0.375,
           "SCH 40": 0.438, "XS": 0.500, "SCH 60": 0.594, "SCH 80": 0.750,
           "SCH 100": 0.938, "SCH 120": 1.094, "SCH 140": 1.250,
           "SCH 160": 1.406},
    16.0: {"SCH 10": 0.250, "SCH 20": 0.312, "SCH 30": 0.375, "STD": 0.375,
           "SCH 40": 0.500, "XS": 0.500, "SCH 60": 0.656, "SCH 80": 0.844,
           "SCH 100": 1.031, "SCH 120": 1.219, "SCH 140": 1.438,
           "SCH 160": 1.594},
    18.0: {"SCH 10": 0.250, "SCH 20": 0.312, "STD": 0.375, "SCH 30": 0.438,
           "XS": 0.500, "SCH 40": 0.562, "SCH 60": 0.750, "SCH 80": 0.938,
           "SCH 100": 1.156, "SCH 120": 1.375, "SCH 140": 1.562,
           "SCH 160": 1.781},
    20.0: {"SCH 10": 0.250, "SCH 20": 0.375, "STD": 0.375, "SCH 30": 0.500,
           "XS": 0.500, "SCH 40": 0.594, "SCH 60": 0.812, "SCH 80": 1.031,
           "SCH 100": 1.281, "SCH 120": 1.500, "SCH 140": 1.750,
           "SCH 160": 1.969},
    22.0: {"SCH 10": 0.250, "SCH 20": 0.375, "STD": 0.375, "SCH 30": 0.500,
           "XS": 0.500, "SCH 60": 0.875, "SCH 80": 1.125, "SCH 100": 1.375,
           "SCH 120": 1.625, "SCH 140": 1.875, "SCH 160": 2.125},
    24.0: {"SCH 10": 0.250, "SCH 20": 0.375, "STD": 0.375, "XS": 0.500,
           "SCH 30": 0.562, "SCH 40": 0.688, "SCH 60": 0.969, "SCH 80": 1.219,
           "SCH 100": 1.531, "SCH 120": 1.812, "SCH 140": 2.062,
           "SCH 160": 2.344},
    26.0: {"SCH 10": 0.312, "STD": 0.375, "SCH 20": 0.500, "XS": 0.500},
    28.0: {"SCH 10": 0.312, "STD": 0.375, "SCH 20": 0.500, "XS": 0.500,
           "SCH 30": 0.625},
    30.0: {"SCH 10": 0.312, "STD": 0.375, "SCH 20": 0.500, "XS": 0.500,
           "SCH 30": 0.625},
    32.0: {"SCH 10": 0.312, "STD": 0.375, "SCH 20": 0.500, "XS": 0.500,
           "SCH 30": 0.625, "SCH 40": 0.688},
    34.0: {"SCH 10": 0.312, "STD": 0.375, "SCH 20": 0.500, "XS": 0.500,
           "SCH 30": 0.625, "SCH 40": 0.688},
    36.0: {"SCH 10": 0.312, "STD": 0.375, "SCH 20": 0.500, "XS": 0.500,
           "SCH 30": 0.625, "SCH 40": 0.750},
    42.0: {"STD": 0.375, "XS": 0.500},
    48.0: {"STD": 0.375, "XS": 0.500},
}

# Density used for the plain-end weight (matches the materials matrix default).
_RHO_KG_M3 = 7850.0


# ---------------------------------------------------------------------------
# LinePipe product object
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class LinePipe:
    """A specific API 5L line-pipe product (geometry + grade + reference).

    Geometry is stored in inches; SI and weight properties are derived.

    Attributes:
        od_in: Outside diameter (inches).
        wt_in: Wall thickness (inches).
        grade: The :class:`MaterialGrade` (API 5L) for this product.
        nps: Nominal Pipe Size (inches) if from the catalog, else ``None``.
        schedule: Schedule label (e.g. ``"SCH 80"``) if specified, else ``None``.
    """

    od_in: float
    wt_in: float
    grade: MaterialGrade
    nps: Optional[float] = None
    schedule: Optional[str] = None

    def __post_init__(self) -> None:
        if self.od_in <= 0.0:
            raise ValueError(f"od_in must be > 0, got {self.od_in}")
        if self.wt_in <= 0.0:
            raise ValueError(f"wt_in must be > 0, got {self.wt_in}")
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
        return area_m2 * _RHO_KG_M3

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
        return f"API 5L {self.grade.name} line pipe, {size}{sched}"


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
    if nps not in NPS_OD_IN:
        raise KeyError(f"NPS {nps} not in ASME B36.10M catalog")
    return NPS_OD_IN[nps]


def available_schedules(nps: float) -> list[str]:
    """Schedule labels tabulated for a Nominal Pipe Size."""
    return list(SCHEDULE_WT_IN.get(nps, {}))


def wt_for_schedule(nps: float, schedule: str) -> float:
    """Wall thickness (inches) for an NPS + schedule.

    Raises:
        KeyError: if the NPS/schedule combination is not tabulated.
    """
    table = SCHEDULE_WT_IN.get(nps)
    if not table:
        raise KeyError(f"No schedule table for NPS {nps}")
    key = schedule.strip().upper()
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
) -> LinePipe:
    """Build a :class:`LinePipe` from a grade plus size + wall specification.

    Provide either ``nps`` (catalog OD) or ``od_in`` for the diameter, and
    either ``schedule`` (catalog wall) or ``wt_in`` for the wall thickness.

    Args:
        grade: API 5L grade name or :class:`MaterialGrade`.
        nps: Nominal Pipe Size (inches); looks up the catalog OD.
        schedule: ASME B36.10M schedule label (with ``nps``).
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
    elif wt_in is not None:
        wt = wt_in
    else:
        raise ValueError("Provide schedule or wt_in for the wall thickness")

    return LinePipe(od_in=od, wt_in=wt, grade=g, nps=nps, schedule=schedule)
