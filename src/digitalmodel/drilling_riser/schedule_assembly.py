"""Schedule-assembly path (#1458, epic #1279; follows #1453/#1454).

Builds a rig-specific riser string FROM the wiki-side joint schedule + joint
library on a private llm-wiki source page (never from a transcribed factored
chain), normalizes each component into the ``adapter`` SI vocabulary, and
assembles it through :class:`~digitalmodel.drilling_riser.assembly.RiserStackupModel`.
The result feeds the API RP 16Q minimum-top-tension chain
(:func:`~digitalmodel.drilling_riser.stackup.minimum_slip_ring_tension` +
the 2H methodology 1.25 top-tension factor) for validation against the
wave-3 ``16q-min-tension-endpoints`` contracts (llm-wiki#828).

Discipline mirrors ``calc_contracts``: dm carries only PAGE NAMES, component
class labels and parse shapes; schedule counts, joint weights and geometry
VALUES live wiki-side and resolve at run/test time. Bounded reads,
fail-closed on any unparsed field, no fuzzy matching.

Documented modelling assumptions (issue #1458 scope 4 — fields the wiki
schedules do NOT carry, encoded once here and echoed per-assembly in
:attr:`ScheduleAssembly.assumptions`):

* Tensioned string = outer barrel down to and including the lower flexjoint
  (LMRP/BOP/wellhead/conductor excluded; overpull is not part of the API
  minimum).
* Weight basis is the library's NET submerged weights (f_wt = f_bt = 1.0):
  the sources document no gross-steel/uplift split for buoyed joints.
* Pup/termination joints without library weights take the slick-joint
  per-foot rate (their 21.63 x 1.188 in walls are heavier than the slick
  21 x 0.875 in tube but carry the same aux-line cluster).
* Flexjoint / outer-barrel submerged weight = dry weight x (1 - rho_sw /
  rho_steel) — only dry values are documented.
* Internal fluid columns: mud fills the main bore + both choke/kill lines +
  the mud-boost line from the string base (lower-flexjoint datum) to the
  drill floor; seawater backpressure acts on the same area to the
  waterline. Seawater at 1025 kg/m3.
* Minimum top tension = 1.25 x TSRmin (2H-TNE-0050-03 §3.1 project factor,
  ``stackup.SAFETY_FACTOR_TENSION`` — numerically identical to the 16Q
  N/(Rf(N-n)) allowance with N=6, n=1, Rf=0.96). Validated to <=1.7 % on
  the RSU-0019 endpoints; see ``tests/drilling_riser/test_schedule_assembly.py``
  for the per-RSU pass/finding stance.
"""

from __future__ import annotations

import math
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Optional

from digitalmodel.drilling_riser.assembly import RiserStackupModel
from digitalmodel.drilling_riser.calc_contracts import (
    MAX_PAGE_BYTES,
    SOURCES_REL,
    find_contract,
)
from digitalmodel.drilling_riser.stackup import (
    SAFETY_FACTOR_TENSION,
    minimum_slip_ring_tension,
    top_tension_required,
)

__all__ = [
    "SCHEDULE_RSU_IDS",
    "ScheduleAssembly",
    "ScheduleAssemblyError",
    "StringGeometry",
    "documented_endpoints",
    "load_schedule_assembly",
]

_G = 9.80665  # [m/s2]
_FT_TO_M = 0.3048
_IN_TO_M = 0.0254
_LB_TO_KN = 4.4482216152605e-3  # weight values quoted in lb (force)
_KG_TO_KN = _G / 1000.0  # mass values quoted in kg
_KIPS_TO_KN = 4.4482216152605
_PPG_TO_KG_M3 = 119.82642731689663
SEAWATER_DENSITY_KG_M3 = 1025.0
STEEL_DENSITY_KG_M3 = 7850.0
#: dry -> submerged factor for bare-steel components documented dry-only.
_STEEL_WET_FRACTION = 1.0 - SEAWATER_DENSITY_KG_M3 / STEEL_DENSITY_KG_M3

#: Source pages carrying a text-extractable joint schedule (+ the page that
#: tabulates the joint library — RSU-0040's library RSU-0039 is primary on
#: the sister Das Bump page). RSU-0038 is deliberately ABSENT: its joint
#: counts are raster-only in the source (wiki page caveat) and the loader
#: must fail closed rather than guess.
_SCHEDULE_PAGES: dict[str, tuple[str, str]] = {
    "RSU-0019": (
        "31072-rpt-0001-3-ahawbil-1-drilling-riser-analysis.md",
        "31072-rpt-0001-3-ahawbil-1-drilling-riser-analysis.md",
    ),
    "RSU-0021": (
        "3914-rpt-0001-2-centenario-nen-1-drilling-riser-analysis.md",
        "3914-rpt-0001-2-centenario-nen-1-drilling-riser-analysis.md",
    ),
    "RSU-0040": (
        "3248-rpt-0001-3-mad-dog-deep-drilling-riser-report.md",
        "3191-rpt-0001-1-das-bump-drilling-riser-report.md",
    ),
}
SCHEDULE_RSU_IDS = frozenset(_SCHEDULE_PAGES)

_ENDPOINT_CALC = "16q-min-tension-endpoints"
#: (contract key, mud unit, tension unit, mud column, tension column) per
#: endpoint-row dialect — the wave-3 pages tabulate mud in three units.
_ENDPOINT_DIALECTS: dict[str, dict[str, tuple[str, str, str, int, int]]] = {
    "RSU-0019": {"primary": ("api_min_top_tension", "kg_m3", "kips", 0, 2)},
    "RSU-0021": {"primary": ("api_min_top_tension", "g_cm3", "kips", 0, 1)},
    "RSU-0038": {"primary": ("api_min_top_tension", "ppg", "Mlbs", 0, 1)},
    "RSU-0040": {
        "primary": ("api_16q_stability_tension_15_bare_joints", "ppg", "kips", 0, 1),
        "5-bare": ("api_16q_stability_tension_5_bare_joints", "ppg", "kips", 0, 1),
    },
}
_MUD_TO_KG_M3 = {"kg_m3": 1.0, "g_cm3": 1000.0, "ppg": _PPG_TO_KG_M3}
_TENSION_TO_KN = {"kips": _KIPS_TO_KN, "Mlbs": 1000.0 * _KIPS_TO_KN}


class ScheduleAssemblyError(RuntimeError):
    """Fail-closed schedule/library extraction error (missing page, unparsed
    field, schedule-integrity mismatch)."""


def _num(text: str) -> float:
    """Parse a wiki numeric token (thousands commas, unicode minus, +)."""
    return float(text.replace(",", "").replace("−", "-").lstrip("+"))


def _search(pattern: str, text: str, *, page: str, field: str) -> re.Match:
    match = re.search(pattern, text)
    if match is None:
        raise ScheduleAssemblyError(f"{page}: cannot extract {field}")
    return match


def _page_text(wiki_root: Path, page_name: str) -> str:
    page = Path(wiki_root) / SOURCES_REL / page_name
    if not page.is_file():
        raise ScheduleAssemblyError(f"missing wiki source page: {page}")
    if page.stat().st_size > MAX_PAGE_BYTES:
        raise ScheduleAssemblyError(
            f"{page_name}: size exceeds bounded-read cap {MAX_PAGE_BYTES}"
        )
    return page.read_text(encoding="utf-8")


def _circle_area_m2(diameter_in: float) -> float:
    return math.pi / 4.0 * (diameter_in * _IN_TO_M) ** 2


@dataclass(frozen=True)
class StringGeometry:
    """Fluid-column geometry for the 16Q internal-fluid term. Elevations are
    metres above mudline (the schedule tables' datum)."""

    water_depth_m: float
    drill_floor_above_mudline_m: float
    string_base_above_mudline_m: float
    internal_area_m2: float

    @property
    def mud_column_m(self) -> float:
        return self.drill_floor_above_mudline_m - self.string_base_above_mudline_m

    @property
    def seawater_column_m(self) -> float:
        return self.water_depth_m - self.string_base_above_mudline_m


@dataclass(frozen=True)
class ScheduleAssembly:
    """A wiki-schedule-assembled tensioned string + its 16Q chain inputs."""

    rsu_id: str
    model: RiserStackupModel
    geometry: StringGeometry
    assumptions: tuple[str, ...]
    #: Documented LMRP submerged weight (metadata for overpull/landing work
    #: and the RSU-0021 finding hypothesis; NOT part of the tensioned string).
    lmrp_submerged_kn: Optional[float] = None
    top_tension_factor: float = SAFETY_FACTOR_TENSION

    @property
    def buoyancy_net_lift_kn(self) -> float:
        """Net lift of the buoyed joints (positive up). The sources document
        net wet weights only — this is NOT the gross foam uplift Bn."""
        return -sum(
            float(item.component["submerged_weight_kn"]) * item.count
            for item in self.model.items
            if float(item.component["submerged_weight_kn"]) < 0.0
        )

    def minimum_top_tension_16q_kn(self, mud_density_kg_m3: float) -> float:
        """API RP 16Q minimum top tension [kN] at ``mud_density_kg_m3``:
        1.25 x (net Ws + Ai(dm.Hm - dw.Hw)) — see the module header for the
        factor provenance and the net-weight (f_wt = f_bt = 1) basis."""
        geometry = self.geometry
        t_srmin_kn = minimum_slip_ring_tension(
            submerged_weight_kn=self.model.total_submerged_weight_kn(),
            buoyancy_uplift_kn=0.0,
            internal_area_m2=geometry.internal_area_m2,
            mud_density_kn_m3=mud_density_kg_m3 * _KG_TO_KN,
            mud_column_m=geometry.mud_column_m,
            seawater_density_kn_m3=SEAWATER_DENSITY_KG_M3 * _KG_TO_KN,
            seawater_column_m=geometry.seawater_column_m,
            f_wt=1.0,
            f_bt=1.0,
        )
        return top_tension_required(t_srmin_kn, self.top_tension_factor)


# -- shared parse helpers ---------------------------------------------------------------


def _record(
    rsu_id: str,
    slug: str,
    component_type: str,
    *,
    dry_kn: float,
    wet_kn: float,
    count: int,
    length_m: Optional[float] = None,
) -> dict[str, Any]:
    entry: dict[str, Any] = {
        "component_id": f"{rsu_id}/{slug}",
        "component_type": component_type,
        "weight_air_kn": dry_kn,
        "submerged_weight_kn": wet_kn,
        "count": count,
    }
    if length_m is not None:
        entry["length_m"] = length_m
    return entry


def _internal_area_m2(
    text: str, *, page: str, od_wt_pattern: str, ck_pattern: str, boost_pattern: str
) -> float:
    od_wt = _search(od_wt_pattern, text, page=page, field="main tube OD x WT")
    bore_in = _num(od_wt.group(1)) - 2.0 * _num(od_wt.group(2))
    ck_id_in = _num(
        _search(ck_pattern, text, page=page, field="choke/kill line ID").group(1)
    )
    boost_id_in = _num(
        _search(boost_pattern, text, page=page, field="mud-boost line ID").group(1)
    )
    return (
        _circle_area_m2(bore_in)
        + 2.0 * _circle_area_m2(ck_id_in)
        + _circle_area_m2(boost_id_in)
    )


_ASSUMPTIONS_COMMON = (
    "tensioned string = outer barrel .. lower flexjoint (LMRP/BOP excluded)",
    "net submerged weights, f_wt = f_bt = 1.0 (no gross/uplift split documented)",
    "flexjoint/outer-barrel submerged weight = dry x (1 - rho_sw/rho_steel)",
    "mud in main bore + 2x choke/kill + mud boost, columns to drill floor",
    "seawater at 1025 kg/m3",
    "Tmin = 1.25 x TSRmin (2H-TNE-0050-03 s3.1; = 16Q N/(Rf(N-n)), N=6, n=1, Rf=0.96)",
)


# -- Grupo-R sister pages (RSU-0019 / RSU-0021): table + prose library ------------------

_GRUPO_SCHEDULE_ROW = re.compile(
    r"^\|\s*([^|]+?)\s*\|\s*([\d,.]+)\s*\|\s*(\d+)\s*\|\s*([−\-\d,.]+)\s*\|\s*$",
    re.MULTILINE,
)
_GRUPO_BUOYANT_CLASS = re.compile(
    r"(\d{4})'(?:\s+rating)?\s+([\d,]+\.?\d*)\s*/\s*([−+]?[\d,]+\.?\d*)\s*kg"
)
_BUOYANT_ROW_NAME = re.compile(r"Buoyancy joint (\d{4})' rating")


def _grupo_library(text: str, *, page: str) -> dict[str, tuple[float, float]]:
    """{class: (dry_kn, wet_kn)} for the slick + buoyancy-joint families."""
    slick = _search(
        r"slick 75 ft [\d.]+ in x [\d.]+ in,\s+([\d,]+\.?\d*) kg air / "
        r"([\d,]+\.?\d*) kg\s+water",
        text,
        page=page,
        field="slick-joint library weights",
    )
    library = {
        "slick": (_num(slick.group(1)) * _KG_TO_KN, _num(slick.group(2)) * _KG_TO_KN)
    }
    for match in _GRUPO_BUOYANT_CLASS.finditer(text):
        rating, dry_kg, net_kg = match.groups()
        # The library quotes buoyed-joint NET wet values as "net lift" where
        # negative; keep sign convention: submerged weight, positive down.
        library[rating] = (_num(dry_kg) * _KG_TO_KN, _num(net_kg) * _KG_TO_KN)
    if len(library) < 3:
        raise ScheduleAssemblyError(f"{page}: buoyancy-joint library not extracted")
    return library


def _load_grupo_assembly(rsu_id: str, wiki_root: Path) -> ScheduleAssembly:
    page = _SCHEDULE_PAGES[rsu_id][0]
    text = _page_text(wiki_root, page)
    library = _grupo_library(text, page=page)
    slick_dry_kn, slick_wet_kn = library["slick"]
    lfj_dry_kn = (
        _num(
            _search(
                r"lower [\d,]+ lb-ft/deg \([\d,]+ N-m/deg\), ±10 deg, ([\d,]+) kg",
                text,
                page=page,
                field="lower flexjoint dry mass",
            ).group(1)
        )
        * _KG_TO_KN
    )
    ob_lb = re.search(r"outer barrel [\d.]+ x [\d.]+ in x \d+ ft, ([\d,]+) lb", text)
    ob_kg = re.search(r"outer barrel [\d.]+ x [\d.]+ in, ([\d,]+) kg", text)
    if ob_lb is not None:
        ob_dry_kn = _num(ob_lb.group(1)) * _LB_TO_KN
    elif ob_kg is not None:
        ob_dry_kn = _num(ob_kg.group(1)) * _KG_TO_KN
    else:
        raise ScheduleAssemblyError(f"{page}: cannot extract outer-barrel dry weight")

    # -- schedule table (between the RSU heading and the next section) ------------
    heading = re.search(rf"^## Riser stack-up — {rsu_id}([^\n]*)$", text, re.MULTILINE)
    if heading is None:
        raise ScheduleAssemblyError(f"{page}: no schedule heading for {rsu_id}")
    section = text[heading.end() :].split("\n## ", 1)[0]
    unit_to_m = _FT_TO_M if "ft units" in heading.group(1) else 1.0
    rows = [
        (name, _num(length) * unit_to_m, int(count), _num(elev) * unit_to_m)
        for name, length, count, elev in _GRUPO_SCHEDULE_ROW.findall(section)
    ]
    names = [row[0] for row in rows]
    if "Outer barrel" not in names or "Lower flexjoint" not in names:
        raise ScheduleAssemblyError(f"{page}: schedule table rows not extracted")
    lo = min(names.index("Outer barrel"), names.index("Lower flexjoint"))
    hi = max(names.index("Outer barrel"), names.index("Lower flexjoint"))
    string_rows = rows[lo : hi + 1]

    records: list[dict[str, Any]] = []
    slick_seq = 0
    for name, length_m, count, _elev in string_rows:
        buoyant = _BUOYANT_ROW_NAME.fullmatch(name)
        if name == "Slick joint":
            slick_seq += 1
            records.append(
                _record(
                    rsu_id,
                    f"slick-joint-run{slick_seq}",
                    "riser_joint",
                    dry_kn=slick_dry_kn,
                    wet_kn=slick_wet_kn,
                    count=count,
                    length_m=length_m,
                )
            )
        elif buoyant is not None:
            rating = buoyant.group(1)
            if rating not in library:
                raise ScheduleAssemblyError(
                    f"{page}: schedule uses buoyancy class {rating}' with no "
                    "library weights (buoyancy class mapping gap)"
                )
            dry_kn, wet_kn = library[rating]
            records.append(
                _record(
                    rsu_id,
                    f"buoyancy-joint-{rating}ft",
                    "riser_joint_buoyant",
                    dry_kn=dry_kn,
                    wet_kn=wet_kn,
                    count=count,
                    length_m=length_m,
                )
            )
        elif name == "Lower flexjoint":
            records.append(
                _record(
                    rsu_id,
                    "lower-flexjoint",
                    "flexjoint",
                    dry_kn=lfj_dry_kn,
                    wet_kn=lfj_dry_kn * _STEEL_WET_FRACTION,
                    count=count,
                    length_m=length_m,
                )
            )
        elif name == "Outer barrel":
            records.append(
                _record(
                    rsu_id,
                    "telescopic-outer-barrel",
                    "telescopic_joint",
                    dry_kn=ob_dry_kn,
                    wet_kn=ob_dry_kn * _STEEL_WET_FRACTION,
                    count=count,
                    length_m=length_m,
                )
            )
        elif name.endswith("pup") or name == "Termination joint":
            # [ASSUMED] slick per-foot rate — no pup/termination weights in
            # the library prose.
            dry_kn = slick_dry_kn * length_m / (75.0 * _FT_TO_M)
            records.append(
                _record(
                    rsu_id,
                    name.lower().replace(" ", "-"),
                    "pup_joint",
                    dry_kn=dry_kn,
                    wet_kn=dry_kn * (slick_wet_kn / slick_dry_kn),
                    count=count,
                    length_m=length_m,
                )
            )
        else:
            raise ScheduleAssemblyError(
                f"{page}: unmapped tensioned-string schedule row {name!r}"
            )

    # -- schedule integrity: joint tally + elevation-span cross-checks -------------
    tally = _search(
        r"(\d+) x 75-ft riser joints \((\d+) slick \+ (\d+) buoyant\)",
        text,
        page=page,
        field="riser joint tally",
    )
    slick_count = sum(
        r["count"] for r in records if r["component_type"] == "riser_joint"
    )
    buoyant_count = sum(
        r["count"] for r in records if r["component_type"] == "riser_joint_buoyant"
    )
    if (slick_count, buoyant_count) != (int(tally.group(2)), int(tally.group(3))):
        raise ScheduleAssemblyError(
            f"{page}: schedule joint tally mismatch — parsed "
            f"{slick_count} slick + {buoyant_count} buoyant vs documented "
            f"{tally.group(2)} + {tally.group(3)}"
        )
    elevations = [row[3] for row in string_rows] + [
        string_rows[0][3] + string_rows[0][1],
        string_rows[-1][3] + string_rows[-1][1],
    ]
    span_m = max(elevations) - min(elevations)
    summed_m = sum(row[1] * row[2] for row in string_rows)
    if abs(span_m - summed_m) / summed_m > 0.005:
        raise ScheduleAssemblyError(
            f"{page}: schedule span {span_m:.1f} m != summed lengths "
            f"{summed_m:.1f} m"
        )
    datum_m = next(row[3] for row in string_rows if row[0] == "Lower flexjoint")

    geometry = StringGeometry(
        water_depth_m=_num(
            _search(
                r"\*\*([\d,]+(?:\.\d+)?) m water depth\*\*",
                text,
                page=page,
                field="water depth",
            ).group(1)
        ),
        drill_floor_above_mudline_m=_num(
            _search(
                r"\(([\d,]+\.?\d*) m above\s+mudline",
                text,
                page=page,
                field="drill-floor elevation",
            ).group(1)
        ),
        string_base_above_mudline_m=datum_m,
        internal_area_m2=_internal_area_m2(
            text,
            page=page,
            od_wt_pattern=r"slick 75 ft ([\d.]+) in x ([\d.]+) in",
            ck_pattern=r"C&K [\d.]+ x ([\d.]+) in",
            boost_pattern=r"mud boost [\d.]+ x ([\d.]+) in",
        ),
    )
    lmrp = _search(
        r"LMRP [\d.]+ ft, ([\d,]+) lb air / ([\d,]+) lb\s+water",
        text,
        page=page,
        field="LMRP masses",
    )
    return ScheduleAssembly(
        rsu_id=rsu_id,
        model=RiserStackupModel.from_records(records, rsu_id=rsu_id),
        geometry=geometry,
        assumptions=_ASSUMPTIONS_COMMON
        + ("pup/termination joints at the slick per-foot weight rate",),
        lmrp_submerged_kn=_num(lmrp.group(2)) * _LB_TO_KN,
    )


# -- Ocean Confidence (RSU-0040 schedule + RSU-0039 library on the sister page) ---------

_OC_LIBRARY_LABELS = {
    "Slick joint": "slick",
    "25 ft pup joint": "pup-25ft",
    "35 ft termination joint": "termination-35ft",
    "2000 ft buoyancy joint": "2000",
    "3000 ft buoyancy joint": "3000",
    "5000 ft buoyancy joint": "5000",
    "6600 ft buoyancy joint": "6600",
}


def _oc_library(text: str, *, page: str) -> dict[str, tuple[float, float]]:
    """{class: (dry_kn, wet_kn)} from the RSU-0039 markdown weight table."""
    library: dict[str, tuple[float, float]] = {}
    for line in text.splitlines():
        cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
        if len(cells) != 5:
            continue
        key = _OC_LIBRARY_LABELS.get(cells[0])
        if key is None and cells[0].startswith("Telescopic joint"):
            key = "telescopic-joint"
        if key is None:
            continue
        library[key] = (_num(cells[2]) * _LB_TO_KN, _num(cells[3]) * _LB_TO_KN)
    missing = (set(_OC_LIBRARY_LABELS.values()) | {"telescopic-joint"}) - set(library)
    if missing:
        raise ScheduleAssemblyError(
            f"{page}: joint-library rows not extracted: {sorted(missing)}"
        )
    return library


def _load_oc_assembly(rsu_id: str, wiki_root: Path) -> ScheduleAssembly:
    schedule_page, library_page = _SCHEDULE_PAGES[rsu_id]
    text = _page_text(wiki_root, schedule_page)
    lib_text = _page_text(wiki_root, library_page)
    library = _oc_library(lib_text, page=library_page)
    lfj_dry_kn = (
        _num(
            _search(
                r"lower [\d,]+ ft-lb/deg, ([\d,]+) lb dry",
                lib_text,
                page=library_page,
                field="lower flexjoint dry weight",
            ).group(1)
        )
        * _LB_TO_KN
    )

    buoyant_row = _search(
        r"\| Buoyant joints, 75 ft \| \*\*(\d+) total: ([^*]+)\*\* \| "
        r"([\d,.]+) → ([\d,.]+) \|",
        text,
        page=schedule_page,
        field="buoyant-joint schedule row",
    )
    class_counts = re.findall(r"(\d+)x (\d{4}) ft", buoyant_row.group(2))
    if sum(int(count) for count, _ in class_counts) != int(buoyant_row.group(1)):
        raise ScheduleAssemblyError(
            f"{schedule_page}: buoyant class counts do not sum to the "
            f"documented total {buoyant_row.group(1)}"
        )
    slick_row = _search(
        r"\| Slick joints, 75 ft \| (\d+) \| ([\d,.]+) → ([\d,.]+) \|",
        text,
        page=schedule_page,
        field="slick-joint schedule row",
    )
    # Elevation-span integrity: both 75-ft joint runs vs their m-ASB spans.
    for count, top_m, base_m, label in (
        (
            int(buoyant_row.group(1)),
            buoyant_row.group(3),
            buoyant_row.group(4),
            "buoyant",
        ),
        (int(slick_row.group(1)), slick_row.group(2), slick_row.group(3), "slick"),
    ):
        span_m = _num(top_m) - _num(base_m)
        summed_m = count * 75.0 * _FT_TO_M
        if abs(span_m - summed_m) / summed_m > 0.005:
            raise ScheduleAssemblyError(
                f"{schedule_page}: {label} run span {span_m:.1f} m != "
                f"{count} x 75 ft"
            )
    lfj_row = _search(
        r"\| Lower flex joint \| 1 x ([\d.]+) ft \| ([\d,.]+) \|",
        text,
        page=schedule_page,
        field="lower flexjoint schedule row",
    )
    _search(
        r"\| Pup joints 50 ft \+ 25 ft \| 2 \|",
        text,
        page=schedule_page,
        field="pup-joint schedule row",
    )
    _search(
        r"\| Lower termination joint, 35 ft \| 1 \|",
        text,
        page=schedule_page,
        field="termination-joint schedule row",
    )

    pup25_dry_kn, pup25_wet_kn = library["pup-25ft"]
    records = [
        # [ASSUMED] the tensioners carry the full telescopic-joint weight
        # (the library tabulates it as one item; no IB/OB split documented).
        _record(
            rsu_id,
            "telescopic-joint",
            "telescopic_joint",
            dry_kn=library["telescopic-joint"][0],
            wet_kn=library["telescopic-joint"][1],
            count=1,
        ),
        # [ASSUMED] 50 ft pup at twice the 25 ft pup weight (same family).
        _record(
            rsu_id,
            "pup-50ft",
            "pup_joint",
            dry_kn=2.0 * pup25_dry_kn,
            wet_kn=2.0 * pup25_wet_kn,
            count=1,
            length_m=50.0 * _FT_TO_M,
        ),
        _record(
            rsu_id,
            "pup-25ft",
            "pup_joint",
            dry_kn=pup25_dry_kn,
            wet_kn=pup25_wet_kn,
            count=1,
            length_m=25.0 * _FT_TO_M,
        ),
        _record(
            rsu_id,
            "slick-joint",
            "riser_joint",
            dry_kn=library["slick"][0],
            wet_kn=library["slick"][1],
            count=int(slick_row.group(1)),
            length_m=75.0 * _FT_TO_M,
        ),
        _record(
            rsu_id,
            "termination-joint-35ft",
            "termination_joint",
            dry_kn=library["termination-35ft"][0],
            wet_kn=library["termination-35ft"][1],
            count=1,
            length_m=35.0 * _FT_TO_M,
        ),
        _record(
            rsu_id,
            "lower-flexjoint",
            "flexjoint",
            dry_kn=lfj_dry_kn,
            wet_kn=lfj_dry_kn * _STEEL_WET_FRACTION,
            count=1,
            length_m=_num(lfj_row.group(1)) * _FT_TO_M,
        ),
    ]
    for count, rating in class_counts:
        if rating not in library:
            raise ScheduleAssemblyError(
                f"{schedule_page}: schedule uses buoyancy class {rating} ft "
                "with no library weights (buoyancy class mapping gap)"
            )
        records.append(
            _record(
                rsu_id,
                f"buoyancy-joint-{rating}ft",
                "riser_joint_buoyant",
                dry_kn=library[rating][0],
                wet_kn=library[rating][1],
                count=int(count),
                length_m=75.0 * _FT_TO_M,
            )
        )

    datum_m = _num(lfj_row.group(2)) - _num(lfj_row.group(1)) * _FT_TO_M
    geometry = StringGeometry(
        water_depth_m=_num(
            _search(
                r"\(([\d,]+\.\d+) m\)\*\* water depth",
                text,
                page=schedule_page,
                field="water depth",
            ).group(1)
        ),
        drill_floor_above_mudline_m=_num(
            _search(
                r"drill floor =\s+([\d,]+\.?\d*) m",
                text,
                page=schedule_page,
                field="drill-floor elevation",
            ).group(1)
        ),
        string_base_above_mudline_m=datum_m,
        internal_area_m2=_internal_area_m2(
            lib_text,
            page=library_page,
            od_wt_pattern=r"([\d.]+) in OD x ([\d.]+) in WT",
            ck_pattern=r"choke/kill\s+[\d.]+ x ([\d.]+) in",
            boost_pattern=r"mud booster [\d.]+ x ([\d.]+) in",
        ),
    )
    lmrp = _search(
        r"LMRP ([\d,]+) / ([\d,]+) lb",
        lib_text,
        page=library_page,
        field="LMRP masses",
    )
    return ScheduleAssembly(
        rsu_id=rsu_id,
        model=RiserStackupModel.from_records(records, rsu_id=rsu_id),
        geometry=geometry,
        assumptions=_ASSUMPTIONS_COMMON
        + (
            "50 ft pup at twice the 25 ft pup library weight",
            "full telescopic-joint weight tensioner-carried (no IB/OB split)",
        ),
        lmrp_submerged_kn=_num(lmrp.group(2)) * _LB_TO_KN,
    )


# -- public API -------------------------------------------------------------------------


def load_schedule_assembly(rsu_id: str, wiki_root: Path) -> ScheduleAssembly:
    """Assemble the tensioned riser string for ``rsu_id`` from its wiki-side
    joint schedule + joint library. Fail-closed: RSUs without a
    text-extractable schedule (e.g. RSU-0038, raster-only counts) raise
    :class:`ScheduleAssemblyError`."""
    if rsu_id not in _SCHEDULE_PAGES:
        raise ScheduleAssemblyError(
            f"{rsu_id}: no text-extractable wiki joint schedule "
            "(RSU-0038 counts are raster-only; other RSUs are not "
            "schedule-assembly candidates yet)"
        )
    if rsu_id == "RSU-0040":
        return _load_oc_assembly(rsu_id, Path(wiki_root))
    return _load_grupo_assembly(rsu_id, Path(wiki_root))


def documented_endpoints(
    rsu_id: str, wiki_root: Path, *, variant: str = "primary"
) -> tuple[tuple[float, float], ...]:
    """Documented ``16q-min-tension-endpoints`` rows for ``rsu_id`` as
    ``(mud_density_kg_m3, tension_kn)`` tuples — mud/tension units normalized
    across the per-page dialects. ``variant="5-bare"`` selects RSU-0040's
    second stability sweep."""
    dialects = _ENDPOINT_DIALECTS.get(rsu_id)
    if dialects is None or variant not in dialects:
        raise ScheduleAssemblyError(
            f"{rsu_id}: no known endpoint dialect (variant {variant!r})"
        )
    key, mud_unit, tension_unit, mud_col, tension_col = dialects[variant]
    contract = find_contract(rsu_id, _ENDPOINT_CALC, Path(wiki_root))
    if contract is None:
        raise ScheduleAssemblyError(
            f"{rsu_id}: wiki carries no {_ENDPOINT_CALC} contract"
        )
    rows = (
        contract.get(key, {}).get("rows")
        if isinstance(contract.get(key), dict)
        else None
    )
    if not rows:
        raise ScheduleAssemblyError(f"{rsu_id}: endpoint contract lacks {key}.rows")
    return tuple(
        (
            float(row[mud_col]) * _MUD_TO_KG_M3[mud_unit],
            float(row[tension_col]) * _TENSION_TO_KN[tension_unit],
        )
        for row in rows
    )
