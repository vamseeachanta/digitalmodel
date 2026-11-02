"""Permanent P&A envelope screening with explicit acceptance inputs.

Public basis: D-010 revision-4 chapter-9 seminar (2013), slides 8-9 and 12-14:
barriers per inflow zone, plug MD coverage and formation strength at each base.
OEUK Well Decommissioning Guidelines public scope describes isolation within
the well and from the environment; its paid numeric criteria are not embedded.
This subset does not establish compliance with either current publication.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Sequence

from digitalmodel.citations import Citation

from .basis import boolean, citation, number, positive_integer, text


@dataclass(frozen=True)
class BarrierCriteria:
    """User-selected count, plug length and above-zone coverage, all MD metres.

No universal minimum is assumed: hole type, foundation and jurisdiction change
these criteria. The basis must identify the applicable edition/clause or the
explicit public-source screening assumption. See D-010 seminar slides 8, 12.
"""

    required_barriers: int
    min_plug_length_md_m: float
    min_above_zone_md_m: float
    basis: str


def public_norsok_2013_criteria(
    inflow_class: str, plug_setting: str, *, min_above_zone_md_m: float,
) -> BarrierCriteria:
    """Select a limited historical public basis, not current-standard criteria.

D-010 rev.4 public seminar (2013), slide 8: two envelopes for mobile
hydrocarbons or overpressure with surface-flow potential; one for crossflow-only
or normal water without surface-flow potential. Slide 12 (EAC table 24):
100 m MD open-hole/unfounded cased plugs, 50 m MD cased plugs on verified
foundations. Open-hole coverage is at least 50 m above inflow. Cased coverage
is a required caller input; no number is inferred from a paid publication.
Shoe transitions, immobile hydrocarbon exceptions and combined plugs are
outside this selector. The caller qualifies the inflow class and foundation.
"""
    citation("norsok")
    counts = {"mobile_hydrocarbon": 2, "overpressured_flow_to_surface": 2,
              "normal_water_no_surface_flow": 1, "crossflow_only": 1}
    lengths = {"open_hole": 100.0, "cased_without_foundation": 100.0,
               "cased_on_verified_foundation": 50.0}
    if inflow_class not in counts or plug_setting not in lengths:
        raise ValueError("unsupported inflow_class or plug_setting; supply qualified criteria")
    number("min_above_zone_md_m", min_above_zone_md_m)
    if plug_setting == "open_hole" and min_above_zone_md_m < 50:
        raise ValueError("public 2013 open-hole coverage requires at least 50 m MD")
    return BarrierCriteria(counts[inflow_class], lengths[plug_setting], min_above_zone_md_m,
        f"D-010 rev.4 public seminar 2013, slides 8,12; {inflow_class}; {plug_setting}; "
        f"above-zone coverage selected as {min_above_zone_md_m} m MD; historical screen only")


@dataclass(frozen=True)
class FlowZone:
    zone_id: str
    top_md_m: float
    criteria: BarrierCriteria


@dataclass(frozen=True)
class PermanentBarrier:
    """One complete envelope, rather than one element in an envelope.

Both pressures are MPa at this barrier's base, including the governing future
recharge case. No surface-to-base pressure conversion is performed. The
rock-to-rock assertion covers the entire cross-section, including all annuli;
it requires external verification. Independence groups identify shared jobs
or other dependencies. This simplified screen rejects overlapping envelopes.
"""

    barrier_id: str
    zone_id: str
    top_md_m: float
    base_md_m: float
    formation_integrity_at_base_mpa: float
    maximum_pressure_below_at_base_mpa: float
    rock_to_rock_verified: bool
    placement_verified: bool
    independence_group: str
    evidence: str


@dataclass(frozen=True)
class ZoneScreen:
    zone_id: str
    required_barriers: int
    qualified_barriers: tuple[str, ...]
    pressure_margins_mpa: tuple[float, ...]
    rejections: tuple[tuple[str, tuple[str, ...]], ...]
    flags: tuple[str, ...]
    criteria: BarrierCriteria


@dataclass(frozen=True)
class PermanentScreen:
    meets_screen: bool
    zones: tuple[ZoneScreen, ...]
    flags: tuple[str, ...]
    criteria_basis: tuple[str, ...]
    citations: tuple[Citation, ...]


def screen_permanent_barriers(
    zones: Sequence[FlowZone], barriers: Sequence[PermanentBarrier], *,
    gas_migration: bool | None, sustained_casing_pressure: bool | None,
) -> PermanentScreen:
    """Compare complete envelopes with each zone's supplied criteria.

Length = base MD - top MD; above-zone coverage = zone top MD - plug top MD.
Pressure margin = formation integrity - maximum future pressure, at base.
Length/coverage use >=; base formation strength uses strictly > (D-010 public
seminar slides 9, 12). Geometry alone cannot verify bonding or placement.
True anomaly or unknown observation holds the screen for investigation; this
hold is a screening policy, not a gas-migration diagnostic or standard limit.
"""
    source = citation("norsok")
    if not zones:
        raise ValueError("at least one flow zone is required")
    zone_ids: set[str] = set()
    for zone in zones:
        text("zone_id", zone.zone_id)
        if zone.zone_id in zone_ids:
            raise ValueError("duplicate zone_id")
        zone_ids.add(zone.zone_id)
        number("top_md_m", zone.top_md_m)
        c = zone.criteria
        positive_integer("required_barriers", c.required_barriers)
        number("min_plug_length_md_m", c.min_plug_length_md_m, positive=True)
        number("min_above_zone_md_m", c.min_above_zone_md_m)
        text("criteria basis", c.basis)
    barrier_ids: set[str] = set()
    for b in barriers:
        for name in ("barrier_id", "zone_id", "independence_group", "evidence"):
            text(name, getattr(b, name))
        if b.barrier_id in barrier_ids or b.zone_id not in zone_ids:
            raise ValueError("duplicate barrier_id or unknown zone_id")
        barrier_ids.add(b.barrier_id)
        for name in ("top_md_m", "base_md_m", "formation_integrity_at_base_mpa",
                     "maximum_pressure_below_at_base_mpa"):
            number(name, getattr(b, name))
        if b.base_md_m <= b.top_md_m:
            raise ValueError("base_md_m must exceed top_md_m")
        boolean("rock_to_rock_verified", b.rock_to_rock_verified)
        boolean("placement_verified", b.placement_verified)

    global_flags = []
    for value, true_flag, unknown_flag in (
        (gas_migration, "gas_migration", "gas_migration_unknown"),
        (sustained_casing_pressure, "sustained_casing_pressure", "casing_pressure_unknown"),
    ):
        if value is None:
            global_flags.append(unknown_flag)
        else:
            boolean(true_flag, value)
            if value:
                global_flags.append(true_flag)

    results = []
    for zone in zones:
        qualified: list[PermanentBarrier] = []
        rejected = []
        c = zone.criteria
        for b in barriers:
            if b.zone_id != zone.zone_id:
                continue
            defects = []
            if b.base_md_m - b.top_md_m < c.min_plug_length_md_m:
                defects.append("plug_length")
            if zone.top_md_m - b.top_md_m < c.min_above_zone_md_m:
                defects.append("above_zone")
            if b.formation_integrity_at_base_mpa <= b.maximum_pressure_below_at_base_mpa:
                defects.append("formation_strength")
            if not b.rock_to_rock_verified:
                defects.append("rock_to_rock")
            if not b.placement_verified:
                defects.append("placement")
            if defects:
                rejected.append((b.barrier_id, tuple(defects)))
            else:
                qualified.append(b)
        flags = []
        if len(qualified) < c.required_barriers:
            flags.append("barrier_count")
        for i, a in enumerate(qualified):
            for b in qualified[i + 1:]:
                overlap = max(a.top_md_m, b.top_md_m) < min(a.base_md_m, b.base_md_m)
                if a.independence_group == b.independence_group or overlap:
                    flags.append("independence_not_established")
        results.append(ZoneScreen(
            zone.zone_id, c.required_barriers,
            tuple(b.barrier_id for b in qualified),
            tuple(b.formation_integrity_at_base_mpa - b.maximum_pressure_below_at_base_mpa
                  for b in qualified), tuple(rejected), tuple(sorted(set(flags))), c,
        ))
    return PermanentScreen(
        not global_flags and all(not z.flags for z in results), tuple(results),
        tuple(global_flags), tuple(z.criteria.basis for z in zones), (source,),
    )
