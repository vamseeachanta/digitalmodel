# ABOUTME: Damage-stability screening core — flooded-compartment (added-weight)
# ABOUTME: equilibrium, damaged-GZ analysis, cited survival criteria (CFR/MODU).
"""Damage-stability screening — deterministic core (slice 2 of the
``vessel_stability_screening`` lane).

Pure functions and dataclasses for:

1. **Damage-case definition** — either a flooded compartment group
   (volume x permeability with floodwater centroid and free-surface
   moment) evaluated by the **added-weight method**, or a directly
   supplied **damaged-condition** input (displacement, damaged GM/KG,
   static heel, optionally a damaged GZ/KN table). The direct path is the
   practical screening route: a licensed tool (e.g. GHS) supplies the
   damaged hydrostatics per job; this module screens them against cited
   criteria. **No geometry flooding computation is attempted here.**
2. **Post-damage equilibrium (added-weight)** — floodwater enters the
   loading condition as weight items (reusing the slice-1 loading-
   condition builder), so displacement, KG, LCG, trim and the fluid GM
   come from the *intact* hydrostatic table at the increased
   displacement. Static heel from the transverse flooding moment:
   ``tan(heel) = |sum(w_i * tcg_i)| / (W' * GM'_fluid)`` (small-angle
   screen; None when the post-damage fluid GM is not positive).

   *Free-communication note*: when a compartment is open to the sea the
   floodwater free surface must be included — supply the floodwater FSM
   (``fsm_t_m`` or tank dimensions at the workflow tier) and set
   ``free_communication=True``. The added-weight-plus-FSM treatment is a
   screening approximation of free communication; the rigorous
   lost-buoyancy solution (and the governing submittal) comes from the
   licensed tool.
3. **Damaged-GZ analysis** — from a damaged GZ table or damaged KN
   cross-curve (``GZ = KN - KG_fluid * sin(phi)``): static equilibrium
   against the flooding transverse moment (cosine arm) and/or a constant
   wind heeling arm, range of positive stability beyond equilibrium
   (to the vanishing angle, the downflooding/margin-line limit or the
   tabulated range), residual righting area, and the residual-to-heeling
   area ratio when a heeling arm is present.
4. **Cited survival criteria** — max static heel, min damaged GM,
   min margin to the downflooding/margin-line angle, min range beyond
   equilibrium, min residual area, min area ratio. Thresholds are
   config inputs carried with a mandatory :class:`Citation`
   (standard/edition/clause — same discipline as slice 1); typical
   sources are 46 CFR Part 174 (per the applicable vessel class) and the
   IMO MODU Code 2009 (Res. A.1023(26)) damage criteria.

SCREENING POSTURE: deterministic screen only. The PE-stamped damage-
stability analysis / class or USCG submittal governs; GHS (or equivalent)
remains the licensed cross-check and the source of damaged hydrostatics.

Units: SI marine practice — metres, tonnes, degrees (m*rad for GZ areas).
Longitudinal positions from the aft perpendicular, positive forward; trim
positive by the stern; heel reported as a positive magnitude toward the
flooded side.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

from digitalmodel.naval_architecture.vessel_stability_screening import (
    RHO_SEAWATER_T_M3,
    Citation,
    CriterionResult,
    GZCurve,
    HydrostaticTable,
    LoadingCondition,
    WeightItem,
    _evaluate,
    build_loading_condition,
    solve_equilibrium,
)

__all__ = [
    "FloodedCompartment",
    "DamagedEquilibrium",
    "DirectDamageCondition",
    "DamageCriteria",
    "DamageGZAnalysis",
    "DamageCaseResult",
    "added_weight_equilibrium",
    "damage_gz_analysis",
    "evaluate_damage_case",
]


# -- damage-case definition ----------------------------------------------------


@dataclass(frozen=True)
class FloodedCompartment:
    """One flooded compartment (or compartment-group member).

    ``volume_m3 * permeability * rho`` is the floodwater weight taken on
    by the added-weight method. ``tcg_m`` is the transverse offset of the
    floodwater centroid from centreline (magnitude drives the static
    heel). ``fsm_t_m`` is the floodwater free-surface moment (t*m) —
    mandatory (non-zero) when ``free_communication`` is set, because a
    compartment open to the sea always has a free surface.
    """

    name: str
    volume_m3: float
    permeability: float
    vcg_m: float
    lcg_m: float
    tcg_m: float = 0.0
    fsm_t_m: float = 0.0
    free_communication: bool = False

    def __post_init__(self) -> None:
        if self.volume_m3 <= 0.0:
            raise ValueError(
                f"flooded compartment '{self.name}': volume_m3 must be > 0"
            )
        if not 0.0 < self.permeability <= 1.0:
            raise ValueError(
                f"flooded compartment '{self.name}': permeability must be in "
                "(0, 1]"
            )
        if self.fsm_t_m < 0.0:
            raise ValueError(
                f"flooded compartment '{self.name}': fsm_t_m must be >= 0"
            )
        if self.free_communication and self.fsm_t_m == 0.0:
            raise ValueError(
                f"flooded compartment '{self.name}': free-communication "
                "flooding must supply the floodwater free-surface moment "
                "(fsm_t_m > 0)"
            )

    def floodwater_t(self, water_density_t_m3: float = RHO_SEAWATER_T_M3) -> float:
        """Floodwater weight taken on: ``rho * permeability * volume``."""
        if water_density_t_m3 <= 0.0:
            raise ValueError("water_density_t_m3 must be > 0")
        return water_density_t_m3 * self.permeability * self.volume_m3


@dataclass(frozen=True)
class DirectDamageCondition:
    """Directly supplied damaged-condition hydrostatics (the practical
    screening path — a licensed tool supplies these per damage case).

    ``kg_fluid_m`` is only needed when a damaged KN cross-curve is to be
    converted to GZ; a pre-computed damaged GZ table needs none.
    """

    displacement_t: float
    gm_fluid_m: float
    heel_deg: float | None = None
    trim_m: float | None = None
    kg_fluid_m: float | None = None
    draft_m: float | None = None

    def __post_init__(self) -> None:
        if self.displacement_t <= 0.0:
            raise ValueError("damaged condition: displacement_t must be > 0")
        if self.heel_deg is not None and self.heel_deg < 0.0:
            raise ValueError(
                "damaged condition: heel_deg must be >= 0 (heel magnitude "
                "toward the flooded side)"
            )
        if self.kg_fluid_m is not None and self.kg_fluid_m <= 0.0:
            raise ValueError("damaged condition: kg_fluid_m must be > 0")


# -- post-damage equilibrium (added-weight) --------------------------------------


@dataclass(frozen=True)
class DamagedEquilibrium:
    """Post-damage equilibrium from the added-weight method.

    ``heel_deg`` is the small-angle static heel from the transverse
    flooding moment (None when the post-damage fluid GM is not positive —
    the case fails the screen). Draft/trim come from the intact
    hydrostatic table at the flooded displacement.
    """

    displacement_t: float
    draft_m: float
    km_m: float
    kg_m: float
    fsc_m: float
    kg_fluid_m: float
    gm_fluid_m: float
    trim_m: float | None
    draft_aft_m: float | None
    draft_fwd_m: float | None
    floodwater_t: float
    transverse_moment_t_m: float
    heel_deg: float | None


def added_weight_equilibrium(
    condition: LoadingCondition,
    table: HydrostaticTable,
    compartments: list[FloodedCompartment],
    water_density_t_m3: float = RHO_SEAWATER_T_M3,
    lbp_m: float | None = None,
) -> DamagedEquilibrium:
    """Post-damage equilibrium by the added-weight method.

    Floodwater items (weight, VCG, LCG, FSM) are appended to the intact
    loading condition and the slice-1 equilibrium solver is re-run on the
    *intact* hydrostatic table at the flooded displacement (raises when
    it leaves the tabulated range — no extrapolation). Static heel from
    ``tan(heel) = |M_t| / (W' * GM'_fluid)`` with
    ``M_t = sum(w_i * tcg_i)``; see the module docstring for the
    free-communication note.
    """
    if not compartments:
        raise ValueError("added-weight damage case needs >= 1 flooded compartment")
    flood_items = [
        WeightItem(
            name=f"floodwater:{comp.name}",
            weight_t=comp.floodwater_t(water_density_t_m3),
            vcg_m=comp.vcg_m,
            lcg_m=comp.lcg_m,
            fsm_t_m=comp.fsm_t_m,
        )
        for comp in compartments
    ]
    damaged = build_loading_condition(
        f"{condition.name} (damaged)", list(condition.items) + flood_items
    )
    eq = solve_equilibrium(damaged, table, lbp_m)

    floodwater = sum(item.weight_t for item in flood_items)
    moment = sum(
        comp.floodwater_t(water_density_t_m3) * comp.tcg_m for comp in compartments
    )
    heel: float | None = None
    if eq.gm_fluid_m > 0.0:
        heel = math.degrees(
            math.atan(abs(moment) / (damaged.displacement_t * eq.gm_fluid_m))
        )
    return DamagedEquilibrium(
        displacement_t=damaged.displacement_t,
        draft_m=eq.draft_m,
        km_m=eq.km_m,
        kg_m=damaged.kg_m,
        fsc_m=damaged.fsc_m,
        kg_fluid_m=damaged.kg_fluid_m,
        gm_fluid_m=eq.gm_fluid_m,
        trim_m=eq.trim_m,
        draft_aft_m=eq.draft_aft_m,
        draft_fwd_m=eq.draft_fwd_m,
        floodwater_t=floodwater,
        transverse_moment_t_m=abs(moment),
        heel_deg=heel,
    )


# -- damaged-GZ analysis -----------------------------------------------------------


@dataclass(frozen=True)
class DamageGZAnalysis:
    """Damaged-GZ curve analysis (equilibrium, range, residual areas).

    ``equilibrium_heel_deg`` is None when the heeling arm exceeds GZ over
    the whole tabulated range (no static equilibrium — hard fail).
    ``range_end_deg`` is the least of the vanishing angle, the
    downflooding/margin-line limit and the tabulated range.
    """

    equilibrium_heel_deg: float | None
    range_end_deg: float | None
    range_beyond_equilibrium_deg: float | None
    residual_area_m_rad: float | None
    heeling_area_m_rad: float | None
    area_ratio: float | None
    has_heeling_arm: bool


def damage_gz_analysis(
    curve: GZCurve,
    displacement_t: float,
    transverse_moment_t_m: float = 0.0,
    wind_heeling_arm_m: float | None = None,
    limit_angle_deg: float | None = None,
    static_heel_deg: float | None = None,
    steps: int = 2000,
) -> DamageGZAnalysis:
    """Analyse a damaged GZ curve against the flooding/wind heeling arm.

    The heeling arm is ``HA(phi) = |M_t| * cos(phi) / W + l_wind`` (cosine
    flooding-moment arm, constant wind arm). With a heeling arm present,
    the static equilibrium is the first up-crossing of ``GZ - HA``
    (bisected to 1e-4 deg); without one, ``static_heel_deg`` (e.g. the
    licensed-tool equilibrium for a direct damaged condition) anchors the
    range/area analysis (default upright).

    Range of stability beyond equilibrium runs to the first down-crossing
    of the net arm (vanishing angle), capped at ``limit_angle_deg``
    (downflooding / margin-line immersion) and the tabulated range.
    Residual area is the integral of ``max(GZ - HA, 0)`` over that range;
    the area ratio divides it by the heeling energy up to equilibrium.
    """
    if displacement_t <= 0.0:
        raise ValueError("damage GZ analysis: displacement_t must be > 0")
    if transverse_moment_t_m < 0.0:
        raise ValueError(
            "damage GZ analysis: transverse_moment_t_m must be >= 0 "
            "(heel magnitude convention)"
        )
    if wind_heeling_arm_m is not None and wind_heeling_arm_m < 0.0:
        raise ValueError("damage GZ analysis: wind_heeling_arm_m must be >= 0")
    if limit_angle_deg is not None and limit_angle_deg <= 0.0:
        raise ValueError("damage GZ analysis: limit_angle_deg must be > 0")

    wind = wind_heeling_arm_m or 0.0
    moment_arm_0 = transverse_moment_t_m / displacement_t
    has_arm = moment_arm_0 > 0.0 or wind > 0.0
    span = curve.heel_deg[-1]

    def arm(phi: float) -> float:
        return moment_arm_0 * math.cos(math.radians(phi)) + wind

    def net(phi: float) -> float:
        return curve.gz_at(phi) - arm(phi)

    if has_arm:
        eq = _first_up_crossing(net, span, steps)
    else:
        eq = 0.0 if static_heel_deg is None else float(static_heel_deg)
        if eq < 0.0 or eq > span:
            raise ValueError(
                f"damage GZ analysis: static heel {eq:g} deg outside the "
                f"tabulated GZ range [0, {span:g}]"
            )

    if eq is None:
        return DamageGZAnalysis(
            equilibrium_heel_deg=None,
            range_end_deg=None,
            range_beyond_equilibrium_deg=None,
            residual_area_m_rad=None,
            heeling_area_m_rad=None,
            area_ratio=None,
            has_heeling_arm=has_arm,
        )

    cap = span if limit_angle_deg is None else min(span, limit_angle_deg)
    if eq >= cap:
        range_end = eq
        range_deg = 0.0
        residual = 0.0
    else:
        vanish = _first_down_crossing(net, eq, cap, steps)
        range_end = cap if vanish is None else vanish
        range_deg = range_end - eq
        residual = _positive_net_area_m_rad(net, eq, range_end)

    heeling_area = ratio = None
    if has_arm:
        # Exact: integral of M_t*cos/W is (M_t/W)*sin; wind arm is constant.
        heeling_area = moment_arm_0 * math.sin(math.radians(eq)) + wind * math.radians(
            eq
        )
        ratio = residual / heeling_area if heeling_area > 0.0 else math.inf

    return DamageGZAnalysis(
        equilibrium_heel_deg=eq,
        range_end_deg=range_end,
        range_beyond_equilibrium_deg=range_deg,
        residual_area_m_rad=residual,
        heeling_area_m_rad=heeling_area,
        area_ratio=ratio,
        has_heeling_arm=has_arm,
    )


def _first_up_crossing(net, span: float, steps: int) -> float | None:
    """Smallest heel where the net arm becomes >= 0 (bisected to 1e-4 deg)."""
    if net(0.0) >= 0.0:
        return 0.0
    prev = 0.0
    for i in range(1, steps + 1):
        phi = span * i / steps
        if net(phi) >= 0.0:
            lo, hi = prev, phi
            for _ in range(60):
                if hi - lo < 1e-4:
                    break
                mid = 0.5 * (lo + hi)
                if net(mid) >= 0.0:
                    hi = mid
                else:
                    lo = mid
            return 0.5 * (lo + hi)
        prev = phi
    return None


def _first_down_crossing(net, from_deg: float, to_deg: float, steps: int) -> float | None:
    """First heel after ``from_deg`` where the net arm drops below 0."""
    prev = from_deg
    for i in range(1, steps + 1):
        phi = from_deg + (to_deg - from_deg) * i / steps
        if net(phi) < 0.0:
            lo, hi = prev, phi  # net >= 0 at lo, < 0 at hi
            for _ in range(60):
                if hi - lo < 1e-4:
                    break
                mid = 0.5 * (lo + hi)
                if net(mid) < 0.0:
                    hi = mid
                else:
                    lo = mid
            return 0.5 * (lo + hi)
        prev = phi
    return None


def _positive_net_area_m_rad(
    net, from_deg: float, to_deg: float, steps: int = 400
) -> float:
    """Trapezoidal area of max(net arm, 0) between two heel angles [m*rad]."""
    total = 0.0
    prev_phi = from_deg
    prev_v = max(net(from_deg), 0.0)
    for i in range(1, steps + 1):
        phi = from_deg + (to_deg - from_deg) * i / steps
        v = max(net(phi), 0.0)
        total += 0.5 * (prev_v + v) * math.radians(phi - prev_phi)
        prev_phi, prev_v = phi, v
    return total


# -- cited survival criteria ---------------------------------------------------------


@dataclass(frozen=True)
class DamageCriteria:
    """Post-damage survival criteria — every threshold is a cited config
    input (46 CFR Part 174 per the applicable vessel class, IMO MODU Code
    2009 (Res. A.1023(26)) damage criteria, or the governing flag/class
    set). No thresholds are baked in: rule values vary by vessel class
    and edition, so the config must supply value + citation.

    All thresholds are optional but at least one must be set. The
    GZ-based thresholds (range, residual area, area ratio) require
    damaged GZ data; the area ratio additionally requires a heeling arm
    (flooding transverse moment and/or wind).
    """

    citation: Citation
    max_equilibrium_heel_deg: float | None = None
    min_gm_m: float | None = None
    min_downflooding_margin_deg: float | None = None
    min_range_beyond_equilibrium_deg: float | None = None
    min_residual_area_m_rad: float | None = None
    min_area_ratio: float | None = None

    def __post_init__(self) -> None:
        thresholds = (
            self.max_equilibrium_heel_deg,
            self.min_gm_m,
            self.min_downflooding_margin_deg,
            self.min_range_beyond_equilibrium_deg,
            self.min_residual_area_m_rad,
            self.min_area_ratio,
        )
        if all(value is None for value in thresholds):
            raise ValueError("damage criteria: at least one threshold must be set")
        for name in (
            "max_equilibrium_heel_deg",
            "min_range_beyond_equilibrium_deg",
            "min_residual_area_m_rad",
            "min_area_ratio",
        ):
            value = getattr(self, name)
            if value is not None and value <= 0.0:
                raise ValueError(f"damage criteria: {name} must be > 0")
        if (
            self.min_downflooding_margin_deg is not None
            and self.min_downflooding_margin_deg < 0.0
        ):
            raise ValueError(
                "damage criteria: min_downflooding_margin_deg must be >= 0"
            )


@dataclass(frozen=True)
class DamageCaseResult:
    """One evaluated damage case (report_pack-ready)."""

    case_name: str
    method: str  # "added_weight" or "direct"
    displacement_t: float
    gm_fluid_m: float
    heel_deg: float | None
    limit_angle_deg: float | None
    downflooding_margin_deg: float | None
    range_beyond_equilibrium_deg: float | None
    residual_area_m_rad: float | None
    heeling_area_m_rad: float | None
    area_ratio: float | None
    criteria: tuple[CriterionResult, ...]

    @property
    def passed(self) -> bool:
        return all(c.passed for c in self.criteria)


def evaluate_damage_case(
    name: str,
    method: str,
    displacement_t: float,
    gm_fluid_m: float,
    criteria: DamageCriteria,
    heel_deg: float | None,
    limit_angle_deg: float | None = None,
    analysis: DamageGZAnalysis | None = None,
) -> DamageCaseResult:
    """Evaluate one damage case against the cited survival criteria.

    ``heel_deg`` is the static post-damage heel (added-weight small-angle
    value or the supplied damaged-condition heel); when a GZ analysis
    with a heeling arm is present its curve equilibrium supersedes it.
    ``limit_angle_deg`` is the least immersion angle input (downflooding
    opening / margin line). A criterion whose input is missing entirely
    (e.g. a range threshold without GZ data) raises — a screening run
    must not silently skip a configured criterion; a criterion whose
    *value* is indeterminate (no static equilibrium, non-positive GM)
    fails.
    """
    if method not in ("added_weight", "direct"):
        raise ValueError("damage case method must be 'added_weight' or 'direct'")
    if displacement_t <= 0.0:
        raise ValueError("damage case: displacement_t must be > 0")

    heel = heel_deg
    if analysis is not None and analysis.has_heeling_arm:
        heel = analysis.equilibrium_heel_deg

    cite = criteria.citation
    results: list[CriterionResult] = []

    if criteria.max_equilibrium_heel_deg is not None:
        results.append(
            _evaluate(
                "damage_equilibrium_heel",
                f"Static heel after flooding '{name}'"
                + ("" if heel is not None else " (no static equilibrium)"),
                heel,
                criteria.max_equilibrium_heel_deg,
                "deg",
                "<=",
                cite,
            )
        )
    if criteria.min_gm_m is not None:
        results.append(
            _evaluate(
                "damage_gm",
                "Metacentric height after flooding (fluid)",
                gm_fluid_m,
                criteria.min_gm_m,
                "m",
                ">=",
                cite,
            )
        )
    if criteria.min_downflooding_margin_deg is not None:
        if limit_angle_deg is None:
            raise ValueError(
                f"damage case '{name}': min_downflooding_margin_deg is set "
                "but no downflooding_angle_deg / margin_line_immersion_deg "
                "input was supplied"
            )
        margin = None if heel is None else limit_angle_deg - heel
        results.append(
            _evaluate(
                "damage_downflooding_margin",
                f"Margin from equilibrium to downflooding/margin line "
                f"({limit_angle_deg:g} deg)",
                margin,
                criteria.min_downflooding_margin_deg,
                "deg",
                ">=",
                cite,
            )
        )
    else:
        margin = (
            None
            if heel is None or limit_angle_deg is None
            else limit_angle_deg - heel
        )

    gz_thresholds = (
        criteria.min_range_beyond_equilibrium_deg,
        criteria.min_residual_area_m_rad,
        criteria.min_area_ratio,
    )
    if analysis is None and any(value is not None for value in gz_thresholds):
        raise ValueError(
            f"damage case '{name}': GZ-based criteria (range/residual area/"
            "area ratio) require damaged GZ or KN data"
        )
    if criteria.min_range_beyond_equilibrium_deg is not None:
        results.append(
            _evaluate(
                "damage_range",
                "Range of positive stability beyond equilibrium",
                analysis.range_beyond_equilibrium_deg,
                criteria.min_range_beyond_equilibrium_deg,
                "deg",
                ">=",
                cite,
            )
        )
    if criteria.min_residual_area_m_rad is not None:
        results.append(
            _evaluate(
                "damage_residual_area",
                "Residual righting area beyond equilibrium",
                analysis.residual_area_m_rad,
                criteria.min_residual_area_m_rad,
                "m*rad",
                ">=",
                cite,
            )
        )
    if criteria.min_area_ratio is not None:
        if not analysis.has_heeling_arm:
            raise ValueError(
                f"damage case '{name}': min_area_ratio requires a heeling "
                "arm (flooding transverse moment and/or wind heeling moment)"
            )
        ratio = analysis.area_ratio
        results.append(
            _evaluate(
                "damage_area_ratio",
                "Residual righting energy / heeling energy ratio",
                None if ratio is None else (ratio if ratio != math.inf else 1e9),
                criteria.min_area_ratio,
                "-",
                ">=",
                cite,
            )
        )

    return DamageCaseResult(
        case_name=name,
        method=method,
        displacement_t=displacement_t,
        gm_fluid_m=gm_fluid_m,
        heel_deg=heel,
        limit_angle_deg=limit_angle_deg,
        downflooding_margin_deg=margin,
        range_beyond_equilibrium_deg=(
            None if analysis is None else analysis.range_beyond_equilibrium_deg
        ),
        residual_area_m_rad=(
            None if analysis is None else analysis.residual_area_m_rad
        ),
        heeling_area_m_rad=(
            None if analysis is None else analysis.heeling_area_m_rad
        ),
        area_ratio=None if analysis is None else analysis.area_ratio,
        criteria=tuple(results),
    )
