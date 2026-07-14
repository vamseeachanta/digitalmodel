# ABOUTME: Intact vessel stability screening core — hydrostatic-table interface,
# ABOUTME: loading condition + FSC, GZ/KN criteria (IMO IS 2008, 46 CFR), max-KG.
"""Vessel intact-stability screening — deterministic core (L1 slice).

Pure functions and dataclasses for:

1. **Hydrostatic-table interface**: a draft-indexed table (displacement, KM,
   LCB, LCF, MTC, TPC) supplied by the user (from GHS, a curves-of-form
   sheet, or a published example) — geometry-based hydrostatics is a later
   slice.
2. **Loading-condition builder**: lightship + tank/cargo weight items ->
   displacement, KG and LCG, with a free-surface correction from per-tank
   free-surface moments (FSM) or rectangular tank dimensions
   (``FSM = rho * l * b^3 / 12``).
3. **Equilibrium estimate**: mean draft at displacement, KM/GM (solid and
   fluid), trim from ``trim = W * (LCB - LCG) / MTC`` distributed about the
   LCF.
4. **GZ-curve analysis** from a supplied GZ table or cross-curves (KN):
   ``GZ = KN - KG_fluid * sin(phi)``.
5. **Criteria checks** — IMO IS Code 2008 Part A intact criteria (areas
   0-30 / 0-40 / 30-40, GZ at 30, angle of max GZ, GM0) and the
   46 CFR 170.170 weather criterion ``GM >= P*A*H / (W * tan(T))``. Every
   criterion row carries a :class:`Citation` (standard/edition/clause —
   foam_system pattern); default IMO values are cited to IS Code 2008
   (Res. MSC.267(85)) and are config-overridable.
6. **Lifting/crane-heel load case**: cosine heeling-arm curve from a hook
   load x transverse outreach (or a direct heeling moment), static
   equilibrium heel angle, and 46 CFR 173.005-series style criteria
   (max equilibrium heel, residual righting-energy ratio) — thresholds are
   config-supplied and MUST be cited.
7. **Max-KG screening**: iterate the fluid KG to the limiting value per
   criterion at a given displacement -> KG-limit table.

SCREENING POSTURE: this is the deterministic screen. The PE-stamped
stability booklet / class or USCG submittal governs; criteria thresholds
here are traceability-cited config inputs, not a substitute for the
approved calculation (GHS remains the licensed cross-check).

Units: SI marine practice — metres, tonnes, degrees at the API surface
(m*rad for GZ areas), MTC in t*m per cm of trim. Longitudinal positions
are measured from the aft perpendicular, positive forward. Trim is
positive by the stern.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field

from digitalmodel.naval_architecture.damage_stability import (
    gz_area_under_curve,
    interpolate_gz,
)

RHO_SEAWATER_T_M3 = 1.025
"""Standard seawater density [t/m3]."""


# -- citations (foam_system pattern) ------------------------------------------


@dataclass(frozen=True)
class Citation:
    """Traceability record for a criteria value.

    All three of ``standard``, ``edition`` and ``clause`` are mandatory —
    a criterion threshold without them is not usable in a deliverable.
    """

    standard: str
    edition: str
    clause: str
    note: str = ""

    def __post_init__(self) -> None:
        for name in ("standard", "edition", "clause"):
            if not str(getattr(self, name)).strip():
                raise ValueError(f"citation.{name} must be a non-empty string")

    def label(self) -> str:
        text = f"{self.standard} ({self.edition}) {self.clause}"
        return f"{text} — {self.note}" if self.note else text


# -- hydrostatic-table interface ----------------------------------------------


@dataclass(frozen=True)
class HydroRow:
    """One draft-indexed row of a vessel hydrostatic table."""

    draft_m: float
    displacement_t: float
    km_m: float
    lcb_m: float | None = None
    lcf_m: float | None = None
    mct_t_m_per_cm: float | None = None
    tpc_t_per_cm: float | None = None

    def __post_init__(self) -> None:
        if self.draft_m <= 0.0:
            raise ValueError("hydrostatic row: draft_m must be > 0")
        if self.displacement_t <= 0.0:
            raise ValueError("hydrostatic row: displacement_t must be > 0")
        if self.km_m <= 0.0:
            raise ValueError("hydrostatic row: km_m must be > 0")


@dataclass(frozen=True)
class HydrostaticProperties:
    """Hydrostatic properties interpolated at one displacement."""

    draft_m: float
    displacement_t: float
    km_m: float
    lcb_m: float | None
    lcf_m: float | None
    mct_t_m_per_cm: float | None
    tpc_t_per_cm: float | None


@dataclass(frozen=True)
class HydrostaticTable:
    """Draft-indexed hydrostatic table with linear interpolation.

    Rows must be strictly increasing in both draft and displacement.
    Interpolation is linear in displacement (the loading-condition entry
    point) or draft. Queries outside the tabulated range raise — a
    screening run must not silently extrapolate the hull.
    """

    rows: tuple[HydroRow, ...]

    def __post_init__(self) -> None:
        if len(self.rows) < 2:
            raise ValueError("hydrostatic table needs >= 2 rows")
        rows = tuple(sorted(self.rows, key=lambda r: r.draft_m))
        object.__setattr__(self, "rows", rows)
        for prev, cur in zip(rows, rows[1:]):
            if cur.draft_m <= prev.draft_m:
                raise ValueError("hydrostatic table drafts must be strictly increasing")
            if cur.displacement_t <= prev.displacement_t:
                raise ValueError(
                    "hydrostatic table displacements must increase with draft"
                )

    def at_displacement(self, displacement_t: float) -> HydrostaticProperties:
        return self._interpolate(
            displacement_t, [r.displacement_t for r in self.rows], "displacement_t"
        )

    def at_draft(self, draft_m: float) -> HydrostaticProperties:
        return self._interpolate(draft_m, [r.draft_m for r in self.rows], "draft_m")

    def _interpolate(
        self, target: float, keys: list[float], label: str
    ) -> HydrostaticProperties:
        if target < keys[0] or target > keys[-1]:
            raise ValueError(
                f"{label}={target:g} outside hydrostatic table range "
                f"[{keys[0]:g}, {keys[-1]:g}] — no extrapolation in screening"
            )
        hi = next(i for i, k in enumerate(keys) if k >= target)
        if keys[hi] == target:
            row = self.rows[hi]
            return HydrostaticProperties(
                draft_m=row.draft_m,
                displacement_t=row.displacement_t,
                km_m=row.km_m,
                lcb_m=row.lcb_m,
                lcf_m=row.lcf_m,
                mct_t_m_per_cm=row.mct_t_m_per_cm,
                tpc_t_per_cm=row.tpc_t_per_cm,
            )
        lo = hi - 1
        frac = (target - keys[lo]) / (keys[hi] - keys[lo])
        a, b = self.rows[lo], self.rows[hi]

        def lerp(x: float | None, y: float | None) -> float | None:
            if x is None or y is None:
                return None
            return x + frac * (y - x)

        return HydrostaticProperties(
            draft_m=lerp(a.draft_m, b.draft_m),
            displacement_t=lerp(a.displacement_t, b.displacement_t),
            km_m=lerp(a.km_m, b.km_m),
            lcb_m=lerp(a.lcb_m, b.lcb_m),
            lcf_m=lerp(a.lcf_m, b.lcf_m),
            mct_t_m_per_cm=lerp(a.mct_t_m_per_cm, b.mct_t_m_per_cm),
            tpc_t_per_cm=lerp(a.tpc_t_per_cm, b.tpc_t_per_cm),
        )


# -- loading condition ----------------------------------------------------------


def rectangular_fsm_t_m(
    length_m: float, breadth_m: float, density_t_m3: float
) -> float:
    """Free-surface moment of a slack rectangular tank: ``rho * l * b^3 / 12``."""
    if length_m <= 0.0 or breadth_m <= 0.0 or density_t_m3 <= 0.0:
        raise ValueError("rectangular tank FSM needs positive l, b and density")
    return density_t_m3 * length_m * breadth_m**3 / 12.0


@dataclass(frozen=True)
class WeightItem:
    """One loading-condition weight item (lightship, tank, cargo, hook load).

    ``fsm_t_m`` is the free-surface moment of the item when it is a slack
    tank (t*m); zero for solid weights and pressed-full/empty tanks.
    """

    name: str
    weight_t: float
    vcg_m: float
    lcg_m: float
    fsm_t_m: float = 0.0

    def __post_init__(self) -> None:
        if self.weight_t <= 0.0:
            raise ValueError(f"weight item '{self.name}': weight_t must be > 0")
        if self.fsm_t_m < 0.0:
            raise ValueError(f"weight item '{self.name}': fsm_t_m must be >= 0")


@dataclass(frozen=True)
class LoadingCondition:
    """Summed loading condition with free-surface correction."""

    name: str
    items: tuple[WeightItem, ...]
    displacement_t: float
    kg_m: float
    lcg_m: float
    fsm_total_t_m: float
    fsc_m: float
    kg_fluid_m: float


def build_loading_condition(name: str, items: list[WeightItem]) -> LoadingCondition:
    """Sum weight items into displacement, KG, LCG and the fluid (FSC) KG.

    ``FSC = sum(FSM_i) / displacement``; ``KG_fluid = KG + FSC`` — the
    standard virtual-rise-of-G free-surface correction.
    """
    if not items:
        raise ValueError("loading condition needs at least one weight item")
    displacement = sum(item.weight_t for item in items)
    kg = sum(item.weight_t * item.vcg_m for item in items) / displacement
    lcg = sum(item.weight_t * item.lcg_m for item in items) / displacement
    fsm = sum(item.fsm_t_m for item in items)
    fsc = fsm / displacement
    return LoadingCondition(
        name=name,
        items=tuple(items),
        displacement_t=displacement,
        kg_m=kg,
        lcg_m=lcg,
        fsm_total_t_m=fsm,
        fsc_m=fsc,
        kg_fluid_m=kg + fsc,
    )


@dataclass(frozen=True)
class Equilibrium:
    """Upright equilibrium estimate at the condition displacement."""

    draft_m: float
    km_m: float
    gm_solid_m: float
    gm_fluid_m: float
    lcb_m: float | None
    lcf_m: float | None
    mct_t_m_per_cm: float | None
    trim_m: float | None
    draft_aft_m: float | None
    draft_fwd_m: float | None


def solve_equilibrium(
    condition: LoadingCondition,
    table: HydrostaticTable,
    lbp_m: float | None = None,
) -> Equilibrium:
    """Draft, GM and trim estimate at the condition displacement.

    Trim (positive by the stern) from ``trim_cm = W * (LCB - LCG) / MTC``;
    end drafts distribute the trim about the LCF when ``lbp_m`` is given.
    The mean draft from the table is taken as the draft at the LCF
    (standard hydrostatic-table convention).
    """
    props = table.at_displacement(condition.displacement_t)
    gm_solid = props.km_m - condition.kg_m
    gm_fluid = props.km_m - condition.kg_fluid_m

    trim_m = draft_aft = draft_fwd = None
    if props.lcb_m is not None and props.mct_t_m_per_cm is not None:
        trim_cm = (
            condition.displacement_t
            * (props.lcb_m - condition.lcg_m)
            / props.mct_t_m_per_cm
        )
        trim_m = trim_cm / 100.0
        if lbp_m is not None and props.lcf_m is not None:
            if lbp_m <= 0.0:
                raise ValueError("lbp_m must be > 0")
            draft_aft = props.draft_m + trim_m * (props.lcf_m / lbp_m)
            draft_fwd = props.draft_m - trim_m * (1.0 - props.lcf_m / lbp_m)

    return Equilibrium(
        draft_m=props.draft_m,
        km_m=props.km_m,
        gm_solid_m=gm_solid,
        gm_fluid_m=gm_fluid,
        lcb_m=props.lcb_m,
        lcf_m=props.lcf_m,
        mct_t_m_per_cm=props.mct_t_m_per_cm,
        trim_m=trim_m,
        draft_aft_m=draft_aft,
        draft_fwd_m=draft_fwd,
    )


# -- GZ curve -------------------------------------------------------------------


@dataclass(frozen=True)
class GZCurve:
    """Righting-arm curve, tabulated at increasing heel angles from 0."""

    heel_deg: tuple[float, ...]
    gz_m: tuple[float, ...]

    def __post_init__(self) -> None:
        if len(self.heel_deg) != len(self.gz_m):
            raise ValueError("GZ curve: heel_deg and gz_m lengths differ")
        if len(self.heel_deg) < 3:
            raise ValueError("GZ curve needs >= 3 points")
        if any(b <= a for a, b in zip(self.heel_deg, self.heel_deg[1:])):
            raise ValueError("GZ curve heel angles must be strictly increasing")
        if self.heel_deg[0] != 0.0:
            raise ValueError("GZ curve must start at 0 deg heel")

    def gz_at(self, heel_deg: float) -> float:
        return interpolate_gz(list(self.heel_deg), list(self.gz_m), heel_deg)

    def area_m_rad(self, from_deg: float, to_deg: float) -> float:
        """Area under the curve between two heel angles [m*rad]."""
        if to_deg < from_deg:
            raise ValueError("GZ area: to_deg must be >= from_deg")
        angles, values = list(self.heel_deg), list(self.gz_m)
        return gz_area_under_curve(angles, values, to_deg) - gz_area_under_curve(
            angles, values, from_deg
        )

    def max_gz(self) -> tuple[float, float]:
        """(angle_deg, gz_m) at the tabulated maximum righting arm."""
        idx = max(range(len(self.gz_m)), key=lambda i: self.gz_m[i])
        return self.heel_deg[idx], self.gz_m[idx]


def gz_from_kn(
    heel_deg: list[float], kn_m: list[float], kg_fluid_m: float
) -> GZCurve:
    """GZ curve from cross-curves of stability: ``GZ = KN - KG * sin(phi)``.

    ``kg_fluid_m`` is the free-surface-corrected KG (KG + FSC).
    """
    if len(heel_deg) != len(kn_m):
        raise ValueError("KN table: heel_deg and kn_m lengths differ")
    gz = [
        kn - kg_fluid_m * math.sin(math.radians(phi))
        for phi, kn in zip(heel_deg, kn_m)
    ]
    return GZCurve(heel_deg=tuple(heel_deg), gz_m=tuple(gz))


def kn_at_displacement(
    heel_deg: list[float],
    displacements_t: list[float],
    kn_grid_m: list[list[float]],
    displacement_t: float,
) -> list[float]:
    """Interpolate a KN cross-curve grid (rows = displacements) linearly in
    displacement. No extrapolation."""
    if len(displacements_t) != len(kn_grid_m):
        raise ValueError("KN grid: displacements_t and kn_grid_m lengths differ")
    if any(len(row) != len(heel_deg) for row in kn_grid_m):
        raise ValueError("KN grid: every row must match heel_deg length")
    if any(b <= a for a, b in zip(displacements_t, displacements_t[1:])):
        raise ValueError("KN grid displacements must be strictly increasing")
    if not displacements_t[0] <= displacement_t <= displacements_t[-1]:
        raise ValueError(
            f"displacement {displacement_t:g} outside KN grid range "
            f"[{displacements_t[0]:g}, {displacements_t[-1]:g}]"
        )
    hi = next(i for i, d in enumerate(displacements_t) if d >= displacement_t)
    if displacements_t[hi] == displacement_t:
        return list(kn_grid_m[hi])
    lo = hi - 1
    frac = (displacement_t - displacements_t[lo]) / (
        displacements_t[hi] - displacements_t[lo]
    )
    return [
        a + frac * (b - a) for a, b in zip(kn_grid_m[lo], kn_grid_m[hi])
    ]


# -- criteria engine --------------------------------------------------------------


@dataclass(frozen=True)
class CriterionResult:
    """One evaluated criteria-table row (report_pack-ready)."""

    key: str
    description: str
    value: float | None
    required: float
    unit: str
    comparison: str  # ">=" or "<="
    passed: bool
    citation: str  # Citation.label()

    @property
    def margin(self) -> float | None:
        if self.value is None:
            return None
        if self.comparison == ">=":
            return self.value - self.required
        return self.required - self.value


def _evaluate(
    key: str,
    description: str,
    value: float | None,
    required: float,
    unit: str,
    comparison: str,
    citation: Citation,
) -> CriterionResult:
    if value is None:
        passed = False
    elif comparison == ">=":
        passed = value >= required
    elif comparison == "<=":
        passed = value <= required
    else:
        raise ValueError(f"criterion '{key}': comparison must be >= or <=")
    return CriterionResult(
        key=key,
        description=description,
        value=value,
        required=required,
        unit=unit,
        comparison=comparison,
        passed=passed,
        citation=citation.label(),
    )


# -- IMO IS Code 2008 Part A intact criteria ---------------------------------------


@dataclass(frozen=True)
class ImoIntactCriteria:
    """IMO IS Code 2008 Part A 2.2 intact-criteria thresholds, cited.

    Defaults carry the published IS Code 2008 (Res. MSC.267(85)) values;
    override values and citations from config for other flag/edition sets.
    """

    area_0_30_min_m_rad: float = 0.055
    area_0_40_min_m_rad: float = 0.09
    area_30_40_min_m_rad: float = 0.03
    gz_30_min_m: float = 0.20
    angle_max_gz_min_deg: float = 25.0
    gm0_min_m: float = 0.15
    citation: Citation = field(
        default_factory=lambda: Citation(
            standard="IMO IS Code 2008 (Res. MSC.267(85))",
            edition="2008",
            clause="Part A 2.2.1-2.2.4",
            note="general intact criteria — verify against the governing edition",
        )
    )


def imo_intact_criteria(
    curve: GZCurve,
    gm0_m: float,
    criteria: ImoIntactCriteria | None = None,
    downflooding_angle_deg: float | None = None,
) -> list[CriterionResult]:
    """Evaluate the six IS Code 2008 Part A 2.2 intact criteria.

    The 0-40 and 30-40 areas cap at the downflooding angle when it is less
    than 40 deg (IS Code 2008 Part A 2.2.1). Angle of maximum GZ uses the
    tabulated maximum.
    """
    c = criteria or ImoIntactCriteria()
    cap_40 = 40.0
    if downflooding_angle_deg is not None:
        if downflooding_angle_deg <= 0.0:
            raise ValueError("downflooding_angle_deg must be > 0")
        cap_40 = min(40.0, downflooding_angle_deg)
    angle_max, _ = curve.max_gz()
    cite = c.citation
    return [
        _evaluate(
            "imo_area_0_30",
            "Area under GZ, 0-30 deg",
            curve.area_m_rad(0.0, 30.0),
            c.area_0_30_min_m_rad,
            "m*rad",
            ">=",
            cite,
        ),
        _evaluate(
            "imo_area_0_40",
            f"Area under GZ, 0-{cap_40:g} deg (40 deg or downflooding)",
            curve.area_m_rad(0.0, cap_40),
            c.area_0_40_min_m_rad,
            "m*rad",
            ">=",
            cite,
        ),
        _evaluate(
            "imo_area_30_40",
            f"Area under GZ, 30-{cap_40:g} deg",
            curve.area_m_rad(30.0, cap_40) if cap_40 > 30.0 else 0.0,
            c.area_30_40_min_m_rad,
            "m*rad",
            ">=",
            cite,
        ),
        _evaluate(
            "imo_gz_30",
            "Righting arm GZ at 30 deg",
            curve.gz_at(30.0),
            c.gz_30_min_m,
            "m",
            ">=",
            cite,
        ),
        _evaluate(
            "imo_angle_max_gz",
            "Angle of maximum GZ",
            angle_max,
            c.angle_max_gz_min_deg,
            "deg",
            ">=",
            cite,
        ),
        _evaluate(
            "imo_gm0",
            "Initial metacentric height GM0 (fluid)",
            gm0_m,
            c.gm0_min_m,
            "m",
            ">=",
            cite,
        ),
    ]


# -- 46 CFR 170.170 weather criterion -----------------------------------------------


@dataclass(frozen=True)
class WeatherCriterion:
    """46 CFR 170.170 weather-criterion inputs — all config-supplied, cited.

    ``GM_required = P * A * H / (W * tan(T))`` with T the lesser of
    ``max_heel_deg`` (14 deg in the rule) and the deck-edge immersion
    angle. The wind pressure ``P`` depends on service (ocean / partially
    protected / protected) and the rule edition — it is a cited config
    input, never a baked-in value.
    """

    wind_pressure_t_m2: float
    windage_area_m2: float
    windage_lever_m: float
    citation: Citation
    max_heel_deg: float = 14.0

    def __post_init__(self) -> None:
        for name in ("wind_pressure_t_m2", "windage_area_m2", "windage_lever_m"):
            if getattr(self, name) <= 0.0:
                raise ValueError(f"weather criterion: {name} must be > 0")
        if not 0.0 < self.max_heel_deg < 90.0:
            raise ValueError("weather criterion: max_heel_deg must be in (0, 90)")


def weather_criterion_cfr_170_170(
    weather: WeatherCriterion,
    displacement_t: float,
    gm_fluid_m: float,
    deck_edge_immersion_deg: float | None = None,
) -> CriterionResult:
    """46 CFR 170.170 weather criterion: GM (fluid) >= P*A*H / (W*tan(T))."""
    if displacement_t <= 0.0:
        raise ValueError("weather criterion: displacement_t must be > 0")
    heel = weather.max_heel_deg
    if deck_edge_immersion_deg is not None and deck_edge_immersion_deg > 0.0:
        heel = min(heel, deck_edge_immersion_deg)
    required = (
        weather.wind_pressure_t_m2
        * weather.windage_area_m2
        * weather.windage_lever_m
        / (displacement_t * math.tan(math.radians(heel)))
    )
    return _evaluate(
        "cfr_170_170_weather_gm",
        f"Weather criterion GM >= PAH/(W tan T), T = {heel:g} deg",
        gm_fluid_m,
        required,
        "m",
        ">=",
        weather.citation,
    )


# -- lifting / crane-heel load case ---------------------------------------------------


@dataclass(frozen=True)
class CraneHeelingCase:
    """Crane/davit lifting heeling-moment input.

    Either a direct ``heeling_moment_t_m`` or ``hook_load_t`` x
    ``transverse_outreach_m`` (moment about centreline at zero heel).
    The heeling arm varies as ``HA(phi) = HM * cos(phi) / W`` — the
    standard 46 CFR 173 Subpart B heeling-moment form.
    """

    name: str
    heeling_moment_t_m: float

    def __post_init__(self) -> None:
        if self.heeling_moment_t_m <= 0.0:
            raise ValueError(
                f"crane case '{self.name}': heeling_moment_t_m must be > 0"
            )

    @classmethod
    def from_hook_load(
        cls, name: str, hook_load_t: float, transverse_outreach_m: float
    ) -> CraneHeelingCase:
        if hook_load_t <= 0.0 or transverse_outreach_m <= 0.0:
            raise ValueError(
                f"crane case '{name}': hook_load_t and transverse_outreach_m "
                "must be > 0"
            )
        return cls(name=name, heeling_moment_t_m=hook_load_t * transverse_outreach_m)

    def heeling_arm_m(self, displacement_t: float, heel_deg: float) -> float:
        return (
            self.heeling_moment_t_m
            * math.cos(math.radians(heel_deg))
            / displacement_t
        )


@dataclass(frozen=True)
class LiftingCriteria:
    """46 CFR 173.005-series style lifting criteria — thresholds cited.

    ``max_equilibrium_heel_deg``: static heel under the lift must not
    exceed this angle (and never the deck-edge immersion angle when one
    is supplied). ``residual_area_ratio_min``: ratio of the residual
    righting energy (area between GZ and the heeling arm from the
    equilibrium heel to the residual-range limit) to the heeling energy
    (area under the heeling arm to the equilibrium heel).
    """

    max_equilibrium_heel_deg: float
    citation: Citation
    residual_area_ratio_min: float | None = None
    residual_range_limit_deg: float = 40.0

    def __post_init__(self) -> None:
        if self.max_equilibrium_heel_deg <= 0.0:
            raise ValueError("lifting criteria: max_equilibrium_heel_deg must be > 0")
        if self.residual_area_ratio_min is not None and self.residual_area_ratio_min <= 0.0:
            raise ValueError("lifting criteria: residual_area_ratio_min must be > 0")
        if self.residual_range_limit_deg <= 0.0:
            raise ValueError("lifting criteria: residual_range_limit_deg must be > 0")


@dataclass(frozen=True)
class LiftingResult:
    """Evaluated crane-heel load case."""

    case_name: str
    heeling_moment_t_m: float
    heeling_arm_0_m: float
    equilibrium_heel_deg: float | None
    residual_range_to_deg: float | None
    residual_area_m_rad: float | None
    heeling_area_m_rad: float | None
    residual_area_ratio: float | None
    criteria: tuple[CriterionResult, ...]

    @property
    def passed(self) -> bool:
        return all(c.passed for c in self.criteria)


def equilibrium_heel_deg(
    curve: GZCurve, case: CraneHeelingCase, displacement_t: float
) -> float | None:
    """First static equilibrium: smallest heel where GZ(phi) = HA(phi).

    Returns None when the heeling arm exceeds GZ over the whole tabulated
    range (no static equilibrium — the lift capsizes the screen).
    Solved by scanning the residual ``GZ - HA`` for its first sign change
    and bisecting to 1e-4 deg.
    """

    def residual(phi: float) -> float:
        return curve.gz_at(phi) - case.heeling_arm_m(displacement_t, phi)

    lo, hi = None, None
    steps = 2000
    span = curve.heel_deg[-1]
    prev_phi = 0.0
    if residual(0.0) >= 0.0:
        return 0.0
    for i in range(1, steps + 1):
        phi = span * i / steps
        if residual(phi) >= 0.0:
            lo, hi = prev_phi, phi
            break
        prev_phi = phi
    if lo is None:
        return None
    for _ in range(60):
        mid = 0.5 * (lo + hi)
        if hi - lo < 1e-4:
            break
        if residual(mid) >= 0.0:
            hi = mid
        else:
            lo = mid
    return 0.5 * (lo + hi)


def lifting_check(
    curve: GZCurve,
    case: CraneHeelingCase,
    displacement_t: float,
    criteria: LiftingCriteria,
    downflooding_angle_deg: float | None = None,
    deck_edge_immersion_deg: float | None = None,
) -> LiftingResult:
    """Evaluate a crane-heel load case against the cited lifting criteria.

    The residual range runs from the equilibrium heel to the least of the
    configured limit, the downflooding angle and the tabulated GZ range.
    """
    if displacement_t <= 0.0:
        raise ValueError("lifting check: displacement_t must be > 0")
    eq = equilibrium_heel_deg(curve, case, displacement_t)
    cite = criteria.citation

    heel_limit = criteria.max_equilibrium_heel_deg
    if deck_edge_immersion_deg is not None and deck_edge_immersion_deg > 0.0:
        heel_limit = min(heel_limit, deck_edge_immersion_deg)

    results = [
        _evaluate(
            "lifting_equilibrium_heel",
            f"Equilibrium heel under lift '{case.name}'"
            + ("" if eq is not None else " (no static equilibrium)"),
            eq,
            heel_limit,
            "deg",
            "<=",
            cite,
        )
    ]

    residual_to = residual_area = heeling_area = ratio = None
    if criteria.residual_area_ratio_min is not None:
        if eq is not None:
            residual_to = min(
                criteria.residual_range_limit_deg,
                curve.heel_deg[-1],
                *(
                    [downflooding_angle_deg]
                    if downflooding_angle_deg is not None
                    else []
                ),
            )
            if residual_to > eq:
                residual_area = _net_area_m_rad(
                    curve, case, displacement_t, eq, residual_to
                )
                heeling_area = _arm_area_m_rad(case, displacement_t, 0.0, eq)
                ratio = (
                    residual_area / heeling_area if heeling_area > 0.0 else math.inf
                )
            else:
                residual_area, heeling_area, ratio = 0.0, None, 0.0
        results.append(
            _evaluate(
                "lifting_residual_area_ratio",
                "Residual righting energy / heeling energy ratio",
                None if ratio is None else (ratio if ratio != math.inf else 1e9),
                criteria.residual_area_ratio_min,
                "-",
                ">=",
                cite,
            )
        )

    return LiftingResult(
        case_name=case.name,
        heeling_moment_t_m=case.heeling_moment_t_m,
        heeling_arm_0_m=case.heeling_arm_m(displacement_t, 0.0),
        equilibrium_heel_deg=eq,
        residual_range_to_deg=residual_to,
        residual_area_m_rad=residual_area,
        heeling_area_m_rad=heeling_area,
        residual_area_ratio=ratio,
        criteria=tuple(results),
    )


def _net_area_m_rad(
    curve: GZCurve,
    case: CraneHeelingCase,
    displacement_t: float,
    from_deg: float,
    to_deg: float,
    steps: int = 400,
) -> float:
    """Trapezoidal area of max(GZ - HA, 0) between two heel angles [m*rad]."""
    total = 0.0
    prev_phi = from_deg
    prev_v = max(
        curve.gz_at(from_deg) - case.heeling_arm_m(displacement_t, from_deg), 0.0
    )
    for i in range(1, steps + 1):
        phi = from_deg + (to_deg - from_deg) * i / steps
        v = max(curve.gz_at(phi) - case.heeling_arm_m(displacement_t, phi), 0.0)
        total += 0.5 * (prev_v + v) * math.radians(phi - prev_phi)
        prev_phi, prev_v = phi, v
    return total


def _arm_area_m_rad(
    case: CraneHeelingCase,
    displacement_t: float,
    from_deg: float,
    to_deg: float,
) -> float:
    """Exact area under HA(phi) = (HM/W) cos(phi): (HM/W)(sin(b) - sin(a))."""
    scale = case.heeling_moment_t_m / displacement_t
    return scale * (
        math.sin(math.radians(to_deg)) - math.sin(math.radians(from_deg))
    )


# -- max-KG screening -------------------------------------------------------------


@dataclass(frozen=True)
class KgLimit:
    """Limiting fluid KG for one criterion at a fixed displacement."""

    key: str
    description: str
    kg_limit_m: float | None
    limited_by_km: bool
    citation: str


def max_kg_screening(
    heel_deg: list[float],
    kn_m: list[float],
    km_m: float,
    displacement_t: float,
    imo: ImoIntactCriteria | None = None,
    weather: WeatherCriterion | None = None,
    lifting_case: CraneHeelingCase | None = None,
    lifting: LiftingCriteria | None = None,
    downflooding_angle_deg: float | None = None,
    deck_edge_immersion_deg: float | None = None,
    tolerance_m: float = 1e-4,
) -> list[KgLimit]:
    """Limiting fluid KG per criterion at one displacement (KG-limit row).

    For each criterion, bisects the fluid KG on [0, KM] for the largest
    value that still passes (criteria margins decrease monotonically with
    rising KG for the intact set screened here). ``kg_limit_m`` is None
    when the criterion fails even at KG = 0; ``limited_by_km`` marks
    criteria that still pass at KG = KM (not limiting below KM).
    """
    if km_m <= 0.0:
        raise ValueError("max KG screening: km_m must be > 0")

    def criteria_at(kg: float) -> list[CriterionResult]:
        curve = gz_from_kn(heel_deg, kn_m, kg)
        results = imo_intact_criteria(
            curve, km_m - kg, imo, downflooding_angle_deg
        )
        if weather is not None:
            results.append(
                weather_criterion_cfr_170_170(
                    weather, displacement_t, km_m - kg, deck_edge_immersion_deg
                )
            )
        if lifting_case is not None and lifting is not None:
            results.extend(
                lifting_check(
                    curve,
                    lifting_case,
                    displacement_t,
                    lifting,
                    downflooding_angle_deg,
                    deck_edge_immersion_deg,
                ).criteria
            )
        return results

    base = criteria_at(0.0)
    limits: list[KgLimit] = []
    for idx, criterion in enumerate(base):
        if not criterion.passed:
            limits.append(
                KgLimit(
                    key=criterion.key,
                    description=criterion.description,
                    kg_limit_m=None,
                    limited_by_km=False,
                    citation=criterion.citation,
                )
            )
            continue
        if criteria_at(km_m)[idx].passed:
            limits.append(
                KgLimit(
                    key=criterion.key,
                    description=criterion.description,
                    kg_limit_m=km_m,
                    limited_by_km=True,
                    citation=criterion.citation,
                )
            )
            continue
        lo, hi = 0.0, km_m  # passes at lo, fails at hi
        while hi - lo > tolerance_m:
            mid = 0.5 * (lo + hi)
            if criteria_at(mid)[idx].passed:
                lo = mid
            else:
                hi = mid
        limits.append(
            KgLimit(
                key=criterion.key,
                description=criterion.description,
                kg_limit_m=lo,
                limited_by_km=False,
                citation=criterion.citation,
            )
        )
    return limits


def governing_kg_limit(limits: list[KgLimit]) -> KgLimit | None:
    """The lowest KG limit (the governing criterion); None when any
    criterion fails even at KG = 0 or the list is empty."""
    if not limits:
        return None
    if any(limit.kg_limit_m is None for limit in limits):
        return next(limit for limit in limits if limit.kg_limit_m is None)
    return min(limits, key=lambda limit: limit.kg_limit_m)
