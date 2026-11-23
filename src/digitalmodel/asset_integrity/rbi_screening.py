# ABOUTME: Qualitative API 580-style RBI screening — PoF proxy from FFS outputs,
# ABOUTME: CoF category input, 5x5 risk matrix, risk-scaled inspection interval.
"""Risk-based inspection (RBI) screening layer over FFS outputs.

A thin, deterministic, *qualitative* screening in the spirit of API RP 580
(risk-based inspection framework). It is **not** a quantitative API RP 581
implementation: no damage-factor tables, generic failure frequencies, or
consequence-area models are reproduced here. Every band edge and scaling
factor is a **practice parameter** with a documented default in
:class:`RBIParameters` — override them to match an operator's own RBI
procedure. Defaults are engineering-practice values chosen for screening,
not values claimed from API 581.

Pipeline (feeds the risk-prioritization phase of a 7-phase RBI cycle):

1. **PoF proxy category (1-5)** from FFS outputs — the most conservative of
   up to three sub-scores, each banded by parameterized edges:

   * remaining-strength margin ``rsf / rsf_a`` (from the Level-2 FFS result),
   * remaining-life ratio ``remaining_life / reference planned interval``,
   * plus a +1 escalation when the corrosion rate exceeds a "high/active
     corrosion" threshold (trend flag).

2. **CoF category (1-5)** — a direct operator input: consequence class
   ``A``-``E`` (safety / environment / production descriptors) mapped 1-5.
   No consequence modelling is attempted here.

3. **Risk matrix** — 5x5 cell ``(PoF, CoF)`` scored ``PoF * CoF`` and banded
   LOW / MEDIUM / MEDIUM-HIGH / HIGH with a standard symmetric banding
   (configurable edges).

4. **Interval recommendation** — starts from the validated API 510/570/653
   half-life interval in :mod:`digitalmodel.asset_integrity.inspection_planning`
   (reused, not re-implemented) and applies a per-band interval cap. The
   recommendation therefore never exceeds the half-life rule, the code
   maximum interval, or the band cap.

The seven RBI programme phases mirrored in workflow output
(:data:`RBI_PHASES`): define elements -> grouping/baselining -> risk
prioritization -> plan development -> execution -> results analysis ->
programme updating (with a periodic, typically <= 5-yr, review). This module
computes phase 3 and the interval part of phase 4.

:data:`NON_RBI_BASELINE_INTERVALS_YR` carries fixed-calendar baseline
heuristics (subsea pilot-corpus practice values, shown only as the non-RBI
comparison row) — rigid riser 2 yr, catenary 3 yr, touchdown zone 5 yr,
flexibles 3-5 yr.

References: API RP 580 (risk-based inspection — framework only); API 510 /
570 / 653 half-life interval logic via ``inspection_planning``. Code maximum
intervals differ by code and component class, so ``code_max_interval_yr`` is
a parameter (default 10 yr) rather than a hard-coded code citation.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass, field
from typing import Any, Mapping, Optional, Sequence

import pandas as pd

from .inspection_planning import InspectionPlan, plan_inspection

# 7-phase RBI programme cycle (workflow framing; see module docstring).
RBI_PHASES: tuple[str, ...] = (
    "define elements",
    "grouping and baselining",
    "risk prioritization",
    "plan development",
    "execution",
    "results analysis",
    "programme updating (periodic review)",
)

# Fixed-calendar (non-RBI) baseline interval heuristics [yr] — practice values
# from a subsea RBI pilot corpus, for comparison rows only (not code values).
NON_RBI_BASELINE_INTERVALS_YR: dict[str, tuple[float, float]] = {
    "rigid_riser": (2.0, 2.0),
    "catenary_riser": (3.0, 3.0),
    "touchdown_zone": (5.0, 5.0),
    "flexible_riser": (3.0, 5.0),
}

RISK_BANDS: tuple[str, ...] = ("LOW", "MEDIUM", "MEDIUM-HIGH", "HIGH")

_COF_CLASS_TO_CATEGORY: dict[str, int] = {"A": 1, "B": 2, "C": 3, "D": 4, "E": 5}

# Operator-facing descriptors for the consequence classes (guidance only).
COF_CLASS_DESCRIPTORS: dict[str, str] = {
    "A": "negligible — no injury potential, no release, no production impact",
    "B": "minor — first-aid level, trivial release, brief production upset",
    "C": "moderate — recordable injury, reportable release, partial shutdown",
    "D": "major — serious injury, significant release, extended shutdown",
    "E": "severe — fatality potential, major release, long-term loss",
}


@dataclass(frozen=True)
class RBIParameters:
    """Practice parameters for the qualitative RBI screening.

    All defaults are screening-practice values (documented here, echoed in
    every result's ``basis``), **not** API 581 table values. Band edges are
    descending; a value >= edge[i] lands in category ``i + 1``.

    Attributes:
        rsf_margin_edges: PoF sub-score edges on ``rsf / rsf_a``. Default
            ``(1.10, 1.05, 1.00, 0.90)`` -> categories 1-5 (below the last
            edge is category 5). ``rsf/rsf_a >= 1`` is an FFS pass, so
            category <= 3 means "passes with margin".
        life_ratio_edges: PoF sub-score edges on
            ``remaining_life / reference_interval_yr``. Default
            ``(8.0, 4.0, 2.0, 1.0)`` — a component whose remaining life is
            below one planned interval is category 5.
        reference_interval_yr: planned-interval denominator for the life
            ratio. Default 5.0 yr (typical subsea campaign spacing).
        high_corrosion_rate_mm_yr: corrosion-rate trend flag threshold;
            at/above it the PoF category is escalated by +1 (capped at 5).
            Default 0.25 mm/yr (screening value for "active corrosion").
        risk_band_edges: 5x5 matrix banding on ``score = PoF * CoF``:
            LOW <= edges[0] < MEDIUM <= edges[1] < MEDIUM-HIGH <= edges[2]
            < HIGH. Default ``(4, 9, 15)`` — a standard symmetric banding.
        band_interval_cap_yr: per-band inspection-interval cap [yr] applied
            on top of the API 510/570/653 half-life interval. Default
            LOW 10 / MEDIUM 6 / MEDIUM-HIGH 4 / HIGH 2.
        code_max_interval_yr: code maximum interval passed to
            ``inspection_planning.plan_inspection`` (default 10 yr;
            set per the governing code and component class).
    """

    rsf_margin_edges: tuple[float, float, float, float] = (1.10, 1.05, 1.00, 0.90)
    life_ratio_edges: tuple[float, float, float, float] = (8.0, 4.0, 2.0, 1.0)
    reference_interval_yr: float = 5.0
    high_corrosion_rate_mm_yr: float = 0.25
    risk_band_edges: tuple[int, int, int] = (4, 9, 15)
    band_interval_cap_yr: Mapping[str, float] = field(
        default_factory=lambda: {
            "LOW": 10.0,
            "MEDIUM": 6.0,
            "MEDIUM-HIGH": 4.0,
            "HIGH": 2.0,
        }
    )
    code_max_interval_yr: float = 10.0

    def basis_notes(self) -> list[str]:
        """Every parameter default in play, for the result's ``basis``."""
        caps = ", ".join(
            f"{band}={self.band_interval_cap_yr[band]:g} yr" for band in RISK_BANDS
        )
        return [
            "qualitative API RP 580-style screening; practice parameters, "
            "not API RP 581 table values",
            f"PoF rsf/rsf_a band edges: {self.rsf_margin_edges}",
            f"PoF remaining-life ratio band edges: {self.life_ratio_edges} "
            f"(reference interval {self.reference_interval_yr:g} yr)",
            "corrosion trend flag: +1 PoF at rate >= "
            f"{self.high_corrosion_rate_mm_yr:g} mm/yr",
            f"risk = PoF x CoF banded LOW/MEDIUM/MEDIUM-HIGH/HIGH at "
            f"{self.risk_band_edges}",
            f"interval = min(API 510/570/653 half-life interval, band cap): {caps}",
            f"code maximum interval parameter: {self.code_max_interval_yr:g} yr",
        ]


@dataclass
class RBIScreeningResult:
    """Per-component RBI screening record (inputs echoed + parameter basis)."""

    component_id: str
    pof_category: int
    cof_category: int
    cof_class: str
    risk_score: int
    risk_band: str
    recommended_interval_yr: float
    half_life_interval_yr: Optional[float]
    code_max_interval_yr: float
    governing: str  # which limit set the interval
    pof_subscores: dict[str, Optional[int]]
    inputs: dict[str, Any]  # every input echoed
    inspection_plan: Optional[InspectionPlan]
    basis: list[str]
    phases: tuple[str, ...] = RBI_PHASES

    def to_dict(self) -> dict:
        out = asdict(self)
        out["phases"] = list(self.phases)
        return out


def _band_descending(value: float, edges: Sequence[float]) -> int:
    """Category 1..len(edges)+1 — value >= edges[i] lands in category i+1."""
    for i, edge in enumerate(edges):
        if value >= edge:
            return i + 1
    return len(edges) + 1


def cof_category(consequence_class: str | int) -> tuple[int, str]:
    """Map an operator consequence class (A-E or 1-5) to (category, class)."""
    if isinstance(consequence_class, str):
        cls = consequence_class.strip().upper()
        if cls not in _COF_CLASS_TO_CATEGORY:
            raise ValueError(
                f"consequence class must be A-E or 1-5, got {consequence_class!r}"
            )
        return _COF_CLASS_TO_CATEGORY[cls], cls
    cat = int(consequence_class)
    if not 1 <= cat <= 5:
        raise ValueError(f"consequence category must be 1-5, got {consequence_class!r}")
    return cat, "ABCDE"[cat - 1]


def pof_category(
    *,
    rsf: Optional[float] = None,
    rsf_a: Optional[float] = None,
    remaining_life_yr: Optional[float] = None,
    corrosion_rate_mm_yr: Optional[float] = None,
    params: RBIParameters = RBIParameters(),
) -> tuple[int, dict[str, Optional[int]]]:
    """PoF proxy category 1-5 from FFS outputs (most conservative sub-score).

    Returns ``(category, subscores)`` where ``subscores`` records each
    contribution (``None`` when that input was not supplied). At least one of
    the strength-margin or remaining-life inputs is required.
    """
    subscores: dict[str, Optional[int]] = {
        "rsf_margin": None,
        "life_ratio": None,
        "corrosion_trend_bump": None,
    }

    if rsf is not None:
        if rsf_a is None or rsf_a <= 0.0:
            raise ValueError("rsf_a must be positive when rsf is supplied")
        subscores["rsf_margin"] = _band_descending(rsf / rsf_a, params.rsf_margin_edges)

    if remaining_life_yr is not None:
        if params.reference_interval_yr <= 0.0:
            raise ValueError("reference_interval_yr must be positive")
        ratio = remaining_life_yr / params.reference_interval_yr
        subscores["life_ratio"] = _band_descending(ratio, params.life_ratio_edges)

    base_scores = [s for s in (subscores["rsf_margin"], subscores["life_ratio"]) if s]
    if not base_scores:
        raise ValueError(
            "supply rsf (with rsf_a) and/or remaining_life_yr for a PoF category"
        )
    category = max(base_scores)

    bump = 0
    if (
        corrosion_rate_mm_yr is not None
        and corrosion_rate_mm_yr >= params.high_corrosion_rate_mm_yr
    ):
        bump = 1
    subscores["corrosion_trend_bump"] = bump
    return min(5, category + bump), subscores


def risk_band(
    pof: int, cof: int, params: RBIParameters = RBIParameters()
) -> tuple[int, str]:
    """5x5 matrix cell -> (score = PoF*CoF, band) with symmetric banding."""
    if not (1 <= pof <= 5 and 1 <= cof <= 5):
        raise ValueError(f"PoF and CoF categories must be 1-5, got ({pof}, {cof})")
    score = pof * cof
    low, med, med_high = params.risk_band_edges
    if score <= low:
        return score, "LOW"
    if score <= med:
        return score, "MEDIUM"
    if score <= med_high:
        return score, "MEDIUM-HIGH"
    return score, "HIGH"


def recommend_interval(
    band: str,
    inspection_plan: InspectionPlan,
    params: RBIParameters = RBIParameters(),
) -> tuple[float, str]:
    """Risk-scaled interval = min(half-life-rule interval, band cap).

    ``inspection_plan.next_inspection_interval_years`` already encodes the
    API 510/570/653 rule ``min(remaining_life / 2, code_max)`` — this only
    tightens it by the band cap, so the recommendation can never exceed the
    half-life interval or the code maximum.
    """
    if band not in params.band_interval_cap_yr:
        raise ValueError(f"unknown risk band {band!r}")
    cap = float(params.band_interval_cap_yr[band])
    base = inspection_plan.next_inspection_interval_years
    interval = min(base, cap)
    if inspection_plan.screening_status == "fail" and base <= 0.0:
        return 0.0, "FFS/half-life screening failed — inspect/repair now"
    if cap < base:
        return interval, f"risk-band cap governs ({band}: {cap:g} yr)"
    return interval, "API 510/570/653 half-life / code-max interval governs"


def screen_component(
    component_id: str,
    consequence_class: str | int,
    *,
    current_mm: float,
    required_mm: float,
    corrosion_rate_mm_yr: float,
    rsf: Optional[float] = None,
    rsf_a: Optional[float] = None,
    params: RBIParameters = RBIParameters(),
) -> RBIScreeningResult:
    """Full screening for one component from FFS-style condition data.

    Reuses :func:`inspection_planning.plan_inspection` for remaining life and
    the half-life interval, then layers PoF/CoF/risk-band/interval on top.
    """
    plan = plan_inspection(
        current_mm=current_mm,
        required_mm=required_mm,
        code_max_interval_years=params.code_max_interval_yr,
        corrosion_rate=corrosion_rate_mm_yr,
    )
    # None remaining life (zero corrosion rate) => effectively unbounded.
    remaining_life = plan.remaining_life_years
    life_for_pof = float("inf") if remaining_life is None else remaining_life

    pof, subscores = pof_category(
        rsf=rsf,
        rsf_a=rsf_a,
        remaining_life_yr=life_for_pof,
        corrosion_rate_mm_yr=corrosion_rate_mm_yr,
        params=params,
    )
    cof, cof_class = cof_category(consequence_class)
    score, band = risk_band(pof, cof, params)
    interval, governing = recommend_interval(band, plan, params)

    return RBIScreeningResult(
        component_id=component_id,
        pof_category=pof,
        cof_category=cof,
        cof_class=cof_class,
        risk_score=score,
        risk_band=band,
        recommended_interval_yr=interval,
        half_life_interval_yr=plan.half_life_interval_years,
        code_max_interval_yr=params.code_max_interval_yr,
        governing=governing,
        pof_subscores=subscores,
        inputs={
            "consequence_class": consequence_class,
            "current_mm": current_mm,
            "required_mm": required_mm,
            "corrosion_rate_mm_yr": corrosion_rate_mm_yr,
            "rsf": rsf,
            "rsf_a": rsf_a,
            "remaining_life_yr": remaining_life,
        },
        inspection_plan=plan,
        basis=params.basis_notes(),
    )


def screen_ffs_result(
    result: Any,
    consequence_class: str | int,
    *,
    corrosion_rate_in_per_yr: Optional[float] = None,
    params: RBIParameters = RBIParameters(),
) -> RBIScreeningResult:
    """Screen a :class:`~.assessment.ffs_coordinator.FFSAssessmentResult`.

    Uses the coordinator's inch-based fields (converted to mm). When the
    corrosion rate is not supplied it is back-calculated from the result's
    ``remaining_life_yr`` so that ``plan_inspection`` reproduces the FFS
    remaining life exactly (single source, no duplicated half-life logic).
    """
    mm = 25.4
    current_mm = float(result.t_measured_min_in) * mm
    required_mm = float(result.t_min_in) * mm
    allowance_mm = current_mm - required_mm

    if corrosion_rate_in_per_yr is not None:
        rate_mm_yr = float(corrosion_rate_in_per_yr) * mm
    else:
        life = float(result.remaining_life_yr)
        if allowance_mm > 0.0 and life > 0.0 and life != float("inf"):
            rate_mm_yr = allowance_mm / life
        else:
            rate_mm_yr = 0.0 if allowance_mm > 0.0 else 1e-9  # fail path if <= t_min

    return screen_component(
        component_id=str(result.component_id),
        consequence_class=consequence_class,
        current_mm=current_mm,
        required_mm=required_mm,
        corrosion_rate_mm_yr=rate_mm_yr,
        rsf=float(result.rsf),
        rsf_a=float(result.rsf_a),
        params=params,
    )


def rbi_rank_register(
    register_df: pd.DataFrame,
    *,
    cof_by_joint: Optional[Mapping[str, str | int]] = None,
    default_cof: str | int = "C",
    params: RBIParameters = RBIParameters(),
) -> pd.DataFrame:
    """Rank an inspection register (one row per scan) by RBI screening.

    Expects the GML results-register schema: ``component``, ``joint_id``,
    ``measured_avg_wt_mm``, ``min_required_wt_mm``,
    ``corrosion_rate_mm_per_year`` (``scan_location`` echoed when present).
    ``cof_by_joint`` maps joint_id -> consequence class (A-E or 1-5); joints
    not in the map fall back to ``default_cof``.

    Returns one row per input row with PoF/CoF/risk band/recommended
    interval, sorted most-critical first (risk score desc, remaining life
    asc). This is the "risk prioritization" phase-3 feed of the RBI cycle.
    """
    cof_by_joint = cof_by_joint or {}
    rows: list[dict[str, Any]] = []
    for _, rec in register_df.iterrows():
        joint = str(rec["joint_id"])
        res = screen_component(
            component_id=joint,
            consequence_class=cof_by_joint.get(joint, default_cof),
            current_mm=float(rec["measured_avg_wt_mm"]),
            required_mm=float(rec["min_required_wt_mm"]),
            corrosion_rate_mm_yr=float(rec["corrosion_rate_mm_per_year"]),
            params=params,
        )
        rows.append(
            {
                "component": rec.get("component"),
                "joint_id": joint,
                "scan_location": rec.get("scan_location"),
                "remaining_life_yr": res.inputs["remaining_life_yr"],
                "corrosion_rate_mm_yr": float(rec["corrosion_rate_mm_per_year"]),
                "pof_category": res.pof_category,
                "cof_category": res.cof_category,
                "cof_class": res.cof_class,
                "risk_score": res.risk_score,
                "risk_band": res.risk_band,
                "half_life_interval_yr": res.half_life_interval_yr,
                "recommended_interval_yr": res.recommended_interval_yr,
                "governing": res.governing,
            }
        )
    out = pd.DataFrame(rows)
    return out.sort_values(
        ["risk_score", "remaining_life_yr"],
        ascending=[False, True],
        kind="mergesort",
    ).reset_index(drop=True)


def summarize_ranking(ranked_df: pd.DataFrame) -> dict[str, Any]:
    """Counts per risk band + programme-level rollup (phase-3 output).

    Returns band counts (all four bands always present), PoF distribution,
    the fleet-minimum recommended interval, and the 7-phase framing so the
    summary drops straight into an RBI programme document.
    """
    band_counts = {band: 0 for band in RISK_BANDS}
    for band, n in ranked_df["risk_band"].value_counts().items():
        band_counts[str(band)] = int(n)
    pof_counts = {
        int(k): int(v)
        for k, v in sorted(ranked_df["pof_category"].value_counts().items())
    }
    return {
        "n_rows": int(len(ranked_df)),
        "band_counts": band_counts,
        "pof_counts": pof_counts,
        "min_recommended_interval_yr": float(
            ranked_df["recommended_interval_yr"].min()
        ),
        "max_risk_score": int(ranked_df["risk_score"].max()),
        "most_critical": ranked_df.iloc[0][["joint_id", "risk_band"]].to_dict(),
        "rbi_phases": list(RBI_PHASES),
        "phase": "risk prioritization",
    }
