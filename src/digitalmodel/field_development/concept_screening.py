# ABOUTME: W6 concept screening — four parameter-based axes over field_development concepts.
# ABOUTME: Extends concept_selection; rule/parameter-based, no reservoir/flow/structural sim.
"""
digitalmodel.field_development.concept_screening
================================================

Four-axis concept screening for an offshore prospect. EXTENDS the existing
:mod:`concept_selection` / :mod:`capex_estimator` / :mod:`timeline` framework —
it does not replace it. The existing composite *suitability* score is reused as
a hard-gate prior; this module adds four decision axes the ranker did not compute:

  1. **schedule**     — concept-to-first-oil duration in weeks
  2. **rig_days**     — development drilling+completion rig-days demanded
  3. **capex**        — driver-level CAPEX line-item breakdown (USD bn)
  4. **intervention** — lifecycle intervention index (access-factor x workover freq)

Scope honesty
-------------
Rule/parameter-based screening only. NO reservoir, flow-assurance, or structural
simulation. Production rate and well count are **inputs**, not solved. Dry-tree-unit
(FrPS-class) stroke/buckling limits are screening *flags*, never solved. CAPEX is
driver-level, not an audited cashflow. Rig demand is in **days**, never $/day
(live rig day-rates are a paid feed — deliberately omitted; flag, don't fake).

All numeric parameters come from :mod:`data/concept_params.yml` (generic GoM
defaults). A private client-calibrated override file may be injected at runtime via
``params_path`` — that override stays OUT of this public repo.

Usage
-----
>>> from digitalmodel.field_development.concept_screening import (
...     ProspectSpec, screen_concepts)
>>> spec = ProspectSpec(name="Demo", water_depth_m=1500, well_count=8,
...     reservoir_size_mmbbl=300, production_capacity_bopd=120_000,
...     fluid_type="oil", distance_to_infra_km=80)
>>> result = screen_concepts(spec)
>>> result.ranked[0].host_type  # doctest: +SKIP
<HostType.TLP: 'TLP'>
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Optional

import yaml

from .capex_estimator import estimate_capex
from .concept_selection import (
    HostType,
    _composite_score,
)

# ---------------------------------------------------------------------------
# Defaults
# ---------------------------------------------------------------------------

# Decision-axis weights (sum to 1.0). Lower-cost/shorter/lower-intervention is
# better; every axis is normalised so "higher normalised value = better" before
# the weighted sum. From the W6 spec.
DEFAULT_WEIGHTS: dict[str, float] = {
    "capex": 0.35,
    "schedule": 0.25,
    "rig_days": 0.15,
    "intervention": 0.25,
}

# Blend of decision-axes vs the existing suitability prior.
_AXES_BLEND = 0.85
_SUITABILITY_BLEND = 0.15
# A concept that the suitability prior hard-gates to 0 (outside depth limits,
# wrong reservoir fit) is penalised, not silently dropped, so the note shows why.
_GATED_PENALTY = 0.30

_DATA_DIR = Path(__file__).resolve().parent / "data"
_DEFAULT_PARAMS_PATH = _DATA_DIR / "concept_params.yml"

# Default candidate set screened when the caller does not restrict it.
DEFAULT_CANDIDATES: tuple[HostType, ...] = tuple(HostType)


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class ProspectSpec:
    """Neutral prospect description — every field is an INPUT, nothing is solved."""

    name: str
    water_depth_m: float
    well_count: int
    reservoir_size_mmbbl: float
    production_capacity_bopd: float
    fluid_type: str = "oil"
    distance_to_infra_km: Optional[float] = None
    # Decade workover frequency per well. None -> use the params-file default.
    workover_frequency_per_well_decade: Optional[float] = None

    def __post_init__(self) -> None:
        if self.water_depth_m <= 0:
            raise ValueError(f"water_depth_m must be > 0, got {self.water_depth_m!r}")
        if self.well_count <= 0:
            raise ValueError(f"well_count must be > 0, got {self.well_count!r}")
        if self.reservoir_size_mmbbl <= 0:
            raise ValueError(
                f"reservoir_size_mmbbl must be > 0, got {self.reservoir_size_mmbbl!r}"
            )
        if self.production_capacity_bopd <= 0:
            raise ValueError(
                f"production_capacity_bopd must be > 0, got "
                f"{self.production_capacity_bopd!r}"
            )


@dataclass
class AxisScores:
    """Raw (engineering-unit) and normalised (0-100, higher=better) axis values."""

    schedule_weeks: float
    rig_days: float
    capex_base_usd_bn: float
    intervention_index: float
    # normalised 0-100, higher is better
    norm: dict[str, float] = field(default_factory=dict)
    capex_line_items_usd_bn: dict[str, float] = field(default_factory=dict)


@dataclass
class ConceptScreenResult:
    """One screened concept."""

    host_type: HostType
    suitability: float          # existing composite suitability prior (0-100)
    gated: bool                 # True if suitability hard-gated to 0
    axes: AxisScores
    weighted_axis_score: float  # 0-100 weighted sum of normalised axes
    composite: float            # final blended + gate-penalised score (0-100)


@dataclass
class ScreeningResult:
    """Full screening output for a prospect."""

    prospect: ProspectSpec
    weights: dict[str, float]
    ranked: list[ConceptScreenResult]
    governing_axis: str
    params_basis: str
    rig_day_source: str

    @property
    def selected(self) -> HostType:
        return self.ranked[0].host_type


# ---------------------------------------------------------------------------
# Parameter loading
# ---------------------------------------------------------------------------

def load_params(params_path: Optional[str | Path] = None) -> dict[str, Any]:
    """Load the concept-screening parameter table.

    Defaults to the generic GoM ``data/concept_params.yml`` shipped with this
    public package. ``params_path`` lets a PRIVATE overlay (e.g. client calibration)
    be injected at runtime — that file stays out of this repo.
    """
    path = Path(params_path) if params_path else _DEFAULT_PARAMS_PATH
    with open(path, "r", encoding="utf-8") as fh:
        return yaml.safe_load(fh)


def _host_key(host: HostType) -> str:
    """Params-file key for a host type (matches HostType.value)."""
    return host.value


# ---------------------------------------------------------------------------
# Axis estimators (each rule/parameter-based off existing estimators or params)
# ---------------------------------------------------------------------------

def estimate_schedule_weeks(host: HostType, spec: ProspectSpec, params: dict) -> float:
    """Concept-to-first-oil weeks = base_phase_weeks * mult + wells * per_well_weeks.

    Phase basis anchored on timeline.py P50 concept->first-oil durations.
    """
    sched = params["schedule"]
    mult = sched["concept_schedule_mult"][_host_key(host)]
    base = float(sched["base_phase_weeks"]) * float(mult)
    return round(base + spec.well_count * float(sched["per_well_weeks"]), 1)


def estimate_rig_days(
    host: HostType,
    spec: ProspectSpec,
    params: dict,
    bsee_benchmark: Optional[float] = None,
) -> float:
    """Development rig-days = wells * per_well_rig_days * concept_rig_factor.

    ``bsee_benchmark`` (per-well rig-days from worldenergydata's BSEE
    ``well_rig_days`` analytic) overrides the generic default when supplied —
    optional and default-off to avoid a hard cross-repo dependency.
    """
    rd = params["rig_days"]
    per_well = float(bsee_benchmark) if bsee_benchmark else float(rd["per_well_rig_days"])
    factor = float(rd["concept_rig_factor"][_host_key(host)])
    return round(spec.well_count * per_well * factor, 1)


def capex_line_items(host: HostType, spec: ProspectSpec, params: dict) -> dict[str, float]:
    """Driver-level CAPEX breakdown (USD bn) off the existing estimate_capex base.

    The base P50 CAPEX comes from the GoM-benchmarked :func:`estimate_capex`
    (reused, not duplicated); the params file only supplies the line-item split
    and the eng/PM + contingency uplifts.
    """
    cap = params["capex"]
    # tieback needs a distance; default to spec distance or a nominal 15 km.
    tieback_km = None
    if host is HostType.SUBSEA_TIEBACK:
        tieback_km = spec.distance_to_infra_km if spec.distance_to_infra_km is not None else 15.0
    est = estimate_capex(
        host_type=host,
        production_capacity_bopd=spec.production_capacity_bopd,
        water_depth=spec.water_depth_m,
        tieback_distance_km=tieback_km,
    )
    base = est.base_usd_bn
    fractions = cap["line_item_fractions"][_host_key(host)]
    items = {k: round(base * float(v), 4) for k, v in fractions.items()}
    subtotal = round(sum(items.values()), 4)
    eng_pm = round(subtotal * float(cap["eng_pm_uplift"]), 4)
    contingency = round((subtotal + eng_pm) * float(cap["contingency_uplift"]), 4)
    items["eng_pm"] = eng_pm
    items["contingency"] = contingency
    items["total"] = round(subtotal + eng_pm + contingency, 4)
    return items


def intervention_index(host: HostType, spec: ProspectSpec, params: dict) -> float:
    """Lifecycle intervention index = access_factor_prior * workover_frequency.

    Lower is better. The access factor encodes the dry-tree-continuous-access vs
    subsea-mobilisation-gated thesis. For DRY_TREE_UNIT the access factor is an
    ENGINEERING ASSUMPTION (no direct operating analog) — flagged in the params.
    """
    iv = params["intervention"]
    access = float(iv["access_factor_prior"][_host_key(host)])
    wo = spec.workover_frequency_per_well_decade
    if wo is None:
        wo = float(iv["default_workover_frequency_per_well_decade"])
    return round(access * float(wo) * spec.well_count, 3)


# ---------------------------------------------------------------------------
# Normalisation + scoring
# ---------------------------------------------------------------------------

def _normalise_lower_is_better(value: float, lo: float, hi: float) -> float:
    """Map [lo, hi] -> [100, 0] (lowest value scores best). Clamped to 0-100."""
    if hi <= lo:
        return 100.0
    frac = (value - lo) / (hi - lo)
    return round(max(0.0, min(100.0, 100.0 * (1.0 - frac))), 2)


def _suitability(host: HostType, spec: ProspectSpec) -> float:
    """Reuse the existing concept_selection composite suitability score (0-100)."""
    return _composite_score(
        host=host,
        depth=spec.water_depth_m,
        reservoir_mmbbl=spec.reservoir_size_mmbbl,
        distance_km=spec.distance_to_infra_km,
        fluid_type=spec.fluid_type.lower(),
    )


def screen_concepts(
    spec: ProspectSpec,
    weights: Optional[dict[str, float]] = None,
    params_path: Optional[str | Path] = None,
    candidates: Optional[list[HostType]] = None,
    bsee_benchmark: Optional[float] = None,
) -> ScreeningResult:
    """Screen candidate concepts for a prospect across four parameter-based axes.

    Parameters
    ----------
    spec : ProspectSpec
        Neutral prospect description (all inputs).
    weights : dict, optional
        Decision-axis weights; defaults to :data:`DEFAULT_WEIGHTS`.
    params_path : str or Path, optional
        Override parameter file (private client overlay). Defaults to the generic
        ``data/concept_params.yml``.
    candidates : list[HostType], optional
        Subset of host types to screen; defaults to all.
    bsee_benchmark : float, optional
        Per-well rig-days from a real BSEE benchmark (worldenergydata). Optional.

    Returns
    -------
    ScreeningResult
        Concepts ranked by composite score (best first), with the governing axis.
    """
    weights = {**DEFAULT_WEIGHTS, **(weights or {})}
    total_w = sum(weights.values()) or 1.0
    params = load_params(params_path)
    hosts = list(candidates) if candidates else list(DEFAULT_CANDIDATES)

    # First pass: raw axis values per host.
    raw: dict[HostType, AxisScores] = {}
    suit: dict[HostType, float] = {}
    for host in hosts:
        items = capex_line_items(host, spec, params)
        raw[host] = AxisScores(
            schedule_weeks=estimate_schedule_weeks(host, spec, params),
            rig_days=estimate_rig_days(host, spec, params, bsee_benchmark),
            capex_base_usd_bn=items["total"],
            intervention_index=intervention_index(host, spec, params),
            capex_line_items_usd_bn=items,
        )
        suit[host] = round(_suitability(host, spec), 2)

    # Normalise each axis across the candidate set (lower is better -> higher norm).
    def _span(attr: str) -> tuple[float, float]:
        vals = [getattr(raw[h], attr) for h in hosts]
        return min(vals), max(vals)

    sched_lo, sched_hi = _span("schedule_weeks")
    rig_lo, rig_hi = _span("rig_days")
    cap_lo, cap_hi = _span("capex_base_usd_bn")
    iv_lo, iv_hi = _span("intervention_index")

    results: list[ConceptScreenResult] = []
    for host in hosts:
        a = raw[host]
        a.norm = {
            "schedule": _normalise_lower_is_better(a.schedule_weeks, sched_lo, sched_hi),
            "rig_days": _normalise_lower_is_better(a.rig_days, rig_lo, rig_hi),
            "capex": _normalise_lower_is_better(a.capex_base_usd_bn, cap_lo, cap_hi),
            "intervention": _normalise_lower_is_better(
                a.intervention_index, iv_lo, iv_hi
            ),
        }
        weighted = sum(weights.get(k, 0.0) * a.norm[k] for k in a.norm) / total_w
        gated = suit[host] <= 0.0
        composite = _AXES_BLEND * weighted + _SUITABILITY_BLEND * suit[host]
        if gated:
            composite *= _GATED_PENALTY
        results.append(
            ConceptScreenResult(
                host_type=host,
                suitability=suit[host],
                gated=gated,
                axes=a,
                weighted_axis_score=round(weighted, 2),
                composite=round(composite, 2),
            )
        )

    # Deterministic ordering: composite desc, then host.value asc as tiebreak.
    results.sort(key=lambda r: (-r.composite, r.host_type.value))

    governing = _governing_axis(results, weights)
    return ScreeningResult(
        prospect=spec,
        weights=weights,
        ranked=results,
        governing_axis=governing,
        params_basis=params.get("meta", {}).get("basis", "generic_gom"),
        rig_day_source=(
            "bsee_benchmark (worldenergydata)" if bsee_benchmark else "generic GoM default"
        ),
    )


def _governing_axis(results: list[ConceptScreenResult], weights: dict[str, float]) -> str:
    """Axis that most separates the top-two ranked concepts (the trade-off driver)."""
    if len(results) < 2:
        return "n/a (single candidate)"
    top, second = results[0], results[1]
    best_axis, best_delta = "n/a", -1.0
    for axis in weights:
        delta = weights.get(axis, 0.0) * abs(
            top.axes.norm.get(axis, 0.0) - second.axes.norm.get(axis, 0.0)
        )
        if delta > best_delta:
            best_axis, best_delta = axis, delta
    return best_axis
