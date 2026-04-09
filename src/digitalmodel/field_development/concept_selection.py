# ABOUTME: Host facility concept selection module — ranks TLP/Spar/Semi/FPSO options.
# ABOUTME: Issue #1843/#2053 — Concept Selection Framework with empirical benchmark weighting.
"""
digitalmodel.field_development.concept_selection
================================================

Ranks offshore host facility options (TLP, Spar, Semi-submersible, FPSO, Subsea Tieback)
based on water depth, reservoir size, distance to existing infrastructure, and fluid type.

GoM benchmark data from:
- subseaiq-scan-latest.md / .json (10 reference fields, host selection matrix)
- Water depth ranges: TLP 300-1800 m, Spar 1200-3000 m, Semi 600-2500 m, FPSO 600-3000 m
- Reservoir sizing thresholds: tieback <50 MMbbl, mini-host 50-200 MMbbl, full host >200 MMbbl
- Distance heuristic: tieback preferred <15 km, marginal 15-30 km, standalone >30 km

Usage
-----
>>> from digitalmodel.field_development.concept_selection import concept_selection
>>> result = concept_selection(
...     water_depth=900,
...     reservoir_size_mmbbl=200,
...     distance_to_infra_km=50,
...     fluid_type="oil",
... )
>>> print(result.selected_host)
HostType.TLP
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Optional


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------

class HostType(str, Enum):
    """Enumeration of offshore host facility types."""

    TLP = "TLP"
    SPAR = "Spar"
    SEMI = "Semi"
    FPSO = "FPSO"
    SUBSEA_TIEBACK = "Subsea_Tieback"


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class ConceptOption:
    """A single ranked host concept option with score and rationale.

    Attributes
    ----------
    host_type : HostType
        The facility type being evaluated.
    score : float
        Composite suitability score 0-100 (higher is better).
    capex_estimate_usd_bn : tuple[float, float]
        Indicative CAPEX range (low, high) in USD billions.
    rationale : str
        Human-readable explanation of score drivers.
    """

    host_type: HostType
    score: float
    capex_estimate_usd_bn: tuple[float, float]
    rationale: str


@dataclass
class ConceptSelectionResult:
    """Full concept selection output with ranked options.

    Attributes
    ----------
    ranked_options : list[ConceptOption]
        Host types ordered by score descending (best first).
    selected_host : HostType
        The top-ranked recommendation.
    summary : str
        One-paragraph plain-text summary for FDP reports.
    """

    ranked_options: list[ConceptOption]
    selected_host: HostType
    summary: str


# ---------------------------------------------------------------------------
# Internal constants (GoM benchmark data)
# ---------------------------------------------------------------------------

# Hard depth limits per host type (metres)
_DEPTH_LIMITS: dict[HostType, tuple[float, float]] = {
    HostType.TLP:            (300.0,   1800.0),
    HostType.SPAR:           (1200.0,  3000.0),
    HostType.SEMI:           (600.0,   2500.0),
    HostType.FPSO:           (600.0,   3000.0),
    HostType.SUBSEA_TIEBACK: (100.0,   3000.0),
}

# Indicative CAPEX ranges (low_bn, high_bn) from GoM benchmarks
_CAPEX_RANGES: dict[HostType, tuple[float, float]] = {
    HostType.TLP:            (2.0,  6.0),
    HostType.SPAR:           (3.0,  7.0),
    HostType.SEMI:           (4.0, 10.0),
    HostType.FPSO:           (5.0, 12.0),
    HostType.SUBSEA_TIEBACK: (0.2,  1.2),
}

# Reservoir size thresholds (MMbbl)
_TIEBACK_MAX_RESERVOIR = 50.0    # below this → tieback preferred
_TIEBACK_MARGINAL_RESERVOIR = 150.0
_STANDALONE_THRESHOLD = 200.0    # above this → full standalone host

# Distance thresholds (km)
_TIEBACK_PREFERRED_KM = 15.0
_TIEBACK_MARGINAL_KM = 30.0
_TIEBACK_MAX_VIABLE_KM = 60.0

# Valid fluid types
_VALID_FLUID_TYPES = {"oil", "gas", "condensate"}


# ---------------------------------------------------------------------------
# Scoring helpers
# ---------------------------------------------------------------------------

def _depth_score(host: HostType, depth: float) -> float:
    """Return 0-100 depth suitability score. 0 if outside hard limits."""
    lo, hi = _DEPTH_LIMITS[host]
    if depth < lo or depth > hi:
        return 0.0

    # Optimal depth band centres (based on GoM analogues)
    _OPTIMAL: dict[HostType, tuple[float, float]] = {
        HostType.TLP:            (500.0,  1600.0),
        HostType.SPAR:           (1500.0, 2500.0),
        HostType.SEMI:           (1000.0, 2200.0),
        HostType.FPSO:           (800.0,  2500.0),
        HostType.SUBSEA_TIEBACK: (100.0,  2000.0),
    }
    opt_lo, opt_hi = _OPTIMAL[host]
    if opt_lo <= depth <= opt_hi:
        return 100.0
    # Linear ramp from edge to optimal
    if depth < opt_lo:
        return 60.0 + 40.0 * (depth - lo) / max(opt_lo - lo, 1.0)
    # depth > opt_hi
    return 60.0 + 40.0 * (hi - depth) / max(hi - opt_hi, 1.0)


def _reservoir_score(host: HostType, reservoir_mmbbl: float, distance_km: Optional[float]) -> float:
    """Score 0-100 based on reservoir size fit for host type."""
    if host is HostType.SUBSEA_TIEBACK:
        if reservoir_mmbbl <= _TIEBACK_MAX_RESERVOIR:
            return 100.0
        if reservoir_mmbbl <= _TIEBACK_MARGINAL_RESERVOIR:
            # linearly reduce from 100→50
            frac = (reservoir_mmbbl - _TIEBACK_MAX_RESERVOIR) / (
                _TIEBACK_MARGINAL_RESERVOIR - _TIEBACK_MAX_RESERVOIR
            )
            return 100.0 - 50.0 * frac
        # Large reservoir — tieback technically possible but suboptimal
        return max(5.0, 50.0 - (reservoir_mmbbl - _TIEBACK_MARGINAL_RESERVOIR) / 10.0)
    else:
        # Standalone hosts: penalise small reservoirs (economics marginal)
        if reservoir_mmbbl < 50.0:
            return 30.0
        if reservoir_mmbbl < _STANDALONE_THRESHOLD:
            return 60.0 + 40.0 * (reservoir_mmbbl - 50.0) / (_STANDALONE_THRESHOLD - 50.0)
        return 100.0


def _distance_score(host: HostType, distance_km: Optional[float]) -> float:
    """Score 0-100 based on distance to nearest existing infrastructure."""
    if host is HostType.SUBSEA_TIEBACK:
        if distance_km is None:
            return 0.0
        if distance_km <= _TIEBACK_PREFERRED_KM:
            return 100.0
        if distance_km <= _TIEBACK_MARGINAL_KM:
            # 100→60 linearly
            frac = (distance_km - _TIEBACK_PREFERRED_KM) / (
                _TIEBACK_MARGINAL_KM - _TIEBACK_PREFERRED_KM
            )
            return 100.0 - 40.0 * frac
        if distance_km <= _TIEBACK_MAX_VIABLE_KM:
            frac = (distance_km - _TIEBACK_MARGINAL_KM) / (
                _TIEBACK_MAX_VIABLE_KM - _TIEBACK_MARGINAL_KM
            )
            return 60.0 - 55.0 * frac
        return max(0.0, 5.0 - (distance_km - _TIEBACK_MAX_VIABLE_KM) * 0.1)
    else:
        # Standalone hosts: close infra is nice (topsides synergy) but not critical
        if distance_km is None:
            return 70.0  # neutral — standalone doesn't depend on nearby infra
        if distance_km < 10.0:
            return 60.0  # slightly penalised vs tieback if very close
        return 80.0


def _fluid_score(host: HostType, fluid_type: str) -> float:
    """Score 0-100 based on fluid type suitability."""
    if fluid_type in ("gas", "condensate"):
        # FPSO and Semi have superior gas processing / storage
        weights = {
            HostType.FPSO:           95.0,
            HostType.SEMI:           85.0,
            HostType.SPAR:           75.0,
            HostType.TLP:            65.0,
            HostType.SUBSEA_TIEBACK: 70.0,
        }
    else:  # oil
        weights = {
            HostType.TLP:            90.0,
            HostType.SEMI:           85.0,
            HostType.SPAR:           85.0,
            HostType.FPSO:           80.0,
            HostType.SUBSEA_TIEBACK: 80.0,
        }
    return weights[host]


def _composite_score(
    host: HostType,
    depth: float,
    reservoir_mmbbl: float,
    distance_km: Optional[float],
    fluid_type: str,
) -> float:
    """Weighted composite score 0-100."""
    depth_s = _depth_score(host, depth)
    if depth_s == 0.0:
        return 0.0  # outside hard depth limit → immediately disqualified

    reservoir_s = _reservoir_score(host, reservoir_mmbbl, distance_km)
    distance_s = _distance_score(host, distance_km)
    fluid_s = _fluid_score(host, fluid_type)

    # Weights: depth is most critical, then reservoir, distance, fluid
    return (
        0.40 * depth_s
        + 0.30 * reservoir_s
        + 0.20 * distance_s
        + 0.10 * fluid_s
    )


def _build_rationale(
    host: HostType,
    score: float,
    depth: float,
    reservoir_mmbbl: float,
    distance_km: Optional[float],
    fluid_type: str,
) -> str:
    """Construct a short human-readable rationale string."""
    depth_s = _depth_score(host, depth)
    res_s = _reservoir_score(host, reservoir_mmbbl, distance_km)
    dist_s = _distance_score(host, distance_km)
    flu_s = _fluid_score(host, fluid_type)
    dist_str = f"{distance_km:.0f} km" if distance_km is not None else "unknown"
    return (
        f"{host.value} score={score:.1f}: "
        f"depth={depth:.0f}m (score {depth_s:.0f}), "
        f"reservoir={reservoir_mmbbl:.0f} MMbbl (score {res_s:.0f}), "
        f"distance={dist_str} (score {dist_s:.0f}), "
        f"fluid={fluid_type} (score {flu_s:.0f})."
    )


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def concept_selection(
    water_depth: float,
    reservoir_size_mmbbl: float,
    distance_to_infra_km: Optional[float],
    fluid_type: str,
    empirical_weights: Optional[dict[str, float]] = None,
) -> ConceptSelectionResult:
    """Rank offshore host facility options for a deepwater field development.

    Uses GoM benchmarks (Perdido, Mars, Atlantis, Thunder Horse, Appomattox,
    Whale, Mad Dog, Stones, Lucius, Ursa) to score TLP, Spar, Semi,
    FPSO, and Subsea Tieback options.

    Parameters
    ----------
    water_depth : float
        Field water depth in metres (must be > 0).
    reservoir_size_mmbbl : float
        Estimated recoverable reserves in million barrels (must be > 0).
    distance_to_infra_km : float or None
        Distance to the nearest existing host facility or pipeline in km.
        Pass None if no infrastructure exists in the region.
    fluid_type : str
        Primary fluid type: 'oil', 'gas', or 'condensate'.
    empirical_weights : dict[str, float], optional
        Empirical probability weights from SubseaIQ benchmark data,
        keyed by concept type name (e.g. ``{"TLP": 0.45, "Semi": 0.30}``).
        When provided, these are blended into the composite score as
        an additional factor (10% weight).  Generated by
        :func:`benchmarks.concept_probability_matrix` or
        :func:`benchmarks.predict_concept_type`.

    Returns
    -------
    ConceptSelectionResult
        Ranked options (highest score first), selected host, and summary text.

    Raises
    ------
    ValueError
        If water_depth <= 0, reservoir_size_mmbbl <= 0,
        distance_to_infra_km < 0, or fluid_type is unrecognised.

    Examples
    --------
    >>> result = concept_selection(896, 100, 50, "oil")
    >>> result.selected_host.value
    'TLP'
    """
    # --- Validation ---
    if water_depth <= 0:
        raise ValueError(
            f"water_depth must be positive, got {water_depth!r}. "
            "Provide water depth in metres (e.g. 900 for 900 m)."
        )
    if reservoir_size_mmbbl <= 0:
        raise ValueError(
            f"reservoir_size_mmbbl must be positive, got {reservoir_size_mmbbl!r}. "
            "Provide recoverable reserves in million barrels."
        )
    if distance_to_infra_km is not None and distance_to_infra_km < 0:
        raise ValueError(
            f"distance_to_infra_km must be >= 0 or None, got {distance_to_infra_km!r}."
        )
    fluid_lower = fluid_type.lower() if isinstance(fluid_type, str) else ""
    if fluid_lower not in _VALID_FLUID_TYPES:
        raise ValueError(
            f"fluid_type must be one of {sorted(_VALID_FLUID_TYPES)}, got {fluid_type!r}."
        )

    # --- Score all host types ---
    options: list[ConceptOption] = []
    for host in HostType:
        score = _composite_score(
            host=host,
            depth=water_depth,
            reservoir_mmbbl=reservoir_size_mmbbl,
            distance_km=distance_to_infra_km,
            fluid_type=fluid_lower,
        )

        # Blend empirical weights if provided (#2053)
        if empirical_weights is not None and score > 0.0:
            empirical_prob = empirical_weights.get(host.value, 0.0)
            # Scale probability (0-1) to score space (0-100)
            empirical_score = empirical_prob * 100.0
            # Rebalance: 90% deterministic + 10% empirical
            score = 0.90 * score + 0.10 * empirical_score

        rationale = _build_rationale(
            host=host,
            score=score,
            depth=water_depth,
            reservoir_mmbbl=reservoir_size_mmbbl,
            distance_km=distance_to_infra_km,
            fluid_type=fluid_lower,
        )
        options.append(
            ConceptOption(
                host_type=host,
                score=round(score, 2),
                capex_estimate_usd_bn=_CAPEX_RANGES[host],
                rationale=rationale,
            )
        )

    # Sort descending by score
    options.sort(key=lambda o: o.score, reverse=True)

    selected = options[0].host_type
    dist_str = f"{distance_to_infra_km:.0f} km" if distance_to_infra_km is not None else "no nearby infrastructure"
    empirical_note = " Empirical SubseaIQ weights applied." if empirical_weights else ""
    summary = (
        f"Concept Selection Result for {water_depth:.0f} m water depth, "
        f"{reservoir_size_mmbbl:.0f} MMbbl {fluid_lower} field, "
        f"{dist_str} from nearest infrastructure. "
        f"Recommended host: {selected.value} "
        f"(composite score {options[0].score:.1f}/100). "
        f"GoM analogues considered: Perdido (Spar, 2438 m), "
        f"Mars (TLP, 896 m), Atlantis (Semi, 2150 m), "
        f"Thunder Horse (Semi, 1844 m), Stones (ETLP, 2900 m)."
        f"{empirical_note}"
    )

    return ConceptSelectionResult(
        ranked_options=options,
        selected_host=selected,
        summary=summary,
    )


def concept_selection_with_benchmarks(
    water_depth: float,
    reservoir_size_mmbbl: float,
    distance_to_infra_km: Optional[float],
    fluid_type: str,
    benchmark_projects: list,
) -> ConceptSelectionResult:
    """Concept selection with empirical weighting from SubseaIQ benchmarks.

    Convenience wrapper that derives empirical probability weights from
    benchmark data and feeds them into :func:`concept_selection`.

    Parameters
    ----------
    water_depth : float
        Field water depth in metres (must be > 0).
    reservoir_size_mmbbl : float
        Estimated recoverable reserves in MMbbl (must be > 0).
    distance_to_infra_km : float or None
        Distance to nearest infrastructure in km, or None.
    fluid_type : str
        Primary fluid: 'oil', 'gas', or 'condensate'.
    benchmark_projects : list
        List of :class:`benchmarks.SubseaProject` objects from SubseaIQ data.

    Returns
    -------
    ConceptSelectionResult
        Same as :func:`concept_selection` but with empirical weights applied.
    """
    from .benchmarks import predict_concept_type

    prediction = predict_concept_type(
        projects=benchmark_projects,
        water_depth=water_depth,
        reservoir_size_mmbbl=reservoir_size_mmbbl,
        distance_to_infra_km=distance_to_infra_km,
    )

    # Map benchmark concept names to HostType values for compatibility
    # SubseaIQ uses "Subsea Tieback" but HostType uses "Subsea_Tieback"
    mapped_weights: dict[str, float] = {}
    _NAME_MAP = {
        "Subsea Tieback": "Subsea_Tieback",
        "Semi-submersible": "Semi",
        "Fixed Platform": "TLP",  # closest standalone analogue
    }
    for concept, prob in prediction.probabilities.items():
        mapped_name = _NAME_MAP.get(concept, concept)
        mapped_weights[mapped_name] = mapped_weights.get(mapped_name, 0.0) + prob

    return concept_selection(
        water_depth=water_depth,
        reservoir_size_mmbbl=reservoir_size_mmbbl,
        distance_to_infra_km=distance_to_infra_km,
        fluid_type=fluid_type,
        empirical_weights=mapped_weights,
    )
