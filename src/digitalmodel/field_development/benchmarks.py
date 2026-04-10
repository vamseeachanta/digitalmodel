# ABOUTME: SubseaIQ benchmark bridge — concept-selection bands, probability matrix, and decision tree.
# ABOUTME: Issue #1861/#2053 — loads normalized project records and derives benchmark aggregations.
"""
digitalmodel.field_development.benchmarks
=========================================

Loads normalized SubseaIQ project records and derives:
- concept-selection benchmark bands (concept type counts by water depth)
- subsea architecture statistics (tieback distance, equipment counts)
- concept probability matrix (concept type probabilities by depth band)
- decision tree predictor (predicted concept given field parameters)

This is the *bridge layer* between raw SubseaIQ scraped data and the
field_development analysis modules (concept_selection, capex_estimator).

Usage
-----
>>> from digitalmodel.field_development.benchmarks import (
...     load_projects, concept_benchmark_bands, subsea_architecture_stats,
...     concept_probability_matrix, predict_concept_type,
... )
>>> projects = load_projects(raw_records)
>>> bands = concept_benchmark_bands(projects)
>>> stats = subsea_architecture_stats(projects)
>>> matrix = concept_probability_matrix(projects)
>>> prediction = predict_concept_type(projects, water_depth=1000,
...     reservoir_size_mmbbl=200, distance_to_infra_km=50)
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

DEPTH_BANDS: tuple[str, ...] = (
    "0-300m",       # Shallow
    "300-800m",     # Mid-water
    "800-1500m",    # Deepwater
    "1500m+",       # Ultra-deepwater
)

_DEPTH_THRESHOLDS: tuple[tuple[float, float], ...] = (
    (0.0, 300.0),
    (300.0, 800.0),
    (800.0, 1500.0),
    (1500.0, float("inf")),
)


# ---------------------------------------------------------------------------
# Data class
# ---------------------------------------------------------------------------

@dataclass
class SubseaProject:
    """A normalized SubseaIQ project record.

    All fields except ``name`` are optional to handle incomplete scraped data.
    Timeline fields (``year_concept``, ``year_feed``, ``year_fid``,
    ``year_first_oil``) support schedule benchmark analysis in
    :mod:`digitalmodel.field_development.timeline`.
    """

    name: str
    operator: Optional[str] = None
    water_depth_m: Optional[float] = None
    concept_type: Optional[str] = None
    tieback_distance_km: Optional[float] = None
    num_wells: Optional[int] = None
    num_trees: Optional[int] = None
    num_manifolds: Optional[int] = None
    fluid_type: Optional[str] = None
    region: Optional[str] = None
    flowline_diameter_in: Optional[float] = None
    flowline_material: Optional[str] = None
    layout_type: Optional[str] = None
    year_concept: Optional[int] = None
    year_feed: Optional[int] = None
    year_fid: Optional[int] = None
    year_first_oil: Optional[int] = None


@dataclass
class ConceptPrediction:
    """Result of the empirical concept-type decision tree.

    Attributes
    ----------
    predicted_concept : str
        The concept type with the highest adjusted probability.
    probabilities : dict[str, float]
        All concept types and their adjusted probabilities (sum to 1.0).
    depth_band : str
        The water-depth band used for the prediction.
    rationale : str
        Human-readable explanation of the prediction drivers.
    """

    predicted_concept: str
    probabilities: dict[str, float]
    depth_band: str
    rationale: str


# ---------------------------------------------------------------------------
# Loading
# ---------------------------------------------------------------------------

def load_projects(records: list[dict]) -> list[SubseaProject]:
    """Parse raw SubseaIQ dicts into a list of :class:`SubseaProject`.

    Parameters
    ----------
    records : list[dict]
        Raw project dicts.  Each must contain at least a ``"name"`` key.
        All other fields are optional and will default to ``None``.

    Returns
    -------
    list[SubseaProject]

    Raises
    ------
    KeyError
        If a record is missing the ``"name"`` key.
    """
    projects: list[SubseaProject] = []
    for rec in records:
        if "name" not in rec:
            raise KeyError(
                f"Record missing required 'name' key: {rec!r}"
            )
        projects.append(
            SubseaProject(
                name=str(rec["name"]),
                operator=rec.get("operator"),
                water_depth_m=_opt_float(rec.get("water_depth_m")),
                concept_type=rec.get("concept_type"),
                tieback_distance_km=_opt_float(rec.get("tieback_distance_km")),
                num_wells=_opt_int(rec.get("num_wells")),
                num_trees=_opt_int(rec.get("num_trees")),
                num_manifolds=_opt_int(rec.get("num_manifolds")),
                fluid_type=rec.get("fluid_type"),
                region=rec.get("region"),
                flowline_diameter_in=_opt_float(rec.get("flowline_diameter_in")),
                flowline_material=rec.get("flowline_material"),
                layout_type=rec.get("layout_type"),
                year_concept=_opt_int(rec.get("year_concept")),
                year_feed=_opt_int(rec.get("year_feed")),
                year_fid=_opt_int(rec.get("year_fid")),
                year_first_oil=_opt_int(rec.get("year_first_oil")),
            )
        )
    return projects


# ---------------------------------------------------------------------------
# Concept benchmark bands
# ---------------------------------------------------------------------------

def concept_benchmark_bands(
    projects: list[SubseaProject],
) -> dict[str, dict[str, int]]:
    """Aggregate concept type counts by water-depth band.

    Projects missing ``water_depth_m`` or ``concept_type`` are excluded.

    Parameters
    ----------
    projects : list[SubseaProject]

    Returns
    -------
    dict[str, dict[str, int]]
        Outer key is the depth band label (e.g. ``"800-1500m"``).
        Inner dict maps concept type → count.
    """
    result: dict[str, dict[str, int]] = {band: {} for band in DEPTH_BANDS}

    for p in projects:
        if p.water_depth_m is None or p.concept_type is None:
            continue
        band_label = _classify_depth(p.water_depth_m)
        if band_label is None:
            continue
        bucket = result[band_label]
        bucket[p.concept_type] = bucket.get(p.concept_type, 0) + 1

    return result


# ---------------------------------------------------------------------------
# Subsea architecture stats
# ---------------------------------------------------------------------------

def subsea_architecture_stats(
    projects: list[SubseaProject],
) -> dict[str, dict[str, float]]:
    """Compute summary statistics for subsea architecture fields.

    Returns statistics for tieback distance, trees per project,
    manifolds per project, and trees-per-manifold ratio.

    Parameters
    ----------
    projects : list[SubseaProject]

    Returns
    -------
    dict[str, dict[str, float]]
        Each key is a metric name.  The value dict contains:
        ``count``, ``min``, ``max``, ``mean``.
    """
    tieback_vals = [
        p.tieback_distance_km
        for p in projects
        if p.tieback_distance_km is not None
    ]
    trees_vals = [p.num_trees for p in projects if p.num_trees is not None]
    manifold_vals = [
        p.num_manifolds for p in projects if p.num_manifolds is not None
    ]
    trees_per_mani = [
        p.num_trees / p.num_manifolds
        for p in projects
        if p.num_trees is not None
        and p.num_manifolds is not None
        and p.num_manifolds > 0
    ]

    return {
        "tieback_distance": _describe(tieback_vals),
        "trees_per_project": _describe(trees_vals),
        "manifolds_per_project": _describe(manifold_vals),
        "trees_per_manifold": _describe(trees_per_mani),
    }


# ---------------------------------------------------------------------------
# Concept probability matrix (#2053)
# ---------------------------------------------------------------------------

def concept_probability_matrix(
    projects: list[SubseaProject],
) -> dict[str, dict[str, float]]:
    """Compute concept-type probability distributions per water-depth band.

    Converts the raw counts from :func:`concept_benchmark_bands` into
    normalised probabilities (0.0–1.0) that sum to 1.0 within each band.

    Parameters
    ----------
    projects : list[SubseaProject]

    Returns
    -------
    dict[str, dict[str, float]]
        Outer key is the depth band label (e.g. ``"800-1500m"``).
        Inner dict maps concept type to probability (0.0–1.0).
        Empty bands return an empty inner dict.

    Examples
    --------
    >>> matrix = concept_probability_matrix(projects)
    >>> matrix["800-1500m"]
    {'TLP': 0.45, 'Semi': 0.30, 'Spar': 0.15, 'Subsea Tieback': 0.10}
    """
    bands = concept_benchmark_bands(projects)
    result: dict[str, dict[str, float]] = {}

    for band_label, counts in bands.items():
        total = sum(counts.values())
        if total == 0:
            result[band_label] = {}
        else:
            result[band_label] = {
                concept: count / total
                for concept, count in counts.items()
            }

    return result


# ---------------------------------------------------------------------------
# Decision tree predictor (#2053)
# ---------------------------------------------------------------------------

# Tieback decision thresholds (consistent with concept_selection.py)
_TIEBACK_RESERVOIR_THRESHOLD = 50.0   # MMbbl — below this, tieback preferred
_TIEBACK_DISTANCE_THRESHOLD = 15.0    # km — below this, tieback viable
_STANDALONE_RESERVOIR_MIN = 200.0     # MMbbl — above this, standalone preferred


def predict_concept_type(
    projects: list[SubseaProject],
    water_depth: float,
    reservoir_size_mmbbl: float,
    distance_to_infra_km: Optional[float],
) -> ConceptPrediction:
    """Predict concept type using an empirical decision tree over benchmark data.

    Combines the statistical probability matrix from SubseaIQ benchmarks
    with heuristic adjustments for reservoir size and distance to
    infrastructure.  The decision tree:

    1. Classify the field into a water-depth band.
    2. Look up base probabilities from the benchmark probability matrix.
    3. Apply reservoir-size adjustments (boost tieback for small fields,
       penalise tieback for large fields).
    4. Apply distance-to-infrastructure adjustments (boost tieback when
       close, penalise when far).
    5. Re-normalise and return the highest-probability concept.

    Parameters
    ----------
    projects : list[SubseaProject]
        Benchmark dataset from which to derive probabilities.
    water_depth : float
        Target field water depth in metres (must be > 0).
    reservoir_size_mmbbl : float
        Estimated recoverable reserves in MMbbl (must be > 0).
    distance_to_infra_km : float or None
        Distance to nearest existing infrastructure in km.
        Pass None if no infrastructure is nearby.

    Returns
    -------
    ConceptPrediction
        Predicted concept type, full probability distribution,
        depth band, and rationale string.

    Raises
    ------
    ValueError
        If ``water_depth <= 0``, ``reservoir_size_mmbbl <= 0``,
        or ``projects`` is empty.
    """
    # --- Validation ---
    if not projects:
        raise ValueError(
            "projects list must not be empty — need benchmark data "
            "to derive probabilities."
        )
    if water_depth <= 0:
        raise ValueError(
            f"water_depth must be positive, got {water_depth!r}."
        )
    if reservoir_size_mmbbl <= 0:
        raise ValueError(
            f"reservoir_size_mmbbl must be positive, got {reservoir_size_mmbbl!r}."
        )

    # --- Step 1: Classify depth band ---
    depth_band = _classify_depth(water_depth)
    if depth_band is None:
        # Depth below 0 already rejected; this handles edge cases
        depth_band = DEPTH_BANDS[-1]  # treat as ultra-deep

    # --- Step 2: Base probabilities from benchmark matrix ---
    matrix = concept_probability_matrix(projects)
    base_probs = dict(matrix.get(depth_band, {}))

    # If the band is empty, seed with uniform priors over common types
    if not base_probs:
        _common = ["TLP", "Spar", "Semi", "FPSO", "Subsea Tieback"]
        base_probs = {c: 1.0 / len(_common) for c in _common}

    # Ensure Subsea Tieback is in the distribution (may not appear in data)
    if "Subsea Tieback" not in base_probs:
        base_probs["Subsea Tieback"] = 0.0

    # --- Step 3: Reservoir size adjustments ---
    adjusted = dict(base_probs)
    if reservoir_size_mmbbl <= _TIEBACK_RESERVOIR_THRESHOLD:
        # Small reservoir → boost tieback
        adjusted["Subsea Tieback"] = adjusted.get("Subsea Tieback", 0.0) + 0.30
    elif reservoir_size_mmbbl >= _STANDALONE_RESERVOIR_MIN:
        # Large reservoir → penalise tieback, boost standalone hosts
        adjusted["Subsea Tieback"] = max(
            0.01, adjusted.get("Subsea Tieback", 0.0) * 0.2
        )
        # Redistribute the freed probability to standalone hosts
        standalone_types = [k for k in adjusted if k != "Subsea Tieback"]
        if standalone_types:
            boost = 0.05
            for st in standalone_types:
                adjusted[st] = adjusted.get(st, 0.0) + boost

    # --- Step 4: Distance to infrastructure adjustments ---
    if distance_to_infra_km is not None:
        if distance_to_infra_km <= _TIEBACK_DISTANCE_THRESHOLD:
            # Close to infra → boost tieback
            adjusted["Subsea Tieback"] = adjusted.get("Subsea Tieback", 0.0) + 0.15
        elif distance_to_infra_km > 60.0:
            # Far from infra → penalise tieback
            adjusted["Subsea Tieback"] = max(
                0.01, adjusted.get("Subsea Tieback", 0.0) * 0.3
            )
    else:
        # No infrastructure → tieback not viable
        adjusted["Subsea Tieback"] = max(
            0.01, adjusted.get("Subsea Tieback", 0.0) * 0.1
        )

    # --- Step 5: Re-normalise ---
    total = sum(adjusted.values())
    if total > 0:
        normalised = {k: v / total for k, v in adjusted.items()}
    else:
        # Fallback: uniform
        normalised = {k: 1.0 / len(adjusted) for k in adjusted}

    # Remove zero-probability entries for cleanliness, then re-normalise
    normalised = {k: v for k, v in normalised.items() if v > 1e-12}
    norm_total = sum(normalised.values())
    if norm_total > 0:
        normalised = {k: v / norm_total for k, v in normalised.items()}

    # --- Select prediction ---
    predicted = max(normalised, key=normalised.get)  # type: ignore[arg-type]

    # --- Build rationale ---
    dist_str = f"{distance_to_infra_km:.0f} km" if distance_to_infra_km is not None else "unknown"
    top_3 = sorted(normalised.items(), key=lambda x: x[1], reverse=True)[:3]
    top_3_str = ", ".join(f"{c} {p:.0%}" for c, p in top_3)
    rationale = (
        f"Depth band {depth_band}: empirical prediction is {predicted}. "
        f"Top options: {top_3_str}. "
        f"Inputs: depth={water_depth:.0f}m, reservoir={reservoir_size_mmbbl:.0f} MMbbl, "
        f"distance={dist_str}."
    )

    return ConceptPrediction(
        predicted_concept=predicted,
        probabilities=normalised,
        depth_band=depth_band,
        rationale=rationale,
    )


# ---------------------------------------------------------------------------
# Case study validation (#2053)
# ---------------------------------------------------------------------------

def validate_against_cases(
    projects: list[SubseaProject],
    case_studies: list[dict],
) -> dict:
    """Validate concept predictions against known case studies.

    Runs :func:`predict_concept_type` for each case study and compares
    the predicted concept to the expected answer.  Returns accuracy
    metrics and per-case details.

    Parameters
    ----------
    projects : list[SubseaProject]
        Benchmark dataset from which to derive probabilities.
    case_studies : list[dict]
        Each dict must contain:
        - ``name`` (str): Case study label.
        - ``water_depth`` (float): Water depth in metres.
        - ``reservoir_size_mmbbl`` (float): Reserves in MMbbl.
        - ``distance_to_infra_km`` (float or None): Distance to infra.
        - ``expected_concept`` (str or list[str]): Correct concept type(s).
          Pass a list to accept multiple valid answers (e.g.
          ``["Spar", "Semi"]``).

    Returns
    -------
    dict
        Keys:
        - ``accuracy`` (float): Fraction of correct predictions (0.0–1.0).
        - ``total`` (int): Number of case studies evaluated.
        - ``correct`` (int): Number of correct predictions.
        - ``incorrect`` (int): Number of incorrect predictions.
        - ``details`` (list[dict]): Per-case breakdown with fields:
          ``name``, ``expected``, ``predicted``, ``match`` (bool),
          ``top_3`` (list of (concept, probability) tuples).

    Examples
    --------
    >>> result = validate_against_cases(projects, case_studies)
    >>> result["accuracy"]
    0.75
    """
    if not case_studies:
        return {
            "accuracy": 0.0,
            "total": 0,
            "correct": 0,
            "incorrect": 0,
            "details": [],
        }

    details: list[dict] = []
    correct = 0

    for case in case_studies:
        prediction = predict_concept_type(
            projects=projects,
            water_depth=case["water_depth"],
            reservoir_size_mmbbl=case["reservoir_size_mmbbl"],
            distance_to_infra_km=case.get("distance_to_infra_km"),
        )

        expected = case["expected_concept"]
        if isinstance(expected, str):
            match = prediction.predicted_concept == expected
        else:
            # Accept any of the listed concepts as correct
            match = prediction.predicted_concept in expected

        if match:
            correct += 1

        # Top 3 concepts by probability
        sorted_probs = sorted(
            prediction.probabilities.items(),
            key=lambda x: x[1],
            reverse=True,
        )
        top_3 = [(c, round(p, 4)) for c, p in sorted_probs[:3]]

        details.append({
            "name": case["name"],
            "expected": expected,
            "predicted": prediction.predicted_concept,
            "match": match,
            "top_3": top_3,
        })

    total = len(case_studies)
    return {
        "accuracy": correct / total,
        "total": total,
        "correct": correct,
        "incorrect": total - correct,
        "details": details,
    }


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _classify_depth(depth_m: float) -> Optional[str]:
    """Return the DEPTH_BANDS label for a given water depth."""
    for label, (lo, hi) in zip(DEPTH_BANDS, _DEPTH_THRESHOLDS):
        if lo <= depth_m < hi:
            return label
    return None


def _describe(values: list) -> dict[str, float]:
    """Return min/max/mean/count for a numeric list."""
    if not values:
        return {"count": 0, "min": 0.0, "max": 0.0, "mean": 0.0}
    return {
        "count": len(values),
        "min": min(values),
        "max": max(values),
        "mean": sum(values) / len(values),
    }


def _opt_float(val) -> Optional[float]:
    """Convert to float if not None.  Returns None for unparseable values."""
    if val is None:
        return None
    try:
        return float(val)
    except (TypeError, ValueError):
        return None


def _opt_int(val) -> Optional[int]:
    """Convert to int if not None.  Returns None for unparseable values."""
    if val is None:
        return None
    try:
        return int(float(val))
    except (TypeError, ValueError):
        return None
