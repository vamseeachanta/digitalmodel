# ABOUTME: SubseaIQ benchmark bridge — concept-selection bands and subsea architecture stats.
# ABOUTME: Issue #1861 — loads normalized project records and derives benchmark aggregations.
"""
digitalmodel.field_development.benchmarks
=========================================

Loads normalized SubseaIQ project records and derives:
- concept-selection benchmark bands (concept type counts by water depth)
- subsea architecture statistics (tieback distance, equipment counts)

This is the *bridge layer* between raw SubseaIQ scraped data and the
field_development analysis modules (concept_selection, capex_estimator).

Usage
-----
>>> from digitalmodel.field_development.benchmarks import (
...     load_projects, concept_benchmark_bands, subsea_architecture_stats,
... )
>>> projects = load_projects(raw_records)
>>> bands = concept_benchmark_bands(projects)
>>> stats = subsea_architecture_stats(projects)
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
    """Convert to float if not None."""
    return float(val) if val is not None else None


def _opt_int(val) -> Optional[int]:
    """Convert to int if not None."""
    return int(val) if val is not None else None
