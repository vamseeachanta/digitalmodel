# ABOUTME: Subsea architecture pattern analysis — layout, tieback, equipment, flowline trends.
# ABOUTME: Issue #2058 — split from benchmarks.py to stay under 500-line limit.
"""
digitalmodel.field_development.architecture_patterns
=====================================================

Analyses subsea architecture patterns from :class:`SubseaProject` records:
- layout distribution by concept type
- tieback distance statistics segmented by depth band and fluid type
- equipment counts (trees, manifolds) by concept type
- flowline diameter and material trends by water-depth band

All functions accept a ``list[SubseaProject]`` and return plain dicts
suitable for JSON serialisation or further analysis.

Usage
-----
>>> from digitalmodel.field_development.benchmarks import load_projects
>>> from digitalmodel.field_development.architecture_patterns import (
...     layout_distribution, tieback_stats_segmented,
...     equipment_stats_by_concept, flowline_trends_by_depth,
... )
>>> projects = load_projects(raw_records)
>>> layouts = layout_distribution(projects)
>>> tieback = tieback_stats_segmented(projects)
>>> equip = equipment_stats_by_concept(projects)
>>> flowline = flowline_trends_by_depth(projects)
"""

from __future__ import annotations

from digitalmodel.field_development.benchmarks import (
    SubseaProject,
    DEPTH_BANDS,
    _classify_depth,
    _describe,
)


# ---------------------------------------------------------------------------
# Layout distribution by concept type
# ---------------------------------------------------------------------------

def layout_distribution(
    projects: list[SubseaProject],
) -> dict[str, dict[str, int]]:
    """Count layout types per concept type.

    Only projects with both ``concept_type`` and ``layout_type`` set
    are included.

    Returns
    -------
    dict[str, dict[str, int]]
        Outer key is concept type, inner dict maps layout type to count.
    """
    result: dict[str, dict[str, int]] = {}
    for p in projects:
        if p.concept_type is None or p.layout_type is None:
            continue
        bucket = result.setdefault(p.concept_type, {})
        bucket[p.layout_type] = bucket.get(p.layout_type, 0) + 1
    return result


# ---------------------------------------------------------------------------
# Tieback stats segmented by depth band and fluid type
# ---------------------------------------------------------------------------

def tieback_stats_segmented(
    projects: list[SubseaProject],
) -> dict[str, dict]:
    """Compute tieback distance statistics segmented two ways.

    Returns a dict with two keys:
    - ``by_depth_band``: tieback distance stats per water-depth band
    - ``by_fluid_type``: tieback distance stats per fluid type

    Projects missing ``tieback_distance_km`` are excluded.
    For ``by_depth_band``, projects also missing ``water_depth_m`` are excluded.
    For ``by_fluid_type``, projects also missing ``fluid_type`` are excluded.

    Returns
    -------
    dict[str, dict]
        Each inner dict maps a segment label to a stats dict
        (``count``, ``min``, ``max``, ``mean``).
    """
    by_depth: dict[str, list[float]] = {}
    by_fluid: dict[str, list[float]] = {}

    for p in projects:
        if p.tieback_distance_km is None:
            continue

        # Segment by depth band
        if p.water_depth_m is not None:
            band = _classify_depth(p.water_depth_m)
            if band is not None:
                by_depth.setdefault(band, []).append(p.tieback_distance_km)

        # Segment by fluid type
        if p.fluid_type is not None:
            by_fluid.setdefault(p.fluid_type, []).append(p.tieback_distance_km)

    return {
        "by_depth_band": {k: _describe(v) for k, v in by_depth.items()},
        "by_fluid_type": {k: _describe(v) for k, v in by_fluid.items()},
    }


# ---------------------------------------------------------------------------
# Equipment stats by concept type
# ---------------------------------------------------------------------------

def equipment_stats_by_concept(
    projects: list[SubseaProject],
) -> dict[str, dict[str, dict[str, float]]]:
    """Compute trees and manifolds statistics per concept type.

    Only projects with ``concept_type`` set are included.  Within each
    concept, only projects with non-None equipment counts contribute
    to that metric.

    Returns
    -------
    dict[str, dict[str, dict[str, float]]]
        Outer key is concept type.  Inner dict has keys
        ``trees_per_project`` and ``manifolds_per_project``, each
        containing a stats dict (``count``, ``min``, ``max``, ``mean``).
    """
    trees_by_concept: dict[str, list[int]] = {}
    mani_by_concept: dict[str, list[int]] = {}

    for p in projects:
        if p.concept_type is None:
            continue
        if p.num_trees is not None:
            trees_by_concept.setdefault(p.concept_type, []).append(p.num_trees)
        if p.num_manifolds is not None:
            mani_by_concept.setdefault(p.concept_type, []).append(p.num_manifolds)

    all_concepts = set(trees_by_concept) | set(mani_by_concept)
    result: dict[str, dict[str, dict[str, float]]] = {}
    for concept in sorted(all_concepts):
        result[concept] = {
            "trees_per_project": _describe(trees_by_concept.get(concept, [])),
            "manifolds_per_project": _describe(mani_by_concept.get(concept, [])),
        }
    return result


# ---------------------------------------------------------------------------
# Flowline trends by depth band
# ---------------------------------------------------------------------------

def flowline_trends_by_depth(
    projects: list[SubseaProject],
) -> dict[str, dict]:
    """Compute flowline diameter statistics and material distribution per depth band.

    Only projects with ``water_depth_m`` set are included.  Within each
    band, diameter stats use projects with ``flowline_diameter_in`` set,
    and material distribution uses projects with ``flowline_material`` set.

    Returns
    -------
    dict[str, dict]
        Outer key is depth band label.  Inner dict contains:
        - ``diameter``: stats dict (``count``, ``min``, ``max``, ``mean``)
        - ``materials``: dict mapping material name to count
        Only bands with at least one data point are included.
    """
    diam_by_band: dict[str, list[float]] = {}
    mat_by_band: dict[str, dict[str, int]] = {}

    for p in projects:
        if p.water_depth_m is None:
            continue
        band = _classify_depth(p.water_depth_m)
        if band is None:
            continue

        if p.flowline_diameter_in is not None:
            diam_by_band.setdefault(band, []).append(p.flowline_diameter_in)

        if p.flowline_material is not None:
            mat_bucket = mat_by_band.setdefault(band, {})
            mat_bucket[p.flowline_material] = mat_bucket.get(p.flowline_material, 0) + 1

    all_bands = set(diam_by_band) | set(mat_by_band)
    result: dict[str, dict] = {}
    for band in sorted(all_bands, key=lambda b: DEPTH_BANDS.index(b) if b in DEPTH_BANDS else 999):
        entry: dict = {}
        if band in diam_by_band:
            entry["diameter"] = _describe(diam_by_band[band])
        if band in mat_by_band:
            entry["materials"] = mat_by_band[band]
        result[band] = entry
    return result
