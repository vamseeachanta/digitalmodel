"""Tests for vessel_silhouettes module (digitalmodel#616 — TDD #9, #10, #11).

See docs/plans/2026-05-20-issue-616-*.md §TDD.
"""
from __future__ import annotations

import pytest

from digitalmodel.marine_ops.marine_engineering.visualization.vessel_silhouettes import (
    get_polygon,
)


# ---------- TDD #9: tanker silhouette has bow forward (max x is at bow) ----------

def test_tanker_silhouette_bow_forward():
    """Bow-up convention: the silhouette polygon's max-x point is the bow.

    Polygon is returned in body-fixed coords (x = longitudinal, +x = bow direction,
    y = transverse, +y = starboard). At rendering time the polar projection's
    rotation=90 places +x at the top (bow up).
    """
    poly = get_polygon("tanker", length_bp_m=300.0, beam_m=50.0)
    assert len(poly) >= 4, "tanker silhouette must have at least 4 vertices"
    xs = [pt[0] for pt in poly]
    max_x = max(xs)
    min_x = min(xs)
    # bow at +x extreme; stern at min_x. Bow x_extent > stern x_extent in any sane silhouette.
    assert max_x > 0, f"silhouette max-x must be > 0 (bow forward); got {max_x}"
    # Length proportionality: max_x - min_x should approximate length_bp
    span = max_x - min_x
    assert 0.5 * 300.0 <= span <= 1.5 * 300.0, (
        f"silhouette x-extent {span} must roughly match length_bp=300"
    )


# ---------- TDD #10: silhouette scales linearly with length_bp ----------

def test_silhouette_scales_with_length_bp():
    poly_300 = get_polygon("tanker", length_bp_m=300.0, beam_m=50.0)
    poly_600 = get_polygon("tanker", length_bp_m=600.0, beam_m=50.0)
    span_300 = max(p[0] for p in poly_300) - min(p[0] for p in poly_300)
    span_600 = max(p[0] for p in poly_600) - min(p[0] for p in poly_600)
    ratio = span_600 / span_300
    assert 1.95 <= ratio <= 2.05, (
        f"doubling length_bp should ≈ double x-extent; got ratio {ratio:.4f}"
    )


# ---------- TDD #11: custom_path option accepts caller polygon ----------

def test_silhouette_custom_path_accepted():
    """When silhouette_kind='custom_path', caller-supplied polygon is returned verbatim."""
    custom = [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)]
    poly = get_polygon("custom_path", length_bp_m=1.0, beam_m=1.0,
                      custom_path=custom)
    assert poly == custom, f"custom_path must be returned unchanged; got {poly}"


# ---------- Additional safety: unknown silhouette_kind raises ----------

def test_unknown_silhouette_kind_raises():
    with pytest.raises(ValueError, match=r"silhouette_kind|unknown"):
        get_polygon("xenomorph", length_bp_m=300.0, beam_m=50.0)
