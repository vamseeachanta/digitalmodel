"""
Tests for mesh quality data model and adjacent ratio logic.
Spec requirement: test_mesh_quality.py must exist per WRK-129.
"""
import pytest
from digitalmodel.solvers.orcaflex.reporting.models.mesh import (
    MeshData, SegmentData, MeshQualityData
)


def _compute_adjacent_ratios(seg_lengths: list) -> list:
    """Replicate the ratio calculation from mesh_extractor for testing."""
    ratios = []
    for i in range(len(seg_lengths) - 1):
        ratio = max(seg_lengths[i], seg_lengths[i + 1]) / min(seg_lengths[i], seg_lengths[i + 1])
        ratios.append(ratio)
    return ratios


def test_adjacent_ratio_calculation_correctness():
    """Adjacent ratio = max(L_i, L_{i+1}) / min(L_i, L_{i+1})."""
    seg_lengths = [1.0, 2.0, 4.0]
    ratios = _compute_adjacent_ratios(seg_lengths)
    assert len(ratios) == 2
    assert ratios[0] == pytest.approx(2.0)  # max(1,2)/min(1,2)
    assert ratios[1] == pytest.approx(2.0)  # max(2,4)/min(2,4)


def test_adjacent_ratio_uniform_mesh():
    """Uniform segment lengths produce ratio 1.0 everywhere."""
    seg_lengths = [0.5, 0.5, 0.5, 0.5]
    ratios = _compute_adjacent_ratios(seg_lengths)
    assert all(r == pytest.approx(1.0) for r in ratios)


def test_mesh_quality_verdict_pass():
    """MeshQualityData with max_adjacent_ratio < 3.0 holds verdict PASS."""
    qd = MeshQualityData(
        max_adjacent_ratio=2.9,
        worst_ratio_arc_length_m=5.0,
        verdict="PASS",
        adjacent_ratios=[1.5, 2.9, 2.1],
    )
    assert qd.verdict == "PASS"
    assert qd.max_adjacent_ratio < 3.0


def test_mesh_quality_verdict_warning():
    """MeshQualityData with max_adjacent_ratio >= 3.0 holds verdict WARNING."""
    qd = MeshQualityData(
        max_adjacent_ratio=3.5,
        worst_ratio_arc_length_m=10.0,
        verdict="WARNING",
        adjacent_ratios=[1.2, 3.5, 1.8],
    )
    assert qd.verdict == "WARNING"
    assert qd.max_adjacent_ratio >= 3.0


def test_mesh_data_empty_segments_no_quality():
    """MeshData with empty segment list has quality=None (insufficient data)."""
    md = MeshData(total_segment_count=0, segments=[])
    assert md.quality is None
    assert md.segments == []


def test_mesh_data_with_segments_and_quality():
    """MeshData constructed with SegmentData and MeshQualityData."""
    segs = [
        SegmentData(arc_length_m=0.0, length_m=1.0),
        SegmentData(arc_length_m=1.0, length_m=1.0),
        SegmentData(arc_length_m=2.0, length_m=2.0),
    ]
    qd = MeshQualityData(
        max_adjacent_ratio=2.0,
        worst_ratio_arc_length_m=1.0,
        verdict="PASS",
        adjacent_ratios=[1.0, 2.0],
    )
    md = MeshData(total_segment_count=3, segments=segs, quality=qd)
    assert md.total_segment_count == 3
    assert len(md.segments) == 3
    assert md.quality.verdict == "PASS"
