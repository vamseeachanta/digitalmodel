# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic Cartesian examples; expected values are hand calculations below.

All coordinates and radii are in metres. No design spacing is prescribed.
"""

import math

import pytest

from digitalmodel.subsea.riser.trajectory_clearance import minimum_surface_gap


def test_parallel_centerlines_hand_gap():
    # Parallel x axes separated by the 3-4-5 triangle: 5 - 0.2 - 0.3 = 4.5.
    result = minimum_surface_gap(
        [(0, 0, 0), (10, 0, 0)], [(0, 3, 4), (10, 3, 4)], 0.2, 0.3
    )
    assert result.centerline_distance == pytest.approx(5)
    assert result.surface_gap == pytest.approx(4.5)
    assert math.dist(result.witness_a, result.witness_b) == pytest.approx(5)


def test_crossing_segments():
    # Orthogonal axes intersect at the origin: distance 0, overlap gap -0.5.
    result = minimum_surface_gap(
        [(-2, 0, 0), (2, 0, 0)], [(0, -3, 0), (0, 3, 0)], 0.2, 0.3
    )
    assert result.centerline_distance == pytest.approx(0)
    assert result.surface_gap == pytest.approx(-0.5)
    assert result.witness_a == pytest.approx((0, 0, 0))
    assert result.witness_b == pytest.approx((0, 0, 0))


def test_skew_segment_witnesses():
    # Directions (2, 2, 0) and (2, -2, 0) are perpendicular. Interior
    # witnesses (1, 1, 0), (1, 1, 3) differ only in z: distance 3, gap 2.5.
    result = minimum_surface_gap(
        [(0, 0, 0), (2, 2, 0)], [(0, 2, 3), (2, 0, 3)], 0.2, 0.3
    )
    assert result.witness_a == pytest.approx((1, 1, 0))
    assert result.witness_b == pytest.approx((1, 1, 3))
    assert result.centerline_distance == pytest.approx(math.dist((1, 1, 0), (1, 1, 3)))
    assert result.surface_gap == pytest.approx(2.5)
    assert result.parameter_a == pytest.approx(0.5)
    assert result.parameter_b == pytest.approx(0.5)


@pytest.mark.parametrize("degenerate", [False, True])
def test_endpoint_minimum(degenerate):
    # The nearest endpoint is (1, 0, 0), or (0, 0, 0) for a point segment.
    # B starts at (4, 4, 0) and extends away, so direct Cartesian distance wins.
    endpoint = (0, 0, 0) if degenerate else (1, 0, 0)
    result = minimum_surface_gap(
        [(0, 0, 0), endpoint], [(4, 4, 0), (4, 8, 0)], 0, 0
    )
    assert result.witness_a == pytest.approx(endpoint)
    assert result.witness_b == pytest.approx((4, 4, 0))
    assert result.surface_gap == pytest.approx(math.dist(endpoint, (4, 4, 0)))


def test_point_to_segment_interior():
    # A repeated point projects to x=2 on the x axis; distance is 3.
    result = minimum_surface_gap(
        [(2, 3, 0)] * 2, [(0, 0, 0), (4, 0, 0)], 0, 0
    )
    assert result.witness_a == pytest.approx((2, 3, 0))
    assert result.witness_b == pytest.approx((2, 0, 0))
    assert result.centerline_distance == pytest.approx(3)
    assert result.parameter_a == 0
    assert result.parameter_b == pytest.approx(0.5)


def test_two_point_segments():
    # Repeated vertices define points, again a 3-4-5 triangle.
    result = minimum_surface_gap([(0, 0, 0)] * 2, [(0, 3, 4)] * 2, 0, 0)
    assert result.centerline_distance == pytest.approx(5)
    assert (result.parameter_a, result.parameter_b) == (0, 0)


def test_polyline_uses_all_segments_and_reports_indices():
    # Only the last pair intersects, at (2, 1, 0).
    a = [(0, 0, 0), (2, 0, 0), (2, 2, 0)]
    b = [(4, 4, 0), (4, 1, 0), (1, 1, 0)]
    result = minimum_surface_gap(a, b, 0, 0)
    assert result.centerline_distance == pytest.approx(0)
    assert (result.segment_index_a, result.segment_index_b) == (1, 1)
    assert result.witness_a == pytest.approx((2, 1, 0))
    assert result.witness_b == pytest.approx((2, 1, 0))
    assert result.parameter_a == pytest.approx(0.5)
    assert result.parameter_b == pytest.approx(2 / 3)


@pytest.mark.parametrize("reverse_a,reverse_b,swap", [
    (False, False, False), (True, False, False), (False, True, False),
    (True, True, False), (False, False, True), (True, True, True),
])
def test_translation_and_trajectory_order_invariance(reverse_a, reverse_b, swap):
    # Same skew example translated by (7, -11, 13); the gap remains 2.5.
    a = [(7, -11, 13), (9, -9, 13)]
    b = [(7, -9, 16), (9, -11, 16)]
    a = a[::-1] if reverse_a else a
    b = b[::-1] if reverse_b else b
    result = minimum_surface_gap(b, a, 0.3, 0.2) if swap else minimum_surface_gap(a, b, 0.2, 0.3)
    assert result.surface_gap == pytest.approx(2.5)
    assert result.centerline_distance == pytest.approx(3)
    for line, point, parameter in (
        (b if swap else a, result.witness_a, result.parameter_a),
        (a if swap else b, result.witness_b, result.parameter_b),
    ):
        expected = tuple(start + parameter * (end - start) for start, end in zip(*line))
        assert point == pytest.approx(expected)


@pytest.mark.parametrize("b", [
    [(1, 0, 0), (3, 0, 0)],  # Collinear overlap: distance zero.
    [(3, 0, 0), (1, 0, 0)],  # Antiparallel overlap: distance zero.
    [(2, 0, 0), (3, 1, 0)],  # Shared endpoint: distance zero.
])
def test_contact_and_collinear_overlap(b):
    assert minimum_surface_gap([(0, 0, 0), (2, 0, 0)], b, 0, 0).surface_gap == 0


def test_nearly_parallel_interior_crossing():
    # y=0 crosses y=1e-12*(1-2*x) at x=0.5; no angular cutoff is justified.
    result = minimum_surface_gap(
        [(0, 0, 0), (1, 0, 0)], [(0, 1e-12, 0), (1, -1e-12, 0)], 0, 0
    )
    assert result.centerline_distance == 0
    assert result.witness_a == pytest.approx((0.5, 0, 0))
    assert result.witness_b == pytest.approx((0.5, 0, 0))


@pytest.mark.parametrize("scale", [1e-200, 1, 1e200])
def test_scale_range(scale):
    # Scaled perpendicular skew lines: distance/scale=3, gap/scale=2.5.
    a = [(0, 0, 0), (2 * scale, 0, 0)]
    b = [(scale, -scale, 3 * scale), (scale, scale, 3 * scale)]
    result = minimum_surface_gap(a, b, 0.2 * scale, 0.3 * scale)
    assert result.centerline_distance / scale == pytest.approx(3)
    assert result.surface_gap / scale == pytest.approx(2.5)


@pytest.mark.parametrize("bad", [
    [], [(0, 0, 0)], [(0, 0), (1, 1)], [(0, 0, 0, 0)] * 2,
    [(0, 0, 0), (math.nan, 0, 0)], [(0, math.inf, 0)] * 2,
    [(0, 0, -math.inf)] * 2, [("0", 0, 0)] * 2,
    [(True, 0, 0)] * 2, [(0j, 0, 0)] * 2, None, [None, None],
])
@pytest.mark.parametrize("side", ["a", "b"])
def test_invalid_centerlines(bad, side):
    good = [(0, 0, 0), (1, 0, 0)]
    with pytest.raises(ValueError, match="centerline"):
        minimum_surface_gap(bad if side == "a" else good, bad if side == "b" else good, 0, 0)


@pytest.mark.parametrize("bad", [-1, math.nan, math.inf, -math.inf, "1", True, None, 1j])
@pytest.mark.parametrize("side", ["a", "b"])
def test_invalid_radii(bad, side):
    line = [(0, 0, 0), (1, 0, 0)]
    with pytest.raises(ValueError, match="radius"):
        minimum_surface_gap(line, line, bad if side == "a" else 0, bad if side == "b" else 0)


def test_unrepresentable_geometry_is_rejected():
    # Each input is finite, but the separation exceeds binary64 range.
    with pytest.raises(ValueError, match="range"):
        minimum_surface_gap([(-1e308, 0, 0)] * 2, [(1e308, 0, 0)] * 2, 0, 0)


def test_unrepresentable_gap_is_rejected():
    # Two finite radii can have an unrepresentable sum.
    with pytest.raises(ValueError, match="range"):
        minimum_surface_gap([(0, 0, 0)] * 2, [(0, 0, 0)] * 2, 1e308, 1e308)


def test_public_exports_preserve_lazy_screening(monkeypatch):
    # Isolate the import contract from optional screening/report dependencies.
    from types import SimpleNamespace

    from digitalmodel.subsea import riser

    assert riser.minimum_surface_gap is minimum_surface_gap
    sentinel = object()
    requested = []

    def load(name):
        requested.append(name)
        return SimpleNamespace(RiserLoadCase=sentinel)

    monkeypatch.setattr(riser, "import_module", load)
    # Record and restore the dictionary entry without invoking lazy lookup.
    monkeypatch.setitem(riser.__dict__, "RiserLoadCase", None)
    riser.__dict__.pop("RiserLoadCase")
    assert riser.RiserLoadCase is sentinel
    assert requested == ["digitalmodel.subsea.riser.riser_case_screening"]
    assert riser.RiserLoadCase is sentinel
    assert len(requested) == 1
    with pytest.raises(AttributeError):
        riser.unknown_export
