"""Pipe-run geometry: line-crossing jumps and run construction."""

from __future__ import annotations

import pytest

from digitalmodel.process_diagrams.geometry import (
    HOP_RADIUS,
    horizontal,
    polyline,
    run_length,
    signal,
    vertical,
)


def _arc_count(path: str) -> int:
    return path.count("A" + str(int(HOP_RADIUS)))


class TestHops:
    def test_run_without_crossings_is_a_straight_line(self):
        assert horizontal(0, 100, 50) == "M0,50 L100,50"
        assert vertical(0, 100, 50) == "M50,0 L50,100"

    def test_each_crossing_inserts_one_arc(self):
        assert _arc_count(horizontal(0, 200, 50, crossings=[40, 120])) == 2
        assert _arc_count(vertical(0, 200, 50, crossings=[40, 120])) == 2

    @pytest.mark.parametrize("crossing", [0, 100, -10, 260])
    def test_crossing_at_or_beyond_an_endpoint_is_not_a_jump(self, crossing):
        # A line that terminates at the crossing is a connection, not a jump.
        assert horizontal(0, 100, 50, crossings=[crossing]) == "M0,50 L100,50"

    def test_arc_straddles_the_crossing_symmetrically(self):
        path = horizontal(0, 100, 50, crossings=[40])
        assert f"L{40 - int(HOP_RADIUS)},50" in path
        assert f"{40 + int(HOP_RADIUS)},50" in path

    def test_right_to_left_run_orders_hops_along_travel(self):
        path = horizontal(200, 0, 50, crossings=[40, 120])
        # Travelling leftward the far crossing (120) is reached first, and the
        # arc is entered on its near side at 127 before landing at 113.
        assert path.index("127") < path.index("47")
        assert path.startswith("M200,50")

    def test_bottom_to_top_run_orders_hops_along_travel(self):
        path = vertical(200, 0, 50, crossings=[40, 120])
        assert path.index("127") < path.index("47")

    def test_signal_lines_never_hop(self):
        # Signal line style already distinguishes them, so they cross freely.
        assert _arc_count(signal([(0, 50), (100, 50)])) == 0


class TestRuns:
    def test_polyline_walks_every_point(self):
        assert polyline([(0, 0), (10, 0), (10, 20)]) == "M0,0 L10,0 L10,20"

    def test_polyline_needs_two_points(self):
        with pytest.raises(ValueError):
            polyline([(0, 0)])

    def test_run_length_is_euclidean(self):
        assert run_length([(0, 0), (3, 4)]) == pytest.approx(5.0)
        assert run_length([(0, 0), (3, 4), (3, 8)]) == pytest.approx(9.0)

    def test_coordinates_drop_trailing_zeros(self):
        assert horizontal(0.0, 100.0, 50.0) == "M0,50 L100,50"
        assert "12.5" in horizontal(0, 100, 12.5)
