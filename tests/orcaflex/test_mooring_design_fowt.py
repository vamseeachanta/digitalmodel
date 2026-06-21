"""Tests for digitalmodel.orcaflex.mooring_design_fowt (#576).

FOWT watch-circle envelope vs dynamic-cable curvature (DNV-RP-0360 MBR).

Hand-verified governing geometry (compliant case, see issue-576.md):
    suspended_length L_s = 250 m, hang-off elevation h = 80 m,
    nominal horizontal span x0 = 200 m, watch-circle radius R_wc = 20 m.
    x_min = x0 - R_wc = 180 m
    chord c = sqrt(80^2 + 180^2) = sqrt(6400 + 32400) = sqrt(38800) = 196.977 m
    R from c = 2 R sin(L_s / 2R): solving sin(theta)/theta = 196.977/250 = 0.78791
        => theta = 0.69788 rad, R = 250 / (2*0.69788) = 107.092 m
    margin = 107.092 - 50 (MBR) = +57.092 m  => PASS.
"""

import math
import warnings

import pytest

from digitalmodel.orcaflex.mooring_design_fowt import (
    DynamicCableConfig,
    FloaterType,
    MooringType,
    WatchCircleConstraint,
    WatchCircleResult,
    _arc_radius_from_chord,
    check_watch_circle_vs_cable,
)


def _suppress_standalone_warning():
    return warnings.catch_warnings()


class TestArcRadiusFromChord:
    """Unit tests for the circular-arc inversion helper."""

    def test_straight_cable_infinite_radius(self):
        assert _arc_radius_from_chord(250.0, 250.0) == math.inf
        assert _arc_radius_from_chord(250.0, 300.0) == math.inf

    def test_known_arc(self):
        # From the hand-verified compliant case.
        r = _arc_radius_from_chord(250.0, math.hypot(80.0, 180.0))
        assert r == pytest.approx(107.092, abs=0.01)

    def test_radius_increases_with_chord(self):
        # Longer chord (straighter) -> larger radius.
        r_short = _arc_radius_from_chord(250.0, 170.0)
        r_long = _arc_radius_from_chord(250.0, 215.0)
        assert r_long > r_short

    def test_invalid_arc_length(self):
        with pytest.raises(ValueError):
            _arc_radius_from_chord(0.0, 100.0)


class TestWatchCircleCompliant:
    """Clearly-compliant case: large MBR margin."""

    def test_compliant_pass_with_margin(self):
        cfg = DynamicCableConfig(
            suspended_length=250.0,
            hang_off_elevation=80.0,
            nominal_horizontal_span=200.0,
            mbr_limit_m=50.0,
            mooring_type=MooringType.CATENARY,
            floater_type=FloaterType.SEMI,
        )
        with _suppress_standalone_warning():
            warnings.simplefilter("ignore")
            res = WatchCircleConstraint(
                watch_circle_radius=20.0, cable=cfg
            ).check()
        assert isinstance(res, WatchCircleResult)
        assert res.passes is True
        # Hand-verified governing radius and margin.
        assert res.governing_bend_radius_m == pytest.approx(107.092, abs=0.01)
        assert res.governing_chord_m == pytest.approx(196.977, abs=0.01)
        assert res.margin_m == pytest.approx(57.092, abs=0.01)
        assert res.governing_offset_m == pytest.approx(20.0)


class TestWatchCircleViolation:
    """Violating case: tight watch circle -> curvature exceeds MBR -> fail."""

    def test_violation_fails_closed(self):
        # Big watch circle pulls the chord far in; large MBR limit.
        cfg = DynamicCableConfig(
            suspended_length=250.0,
            hang_off_elevation=80.0,
            nominal_horizontal_span=200.0,
            mbr_limit_m=110.0,
        )
        res = check_watch_circle_vs_cable(120.0, cfg)
        assert res.passes is False
        # x_min = 200 - 120 = 80, chord = sqrt(80^2+80^2)=113.137,
        # governing radius ~ 62.35 m < 110 m MBR.
        assert res.governing_chord_m == pytest.approx(113.137, abs=0.01)
        assert res.governing_bend_radius_m == pytest.approx(62.35, abs=0.05)
        assert res.margin_m < 0.0
        # Governing radius is reported even on failure.
        assert res.governing_bend_radius_m > 0.0

    def test_offset_past_touchdown_collapses_span(self):
        # Offset exceeds nominal span -> x_min clamped to 0, chord = elevation.
        cfg = DynamicCableConfig(
            suspended_length=250.0,
            hang_off_elevation=80.0,
            nominal_horizontal_span=200.0,
            mbr_limit_m=60.0,
        )
        res = check_watch_circle_vs_cable(220.0, cfg)
        assert res.governing_chord_m == pytest.approx(80.0, abs=0.01)
        assert res.passes is False


class TestWatchCircleBoundary:
    """Boundary case: governing radius straddles the MBR limit."""

    # At R_wc = 50: x_min=150, chord=sqrt(80^2+150^2)=170.0, R=85.507 m.
    GOVERNING_R = 85.507

    def test_boundary_just_passes(self):
        cfg = DynamicCableConfig(
            suspended_length=250.0,
            hang_off_elevation=80.0,
            nominal_horizontal_span=200.0,
            mbr_limit_m=85.0,  # just below governing radius -> pass
        )
        res = check_watch_circle_vs_cable(50.0, cfg)
        assert res.governing_chord_m == pytest.approx(170.0, abs=0.01)
        assert res.governing_bend_radius_m == pytest.approx(self.GOVERNING_R, abs=0.02)
        assert res.passes is True
        assert res.margin_m == pytest.approx(0.507, abs=0.02)

    def test_boundary_just_fails(self):
        cfg = DynamicCableConfig(
            suspended_length=250.0,
            hang_off_elevation=80.0,
            nominal_horizontal_span=200.0,
            mbr_limit_m=86.0,  # just above governing radius -> fail
        )
        res = check_watch_circle_vs_cable(50.0, cfg)
        assert res.passes is False
        assert res.margin_m == pytest.approx(-0.493, abs=0.02)

    def test_equal_radius_passes(self):
        # MBR limit set exactly at the (full-precision) governing radius:
        # governing >= MBR is inclusive -> pass.
        cfg = DynamicCableConfig(
            suspended_length=250.0,
            hang_off_elevation=80.0,
            nominal_horizontal_span=200.0,
            mbr_limit_m=85.5,  # at/just below the true governing radius (85.5067)
        )
        res = check_watch_circle_vs_cable(50.0, cfg)
        assert res.passes is True
        assert res.margin_m == pytest.approx(0.007, abs=0.01)


class TestMooringFloaterCombinations:
    """Cover catenary/taut/hybrid x spar/semi/TLP echoed through the result."""

    @pytest.mark.parametrize("mooring", list(MooringType))
    @pytest.mark.parametrize("floater", list(FloaterType))
    def test_combination_round_trips(self, mooring, floater):
        cfg = DynamicCableConfig(
            suspended_length=300.0,
            hang_off_elevation=90.0,
            nominal_horizontal_span=240.0,
            mbr_limit_m=55.0,
            mooring_type=mooring,
            floater_type=floater,
        )
        res = check_watch_circle_vs_cable(25.0, cfg)
        assert res.mooring_type == mooring
        assert res.floater_type == floater
        assert res.passes is True


class TestMonotonicity:
    """Governing radius must shrink monotonically as the watch circle grows."""

    def test_tighter_watch_circle_reduces_radius(self):
        cfg = DynamicCableConfig(
            suspended_length=250.0,
            hang_off_elevation=80.0,
            nominal_horizontal_span=200.0,
            mbr_limit_m=40.0,
        )
        radii = [
            check_watch_circle_vs_cable(r, cfg).governing_bend_radius_m
            for r in (10.0, 30.0, 60.0, 90.0)
        ]
        assert radii == sorted(radii, reverse=True)
