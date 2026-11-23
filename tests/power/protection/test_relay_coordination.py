"""Tests for protection relay coordination module.

Covers overcurrent relay (50/51) validation, differential relay (87),
IEEE C37.112 inverse-time curve calculations, coordination grading
margin checks, and time-current intersection finding.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.power.protection.relay_coordination import (
    CurveType,
    DifferentialRelay,
    OvercurrentRelay,
    RelayType,
    coordination_check,
    inverse_time_curve,
    time_current_intersection,
)


# ── RelayType enum ──────────────────────────────────────────────────


class TestRelayType:
    def test_overcurrent_50_value(self):
        assert RelayType.OVERCURRENT_50.value == "overcurrent_50"

    def test_overcurrent_51_value(self):
        assert RelayType.OVERCURRENT_51.value == "overcurrent_51"

    def test_differential_87_value(self):
        assert RelayType.DIFFERENTIAL_87.value == "differential_87"


# ── CurveType enum ──────────────────────────────────────────────────


class TestCurveType:
    def test_all_curve_types_exist(self):
        expected = {
            "MODERATELY_INVERSE",
            "VERY_INVERSE",
            "EXTREMELY_INVERSE",
            "DEFINITE_TIME",
        }
        assert {m.name for m in CurveType} == expected


# ── OvercurrentRelay dataclass ──────────────────────────────────────


class TestOvercurrentRelay:
    def test_valid_construction(self):
        relay = OvercurrentRelay(
            relay_id="R1",
            relay_type=RelayType.OVERCURRENT_51,
            curve_type=CurveType.MODERATELY_INVERSE,
            pickup_current_A=100.0,
            time_dial_setting=0.5,
        )
        assert relay.relay_id == "R1"
        assert relay.instantaneous_pickup_A is None

    def test_with_instantaneous_pickup(self):
        relay = OvercurrentRelay(
            relay_id="R2",
            relay_type=RelayType.OVERCURRENT_50,
            curve_type=CurveType.DEFINITE_TIME,
            pickup_current_A=200.0,
            time_dial_setting=1.0,
            instantaneous_pickup_A=2000.0,
        )
        assert relay.instantaneous_pickup_A == 2000.0

    def test_negative_pickup_current_raises(self):
        with pytest.raises(ValueError, match="pickup_current_A must be positive"):
            OvercurrentRelay(
                relay_id="R3",
                relay_type=RelayType.OVERCURRENT_51,
                curve_type=CurveType.MODERATELY_INVERSE,
                pickup_current_A=-10.0,
                time_dial_setting=0.5,
            )

    def test_zero_pickup_current_raises(self):
        with pytest.raises(ValueError, match="pickup_current_A must be positive"):
            OvercurrentRelay(
                relay_id="R4",
                relay_type=RelayType.OVERCURRENT_51,
                curve_type=CurveType.MODERATELY_INVERSE,
                pickup_current_A=0.0,
                time_dial_setting=0.5,
            )

    def test_negative_time_dial_raises(self):
        with pytest.raises(ValueError, match="time_dial_setting must be positive"):
            OvercurrentRelay(
                relay_id="R5",
                relay_type=RelayType.OVERCURRENT_51,
                curve_type=CurveType.MODERATELY_INVERSE,
                pickup_current_A=100.0,
                time_dial_setting=-0.1,
            )


# ── DifferentialRelay dataclass ─────────────────────────────────────


class TestDifferentialRelay:
    def test_valid_construction(self):
        relay = DifferentialRelay(
            relay_id="D1",
            slope_percent=25.0,
            minimum_operate_current_A=0.5,
            restraint_current_A=5.0,
        )
        assert relay.relay_id == "D1"
        assert relay.slope_percent == 25.0

    def test_slope_out_of_range_raises(self):
        with pytest.raises(ValueError, match="slope_percent must be between"):
            DifferentialRelay(
                relay_id="D2",
                slope_percent=150.0,
                minimum_operate_current_A=0.5,
                restraint_current_A=5.0,
            )

    def test_negative_minimum_operate_current_raises(self):
        with pytest.raises(
            ValueError, match="minimum_operate_current_A must be positive"
        ):
            DifferentialRelay(
                relay_id="D3",
                slope_percent=25.0,
                minimum_operate_current_A=-1.0,
                restraint_current_A=5.0,
            )


# ── IEEE C37.112 inverse-time curve ────────────────────────────────


class TestInverseTimeCurve:
    """IEEE C37.112 equation: t = TDS * (A / ((I/Ip)^p - 1) + B)."""

    def test_moderately_inverse_known_value(self):
        # A=0.0515, B=0.114, p=0.02; I/Ip=5, TDS=1.0
        M = (5.0**0.02) - 1.0
        expected = 1.0 * (0.0515 / M + 0.114)
        result = inverse_time_curve(
            fault_current_A=500.0,
            pickup_current_A=100.0,
            time_dial_setting=1.0,
            curve_type=CurveType.MODERATELY_INVERSE,
        )
        assert result == pytest.approx(expected, rel=1e-6)

    def test_very_inverse_known_value(self):
        # A=19.61, B=0.491, p=2.0; I/Ip=3, TDS=0.5
        M = (3.0**2.0) - 1.0  # 8.0
        expected = 0.5 * (19.61 / M + 0.491)
        result = inverse_time_curve(
            fault_current_A=300.0,
            pickup_current_A=100.0,
            time_dial_setting=0.5,
            curve_type=CurveType.VERY_INVERSE,
        )
        assert result == pytest.approx(expected, rel=1e-6)

    def test_extremely_inverse_known_value(self):
        # A=28.2, B=0.1217, p=2.0; I/Ip=10, TDS=2.0
        M = (10.0**2.0) - 1.0  # 99.0
        expected = 2.0 * (28.2 / M + 0.1217)
        result = inverse_time_curve(
            fault_current_A=1000.0,
            pickup_current_A=100.0,
            time_dial_setting=2.0,
            curve_type=CurveType.EXTREMELY_INVERSE,
        )
        assert result == pytest.approx(expected, rel=1e-6)

    def test_definite_time_returns_tds(self):
        result = inverse_time_curve(
            fault_current_A=500.0,
            pickup_current_A=100.0,
            time_dial_setting=0.3,
            curve_type=CurveType.DEFINITE_TIME,
        )
        assert result == pytest.approx(0.3)

    def test_current_at_pickup_raises(self):
        with pytest.raises(ValueError, match="must exceed pickup"):
            inverse_time_curve(
                fault_current_A=100.0,
                pickup_current_A=100.0,
                time_dial_setting=1.0,
                curve_type=CurveType.MODERATELY_INVERSE,
            )

    def test_current_below_pickup_raises(self):
        with pytest.raises(ValueError, match="must exceed pickup"):
            inverse_time_curve(
                fault_current_A=50.0,
                pickup_current_A=100.0,
                time_dial_setting=1.0,
                curve_type=CurveType.VERY_INVERSE,
            )


# ── Coordination check ─────────────────────────────────────────────


class TestCoordinationCheck:
    @pytest.fixture()
    def downstream_relay(self):
        return OvercurrentRelay(
            relay_id="DS",
            relay_type=RelayType.OVERCURRENT_51,
            curve_type=CurveType.VERY_INVERSE,
            pickup_current_A=100.0,
            time_dial_setting=0.5,
        )

    @pytest.fixture()
    def upstream_relay(self):
        return OvercurrentRelay(
            relay_id="US",
            relay_type=RelayType.OVERCURRENT_51,
            curve_type=CurveType.VERY_INVERSE,
            pickup_current_A=100.0,
            time_dial_setting=2.0,
        )

    def test_coordination_pass(self, upstream_relay, downstream_relay):
        result = coordination_check(
            upstream=upstream_relay,
            downstream=downstream_relay,
            fault_current_A=500.0,
            min_margin_s=0.3,
        )
        assert result["pass"] is True
        assert result["margin_s"] >= 0.3
        assert result["upstream_time_s"] > result["downstream_time_s"]

    def test_coordination_fail_insufficient_margin(self):
        # Both relays nearly identical settings -> tiny margin
        r1 = OvercurrentRelay(
            relay_id="US",
            relay_type=RelayType.OVERCURRENT_51,
            curve_type=CurveType.VERY_INVERSE,
            pickup_current_A=100.0,
            time_dial_setting=0.55,
        )
        r2 = OvercurrentRelay(
            relay_id="DS",
            relay_type=RelayType.OVERCURRENT_51,
            curve_type=CurveType.VERY_INVERSE,
            pickup_current_A=100.0,
            time_dial_setting=0.5,
        )
        result = coordination_check(
            upstream=r1, downstream=r2, fault_current_A=500.0, min_margin_s=0.3
        )
        assert result["pass"] is False
        assert result["margin_s"] < 0.3

    def test_coordination_result_keys(self, upstream_relay, downstream_relay):
        result = coordination_check(
            upstream=upstream_relay,
            downstream=downstream_relay,
            fault_current_A=500.0,
        )
        assert set(result.keys()) == {
            "pass",
            "margin_s",
            "upstream_time_s",
            "downstream_time_s",
        }


# ── Time-current intersection ──────────────────────────────────────


class TestTimecurrentIntersection:
    def test_intersection_found(self):
        r1 = OvercurrentRelay(
            relay_id="A",
            relay_type=RelayType.OVERCURRENT_51,
            curve_type=CurveType.MODERATELY_INVERSE,
            pickup_current_A=100.0,
            time_dial_setting=1.0,
        )
        r2 = OvercurrentRelay(
            relay_id="B",
            relay_type=RelayType.OVERCURRENT_51,
            curve_type=CurveType.VERY_INVERSE,
            pickup_current_A=100.0,
            time_dial_setting=1.0,
        )
        current_range = (150.0, 5000.0)
        result = time_current_intersection(r1, r2, current_range)
        assert result is not None
        i_cross, t_cross = result
        # Verify both relays trip at approximately the same time
        t1 = inverse_time_curve(i_cross, r1.pickup_current_A, r1.time_dial_setting, r1.curve_type)
        t2 = inverse_time_curve(i_cross, r2.pickup_current_A, r2.time_dial_setting, r2.curve_type)
        assert t1 == pytest.approx(t2, rel=1e-2)
        assert t_cross == pytest.approx(t1, rel=1e-2)

    def test_no_intersection_returns_none(self):
        # Same curve type, same pickup, different TDS -> parallel, no crossing
        r1 = OvercurrentRelay(
            relay_id="A",
            relay_type=RelayType.OVERCURRENT_51,
            curve_type=CurveType.VERY_INVERSE,
            pickup_current_A=100.0,
            time_dial_setting=1.0,
        )
        r2 = OvercurrentRelay(
            relay_id="B",
            relay_type=RelayType.OVERCURRENT_51,
            curve_type=CurveType.VERY_INVERSE,
            pickup_current_A=100.0,
            time_dial_setting=2.0,
        )
        result = time_current_intersection(r1, r2, (150.0, 5000.0))
        assert result is None
