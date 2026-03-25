"""
ABOUTME: Tests for Stroud parachute sizing chart logic
ABOUTME: WRK-1362 — single vs dual threshold, model lookup
"""

from digitalmodel.structural.parachute.stroud_sizing import (
    StroudRecommendation,
    recommend_stroud_chute,
)


class TestStroudRecommendation:
    """Stroud sizing chart logic: single vs dual threshold."""

    def test_3600lb_200mph_single(self):
        """3600 lb at 200 MPH: single chute adequate (430 Std. 32)."""
        rec = recommend_stroud_chute(
            vehicle_weight_lbs=3600.0, speed_mph=200.0
        )
        assert rec.config == "single"
        assert "430" in rec.model

    def test_3600lb_250mph_dual(self):
        """3600 lb at 250 MPH: dual chutes required per NHRA/Stroud."""
        rec = recommend_stroud_chute(
            vehicle_weight_lbs=3600.0, speed_mph=250.0
        )
        assert rec.config == "dual"

    def test_light_car_200mph_single(self):
        """2000 lb at 200 MPH: single chute (model 400-410 range)."""
        rec = recommend_stroud_chute(
            vehicle_weight_lbs=2000.0, speed_mph=200.0
        )
        assert rec.config == "single"

    def test_heavy_car_300mph_dual(self):
        """4000 lb at 300 MPH: dual chutes required."""
        rec = recommend_stroud_chute(
            vehicle_weight_lbs=4000.0, speed_mph=300.0
        )
        assert rec.config == "dual"

    def test_returns_recommendation_dataclass(self):
        rec = recommend_stroud_chute(3600.0, 200.0)
        assert isinstance(rec, StroudRecommendation)
        assert rec.vehicle_weight_lbs == 3600.0
        assert rec.speed_mph == 200.0

    def test_threshold_at_200mph(self):
        """At exactly 200 MPH, single chute is still adequate."""
        rec = recommend_stroud_chute(3600.0, 200.0)
        assert rec.config == "single"

    def test_threshold_above_200mph(self):
        """At 201 MPH, dual chutes required."""
        rec = recommend_stroud_chute(3600.0, 201.0)
        assert rec.config == "dual"

    def test_import_from_stroud_sizing_module(self):
        """Verify stroud_sizing is importable as standalone module."""
        from digitalmodel.structural.parachute import stroud_sizing
        assert hasattr(stroud_sizing, "recommend_stroud_chute")
        assert hasattr(stroud_sizing, "DUAL_CHUTE_SPEED_THRESHOLD_MPH")
