"""Tests for environmental_correction — environment correction factors."""

import pytest

from digitalmodel.fatigue.environmental_correction import (
    environment_correction,
    zone_factor,
    temperature_derating_factor,
    apply_environment_to_sn,
    EnvironmentInput,
)


class TestZoneFactor:
    """Test structural zone factors."""

    def test_atmospheric_is_one(self):
        assert zone_factor("atmospheric") == 1.0

    def test_splash_less_than_one(self):
        f = zone_factor("splash")
        assert f < 1.0
        assert f > 0.0

    def test_submerged_is_one(self):
        assert zone_factor("submerged") == 1.0

    def test_invalid_zone_raises(self):
        with pytest.raises(ValueError):
            zone_factor("deep_space")


class TestTemperatureDerating:
    """Test temperature derating factor."""

    def test_ambient_no_derating(self):
        """At 20°C, no derating needed."""
        assert temperature_derating_factor(20.0) == 1.0

    def test_100c_no_derating(self):
        """At 100°C, no derating per DNV."""
        assert temperature_derating_factor(100.0) == 1.0

    def test_200c_has_derating(self):
        """At 200°C, derating factor < 1."""
        f = temperature_derating_factor(200.0)
        assert f < 1.0
        assert f > 0.8  # roughly 0.93

    def test_api_derating(self):
        """API derating at 200°C."""
        f_api = temperature_derating_factor(200.0, standard="API")
        f_dnv = temperature_derating_factor(200.0, standard="DNV")
        # API is slightly more conservative
        assert f_api <= f_dnv


class TestEnvironmentCorrection:
    """Test main environment correction function."""

    def test_air_baseline(self):
        """Air environment → life_factor = 1.0."""
        inp = EnvironmentInput(environment="air")
        result = environment_correction(inp)
        assert result.life_factor == 1.0
        assert result.log_a_adjustment == 0.0
        assert result.endurance_limit_factor == 1.0

    def test_seawater_cp_reduces_life(self):
        """Seawater with CP → life_factor < 1.0."""
        inp = EnvironmentInput(environment="seawater_cp")
        result = environment_correction(inp)
        assert result.life_factor < 1.0
        assert result.log_a_adjustment < 0.0

    def test_free_corrosion_removes_endurance_limit(self):
        """Free corrosion → endurance_limit_factor = 0, slope change."""
        inp = EnvironmentInput(environment="free_corrosion")
        result = environment_correction(inp)
        assert result.endurance_limit_factor == 0.0
        assert result.slope_change is True

    def test_splash_zone_penalty(self):
        """Splash zone adds penalty beyond base environment."""
        inp_sub = EnvironmentInput(environment="seawater_cp", zone="submerged")
        inp_splash = EnvironmentInput(environment="seawater_cp", zone="splash")
        r_sub = environment_correction(inp_sub)
        r_splash = environment_correction(inp_splash)
        assert r_splash.life_factor < r_sub.life_factor

    def test_marginal_cp_potential(self):
        """CP potential above -750 mV → extra penalty."""
        inp_good = EnvironmentInput(environment="seawater_cp", cp_potential=-850.0)
        inp_bad = EnvironmentInput(environment="seawater_cp", cp_potential=-700.0)
        r_good = environment_correction(inp_good)
        r_bad = environment_correction(inp_bad)
        assert r_bad.life_factor < r_good.life_factor


class TestApplyToSN:
    """Test applying environmental correction to S-N parameters."""

    def test_air_no_change(self):
        inp = EnvironmentInput(environment="air")
        result = apply_environment_to_sn(12.164, 3.0, 52.63, inp)
        assert abs(result["log_a"] - 12.164) < 0.001
        assert result["endurance_limit"] == 52.63

    def test_free_corrosion_removes_limit(self):
        inp = EnvironmentInput(environment="free_corrosion")
        result = apply_environment_to_sn(12.164, 3.0, 52.63, inp)
        assert result["endurance_limit"] is None
        assert result["log_a"] < 12.164

    def test_seawater_cp_reduces_intercept(self):
        inp = EnvironmentInput(environment="seawater_cp")
        result = apply_environment_to_sn(12.164, 3.0, 52.63, inp)
        assert result["log_a"] < 12.164
