# ABOUTME: Skeleton tests for naval_architecture fundamentals module
# ABOUTME: Covers unit conversions, buoyancy, water density interpolation

import pytest

from digitalmodel.naval_architecture.fundamentals import (
    G,
    GAMMA_SW,
    GAMMA_FW,
    LT_TO_LB,
    mass_to_weight,
    displaced_volume_to_weight_lt,
    interpolate_water_density,
)


class TestConstants:
    def test_gravity(self):
        assert G == pytest.approx(32.17, rel=1e-4)

    def test_saltwater_gamma(self):
        assert GAMMA_SW == pytest.approx(64.0, rel=1e-4)

    def test_freshwater_gamma(self):
        assert GAMMA_FW == pytest.approx(62.4, rel=1e-4)

    def test_lt_to_lb(self):
        assert LT_TO_LB == pytest.approx(2240.0, rel=1e-6)


class TestMassToWeight:
    def test_one_slug(self):
        w = mass_to_weight(1.0)
        assert w == pytest.approx(G, rel=1e-6)

    def test_zero_mass(self):
        assert mass_to_weight(0.0) == pytest.approx(0.0, abs=1e-9)

    def test_proportional(self):
        w1 = mass_to_weight(2.0)
        w2 = mass_to_weight(4.0)
        assert w2 == pytest.approx(2 * w1, rel=1e-6)


class TestDisplacedVolumeToWeight:
    def test_saltwater(self):
        v = 2240.0 / 64.0  # volume that displaces exactly 1 LT in saltwater
        w_lt = displaced_volume_to_weight_lt(v, water="saltwater")
        assert w_lt == pytest.approx(1.0, rel=1e-4)

    def test_freshwater_heavier_volume_needed(self):
        # freshwater less dense, so same volume -> less buoyancy
        v = 1000.0
        w_sw = displaced_volume_to_weight_lt(v, water="saltwater")
        w_fw = displaced_volume_to_weight_lt(v, water="freshwater")
        assert w_sw > w_fw

    def test_zero_volume(self):
        assert displaced_volume_to_weight_lt(0.0) == pytest.approx(0.0, abs=1e-9)


class TestInterpolateWaterDensity:
    def test_at_exact_table_point_60f(self):
        rho = interpolate_water_density(60.0, water="freshwater")
        assert rho == pytest.approx(1.9383, rel=1e-4)

    def test_interpolation_between_points(self):
        rho = interpolate_water_density(62.5, water="freshwater")
        # Between 62 (1.9379) and 63 (1.9377)
        assert 1.9377 <= rho <= 1.9379

    def test_below_range_returns_first_value(self):
        rho = interpolate_water_density(40.0, water="freshwater")
        assert rho == pytest.approx(1.9400, rel=1e-4)

    def test_above_range_returns_last_value(self):
        rho = interpolate_water_density(100.0, water="freshwater")
        assert rho == pytest.approx(1.9333, rel=1e-4)

    def test_saltwater_raises(self):
        with pytest.raises(NotImplementedError):
            interpolate_water_density(60.0, water="saltwater")
