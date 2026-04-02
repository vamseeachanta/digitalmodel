"""Tests for digitalmodel.orcaflex.pipelay_analysis module."""

import math

import pytest

from digitalmodel.orcaflex.pipelay_analysis import (
    JLayConfig,
    LayOperability,
    PipelayPipeProperties,
    SLayConfig,
    calculate_stinger_radius,
)


class TestPipelayPipeProperties:
    """Tests for pipelay pipe properties."""

    def test_moment_of_inertia_positive(self):
        """Moment of inertia should be positive."""
        pipe = PipelayPipeProperties()
        assert pipe.moment_of_inertia > 0

    def test_bending_stiffness_positive(self):
        """EI should be positive."""
        pipe = PipelayPipeProperties()
        assert pipe.bending_stiffness > 0

    def test_submerged_weight_positive(self):
        """Concrete-coated pipe should sink."""
        pipe = PipelayPipeProperties(coating_thickness=0.06, coating_density=2400.0)
        assert pipe.submerged_weight_per_m > 0


class TestSLayConfig:
    """Tests for S-Lay configuration and analysis."""

    def test_overbend_stress(self):
        """Overbend stress should be proportional to D/R."""
        slay = SLayConfig(stinger_radius=120.0)
        result = slay.calculate_overbend_stress()
        assert result["overbend_bending_strain"] > 0
        assert result["overbend_bending_stress_MPa"] > 0

    def test_min_stinger_radius(self):
        """Minimum stinger radius should satisfy strain limit."""
        slay = SLayConfig(strain_limit_overbend=0.002)
        result = slay.calculate_overbend_stress()
        assert result["min_stinger_radius_m"] > 0
        # If utilisation > 1, stinger radius is too small
        assert result["overbend_strain_utilisation"] > 0

    def test_sagbend_stress(self):
        """Sagbend stress should be computed."""
        slay = SLayConfig(lay_tension=500.0, departure_angle=60.0)
        result = slay.calculate_sagbend_stress()
        assert result["sagbend_radius_m"] > 0
        assert result["sagbend_bending_stress_MPa"] > 0

    def test_departure_angle_table(self):
        """Departure angle table should have multiple entries."""
        slay = SLayConfig(water_depth=200.0)
        table = slay.calculate_departure_angle_table()
        assert len(table) > 0
        for row in table:
            assert "water_depth_m" in row
            assert "departure_angle_deg" in row


class TestJLayConfig:
    """Tests for J-Lay configuration."""

    def test_sagbend_calculation(self):
        """J-Lay sagbend should produce valid results."""
        jlay = JLayConfig(water_depth=1500.0, top_tension=800.0, tower_angle=3.0)
        result = jlay.calculate_sagbend()
        assert result["sagbend_radius_m"] > 0
        assert result["suspended_length_m"] > 0

    def test_near_vertical_departure(self):
        """J-Lay with small tower angle should have long suspended length."""
        jlay = JLayConfig(water_depth=2000.0, tower_angle=2.0)
        result = jlay.calculate_sagbend()
        assert result["suspended_length_m"] > 2000.0

    def test_deeper_water_longer_span(self):
        """Deeper water should give longer horizontal offset."""
        jlay_deep = JLayConfig(water_depth=2000.0)
        jlay_shallow = JLayConfig(water_depth=500.0)
        deep_result = jlay_deep.calculate_sagbend()
        shallow_result = jlay_shallow.calculate_sagbend()
        assert deep_result["horizontal_offset_m"] > shallow_result["horizontal_offset_m"]


class TestLayOperability:
    """Tests for lay rate operability."""

    def test_calm_seas_max_rate(self):
        """Calm seas should give maximum lay rate."""
        op = LayOperability(lay_rate_calm=4.0)
        assert op.lay_rate_at_hs(0.0) == pytest.approx(4.0)

    def test_above_limit_zero_rate(self):
        """Sea state above limit should give zero lay rate."""
        op = LayOperability(max_hs_lay=2.5)
        assert op.lay_rate_at_hs(3.0) == 0.0

    def test_operability_table_format(self):
        """Operability table should have correct structure."""
        op = LayOperability()
        table = op.generate_operability_table()
        assert len(table) > 0
        for row in table:
            assert "hs_m" in row
            assert "lay_rate_km_per_day" in row


class TestStingerRadius:
    """Tests for stinger radius calculation."""

    def test_basic_calculation(self):
        """Min stinger radius should be D/(2*strain)."""
        R = calculate_stinger_radius(pipe_od=0.3, max_overbend_strain=0.002)
        expected = 0.3 / (2 * 0.002)
        assert R == pytest.approx(expected)

    def test_larger_pipe_needs_larger_radius(self):
        """Larger pipe should need larger stinger radius."""
        R_small = calculate_stinger_radius(pipe_od=0.2)
        R_large = calculate_stinger_radius(pipe_od=0.4)
        assert R_large > R_small

    def test_tighter_strain_needs_larger_radius(self):
        """Tighter strain limit should need larger radius."""
        R_loose = calculate_stinger_radius(pipe_od=0.3, max_overbend_strain=0.003)
        R_tight = calculate_stinger_radius(pipe_od=0.3, max_overbend_strain=0.001)
        assert R_tight > R_loose
