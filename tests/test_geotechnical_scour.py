# ABOUTME: Tests for scour depth prediction per DNV-RP-F107.
# ABOUTME: Pipeline scour, monopile scour, and rock armour protection.
"""Tests for scour depth prediction — DNV-RP-F107."""
import pytest


class TestPipelineScour:
    def test_equilibrium_scour_depth_sand(self):
        """Equilibrium scour depth for pipeline on sandy seabed."""
        from digitalmodel.geotechnical.scour import pipeline_scour_depth

        result = pipeline_scour_depth(
            pipe_od_m=0.5,
            current_velocity_ms=0.8,
            wave_orbital_velocity_ms=0.3,
            sediment_d50_mm=0.3,
            burial_ratio=0.0,
        )
        assert result.scour_depth_m > 0
        assert result.scour_depth_m < 5 * result.pipe_od_m
        assert result.standard == "DNV-RP-F107"

    def test_buried_pipe_less_scour(self):
        """Partially buried pipe has less scour than fully exposed."""
        from digitalmodel.geotechnical.scour import pipeline_scour_depth

        exposed = pipeline_scour_depth(
            pipe_od_m=0.5,
            current_velocity_ms=0.8,
            wave_orbital_velocity_ms=0.3,
            sediment_d50_mm=0.3,
            burial_ratio=0.0,
        )
        buried = pipeline_scour_depth(
            pipe_od_m=0.5,
            current_velocity_ms=0.8,
            wave_orbital_velocity_ms=0.3,
            sediment_d50_mm=0.3,
            burial_ratio=0.5,
        )
        assert buried.scour_depth_m < exposed.scour_depth_m

    def test_zero_velocity_no_scour(self):
        """No flow means no scour."""
        from digitalmodel.geotechnical.scour import pipeline_scour_depth

        result = pipeline_scour_depth(
            pipe_od_m=0.5,
            current_velocity_ms=0.0,
            wave_orbital_velocity_ms=0.0,
            sediment_d50_mm=0.3,
            burial_ratio=0.0,
        )
        assert result.scour_depth_m == 0.0


class TestMonopileScour:
    def test_equilibrium_scour_depth(self):
        """Monopile scour depth per Breusers/Sumer empirical formula."""
        from digitalmodel.geotechnical.scour import monopile_scour_depth

        result = monopile_scour_depth(
            pile_diameter_m=6.0,
            current_velocity_ms=1.0,
            water_depth_m=30.0,
        )
        assert result.scour_depth_m > 0
        assert result.scour_depth_m / 6.0 < 2.5

    def test_larger_pile_more_scour(self):
        """Larger pile diameter produces deeper scour hole."""
        from digitalmodel.geotechnical.scour import monopile_scour_depth

        small = monopile_scour_depth(
            pile_diameter_m=3.0,
            current_velocity_ms=1.0,
            water_depth_m=30.0,
        )
        large = monopile_scour_depth(
            pile_diameter_m=8.0,
            current_velocity_ms=1.0,
            water_depth_m=30.0,
        )
        assert large.scour_depth_m > small.scour_depth_m

    def test_zero_velocity_no_scour(self):
        """No current means no scour."""
        from digitalmodel.geotechnical.scour import monopile_scour_depth

        result = monopile_scour_depth(
            pile_diameter_m=6.0,
            current_velocity_ms=0.0,
            water_depth_m=30.0,
        )
        assert result.scour_depth_m == 0.0

    def test_zero_diameter_raises(self):
        """Zero pile diameter is physically invalid."""
        from digitalmodel.geotechnical.scour import monopile_scour_depth

        with pytest.raises(ValueError):
            monopile_scour_depth(
                pile_diameter_m=0.0,
                current_velocity_ms=1.0,
                water_depth_m=30.0,
            )


class TestScourProtection:
    def test_rock_armour_thickness(self):
        """Minimum rock armour layer thickness for scour protection."""
        from digitalmodel.geotechnical.scour import rock_armour_thickness

        result = rock_armour_thickness(
            current_velocity_ms=1.0,
            rock_density_kg_m3=2650.0,
            water_density_kg_m3=1025.0,
            rock_d50_m=0.3,
        )
        assert result.thickness_m > 0
        assert result.stability_number > 0
