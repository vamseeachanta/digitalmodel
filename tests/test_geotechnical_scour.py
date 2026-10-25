# ABOUTME: Tests for guarded scour screening and explicit missing design basis.
# ABOUTME: Pipeline scour, monopile scour, and rock armour protection.
"""Legacy calls preserve geometry but no longer assert unsourced depths."""
import pytest


class TestPipelineScour:
    def test_missing_pipeline_depth_model(self):
        """An unqualified model cannot establish a nonzero scour depth."""
        from digitalmodel.geotechnical.scour import pipeline_scour_depth

        result = pipeline_scour_depth(
            pipe_od_m=0.5,
            current_velocity_ms=0.8,
            wave_orbital_velocity_ms=0.3,
            sediment_d50_mm=0.3,
            burial_ratio=0.0,
        )
        assert result.scour_depth_m is None
        assert result.status == "NOT_EVALUATED"
        assert result.standard == "NOT_ESTABLISHED"

    def test_burial_does_not_qualify_depth(self):
        """A burial ratio cannot substitute for a qualified scour model."""
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
        assert buried.scour_depth_m is exposed.scour_depth_m is None
        assert buried.status == exposed.status == "NOT_EVALUATED"

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
    def test_missing_monopile_depth_model(self):
        """No nonzero depth is returned without a qualified model."""
        from digitalmodel.geotechnical.scour import monopile_scour_depth

        result = monopile_scour_depth(
            pile_diameter_m=6.0,
            current_velocity_ms=1.0,
            water_depth_m=30.0,
        )
        assert result.scour_depth_m is None
        assert result.scour_to_diameter_ratio is None
        assert result.status == "NOT_EVALUATED"

    def test_diameter_does_not_qualify_depth(self):
        """Changing diameter alone cannot qualify a scour-depth result."""
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
        assert large.scour_depth_m is small.scour_depth_m is None
        assert large.pile_diameter_m == 8.0
        assert small.pile_diameter_m == 3.0

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
        """Missing criteria cannot establish armour thickness or stability."""
        from digitalmodel.geotechnical.scour import rock_armour_thickness

        result = rock_armour_thickness(
            current_velocity_ms=1.0,
            rock_density_kg_m3=2650.0,
            water_density_kg_m3=1025.0,
            rock_d50_m=0.3,
        )
        assert result.thickness_m is None
        assert result.stability_number is None
        assert result.stability_status == "NOT_EVALUATED"
