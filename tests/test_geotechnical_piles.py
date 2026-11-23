# ABOUTME: Tests for geotechnical pile capacity — API RP 2GEO method.
"""Tests for geotechnical pile capacity — API RP 2GEO method."""
import pytest


class TestPileAxialCapacity:
    def test_skin_friction_clay_alpha_method(self):
        """Alpha method for skin friction in clay per API RP 2GEO Sec 7."""
        from digitalmodel.geotechnical.piles import skin_friction_clay

        # Soft clay: Su=50 kPa, alpha~0.7 for Su/sigma_v'=0.5
        result = skin_friction_clay(
            undrained_shear_strength_kpa=50.0,
            effective_overburden_kpa=100.0,
        )
        assert result.unit_friction_kpa > 0
        assert result.unit_friction_kpa < 50.0  # alpha < 1.0
        assert result.alpha > 0 and result.alpha <= 1.0

    def test_skin_friction_clay_high_ratio(self):
        """Alpha method when Su/sigma_v' > 1.0 uses different exponent."""
        from digitalmodel.geotechnical.piles import skin_friction_clay

        result = skin_friction_clay(
            undrained_shear_strength_kpa=200.0,
            effective_overburden_kpa=100.0,
        )
        assert result.alpha > 0 and result.alpha <= 1.0
        assert result.unit_friction_kpa > 0

    def test_skin_friction_sand_beta_method(self):
        """Beta method for skin friction in sand per API RP 2GEO Sec 8."""
        from digitalmodel.geotechnical.piles import skin_friction_sand

        result = skin_friction_sand(
            effective_overburden_kpa=100.0,
            friction_angle_deg=30.0,
            k0=0.8,
        )
        assert result.unit_friction_kpa > 0
        assert result.beta > 0

    def test_end_bearing_clay(self):
        """End bearing in clay per API RP 2GEO Sec 7.4."""
        from digitalmodel.geotechnical.piles import end_bearing_clay

        result = end_bearing_clay(
            undrained_shear_strength_kpa=100.0,
            nc=9.0,
        )
        assert abs(result.unit_bearing_kpa - 900.0) < 1e-6

    def test_end_bearing_sand(self):
        """End bearing in sand per API RP 2GEO Sec 8.3."""
        from digitalmodel.geotechnical.piles import end_bearing_sand

        result = end_bearing_sand(
            effective_overburden_kpa=200.0,
            nq=40.0,
        )
        assert abs(result.unit_bearing_kpa - 8000.0) < 1e-6

    def test_total_axial_capacity_single_clay(self):
        """Total pile capacity = sum of skin friction + end bearing."""
        from digitalmodel.geotechnical.piles import axial_capacity

        result = axial_capacity(
            pile_diameter_m=1.0,
            pile_length_m=30.0,
            layers=[
                {
                    "type": "clay",
                    "thickness_m": 30.0,
                    "su_kpa": 60.0,
                    "sigma_v_kpa": 150.0,
                }
            ],
        )
        assert result.total_capacity_kn > 0
        assert result.skin_friction_kn > 0
        assert result.end_bearing_kn > 0
        assert (
            abs(
                result.total_capacity_kn
                - result.skin_friction_kn
                - result.end_bearing_kn
            )
            < 1e-6
        )
        assert result.standard == "API RP 2GEO"

    def test_total_axial_capacity_single_sand(self):
        """Total pile capacity for a sand layer."""
        from digitalmodel.geotechnical.piles import axial_capacity

        result = axial_capacity(
            pile_diameter_m=1.0,
            pile_length_m=20.0,
            layers=[
                {
                    "type": "sand",
                    "thickness_m": 20.0,
                    "phi_deg": 30.0,
                    "sigma_v_kpa": 100.0,
                    "k0": 0.8,
                }
            ],
        )
        assert result.total_capacity_kn > 0
        assert result.skin_friction_kn > 0
        assert result.end_bearing_kn > 0

    def test_empty_layers_raises(self):
        """Empty layers list should raise ValueError."""
        from digitalmodel.geotechnical.piles import axial_capacity

        with pytest.raises(ValueError):
            axial_capacity(pile_diameter_m=1.0, pile_length_m=10.0, layers=[])

    def test_zero_diameter_raises(self):
        """Zero or negative pile diameter should raise ValueError."""
        from digitalmodel.geotechnical.piles import axial_capacity

        with pytest.raises(ValueError):
            axial_capacity(
                pile_diameter_m=0.0,
                pile_length_m=10.0,
                layers=[
                    {
                        "type": "clay",
                        "thickness_m": 10.0,
                        "su_kpa": 50.0,
                        "sigma_v_kpa": 100.0,
                    }
                ],
            )
