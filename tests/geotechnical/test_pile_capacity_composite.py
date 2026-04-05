# ABOUTME: Tests for pile_capacity.py composite API with alpha_method_capacity() (#1814).
# ABOUTME: Written before implementation (TDD) — tests multi-layer and single-layer cases.
"""
Tests for composite pile capacity API (pile_capacity.py).

Tests:
- alpha_method_capacity(D, L, Su, sigma_v, Nc=9.0) -> PileCapacityResult
- Multi-layer support: list of (thickness, Su, sigma_v) tuples
- Result dataclass fields
- Numeric assertions against dark-intel worked example
"""

import pytest
from digitalmodel.geotechnical.pile_capacity import (
    alpha_method_capacity,
    alpha_method_capacity_multilayer,
    PileCapacityResult,
)


class TestPileCapacityResultDataclass:
    """PileCapacityResult must expose expected fields."""

    def test_result_fields(self):
        result = alpha_method_capacity(D=1.0, L=30.0, Su=60.0, sigma_v=150.0)
        assert hasattr(result, "total_capacity_kn")
        assert hasattr(result, "skin_friction_kn")
        assert hasattr(result, "end_bearing_kn")
        assert hasattr(result, "alpha")
        assert hasattr(result, "unit_skin_friction_kpa")
        assert hasattr(result, "standard")

    def test_standard_tag(self):
        result = alpha_method_capacity(D=1.0, L=30.0, Su=60.0, sigma_v=150.0)
        assert result.standard == "API RP 2GEO"


class TestAlphaMethodCapacitySingleLayer:
    """Numeric assertions from dark-intel worked example."""

    def test_alpha_factor(self):
        result = alpha_method_capacity(D=1.0, L=30.0, Su=60.0, sigma_v=150.0)
        assert result.alpha == pytest.approx(0.7906, abs=0.001)

    def test_unit_skin_friction(self):
        result = alpha_method_capacity(D=1.0, L=30.0, Su=60.0, sigma_v=150.0)
        assert result.unit_skin_friction_kpa == pytest.approx(47.43, abs=0.01)

    def test_skin_friction_kn(self):
        result = alpha_method_capacity(D=1.0, L=30.0, Su=60.0, sigma_v=150.0)
        assert result.skin_friction_kn == pytest.approx(4470.56, abs=0.5)

    def test_end_bearing_kn(self):
        result = alpha_method_capacity(D=1.0, L=30.0, Su=60.0, sigma_v=150.0)
        assert result.end_bearing_kn == pytest.approx(424.12, abs=0.5)

    def test_total_capacity_kn(self):
        result = alpha_method_capacity(D=1.0, L=30.0, Su=60.0, sigma_v=150.0)
        assert result.total_capacity_kn == pytest.approx(4894.68, abs=1.0)

    def test_components_sum_to_total(self):
        result = alpha_method_capacity(D=1.0, L=30.0, Su=60.0, sigma_v=150.0)
        assert result.total_capacity_kn == pytest.approx(
            result.skin_friction_kn + result.end_bearing_kn, abs=1e-6
        )

    def test_custom_nc(self):
        """Using a different Nc should change end bearing."""
        result_nc9 = alpha_method_capacity(D=1.0, L=30.0, Su=60.0, sigma_v=150.0, Nc=9.0)
        result_nc8 = alpha_method_capacity(D=1.0, L=30.0, Su=60.0, sigma_v=150.0, Nc=8.0)
        assert result_nc8.end_bearing_kn < result_nc9.end_bearing_kn

    def test_larger_pile_larger_capacity(self):
        """Doubling diameter should roughly increase capacity."""
        result_1m = alpha_method_capacity(D=1.0, L=30.0, Su=60.0, sigma_v=150.0)
        result_2m = alpha_method_capacity(D=2.0, L=30.0, Su=60.0, sigma_v=150.0)
        assert result_2m.total_capacity_kn > result_1m.total_capacity_kn

    def test_longer_pile_larger_skin_friction(self):
        """Longer pile should have more skin friction."""
        result_30 = alpha_method_capacity(D=1.0, L=30.0, Su=60.0, sigma_v=150.0)
        result_60 = alpha_method_capacity(D=1.0, L=60.0, Su=60.0, sigma_v=150.0)
        assert result_60.skin_friction_kn == pytest.approx(2.0 * result_30.skin_friction_kn, rel=1e-6)


class TestAlphaMethodCapacityMultiLayer:
    """Multi-layer pile capacity with list of (thickness, Su, sigma_v) tuples."""

    def test_single_layer_matches_simple_api(self):
        """Multi-layer with one layer should match alpha_method_capacity."""
        result_simple = alpha_method_capacity(D=1.0, L=30.0, Su=60.0, sigma_v=150.0)
        result_multi = alpha_method_capacity_multilayer(
            D=1.0,
            layers=[(30.0, 60.0, 150.0)],
        )
        assert result_multi.skin_friction_kn == pytest.approx(result_simple.skin_friction_kn, abs=0.5)
        assert result_multi.end_bearing_kn == pytest.approx(result_simple.end_bearing_kn, abs=0.5)
        assert result_multi.total_capacity_kn == pytest.approx(result_simple.total_capacity_kn, abs=1.0)

    def test_two_layers_skin_friction_sums(self):
        """Two equal layers should give 2x the skin friction of one layer."""
        result_single = alpha_method_capacity(D=1.0, L=15.0, Su=60.0, sigma_v=150.0)
        result_double = alpha_method_capacity_multilayer(
            D=1.0,
            layers=[(15.0, 60.0, 150.0), (15.0, 60.0, 150.0)],
        )
        assert result_double.skin_friction_kn == pytest.approx(
            2.0 * result_single.skin_friction_kn, abs=0.5
        )

    def test_two_layers_end_bearing_from_last(self):
        """End bearing should come from the last (tip) layer."""
        result_tip_60 = alpha_method_capacity_multilayer(
            D=1.0,
            layers=[(15.0, 80.0, 200.0), (15.0, 60.0, 150.0)],
        )
        result_tip_80 = alpha_method_capacity_multilayer(
            D=1.0,
            layers=[(15.0, 60.0, 150.0), (15.0, 80.0, 200.0)],
        )
        # End bearing for Su=60 vs Su=80 should differ
        assert result_tip_60.end_bearing_kn != pytest.approx(result_tip_80.end_bearing_kn, abs=1.0)

    def test_multilayer_returns_pile_capacity_result(self):
        result = alpha_method_capacity_multilayer(
            D=1.0,
            layers=[(30.0, 60.0, 150.0)],
        )
        assert isinstance(result, PileCapacityResult)

    def test_empty_layers_raises(self):
        with pytest.raises(ValueError, match="layers"):
            alpha_method_capacity_multilayer(D=1.0, layers=[])

    def test_three_layers_total_capacity_positive(self):
        result = alpha_method_capacity_multilayer(
            D=1.0,
            layers=[
                (10.0, 40.0, 100.0),
                (10.0, 60.0, 150.0),
                (10.0, 80.0, 200.0),
            ],
        )
        assert result.total_capacity_kn > 0
        assert result.skin_friction_kn > 0
        assert result.end_bearing_kn > 0


class TestAlphaMethodCapacityEdgeCases:
    """Edge cases for single-layer alpha_method_capacity."""

    def test_positive_inputs_give_positive_output(self):
        result = alpha_method_capacity(D=0.5, L=20.0, Su=30.0, sigma_v=80.0)
        assert result.total_capacity_kn > 0

    def test_high_su_sigma_v_ratio_uses_upper_branch(self):
        """For Su/sigma_v' > 1.0, uses -0.25 exponent (alpha capped behavior)."""
        # Su > sigma_v means psi > 1.0 → upper branch
        result = alpha_method_capacity(D=1.0, L=30.0, Su=200.0, sigma_v=100.0)
        # alpha = 0.5 * 2.0^(-0.25) = 0.5 * 0.8409 = 0.4204
        import math
        expected_alpha = 0.5 * (200.0 / 100.0) ** (-0.25)
        assert result.alpha == pytest.approx(expected_alpha, abs=0.001)
