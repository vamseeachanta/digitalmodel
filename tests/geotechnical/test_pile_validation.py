# ABOUTME: Boundary / validation tests for pile input ranges per API RP 2GEO spec (#1814).
# ABOUTME: Tests written before implementation (TDD) — validates validate_pile_inputs().
"""
Tests for input range validation in piles.py per API RP 2GEO spec.

Valid ranges (from dark-intel YAML):
  D (pile_diameter_m):    [0.3, 3.0] m
  L (pile_length_m):      [10, 100]  m
  Su (su_kpa):            [10, 200]  kPa
  sigma_v (sigma_v_kpa):  [10, 500]  kPa
"""

import pytest
from digitalmodel.geotechnical.piles import (
    validate_pile_inputs,
    skin_friction_clay,
    axial_capacity,
)


class TestValidatePileInputsFunction:
    """Unit tests for the validate_pile_inputs() helper."""

    # --- Diameter bounds ---
    def test_diameter_at_min_valid(self):
        validate_pile_inputs(pile_diameter_m=0.3, pile_length_m=30.0, su_kpa=60.0, sigma_v_kpa=150.0)

    def test_diameter_at_max_valid(self):
        validate_pile_inputs(pile_diameter_m=3.0, pile_length_m=30.0, su_kpa=60.0, sigma_v_kpa=150.0)

    def test_diameter_below_min_raises(self):
        with pytest.raises(ValueError, match="pile_diameter_m"):
            validate_pile_inputs(pile_diameter_m=0.1, pile_length_m=30.0, su_kpa=60.0, sigma_v_kpa=150.0)

    def test_diameter_above_max_raises(self):
        with pytest.raises(ValueError, match="pile_diameter_m"):
            validate_pile_inputs(pile_diameter_m=5.0, pile_length_m=30.0, su_kpa=60.0, sigma_v_kpa=150.0)

    # --- Length bounds ---
    def test_length_at_min_valid(self):
        validate_pile_inputs(pile_diameter_m=1.0, pile_length_m=10.0, su_kpa=60.0, sigma_v_kpa=150.0)

    def test_length_at_max_valid(self):
        validate_pile_inputs(pile_diameter_m=1.0, pile_length_m=100.0, su_kpa=60.0, sigma_v_kpa=150.0)

    def test_length_below_min_raises(self):
        with pytest.raises(ValueError, match="pile_length_m"):
            validate_pile_inputs(pile_diameter_m=1.0, pile_length_m=5.0, su_kpa=60.0, sigma_v_kpa=150.0)

    def test_length_above_max_raises(self):
        with pytest.raises(ValueError, match="pile_length_m"):
            validate_pile_inputs(pile_diameter_m=1.0, pile_length_m=150.0, su_kpa=60.0, sigma_v_kpa=150.0)

    # --- Su bounds ---
    def test_su_at_min_valid(self):
        validate_pile_inputs(pile_diameter_m=1.0, pile_length_m=30.0, su_kpa=10.0, sigma_v_kpa=150.0)

    def test_su_at_max_valid(self):
        validate_pile_inputs(pile_diameter_m=1.0, pile_length_m=30.0, su_kpa=200.0, sigma_v_kpa=500.0)

    def test_su_below_min_raises(self):
        with pytest.raises(ValueError, match="su_kpa"):
            validate_pile_inputs(pile_diameter_m=1.0, pile_length_m=30.0, su_kpa=5.0, sigma_v_kpa=150.0)

    def test_su_above_max_raises(self):
        with pytest.raises(ValueError, match="su_kpa"):
            validate_pile_inputs(pile_diameter_m=1.0, pile_length_m=30.0, su_kpa=250.0, sigma_v_kpa=150.0)

    # --- sigma_v bounds ---
    def test_sigma_v_at_min_valid(self):
        validate_pile_inputs(pile_diameter_m=1.0, pile_length_m=30.0, su_kpa=60.0, sigma_v_kpa=10.0)

    def test_sigma_v_at_max_valid(self):
        validate_pile_inputs(pile_diameter_m=1.0, pile_length_m=30.0, su_kpa=60.0, sigma_v_kpa=500.0)

    def test_sigma_v_below_min_raises(self):
        with pytest.raises(ValueError, match="sigma_v_kpa"):
            validate_pile_inputs(pile_diameter_m=1.0, pile_length_m=30.0, su_kpa=60.0, sigma_v_kpa=5.0)

    def test_sigma_v_above_max_raises(self):
        with pytest.raises(ValueError, match="sigma_v_kpa"):
            validate_pile_inputs(pile_diameter_m=1.0, pile_length_m=30.0, su_kpa=60.0, sigma_v_kpa=600.0)


class TestSkinFrictionClayValidation:
    """Tests for optional validate=True in skin_friction_clay()."""

    def test_valid_inputs_no_error(self):
        result = skin_friction_clay(
            undrained_shear_strength_kpa=60.0,
            effective_overburden_kpa=150.0,
            validate=True,
        )
        assert result.alpha == pytest.approx(0.7906, abs=0.001)

    def test_su_out_of_range_raises_with_validate(self):
        with pytest.raises(ValueError, match="su_kpa"):
            skin_friction_clay(
                undrained_shear_strength_kpa=5.0,
                effective_overburden_kpa=150.0,
                validate=True,
            )

    def test_sigma_v_out_of_range_raises_with_validate(self):
        with pytest.raises(ValueError, match="sigma_v_kpa"):
            skin_friction_clay(
                undrained_shear_strength_kpa=60.0,
                effective_overburden_kpa=5.0,
                validate=True,
            )

    def test_no_validation_by_default_out_of_range(self):
        """Without validate=True, out-of-range values should NOT raise."""
        result = skin_friction_clay(
            undrained_shear_strength_kpa=5.0,
            effective_overburden_kpa=150.0,
        )
        assert result.alpha > 0


class TestAxialCapacityValidation:
    """Tests for optional validate=True in axial_capacity()."""

    def _layer(self, su=60.0, sigma_v=150.0, thickness=30.0):
        return [{"type": "clay", "thickness_m": thickness, "su_kpa": su, "sigma_v_kpa": sigma_v}]

    def test_valid_inputs_no_error(self):
        result = axial_capacity(
            pile_diameter_m=1.0,
            pile_length_m=30.0,
            layers=self._layer(),
            validate=True,
        )
        assert result.total_capacity_kn > 0

    def test_diameter_out_of_range_raises_with_validate(self):
        with pytest.raises(ValueError, match="pile_diameter_m"):
            axial_capacity(
                pile_diameter_m=0.1,
                pile_length_m=30.0,
                layers=self._layer(),
                validate=True,
            )

    def test_length_out_of_range_raises_with_validate(self):
        with pytest.raises(ValueError, match="pile_length_m"):
            axial_capacity(
                pile_diameter_m=1.0,
                pile_length_m=5.0,
                layers=self._layer(),
                validate=True,
            )

    def test_no_validation_by_default(self):
        """Without validate=True, out-of-range pile dims should NOT raise from validation."""
        # piles.py basic guard still triggers for <=0, so use in-range but
        # outside API spec to confirm validation isn't applied by default
        result = axial_capacity(
            pile_diameter_m=0.2,   # below spec min of 0.3 but > 0
            pile_length_m=30.0,
            layers=self._layer(),
        )
        assert result.total_capacity_kn > 0
