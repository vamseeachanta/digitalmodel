# ABOUTME: Dark-intelligence worked example assertions for API RP 2GEO alpha method (#1814).
# ABOUTME: Exact numeric assertions sourced from dark-intelligence-api-rp-2geo-alpha-method.yaml.
"""
Dark-intel test assertions for API RP 2GEO alpha method pile capacity.

Source YAML:
  knowledge/dark-intelligence/geotechnical/pile_capacity/
  dark-intelligence-api-rp-2geo-alpha-method.yaml

Worked example: 1.0m diameter pile, 30m long, in firm clay
  Su = 60 kPa, sigma_v' = 150 kPa, Nc = 9.0

Expected outputs (per dark-intel YAML):
  alpha                    = 0.7906  (tol 0.001)
  unit_skin_friction_kpa   = 47.43   (tol 0.01)
  skin_friction_capacity   = 4470.56 kN (tol 0.5)
  end_bearing_capacity     = 424.12  kN (tol 0.5)
  total_axial_capacity     = 4894.68 kN (tol 1.0)
"""

import pytest
from digitalmodel.geotechnical.piles import (
    skin_friction_clay,
    axial_capacity,
    SkinFrictionClayResult,
    AxialCapacityResult,
)


# ---------------------------------------------------------------------------
# Test constants from dark-intel YAML
# ---------------------------------------------------------------------------
SU_KPA = 60.0
SIGMA_V_KPA = 150.0
PILE_DIAMETER_M = 1.0
PILE_LENGTH_M = 30.0
NC = 9.0

EXPECTED_ALPHA = 0.7906
EXPECTED_UNIT_FRICTION_KPA = 47.43
EXPECTED_SKIN_FRICTION_KN = 4470.56
EXPECTED_END_BEARING_KN = 424.12
EXPECTED_TOTAL_KN = 4894.68


class TestDarkIntelAlphaFactor:
    """Assert alpha factor against dark-intel worked example."""

    def test_alpha_factor_value(self):
        """Alpha = 0.5 * (Su/sigma_v')^(-0.5) for psi <= 1.0."""
        result = skin_friction_clay(
            undrained_shear_strength_kpa=SU_KPA,
            effective_overburden_kpa=SIGMA_V_KPA,
        )
        assert isinstance(result, SkinFrictionClayResult)
        assert result.alpha == pytest.approx(EXPECTED_ALPHA, abs=0.001), (
            f"alpha: got {result.alpha:.6f}, expected {EXPECTED_ALPHA} +/- 0.001"
        )

    def test_unit_skin_friction_value(self):
        """Unit skin friction f = alpha * Su."""
        result = skin_friction_clay(
            undrained_shear_strength_kpa=SU_KPA,
            effective_overburden_kpa=SIGMA_V_KPA,
        )
        assert result.unit_friction_kpa == pytest.approx(EXPECTED_UNIT_FRICTION_KPA, abs=0.01), (
            f"unit friction: got {result.unit_friction_kpa:.4f} kPa, "
            f"expected {EXPECTED_UNIT_FRICTION_KPA} +/- 0.01"
        )

    def test_standard_tag(self):
        """Result carries the API RP 2GEO standard tag."""
        result = skin_friction_clay(
            undrained_shear_strength_kpa=SU_KPA,
            effective_overburden_kpa=SIGMA_V_KPA,
        )
        assert result.standard == "API RP 2GEO"

    def test_psi_ratio_below_one(self):
        """psi = 60/150 = 0.40 is in the lower branch (psi <= 1.0)."""
        psi = SU_KPA / SIGMA_V_KPA
        assert psi < 1.0, f"psi should be < 1.0, got {psi}"


class TestDarkIntelAxialCapacity:
    """Assert full axial capacity against dark-intel worked example."""

    def _single_clay_layer(self):
        return [
            {
                "type": "clay",
                "thickness_m": PILE_LENGTH_M,
                "su_kpa": SU_KPA,
                "sigma_v_kpa": SIGMA_V_KPA,
            }
        ]

    def test_axial_capacity_returns_dataclass(self):
        result = axial_capacity(
            pile_diameter_m=PILE_DIAMETER_M,
            pile_length_m=PILE_LENGTH_M,
            layers=self._single_clay_layer(),
        )
        assert isinstance(result, AxialCapacityResult)

    def test_skin_friction_capacity_kn(self):
        """Skin friction Q_f = pi * D * L * f ~ 4470.56 kN."""
        result = axial_capacity(
            pile_diameter_m=PILE_DIAMETER_M,
            pile_length_m=PILE_LENGTH_M,
            layers=self._single_clay_layer(),
        )
        assert result.skin_friction_kn == pytest.approx(EXPECTED_SKIN_FRICTION_KN, abs=0.5), (
            f"skin friction: got {result.skin_friction_kn:.3f} kN, "
            f"expected {EXPECTED_SKIN_FRICTION_KN} +/- 0.5"
        )

    def test_end_bearing_capacity_kn(self):
        """End bearing Q_p = Nc * Su * Ap ~ 424.12 kN."""
        result = axial_capacity(
            pile_diameter_m=PILE_DIAMETER_M,
            pile_length_m=PILE_LENGTH_M,
            layers=self._single_clay_layer(),
        )
        assert result.end_bearing_kn == pytest.approx(EXPECTED_END_BEARING_KN, abs=0.5), (
            f"end bearing: got {result.end_bearing_kn:.3f} kN, "
            f"expected {EXPECTED_END_BEARING_KN} +/- 0.5"
        )

    def test_total_axial_capacity_kn(self):
        """Total Q = Q_f + Q_p ~ 4894.68 kN."""
        result = axial_capacity(
            pile_diameter_m=PILE_DIAMETER_M,
            pile_length_m=PILE_LENGTH_M,
            layers=self._single_clay_layer(),
        )
        assert result.total_capacity_kn == pytest.approx(EXPECTED_TOTAL_KN, abs=1.0), (
            f"total capacity: got {result.total_capacity_kn:.3f} kN, "
            f"expected {EXPECTED_TOTAL_KN} +/- 1.0"
        )

    def test_components_sum_to_total(self):
        """skin_friction_kn + end_bearing_kn == total_capacity_kn."""
        result = axial_capacity(
            pile_diameter_m=PILE_DIAMETER_M,
            pile_length_m=PILE_LENGTH_M,
            layers=self._single_clay_layer(),
        )
        assert result.total_capacity_kn == pytest.approx(
            result.skin_friction_kn + result.end_bearing_kn, abs=1e-6
        )

    def test_standard_tag(self):
        result = axial_capacity(
            pile_diameter_m=PILE_DIAMETER_M,
            pile_length_m=PILE_LENGTH_M,
            layers=self._single_clay_layer(),
        )
        assert result.standard == "API RP 2GEO"
