"""Tests for coating breakdown factor calculations."""

import pytest

from digitalmodel.cathodic_protection.coating import (
    CoatingCategory,
    coating_breakdown_factors,
    coating_life_estimate,
    effective_bare_area_coated,
)


def test_fbe_breakdown_factors_25yr():
    """FBE coating: initial=0.02, rate=0.003/yr over 25 years."""
    result = coating_breakdown_factors(
        coating_type=CoatingCategory.FBE,
        design_life_years=25.0,
    )
    assert result.initial_factor == pytest.approx(0.02, abs=0.001)
    # Final: 0.02 + 0.003 * 25 = 0.095
    assert result.final_factor == pytest.approx(0.095, abs=0.001)
    # Mean: 0.02 + 0.003 * 12.5 = 0.0575
    assert result.mean_factor == pytest.approx(0.0575, abs=0.001)


def test_3lpe_coating_low_degradation():
    """3-layer PE has lower degradation than FBE: a=0.01, b=0.002/yr."""
    result = coating_breakdown_factors(
        coating_type=CoatingCategory.THREE_LAYER_PE,
        design_life_years=40.0,
    )
    assert result.initial_factor == pytest.approx(0.01, abs=0.001)
    # Final: 0.01 + 0.002 * 40 = 0.09
    assert result.final_factor == pytest.approx(0.09, abs=0.001)
    assert result.final_factor < 0.10  # 3LPE should stay below 10% at 40 yr


def test_coal_tar_higher_initial_breakdown():
    """Coal tar enamel starts with higher breakdown: a=0.05."""
    result = coating_breakdown_factors(
        coating_type=CoatingCategory.COAL_TAR_ENAMEL,
        design_life_years=20.0,
    )
    assert result.initial_factor == pytest.approx(0.05, abs=0.001)
    # Final: 0.05 + 0.005 * 20 = 0.15
    assert result.final_factor == pytest.approx(0.15, abs=0.01)


def test_temperature_correction_increases_degradation():
    """Elevated temperature (40°C) increases degradation rate."""
    normal = coating_breakdown_factors(
        coating_type=CoatingCategory.FBE,
        design_life_years=25.0,
        temperature_c=20.0,
    )
    elevated = coating_breakdown_factors(
        coating_type=CoatingCategory.FBE,
        design_life_years=25.0,
        temperature_c=40.0,
    )
    assert elevated.final_factor > normal.final_factor


def test_coating_life_estimate_fbe():
    """FBE coating should reach 50% breakdown in ~160 years at 20°C."""
    result = coating_life_estimate(
        coating_type=CoatingCategory.FBE,
        threshold_factor=0.50,
    )
    # (0.50 - 0.02) / 0.003 = 160 years
    assert result.estimated_life_years == pytest.approx(160.0, abs=1.0)
    assert result.time_to_threshold_years is not None


def test_bare_surface_area_increases_with_time():
    """Effective bare area should increase over time."""
    area_5yr = effective_bare_area_coated(
        total_surface_area_m2=1000.0,
        coating_type=CoatingCategory.FBE,
        elapsed_years=5.0,
    )
    area_20yr = effective_bare_area_coated(
        total_surface_area_m2=1000.0,
        coating_type=CoatingCategory.FBE,
        elapsed_years=20.0,
    )
    assert area_20yr > area_5yr
    # At 5 yr: 1000 * (0.02 + 0.003*5) = 1000 * 0.035 = 35 m²
    assert area_5yr == pytest.approx(35.0, abs=1.0)
