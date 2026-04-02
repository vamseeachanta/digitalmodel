"""Tests for pipeline CP design extensions per API RP 1169 / ISO 15589-1.

TDD: These tests were written BEFORE the implementation.
Extends the existing pipeline_cp module with potential criteria checks,
soil resistivity corrections, and end-to-end design workflow.
"""

import math

import pytest

from digitalmodel.cathodic_protection.pipeline_cp import (
    PipelineEnvironment,
    PipelineCPInput,
    CriteriaResult,
    PipelineDesignResult,
    calculate_pipeline_current_demand,
    calculate_anode_spacing,
    check_potential_criteria,
    design_pipeline_cp,
    soil_resistivity_correction,
)


# ────────────────────────────────────────────────
# Test calculate_pipeline_current_demand
# ────────────────────────────────────────────────

class TestCalculatePipelineCurrentDemand:
    """Tests for pipeline current demand with bare vs coated distinction."""

    def test_coated_pipeline_demand(self):
        """Coated 24-inch pipeline, 50 km, buried soil, 3% breakdown."""
        params = PipelineCPInput(
            outer_diameter_m=0.6096,  # 24 inch
            wall_thickness_m=0.01905,  # 0.75 inch
            length_m=50000.0,
            environment=PipelineEnvironment.BURIED_SOIL,
            coating_breakdown_factor=0.03,
            soil_resistivity_ohm_m=50.0,
        )
        result = calculate_pipeline_current_demand(params)
        # Area = pi * 0.6096 * 50000 = ~95,756 m²
        # Effective bare = 95756 * 0.03 = ~2873 m²
        # Current = 2873 * 10 / 1000 = ~28.73 A
        assert result > 0
        assert result == pytest.approx(28.73, rel=0.05)

    def test_bare_pipe_higher_demand(self):
        """Bare pipe (breakdown=1.0) has much higher demand than coated."""
        params_coated = PipelineCPInput(
            outer_diameter_m=0.3048,
            wall_thickness_m=0.0127,
            length_m=1000.0,
            environment=PipelineEnvironment.BURIED_SOIL,
            coating_breakdown_factor=0.03,
        )
        params_bare = PipelineCPInput(
            outer_diameter_m=0.3048,
            wall_thickness_m=0.0127,
            length_m=1000.0,
            environment=PipelineEnvironment.BURIED_SOIL,
            coating_breakdown_factor=1.0,
        )
        coated = calculate_pipeline_current_demand(params_coated)
        bare = calculate_pipeline_current_demand(params_bare)
        assert bare > coated * 10  # 1.0/0.03 ≈ 33x


# ────────────────────────────────────────────────
# Test calculate_anode_spacing
# ────────────────────────────────────────────────

class TestCalculateAnodeSpacing:
    """Tests for anode spacing calculation on long pipelines."""

    def test_basic_spacing(self):
        """10 km pipeline, 3 A demand, 0.5 A per anode → 6 anodes, ~1667 m spacing."""
        spacing_m, n_anodes = calculate_anode_spacing(
            pipeline_length_m=10000.0,
            total_current_demand_A=3.0,
            anode_output_A=0.5,
        )
        assert n_anodes == 6  # ceil(3.0 / 0.5)
        assert spacing_m == pytest.approx(10000.0 / 6.0, rel=0.01)

    def test_low_demand_max_spacing(self):
        """Very low demand → spacing capped at max_spacing_m."""
        spacing_m, n_anodes = calculate_anode_spacing(
            pipeline_length_m=100000.0,
            total_current_demand_A=0.01,
            anode_output_A=0.5,
            max_spacing_m=3000.0,
        )
        assert spacing_m <= 3000.0


# ────────────────────────────────────────────────
# Test soil_resistivity_correction
# ────────────────────────────────────────────────

class TestSoilResistivityCorrection:
    """Tests for soil resistivity correction factors."""

    def test_low_resistivity_correction(self):
        """Low resistivity (< 10 ohm-m) → correction factor > 1."""
        factor = soil_resistivity_correction(5.0)
        assert factor > 1.0

    def test_moderate_resistivity_baseline(self):
        """Moderate resistivity (50 ohm-m) → correction factor near 1."""
        factor = soil_resistivity_correction(50.0)
        assert factor == pytest.approx(1.0, abs=0.2)

    def test_high_resistivity_correction(self):
        """High resistivity (> 200 ohm-m) → correction factor < 1."""
        factor = soil_resistivity_correction(500.0)
        assert factor < 1.0


# ────────────────────────────────────────────────
# Test check_potential_criteria
# ────────────────────────────────────────────────

class TestCheckPotentialCriteria:
    """Tests for CP potential criteria per NACE SP0169 §6.2."""

    def test_adequate_protection_aerobic(self):
        """Potential of -0.900 V CSE in aerobic soil → protected."""
        result = check_potential_criteria(
            measured_potential_V_CSE=-0.900,
            environment="aerobic",
        )
        assert isinstance(result, CriteriaResult)
        assert result.is_protected is True
        assert result.criterion_V == pytest.approx(-0.850, abs=0.001)

    def test_inadequate_protection_aerobic(self):
        """Potential of -0.750 V CSE in aerobic soil → NOT protected."""
        result = check_potential_criteria(
            measured_potential_V_CSE=-0.750,
            environment="aerobic",
        )
        assert result.is_protected is False

    def test_anaerobic_stricter_criterion(self):
        """Anaerobic (SRB) environment requires -0.950 V CSE."""
        result = check_potential_criteria(
            measured_potential_V_CSE=-0.900,
            environment="anaerobic",
        )
        # -0.900 is more positive than -0.950 → NOT protected
        assert result.is_protected is False
        assert result.criterion_V == pytest.approx(-0.950, abs=0.001)

    def test_anaerobic_adequate(self):
        """Potential of -1.000 V CSE in anaerobic soil → protected."""
        result = check_potential_criteria(
            measured_potential_V_CSE=-1.000,
            environment="anaerobic",
        )
        assert result.is_protected is True

    def test_overprotection_warning(self):
        """Very negative potential (< -1.2 V) should flag overprotection."""
        result = check_potential_criteria(
            measured_potential_V_CSE=-1.300,
            environment="aerobic",
        )
        assert result.is_protected is True
        assert result.is_overprotected is True


# ────────────────────────────────────────────────
# Test design_pipeline_cp (end-to-end)
# ────────────────────────────────────────────────

class TestDesignPipelineCP:
    """End-to-end pipeline CP design."""

    def test_basic_design(self):
        """Standard buried soil pipeline design."""
        params = PipelineCPInput(
            outer_diameter_m=0.3048,
            wall_thickness_m=0.0127,
            length_m=10000.0,
            environment=PipelineEnvironment.BURIED_SOIL,
            coating_breakdown_factor=0.03,
            soil_resistivity_ohm_m=50.0,
            design_life_years=25.0,
        )
        result = design_pipeline_cp(
            input_params=params,
            anode_output_A=0.5,
        )
        assert isinstance(result, PipelineDesignResult)
        assert result.current_demand_A > 0
        assert result.number_of_anodes >= 1
        assert result.anode_spacing_m > 0
        assert result.protection_criteria.is_protected is True
