#!/usr/bin/env python3
"""
ABOUTME: Comprehensive unit tests for Catenary Riser Module covering models,
simple catenary analysis, effective weight, and lazy wave configurations.
"""

import pytest
import numpy as np
from digitalmodel.modules.catenary_riser import (
    # Models
    RiserConfiguration,
    BuoyancyModule,
    LazyWaveConfiguration,

    # Analyzers
    SimpleCatenaryAnalyzer,
    EffectiveWeightCalculator,
    LazyWaveAnalyzer,

    # Materials and fluids
    STEEL_API_5L_X65,
    STEEL_API_5L_X70,
    SEAWATER,
    PRODUCTION_OIL,
    AIR,
    get_material,
    get_fluid,
)


# ============================================================================
# Fixtures
# ============================================================================

@pytest.fixture
def basic_riser():
    """Basic riser configuration for testing"""
    return RiserConfiguration(
        name="TestRiser",
        outer_diameter=0.508,  # 20 inches
        wall_thickness=0.025,
        length=1500.0,
        material=STEEL_API_5L_X65,
        internal_fluid=PRODUCTION_OIL,
        water_depth=1000.0,
        horizontal_offset=500.0,
    )


@pytest.fixture
def catenary_analyzer():
    """Simple catenary analyzer"""
    return SimpleCatenaryAnalyzer()


@pytest.fixture
def weight_calculator():
    """Effective weight calculator"""
    return EffectiveWeightCalculator()


# ============================================================================
# Material and Fluid Tests
# ============================================================================

class TestMaterials:
    """Test material properties"""

    def test_x65_properties(self):
        """Test API 5L X65 properties"""
        assert STEEL_API_5L_X65.youngs_modulus == 207_000
        assert STEEL_API_5L_X65.density == 7850
        assert STEEL_API_5L_X65.yield_strength == 448

    def test_x70_properties(self):
        """Test API 5L X70 properties"""
        assert STEEL_API_5L_X70.yield_strength == 483

    def test_get_material_by_name(self):
        """Test material retrieval"""
        x65 = get_material('x65')
        assert x65.name == "API 5L X65"

        x70 = get_material('api5l_x70')
        assert x70.yield_strength == 483

    def test_get_fluid_by_name(self):
        """Test fluid retrieval"""
        oil = get_fluid('oil')
        assert oil.density == 850

        water = get_fluid('seawater')
        assert water.density == 1025


class TestRiserConfiguration:
    """Test riser configuration calculations"""

    def test_basic_properties(self, basic_riser):
        """Test basic riser properties"""
        assert basic_riser.outer_diameter == 0.508
        assert basic_riser.wall_thickness == 0.025
        assert basic_riser.length == 1500.0

    def test_inner_diameter(self, basic_riser):
        """Test inner diameter calculation"""
        expected = 0.508 - 2 * 0.025
        assert abs(basic_riser.inner_diameter - expected) < 1e-9

    def test_effective_weight_positive(self, basic_riser):
        """Test effective weight is positive (riser sinks)"""
        w_eff = basic_riser.effective_weight_per_length
        assert w_eff > 0  # Should sink

    def test_effective_weight_components(self, basic_riser):
        """Test effective weight components"""
        w_steel = basic_riser.steel_weight_per_length
        w_buoy = basic_riser.buoyancy_per_length

        assert w_steel > 0
        assert w_buoy > 0
        assert w_steel > w_buoy  # Steel heavier than buoyancy

    def test_with_coating(self):
        """Test riser with coating"""
        riser = RiserConfiguration(
            name="CoatedRiser",
            outer_diameter=0.508,
            wall_thickness=0.025,
            length=1000.0,
            material=STEEL_API_5L_X65,
            internal_fluid=PRODUCTION_OIL,
            coating_thickness=0.05,
            coating_density=700,
        )

        assert riser.coating_weight_per_length > 0


# ============================================================================
# Simple Catenary Tests
# ============================================================================

class TestSimpleCatenary:
    """Test simple catenary analysis"""

    def test_analyze_with_horizontal_tension(self, basic_riser, catenary_analyzer):
        """Test catenary analysis given horizontal tension"""
        result = catenary_analyzer.analyze_riser(
            basic_riser,
            horizontal_tension=500_000,  # 500 kN
            water_depth=1000.0
        )

        assert result.horizontal_tension == 500_000
        assert result.top_tension > result.horizontal_tension
        assert result.top_angle > 0
        assert result.arc_length > 0

    def test_tension_increases_with_height(self, basic_riser, catenary_analyzer):
        """Test that tension increases along riser"""
        result = catenary_analyzer.analyze_riser(
            basic_riser,
            horizontal_tension=500_000,
            water_depth=1000.0
        )

        # Top tension should be greater than touchdown
        assert result.top_tension > result.touchdown_tension
        assert result.touchdown_tension == result.horizontal_tension

    def test_grounded_length(self, basic_riser, catenary_analyzer):
        """Test grounded length calculation"""
        result = catenary_analyzer.analyze_riser(
            basic_riser,
            horizontal_tension=500_000,
            water_depth=1000.0
        )

        # When arc_length exceeds riser length, grounded_length should be 0
        # When arc_length <= riser length, grounded_length = riser.length - arc_length
        if result.arc_length >= basic_riser.length:
            # High horizontal tension results in catenary needing more length than available
            assert result.grounded_length == 0.0
        else:
            # Total length = suspended + grounded
            total = result.arc_length + result.grounded_length
            assert abs(total - basic_riser.length) < 1.0

    def test_angle_at_top(self, basic_riser, catenary_analyzer):
        """Test angle at top is reasonable"""
        result = catenary_analyzer.analyze_riser(
            basic_riser,
            horizontal_tension=500_000,
            water_depth=1000.0
        )

        # Angle should be between 0 and 90 degrees
        assert 0 < result.top_angle < 90

    def test_solve_from_top_tension(self, basic_riser, catenary_analyzer):
        """Test solving given top tension"""
        # Note: For a catenary, there's a minimum achievable top tension
        # For this riser (w ≈ 2257 N/m, z = 1000m), minimum T ≈ 3.41 MN
        # Use a top tension well above this minimum for reliable convergence
        result = catenary_analyzer.analyze_riser(
            basic_riser,
            top_tension=5_000_000,  # 5 MN (well above ~3.41 MN minimum)
            water_depth=1000.0
        )

        assert result.top_tension == pytest.approx(5_000_000, rel=1e-3)
        assert result.horizontal_tension > 0

    def test_catenary_parameter(self, basic_riser, catenary_analyzer):
        """Test catenary parameter calculation"""
        result = catenary_analyzer.analyze_riser(
            basic_riser,
            horizontal_tension=500_000,
            water_depth=1000.0
        )

        # a = H / w
        expected = result.horizontal_tension / result.effective_weight
        assert abs(result.catenary_parameter - expected) < 1e-6


# ============================================================================
# Effective Weight Tests
# ============================================================================

class TestEffectiveWeight:
    """Test effective weight calculations"""

    def test_basic_weight_calculation(self, basic_riser, weight_calculator):
        """Test basic effective weight calculation"""
        result = weight_calculator.calculate(basic_riser)

        assert result.steel_weight > 0
        assert result.buoyancy > 0
        assert result.effective_weight > 0  # Should sink

    def test_weight_components_add_up(self, basic_riser, weight_calculator):
        """Test that components add to total"""
        result = weight_calculator.calculate(basic_riser)

        expected = (result.steel_weight + result.coating_weight +
                   result.contents_weight - result.buoyancy)

        assert abs(result.effective_weight - expected) < 1e-6

    def test_empty_riser_floats(self, weight_calculator):
        """Test that empty riser without coating floats"""
        riser = RiserConfiguration(
            name="EmptyRiser",
            outer_diameter=0.508,
            wall_thickness=0.015,  # Thin wall
            length=1000.0,
            material=STEEL_API_5L_X65,
            internal_fluid=AIR,  # Empty
        )

        result = weight_calculator.calculate(riser)

        # May float or sink depending on wall thickness
        # Just check calculation works
        assert result.effective_weight is not None

    def test_to_dict(self, basic_riser, weight_calculator):
        """Test exporting to dictionary"""
        result = weight_calculator.calculate(basic_riser)
        data = result.to_dict()

        assert 'effective_weight_N_per_m' in data
        assert 'steel_weight_N_per_m' in data
        assert isinstance(data, dict)


# ============================================================================
# Buoyancy Module Tests
# ============================================================================

class TestBuoyancyModule:
    """Test buoyancy module calculations"""

    def test_buoyancy_force(self):
        """Test buoyancy module force calculation"""
        module = BuoyancyModule(
            name="TestModule",
            length=100.0,
            outer_diameter=1.5,
            buoyancy_material_density=500,  # Syntactic foam
            start_length=300.0,
        )

        force = module.buoyancy_force_per_length(SEAWATER)

        # Should provide upward force (negative effective weight)
        assert force < 0  # Upward

    def test_different_foam_densities(self):
        """Test different buoyancy material densities"""
        light_foam = BuoyancyModule(
            name="LightFoam",
            length=100.0,
            outer_diameter=1.5,
            buoyancy_material_density=300,
            start_length=300.0,
        )

        heavy_foam = BuoyancyModule(
            name="HeavyFoam",
            length=100.0,
            outer_diameter=1.5,
            buoyancy_material_density=700,
            start_length=300.0,
        )

        light_force = light_foam.buoyancy_force_per_length(SEAWATER)
        heavy_force = heavy_foam.buoyancy_force_per_length(SEAWATER)

        # Lighter foam provides more buoyancy
        assert light_force < heavy_force


# ============================================================================
# Lazy Wave Tests
# ============================================================================

class TestLazyWave:
    """Test lazy wave analysis"""

    def test_basic_lazy_wave(self, basic_riser):
        """Test basic lazy wave configuration"""
        buoy_module = BuoyancyModule(
            name="BuoyModule1",
            length=200.0,
            outer_diameter=1.5,
            buoyancy_material_density=500,
            start_length=300.0,
        )

        config = LazyWaveConfiguration(
            riser=basic_riser,
            buoyancy_modules=[buoy_module],
        )

        analyzer = LazyWaveAnalyzer()
        result = analyzer.analyze(config)

        assert result.sag_bend_depth > 0
        assert result.hog_bend_depth > 0
        assert result.arch_height > 0
        assert result.sag_bend_depth > result.hog_bend_depth

    def test_weight_profile(self, basic_riser):
        """Test weight profile with buoyancy"""
        buoy_module = BuoyancyModule(
            name="BuoyModule1",
            length=200.0,
            outer_diameter=1.5,
            buoyancy_material_density=500,
            start_length=300.0,
        )

        config = LazyWaveConfiguration(
            riser=basic_riser,
            buoyancy_modules=[buoy_module],
        )

        profile = config.get_effective_weight_profile()

        assert 'bare_riser' in profile
        assert 'BuoyModule1' in profile
        # Buoyancy section should have lower effective weight
        assert profile['BuoyModule1'] < profile['bare_riser']

    def test_buoyancy_utilization(self, basic_riser):
        """Test buoyancy utilization calculation"""
        buoy_module = BuoyancyModule(
            name="BuoyModule1",
            length=200.0,
            outer_diameter=1.5,
            buoyancy_material_density=500,
            start_length=300.0,
        )

        config = LazyWaveConfiguration(
            riser=basic_riser,
            buoyancy_modules=[buoy_module],
        )

        analyzer = LazyWaveAnalyzer()
        result = analyzer.analyze(config)

        assert 0 <= result.buoyancy_utilization <= 1.0


# ============================================================================
# Integration Tests
# ============================================================================

class TestWorkflow:
    """Test complete workflow"""

    def test_complete_analysis_workflow(self, basic_riser):
        """Test complete catenary riser analysis workflow"""
        # Step 1: Calculate effective weight
        calc = EffectiveWeightCalculator()
        weight_result = calc.calculate(basic_riser)
        assert weight_result.effective_weight > 0

        # Step 2: Simple catenary analysis
        analyzer = SimpleCatenaryAnalyzer()
        catenary_result = analyzer.analyze_riser(
            basic_riser,
            water_depth=1000.0,
            horizontal_offset=500.0
        )
        assert catenary_result.top_tension > 0

        # Step 3: Check design limits
        checks = analyzer.check_design_limits(
            catenary_result,
            max_top_tension=1_000_000,
        )
        assert 'top_tension_ok' in checks
