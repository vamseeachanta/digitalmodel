#!/usr/bin/env python3
"""
ABOUTME: Comprehensive unit tests for VIV Analysis Module covering natural frequency,
vortex shedding, screening, and fatigue calculations per DNV-RP-C205 and DNV-RP-F105.
"""

import pytest
import numpy as np
from digitalmodel.modules.viv_analysis import (
    # Models
    BoundaryCondition,
    CurrentProfileType,
    MaterialProperties,
    TubularMember,
    FluidProperties,
    CurrentProfile,

    # Analyzers
    FrequencyCalculator,
    VortexSheddingAnalyzer,
    VIVScreening,
    VIVFatigueCalculator,

    # Material library
    STEEL_CARBON,
    STEEL_STAINLESS,
    TITANIUM,
    get_material,

    # Constants
    STROUHAL_NUMBERS,
    LOCK_IN_RANGES,
)


# ============================================================================
# Fixtures
# ============================================================================

@pytest.fixture
def basic_riser():
    """Basic pinned-pinned riser for testing"""
    return TubularMember(
        name="TestRiser",
        length=50.0,
        outer_diameter=0.5,
        wall_thickness=0.025,
        material=STEEL_CARBON,
        boundary_condition=BoundaryCondition.PINNED_PINNED
    )


@pytest.fixture
def cantilever_member():
    """Cantilever tubular member"""
    return TubularMember(
        name="Cantilever",
        length=20.0,
        outer_diameter=0.3,
        wall_thickness=0.015,
        material=STEEL_STAINLESS,
        boundary_condition=BoundaryCondition.CANTILEVER
    )


@pytest.fixture
def seawater():
    """Seawater fluid properties"""
    return FluidProperties(
        density=1025.0,
        kinematic_viscosity=1.35e-6,
        added_mass_coefficient=1.0,
        name="Seawater"
    )


@pytest.fixture
def frequency_calculator(seawater):
    """Frequency calculator with seawater"""
    return FrequencyCalculator(fluid=seawater)


@pytest.fixture
def vortex_analyzer(seawater):
    """Vortex shedding analyzer"""
    return VortexSheddingAnalyzer(fluid=seawater)


@pytest.fixture
def viv_screening(seawater):
    """VIV screening analyzer"""
    return VIVScreening(fluid=seawater)


@pytest.fixture
def fatigue_calculator():
    """VIV fatigue calculator"""
    return VIVFatigueCalculator()


# ============================================================================
# Material and Model Tests
# ============================================================================

class TestMaterialProperties:
    """Test material property models"""

    def test_steel_carbon_properties(self):
        """Test STEEL_CARBON has correct properties"""
        assert STEEL_CARBON.youngs_modulus == 207_000  # MPa
        assert STEEL_CARBON.density == 7850  # kg/m³
        assert STEEL_CARBON.poissons_ratio == 0.3
        assert STEEL_CARBON.name == "Carbon Steel"

    def test_steel_stainless_properties(self):
        """Test STEEL_STAINLESS properties"""
        assert STEEL_STAINLESS.youngs_modulus == 193_000
        assert STEEL_STAINLESS.density == 8000

    def test_titanium_properties(self):
        """Test TITANIUM properties"""
        assert TITANIUM.youngs_modulus == 110_000
        assert TITANIUM.density == 4500

    def test_get_material_by_name(self):
        """Test material retrieval by name"""
        steel = get_material("steel")
        assert steel.name == "Carbon Steel"

        stainless = get_material("steel_stainless")
        assert stainless.density == 8000

        ti = get_material("titanium")
        assert ti.youngs_modulus == 110_000

    def test_get_material_invalid_name(self):
        """Test get_material with invalid name raises error"""
        with pytest.raises(ValueError, match="Unknown material"):
            get_material("unobtanium")


class TestTubularMember:
    """Test tubular member calculations"""

    def test_basic_properties(self, basic_riser):
        """Test basic member properties"""
        assert basic_riser.length == 50.0
        assert basic_riser.outer_diameter == 0.5
        assert basic_riser.wall_thickness == 0.025

    def test_inner_diameter(self, basic_riser):
        """Test inner diameter calculation"""
        expected = 0.5 - 2 * 0.025
        assert abs(basic_riser.inner_diameter - expected) < 1e-9

    def test_cross_sectional_area(self, basic_riser):
        """Test cross-sectional area"""
        D_outer = 0.5
        D_inner = basic_riser.inner_diameter
        expected = np.pi / 4 * (D_outer**2 - D_inner**2)
        assert abs(basic_riser.cross_sectional_area - expected) < 1e-9

    def test_second_moment_of_area(self, basic_riser):
        """Test second moment of area"""
        D_outer = 0.5
        D_inner = basic_riser.inner_diameter
        expected = np.pi / 64 * (D_outer**4 - D_inner**4)
        assert abs(basic_riser.second_moment_of_area - expected) < 1e-9

    def test_mass_per_length(self, basic_riser):
        """Test mass per unit length"""
        A = basic_riser.cross_sectional_area
        rho = STEEL_CARBON.density
        expected = A * rho
        assert abs(basic_riser.mass_per_length - expected) < 1e-6

    def test_effective_length_factor(self, basic_riser):
        """Test effective length factor for boundary conditions"""
        assert basic_riser.effective_length_factor == 1.0

        cantilever = TubularMember(
            name="Test",
            length=10.0,
            outer_diameter=0.3,
            wall_thickness=0.01,
            material=STEEL_CARBON,
            boundary_condition=BoundaryCondition.CANTILEVER
        )
        assert cantilever.effective_length_factor == 2.0


# ============================================================================
# Natural Frequency Tests
# ============================================================================

class TestFrequencyCalculator:
    """Test natural frequency calculations"""

    def test_pinned_pinned_first_mode(self, basic_riser, frequency_calculator):
        """Test first mode natural frequency for pinned-pinned"""
        result = frequency_calculator.calculate_natural_frequency(basic_riser, mode=1)

        assert result.mode_number == 1
        assert result.frequency > 0
        assert result.period == pytest.approx(1 / result.frequency)
        assert result.angular_frequency == pytest.approx(2 * np.pi * result.frequency)

    def test_multiple_modes_increasing_frequency(self, basic_riser, frequency_calculator):
        """Test that higher modes have higher frequencies"""
        results = frequency_calculator.calculate_multiple_modes(basic_riser, n_modes=5)

        assert len(results) == 5
        for i in range(len(results) - 1):
            assert results[i + 1].frequency > results[i].frequency
            assert results[i + 1].mode_number == results[i].mode_number + 1

    def test_cantilever_frequency(self, cantilever_member, frequency_calculator):
        """Test cantilever natural frequency"""
        result = frequency_calculator.calculate_natural_frequency(cantilever_member, mode=1)

        assert result.frequency > 0
        assert result.member_name == "Cantilever"

    def test_fixed_fixed_boundary(self, frequency_calculator):
        """Test fixed-fixed boundary condition"""
        member = TubularMember(
            name="Fixed",
            length=30.0,
            outer_diameter=0.4,
            wall_thickness=0.02,
            material=STEEL_CARBON,
            boundary_condition=BoundaryCondition.FIXED_FIXED
        )

        result = frequency_calculator.calculate_natural_frequency(member, mode=1)
        assert result.frequency > 0

    def test_added_mass_effect(self, basic_riser, frequency_calculator):
        """Test that added mass reduces frequency"""
        f_with_added = frequency_calculator.calculate_natural_frequency(
            basic_riser, mode=1, include_added_mass=True
        )

        f_without_added = frequency_calculator.calculate_natural_frequency(
            basic_riser, mode=1, include_added_mass=False
        )

        # Added mass should reduce frequency
        assert f_with_added.frequency < f_without_added.frequency

    def test_longer_member_lower_frequency(self, frequency_calculator):
        """Test that longer members have lower frequencies"""
        short = TubularMember(
            name="Short", length=20.0, outer_diameter=0.5,
            wall_thickness=0.025, material=STEEL_CARBON,
            boundary_condition=BoundaryCondition.PINNED_PINNED
        )

        long = TubularMember(
            name="Long", length=60.0, outer_diameter=0.5,
            wall_thickness=0.025, material=STEEL_CARBON,
            boundary_condition=BoundaryCondition.PINNED_PINNED
        )

        f_short = frequency_calculator.calculate_natural_frequency(short, mode=1)
        f_long = frequency_calculator.calculate_natural_frequency(long, mode=1)

        assert f_short.frequency > f_long.frequency


# ============================================================================
# Vortex Shedding Tests
# ============================================================================

class TestVortexSheddingAnalyzer:
    """Test vortex shedding analysis"""

    def test_shedding_frequency_basic(self, vortex_analyzer):
        """Test basic Strouhal relationship"""
        D = 0.5  # m
        V = 1.0  # m/s
        St = 0.2

        f_s = vortex_analyzer.shedding_frequency(D, V, St)
        expected = St * V / D

        assert abs(f_s - expected) < 1e-9

    def test_reynolds_number(self, vortex_analyzer):
        """Test Reynolds number calculation"""
        D = 0.5
        V = 1.5
        nu = vortex_analyzer.fluid.kinematic_viscosity

        Re = vortex_analyzer.reynolds_number(D, V)
        expected = V * D / nu

        assert abs(Re - expected) < 1e-3

    def test_analyze_member(self, basic_riser, vortex_analyzer):
        """Test complete member vortex shedding analysis"""
        result = vortex_analyzer.analyze_member(basic_riser, current_velocity=1.5)

        assert result.shedding_frequency > 0
        assert result.reynolds_number > 0
        assert result.strouhal_number == pytest.approx(STROUHAL_NUMBERS['smooth'])
        assert result.current_velocity == 1.5

    def test_strouhal_number_selection(self, basic_riser, vortex_analyzer):
        """Test different Strouhal numbers for surface conditions"""
        smooth = vortex_analyzer.analyze_member(
            basic_riser, 1.0, surface_condition='smooth'
        )

        rough = vortex_analyzer.analyze_member(
            basic_riser, 1.0, surface_condition='rough'
        )

        straked = vortex_analyzer.analyze_member(
            basic_riser, 1.0, surface_condition='straked'
        )

        assert smooth.strouhal_number == STROUHAL_NUMBERS['smooth']
        assert rough.strouhal_number == STROUHAL_NUMBERS['rough']
        assert straked.strouhal_number == STROUHAL_NUMBERS['straked']

    def test_current_profile_uniform(self, basic_riser, vortex_analyzer):
        """Test uniform current profile"""
        profile = CurrentProfile(
            profile_type=CurrentProfileType.UNIFORM,
            surface_velocity=1.5
        )

        results = vortex_analyzer.analyze_with_profile(basic_riser, profile, n_points=5)

        assert len(results) == 5
        # All velocities should be the same for uniform profile
        velocities = [r.current_velocity for r in results]
        assert all(abs(v - 1.5) < 1e-6 for v in velocities)


# ============================================================================
# VIV Screening Tests
# ============================================================================

class TestVIVScreening:
    """Test VIV susceptibility screening"""

    def test_reduced_velocity_calculation(self, viv_screening):
        """Test reduced velocity calculation"""
        V = 1.5  # m/s
        f_n = 0.5  # Hz
        D = 0.5  # m

        V_r = viv_screening.reduced_velocity(V, f_n, D)
        expected = V / (f_n * D)

        assert abs(V_r - expected) < 1e-9

    def test_lock_in_detection_in_range(self, viv_screening):
        """Test lock-in detection when V_r is in lock-in range"""
        V_r = 6.0  # In lock-in range [4, 8]

        in_lock_in, margin, status = viv_screening.check_lock_in(V_r)

        assert in_lock_in is True
        assert status == "lock-in"
        assert margin > 0

    def test_lock_in_detection_safe(self, viv_screening):
        """Test when V_r is safely below lock-in range"""
        V_r = 2.0  # Well below 4.0

        in_lock_in, margin, status = viv_screening.check_lock_in(V_r)

        assert in_lock_in is False
        assert status == "safe"
        assert margin == pytest.approx(4.0 - 2.0)

    def test_lock_in_detection_marginal(self, viv_screening):
        """Test marginal case near lock-in boundary"""
        V_r = 3.5  # Just below 4.0

        in_lock_in, margin, status = viv_screening.check_lock_in(V_r)

        assert in_lock_in is False
        assert status == "marginal"
        assert margin < 1.0

    def test_safety_factor_in_lock_in(self, viv_screening):
        """Test safety factor calculation in lock-in"""
        V_r = 6.0

        sf = viv_screening.calculate_safety_factor(V_r)

        assert sf > 0
        assert sf < 1.0  # Should be less than 1 when in lock-in

    def test_safety_factor_safe_region(self, viv_screening):
        """Test safety factor in safe region"""
        V_r = 2.0  # Well below lock-in

        sf = viv_screening.calculate_safety_factor(V_r)

        assert sf > 1.0  # Should be greater than 1 when safe

    def test_screen_member_complete(self, basic_riser, viv_screening):
        """Test complete member screening"""
        result = viv_screening.screen_member(basic_riser, current_velocity=1.5, mode=1)

        assert result.natural_frequency > 0
        assert result.shedding_frequency > 0
        assert result.reduced_velocity > 0
        assert result.safety_factor > 0
        assert result.lock_in_status in ['safe', 'marginal', 'lock-in']
        assert isinstance(result.is_susceptible, bool)
        assert len(result.recommendation) > 0

    def test_recommendation_lock_in(self, basic_riser, viv_screening):
        """Test recommendation when in lock-in"""
        # Tune parameters to force lock-in
        result = viv_screening.screen_member(basic_riser, current_velocity=0.5, mode=1)

        if result.is_susceptible:
            assert "suppression" in result.recommendation.lower() or \
                   "mitigation" in result.recommendation.lower() or \
                   "strakes" in result.recommendation.lower()


# ============================================================================
# VIV Fatigue Tests
# ============================================================================

class TestVIVFatigueCalculator:
    """Test VIV fatigue damage calculations"""

    def test_stress_range_from_amplitude(self, basic_riser, fatigue_calculator):
        """Test stress range calculation from VIV amplitude"""
        A_D = 0.5  # Amplitude as ratio of diameter

        stress_range = fatigue_calculator.calculate_stress_range(A_D, basic_riser)

        assert stress_range > 0
        # Stress should be in reasonable range (MPa)
        assert 0.1 < stress_range < 1000

    def test_damage_calculation(self, fatigue_calculator):
        """Test fatigue damage calculation"""
        stress_range = 50.0  # MPa
        frequency = 0.5  # Hz
        duration = 3600 * 24  # 1 day in seconds

        damage = fatigue_calculator.calculate_damage(
            stress_range, frequency, duration, sn_curve="DNV-D"
        )

        assert damage >= 0
        # Damage should be much less than 1 for 1 day at moderate stress
        assert damage < 0.1

    def test_higher_stress_more_damage(self, fatigue_calculator):
        """Test that higher stress produces more damage"""
        frequency = 0.5
        duration = 3600

        damage_low = fatigue_calculator.calculate_damage(20.0, frequency, duration)
        damage_high = fatigue_calculator.calculate_damage(60.0, frequency, duration)

        assert damage_high > damage_low

    def test_fatigue_life_calculation(self, fatigue_calculator):
        """Test fatigue life calculation"""
        stress_range = 30.0  # MPa
        frequency = 0.3  # Hz

        life_years = fatigue_calculator.calculate_fatigue_life(
            stress_range, frequency, sn_curve="DNV-D"
        )

        assert life_years > 0
        # Should have reasonable fatigue life at moderate stress
        assert life_years > 1.0

    def test_analyze_viv_fatigue_complete(self, basic_riser, fatigue_calculator):
        """Test complete VIV fatigue analysis"""
        result = fatigue_calculator.analyze_viv_fatigue(
            basic_riser,
            viv_amplitude_ratio=0.3,
            viv_frequency=0.5,
            duration_hours=24,
            sn_curve="DNV-D"
        )

        assert result.stress_range > 0
        assert result.frequency == 0.5
        assert result.duration == 24 * 3600
        assert result.num_cycles > 0
        assert result.fatigue_damage >= 0
        assert result.fatigue_life_years > 0
        assert result.sn_curve == "DNV-D"

    def test_cumulative_damage(self, fatigue_calculator):
        """Test cumulative damage from multiple conditions"""
        stress_ranges = [30.0, 45.0, 60.0]  # MPa
        frequencies = [0.3, 0.4, 0.5]  # Hz
        durations = [3600, 3600, 3600]  # seconds

        total_damage = fatigue_calculator.cumulative_damage(
            stress_ranges, frequencies, durations, sn_curve="DNV-D"
        )

        assert total_damage > 0
        # Should be sum of individual damages
        individual_damages = [
            fatigue_calculator.calculate_damage(s, f, d, "DNV-D")
            for s, f, d in zip(stress_ranges, frequencies, durations)
        ]
        expected = sum(individual_damages)
        assert abs(total_damage - expected) < 1e-9

    def test_sn_curve_selection(self, fatigue_calculator):
        """Test different S-N curve selections"""
        stress = 40.0
        freq = 0.5
        duration = 3600

        # Test multiple curves
        curves = ["DNV-B1", "DNV-D", "DNV-F", "DNV-W1"]
        damages = []

        for curve in curves:
            damage = fatigue_calculator.calculate_damage(stress, freq, duration, curve)
            damages.append(damage)
            assert damage > 0

        # Different curves should give different damages
        assert len(set(damages)) > 1

    def test_invalid_sn_curve(self, fatigue_calculator):
        """Test error on invalid S-N curve"""
        with pytest.raises(ValueError, match="Unknown S-N curve"):
            fatigue_calculator.calculate_damage(50.0, 0.5, 3600, sn_curve="INVALID")


# ============================================================================
# Constants Tests
# ============================================================================

class TestConstants:
    """Test VIV constants and libraries"""

    def test_strouhal_numbers_exist(self):
        """Test Strouhal number dictionary"""
        assert 'smooth' in STROUHAL_NUMBERS
        assert 'rough' in STROUHAL_NUMBERS
        assert 'straked' in STROUHAL_NUMBERS
        assert 'helical_strake' in STROUHAL_NUMBERS

        # Strouhal numbers should be around 0.2
        for St in STROUHAL_NUMBERS.values():
            assert 0.1 < St < 0.3

    def test_lock_in_ranges_exist(self):
        """Test lock-in range dictionary"""
        assert 'cross_flow' in LOCK_IN_RANGES
        assert 'in_line' in LOCK_IN_RANGES
        assert 'critical' in LOCK_IN_RANGES

        # Cross-flow lock-in typically 4-8
        cf_min, cf_max = LOCK_IN_RANGES['cross_flow']
        assert cf_min == pytest.approx(4.0)
        assert cf_max == pytest.approx(8.0)


# ============================================================================
# Integration Tests
# ============================================================================

class TestVIVWorkflow:
    """Test complete VIV analysis workflow"""

    def test_complete_workflow(self, basic_riser, seawater):
        """Test complete VIV analysis from frequency to fatigue"""
        # Step 1: Calculate natural frequency
        freq_calc = FrequencyCalculator(fluid=seawater)
        freq_result = freq_calc.calculate_natural_frequency(basic_riser, mode=1)

        assert freq_result.frequency > 0

        # Step 2: Vortex shedding analysis
        vortex = VortexSheddingAnalyzer(fluid=seawater)
        vortex_result = vortex.analyze_member(basic_riser, current_velocity=1.5)

        assert vortex_result.shedding_frequency > 0

        # Step 3: VIV screening
        screening = VIVScreening(fluid=seawater)
        screen_result = screening.screen_member(basic_riser, current_velocity=1.5, mode=1)

        assert screen_result.reduced_velocity > 0
        assert screen_result.safety_factor > 0

        # Step 4: Fatigue if susceptible
        if screen_result.is_susceptible:
            fatigue_calc = VIVFatigueCalculator()
            fatigue_result = fatigue_calc.analyze_viv_fatigue(
                basic_riser,
                viv_amplitude_ratio=0.3,
                viv_frequency=freq_result.frequency,
                duration_hours=8760,  # 1 year
                sn_curve="DNV-D"
            )

            assert fatigue_result.fatigue_damage >= 0
            assert fatigue_result.fatigue_life_years > 0
