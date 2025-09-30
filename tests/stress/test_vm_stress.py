"""
Test cases for Von Mises stress analysis module
"""

import pytest
import numpy as np
import math
from digitalmodel.stress.vm_stress import (
    VonMisesStressCalculator,
    PipeStressAnalyzer,
    StressState,
    LoadingCondition,
    PipeGeometry,
    MaterialProperties,
    calculate_vm_stress,
    calculate_principal_stresses
)


class TestStressState:
    """Test StressState class"""

    def test_stress_state_creation(self):
        """Test basic stress state creation"""
        stress_state = StressState(100e6, 50e6, 25e6, 10e6, 5e6, 0)
        assert stress_state.sigma_x == 100e6
        assert stress_state.tau_xy == 10e6

    def test_stress_state_validation(self):
        """Test stress state validation"""
        # Valid stress state
        stress_state = StressState(100e6, 50e6, 25e6)
        stress_state.validate()  # Should not raise

        # Invalid stress state (NaN)
        stress_state_invalid = StressState(float('nan'), 50e6, 25e6)
        with pytest.raises(ValueError, match="cannot be NaN"):
            stress_state_invalid.validate()

        # Invalid stress state (infinite)
        stress_state_inf = StressState(float('inf'), 50e6, 25e6)
        with pytest.raises(ValueError, match="cannot be NaN"):
            stress_state_inf.validate()


class TestPipeGeometry:
    """Test PipeGeometry class"""

    def test_pipe_geometry_creation(self):
        """Test pipe geometry creation and calculations"""
        geometry = PipeGeometry(0.24765, 0.034925)

        assert geometry.outer_diameter == 0.24765
        assert geometry.wall_thickness == 0.034925
        assert abs(geometry.inner_diameter - 0.177800) < 1e-6
        assert geometry.cross_sectional_area > 0
        assert geometry.moment_of_inertia > 0

    def test_pipe_geometry_validation(self):
        """Test pipe geometry validation"""
        # Valid geometry
        geometry = PipeGeometry(0.2, 0.01)
        geometry.validate()  # Should not raise

        # Invalid: negative diameter
        with pytest.raises(ValueError, match="Outer diameter must be positive"):
            PipeGeometry(-0.2, 0.01)

        # Invalid: wall thickness too large
        with pytest.raises(ValueError, match="must be less than radius"):
            PipeGeometry(0.2, 0.15)


class TestMaterialProperties:
    """Test MaterialProperties class"""

    def test_material_properties_creation(self):
        """Test material properties creation"""
        material = MaterialProperties(
            yield_strength=250e6,
            ultimate_strength=400e6,
            elastic_modulus=200e9,
            poisson_ratio=0.3
        )

        assert material.yield_strength == 250e6
        assert material.ultimate_strength == 400e6
        assert material.elastic_modulus == 200e9
        assert material.poisson_ratio == 0.3

    def test_material_properties_validation(self):
        """Test material properties validation"""
        # Valid properties
        material = MaterialProperties(250e6, 400e6, 200e9, 0.3)
        material.validate()  # Should not raise

        # Invalid: ultimate < yield
        with pytest.raises(ValueError, match="Ultimate strength must be"):
            MaterialProperties(250e6, 200e6, 200e9, 0.3)

        # Invalid: Poisson ratio out of range
        with pytest.raises(ValueError, match="Poisson ratio must be"):
            MaterialProperties(250e6, 400e6, 200e9, 0.6)


class TestVonMisesStressCalculator:
    """Test Von Mises stress calculator"""

    def test_vm_stress_from_stress_state(self):
        """Test Von Mises stress calculation from stress state"""
        # Uniaxial stress case
        stress_state = StressState(100e6, 0, 0)
        vm_stress = VonMisesStressCalculator.calculate_from_stress_state(stress_state)
        assert abs(vm_stress - 100e6) < 1e-3

        # Pure shear case
        stress_state = StressState(0, 0, 0, 100e6)
        vm_stress = VonMisesStressCalculator.calculate_from_stress_state(stress_state)
        expected = 100e6 * math.sqrt(3)
        assert abs(vm_stress - expected) / expected < 1e-6

    def test_vm_stress_from_principal_stresses(self):
        """Test Von Mises stress from principal stresses"""
        # Known case: σ1=300MPa, σ2=200MPa, σ3=100MPa
        sigma1, sigma2, sigma3 = 300e6, 200e6, 100e6
        vm_stress = VonMisesStressCalculator.calculate_from_principal_stresses(
            sigma1, sigma2, sigma3
        )

        expected = math.sqrt(0.5 * ((sigma1-sigma2)**2 + (sigma2-sigma3)**2 + (sigma3-sigma1)**2))
        assert abs(vm_stress - expected) / expected < 1e-12


class TestPipeStressAnalyzer:
    """Test pipe stress analyzer"""

    def setup_method(self):
        """Setup test pipe analyzer"""
        # Based on legacy code values
        self.geometry = PipeGeometry(0.24765, 0.034925)
        self.material = MaterialProperties(
            yield_strength=5.52e8,
            ultimate_strength=6.20e8,
            elastic_modulus=2.1e11,
            poisson_ratio=0.3
        )
        self.analyzer = PipeStressAnalyzer(self.geometry, self.material)

    def test_allowable_stress_calculation(self):
        """Test allowable stress calculation"""
        allowable = self.analyzer.calculate_allowable_stress()
        expected = 0.666 * 1.0 * 5.52e8
        assert abs(allowable - expected) / expected < 1e-12

    def test_pressure_stress_calculation(self):
        """Test pressure stress calculations"""
        internal_pressure = 10e6  # 10 MPa
        stresses = self.analyzer.calculate_pressure_stresses(internal_pressure)

        assert 'radial' in stresses
        assert 'circumferential' in stresses
        assert 'axial_pressure' in stresses

        # Radial stress should be negative (compression)
        assert stresses['radial'] == -internal_pressure

        # Circumferential stress should be positive and larger than axial
        assert stresses['circumferential'] > 0
        assert stresses['circumferential'] > stresses['axial_pressure']

    def test_combined_stress_analysis(self):
        """Test combined stress analysis"""
        loading = LoadingCondition(
            internal_pressure=5e6,
            axial_force=1e6,
            bending_moment=1e6
        )

        results = self.analyzer.calculate_combined_stress(loading)

        assert 'von_mises' in results
        assert 'safety_factor' in results
        assert results['von_mises'] > 0
        assert results['safety_factor'] > 0

    def test_maximum_tension_calculation(self):
        """Test maximum tension calculation based on legacy code"""
        bending_moment = 46e4  # From legacy code

        tension_pos, tension_neg = self.analyzer.calculate_maximum_tension(bending_moment)

        # Both should be real numbers
        assert not math.isnan(tension_pos)
        assert not math.isnan(tension_neg)

        # They should have opposite signs for tension/compression
        assert tension_pos * tension_neg <= 0

    def test_interaction_envelope_generation(self):
        """Test tension-moment interaction envelope"""
        moment_range = [0, 1e4, 5e4, 10e4, 46e4]

        envelope = self.analyzer.generate_interaction_envelope(moment_range)

        assert 'moments' in envelope
        assert 'tensions' in envelope
        assert len(envelope['moments']) == len(envelope['tensions'])

        # Check symmetry
        assert len(envelope['moments']) == 4 * len(moment_range)


class TestConvenienceFunctions:
    """Test convenience functions"""

    def test_calculate_vm_stress_function(self):
        """Test calculate_vm_stress convenience function"""
        vm_stress = calculate_vm_stress(100e6, 50e6, 25e6, 10e6)

        # Should match class method
        stress_state = StressState(100e6, 50e6, 25e6, 10e6)
        expected = VonMisesStressCalculator.calculate_from_stress_state(stress_state)

        assert abs(vm_stress - expected) < 1e-6

    def test_calculate_principal_stresses_function(self):
        """Test calculate_principal_stresses function"""
        stress_state = StressState(100e6, 50e6, 25e6)
        principal_stresses = calculate_principal_stresses(stress_state)

        assert len(principal_stresses) == 3
        assert principal_stresses[0] >= principal_stresses[1] >= principal_stresses[2]

        # For this simple case, should match input values
        sorted_input = sorted([100e6, 50e6, 25e6], reverse=True)
        for i in range(3):
            assert abs(principal_stresses[i] - sorted_input[i]) < 1e-6


class TestLegacyCodeComparison:
    """Test compatibility with legacy code calculations"""

    def setup_method(self):
        """Setup analyzer with exact legacy parameters"""
        self.geometry = PipeGeometry(0.24765, 0.034925)
        self.material = MaterialProperties(
            yield_strength=5.52e8,
            ultimate_strength=6.20e8,
            elastic_modulus=2.1e11,
            poisson_ratio=0.3
        )
        self.analyzer = PipeStressAnalyzer(self.geometry, self.material)

    def test_legacy_geometric_calculations(self):
        """Test that geometric calculations match legacy code"""
        # From legacy: pipeNominalID = pipe1.pipeNominalOD_m - 2*pipe1.pipeNominalWT_m
        expected_id = 0.24765 - 2 * 0.034925
        assert abs(self.geometry.inner_diameter - expected_id) < 1e-12

        # From legacy: pipeA calculation
        outer_area = (math.pi/4) * (0.24765**2)
        inner_area = (math.pi/4) * (expected_id**2)
        expected_area = outer_area - inner_area
        assert abs(self.geometry.cross_sectional_area - expected_area) < 1e-12

        # From legacy: pipeI calculation
        expected_I = (math.pi/64) * (0.24765**4 - expected_id**4)
        assert abs(self.geometry.moment_of_inertia - expected_I) < 1e-12

    def test_legacy_allowable_stress(self):
        """Test allowable stress matches legacy calculation"""
        # From legacy: SigmaA = AllowableStressFac*pipe1.pipeYieldStrength
        allowable_stress_factor = 0.666
        design_case_factor = 1.0

        self.analyzer.set_design_factors(allowable_stress_factor, design_case_factor)
        calculated = self.analyzer.calculate_allowable_stress()
        expected = allowable_stress_factor * design_case_factor * 5.52e8

        assert abs(calculated - expected) < 1e-6

    def test_legacy_moment_range_analysis(self):
        """Test analysis with legacy moment values"""
        # From legacy code: moment values used
        legacy_moments = [0, 1e3, 5e3, 10e3, 30e4, 36e4, 42e4, 46e4]

        for moment in legacy_moments:
            tension_pos, tension_neg = self.analyzer.calculate_maximum_tension(moment)

            # Results should be finite
            assert math.isfinite(tension_pos)
            assert math.isfinite(tension_neg)

            # For zero moment, tensions should be maximum
            if moment == 0:
                assert tension_pos > 0
                assert tension_neg < 0


if __name__ == "__main__":
    pytest.main([__file__])