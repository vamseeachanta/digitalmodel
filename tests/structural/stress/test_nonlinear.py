"""
Test cases for nonlinear stress analysis module
"""

import pytest
import numpy as np
import math
from digitalmodel.stress.nonlinear import (
    NonlinearStressAnalyzer,
    VonMisesYield,
    TrescaYield,
    IsotropicHardening,
    LinearHardening,
    PlasticState,
    LoadStep,
    NonlinearSolution,
    PlasticityModel,
    HardeningType,
    calculate_nonlinear_response,
    solve_nonlinear_stress
)


class TestVonMisesYield:
    """Test Von Mises yield criterion"""

    def test_yield_criterion_creation(self):
        """Test Von Mises yield criterion creation"""
        yield_stress = 250e6
        criterion = VonMisesYield(yield_stress)
        assert criterion.yield_stress == yield_stress

    def test_uniaxial_stress_evaluation(self):
        """Test yield function evaluation for uniaxial stress"""
        yield_stress = 250e6
        criterion = VonMisesYield(yield_stress)

        # Stress at yield
        stress = np.array([yield_stress, 0, 0, 0, 0, 0])
        yield_value = criterion.evaluate(stress)

        # Should be approximately zero at yield
        assert abs(yield_value) < 1e-6

        # Stress below yield
        stress_low = np.array([200e6, 0, 0, 0, 0, 0])
        yield_value_low = criterion.evaluate(stress_low)
        assert yield_value_low < 0

        # Stress above yield
        stress_high = np.array([300e6, 0, 0, 0, 0, 0])
        yield_value_high = criterion.evaluate(stress_high)
        assert yield_value_high > 0

    def test_pure_shear_evaluation(self):
        """Test yield function for pure shear"""
        yield_stress = 250e6
        criterion = VonMisesYield(yield_stress)

        # Pure shear at yield (τ = σy/√3)
        shear_stress = yield_stress / math.sqrt(3)
        stress = np.array([0, 0, 0, shear_stress, 0, 0])
        yield_value = criterion.evaluate(stress)

        # Should be approximately zero
        assert abs(yield_value) < 1e-3

    def test_derivative_calculation(self):
        """Test derivative calculation"""
        yield_stress = 250e6
        criterion = VonMisesYield(yield_stress)

        stress = np.array([100e6, 50e6, 25e6, 10e6, 5e6, 0])
        derivative = criterion.derivative(stress)

        assert len(derivative) == 6
        assert all(math.isfinite(val) for val in derivative)

    def test_hardening_stress_effect(self):
        """Test effect of hardening stress"""
        yield_stress = 250e6
        criterion = VonMisesYield(yield_stress)

        stress = np.array([250e6, 0, 0, 0, 0, 0])

        # Without hardening
        yield_value_no_hardening = criterion.evaluate(stress)
        assert abs(yield_value_no_hardening) < 1e-6

        # With hardening
        hardening_stress = 50e6
        yield_value_with_hardening = criterion.evaluate(stress, hardening_stress)
        assert yield_value_with_hardening < 0  # Now below yield


class TestTrescaYield:
    """Test Tresca yield criterion"""

    def test_tresca_yield_creation(self):
        """Test Tresca yield criterion creation"""
        yield_stress = 250e6
        criterion = TrescaYield(yield_stress)
        assert criterion.yield_stress == yield_stress

    def test_uniaxial_stress_tresca(self):
        """Test Tresca criterion for uniaxial stress"""
        yield_stress = 250e6
        criterion = TrescaYield(yield_stress)

        # Uniaxial tension at yield
        stress = np.array([yield_stress, 0, 0, 0, 0, 0])
        yield_value = criterion.evaluate(stress)

        # Should be approximately zero
        assert abs(yield_value) < 1e-3


class TestHardeningModels:
    """Test hardening models"""

    def test_linear_hardening(self):
        """Test linear hardening model"""
        hardening_modulus = 2e9
        model = LinearHardening(hardening_modulus)

        # Test hardening stress calculation
        plastic_strain = 0.01
        hardening_stress = model.calculate_hardening_stress(plastic_strain)
        expected = hardening_modulus * plastic_strain
        assert abs(hardening_stress - expected) < 1e-6

        # Test hardening modulus (should be constant)
        modulus = model.calculate_hardening_modulus(plastic_strain)
        assert abs(modulus - hardening_modulus) < 1e-6

    def test_isotropic_hardening(self):
        """Test isotropic hardening model"""
        hardening_modulus = 2e9
        hardening_exponent = 0.5
        model = IsotropicHardening(hardening_modulus, hardening_exponent)

        # Test hardening stress calculation
        plastic_strain = 0.01
        hardening_stress = model.calculate_hardening_stress(plastic_strain)
        expected = hardening_modulus * (plastic_strain ** hardening_exponent)
        assert abs(hardening_stress - expected) < 1e-6

        # Test hardening modulus
        modulus = model.calculate_hardening_modulus(plastic_strain)
        expected_modulus = (hardening_modulus * hardening_exponent *
                          (plastic_strain ** (hardening_exponent - 1)))
        assert abs(modulus - expected_modulus) < 1e-6

    def test_hardening_at_zero_strain(self):
        """Test hardening models at zero plastic strain"""
        linear_model = LinearHardening(2e9)
        iso_model = IsotropicHardening(2e9, 0.5)

        # Both should give zero hardening stress at zero plastic strain
        assert linear_model.calculate_hardening_stress(0.0) == 0.0
        assert iso_model.calculate_hardening_stress(0.0) == 0.0


class TestNonlinearStressAnalyzer:
    """Test nonlinear stress analyzer"""

    def setup_method(self):
        """Setup analyzer for testing"""
        self.elastic_modulus = 200e9
        self.poisson_ratio = 0.3
        self.yield_stress = 250e6
        self.hardening_modulus = 2e9

        self.yield_criterion = VonMisesYield(self.yield_stress)
        self.hardening_model = LinearHardening(self.hardening_modulus)

        self.analyzer = NonlinearStressAnalyzer(
            self.elastic_modulus,
            self.poisson_ratio,
            self.yield_criterion,
            self.hardening_model
        )

    def test_analyzer_creation(self):
        """Test analyzer creation and initialization"""
        assert self.analyzer.elastic_modulus == self.elastic_modulus
        assert self.analyzer.poisson_ratio == self.poisson_ratio
        assert self.analyzer.elastic_matrix.shape == (6, 6)

    def test_elastic_matrix_calculation(self):
        """Test elastic constitutive matrix"""
        E = self.elastic_modulus
        nu = self.poisson_ratio

        # Check diagonal terms
        lambda_lame = (E * nu) / ((1 + nu) * (1 - 2*nu))
        mu = E / (2 * (1 + nu))

        expected_c11 = lambda_lame + 2*mu
        assert abs(self.analyzer.elastic_matrix[0, 0] - expected_c11) < 1e-6
        assert abs(self.analyzer.elastic_matrix[3, 3] - mu) < 1e-6

        # Check off-diagonal terms
        assert abs(self.analyzer.elastic_matrix[0, 1] - lambda_lame) < 1e-6

    def test_elastic_loading(self):
        """Test purely elastic loading"""
        # Apply strain below yield
        yield_strain = self.yield_stress / self.elastic_modulus
        applied_strain = yield_strain * 0.8  # 80% of yield strain

        total_strain = np.array([applied_strain, 0, 0, 0, 0, 0])

        solution = self.analyzer.solve_incremental_plasticity(total_strain, 5)

        # Check that all steps converged
        assert all(step.converged for step in solution.load_steps)

        # Check that final stress is elastic
        expected_stress = self.elastic_modulus * applied_strain
        assert abs(solution.final_stress[0] - expected_stress) < 1e-3

        # Check that plastic strain is zero
        assert abs(solution.plastic_strain_history[-1][0]) < 1e-12

    def test_plastic_loading(self):
        """Test plastic loading beyond yield"""
        # Apply strain well beyond yield
        yield_strain = self.yield_stress / self.elastic_modulus
        applied_strain = yield_strain * 2.0  # 200% of yield strain

        total_strain = np.array([applied_strain, 0, 0, 0, 0, 0])

        solution = self.analyzer.solve_incremental_plasticity(total_strain, 10)

        # Check that solution was obtained
        assert solution.final_stress is not None
        assert solution.final_strain is not None

        # Check that plastic strain developed
        final_plastic_strain = solution.plastic_strain_history[-1][0]
        assert final_plastic_strain > 0

        # Check that final stress is above yield
        final_stress = solution.final_stress[0]
        assert final_stress > self.yield_stress

    def test_return_mapping_convergence(self):
        """Test return mapping algorithm convergence"""
        # Create a trial stress above yield
        trial_stress = np.array([300e6, 0, 0, 0, 0, 0])  # Above yield
        plastic_strain = np.zeros(6)
        equivalent_plastic_strain = 0.0
        strain_increment = np.array([0.001, 0, 0, 0, 0, 0])

        result = self.analyzer._return_mapping(
            trial_stress, plastic_strain, equivalent_plastic_strain, strain_increment
        )

        # Should converge
        assert result['converged']
        assert result['iterations'] < self.analyzer.max_iterations

        # Stress should be on yield surface
        final_stress = result['stress']
        yield_value = self.yield_criterion.evaluate(final_stress, 0)
        assert abs(yield_value) < self.analyzer.tolerance

    def test_plastic_work_calculation(self):
        """Test plastic work calculation"""
        # Apply plastic loading
        yield_strain = self.yield_stress / self.elastic_modulus
        applied_strain = yield_strain * 2.0

        total_strain = np.array([applied_strain, 0, 0, 0, 0, 0])
        solution = self.analyzer.solve_incremental_plasticity(total_strain, 10)

        # Calculate plastic work
        plastic_work = self.analyzer.calculate_plastic_work(solution)

        # Plastic work should be positive for plastic deformation
        assert plastic_work > 0

    def test_failure_prediction(self):
        """Test failure prediction"""
        # Create solution with increasing strain
        solution = NonlinearSolution()

        # Add strain history that exceeds failure
        strains = [np.array([i * 0.01, 0, 0, 0, 0, 0]) for i in range(10)]
        solution.strain_history = strains

        # Predict failure at 5% strain
        failure_step = self.analyzer.predict_failure(solution, 0.05)

        assert failure_step is not None
        assert failure_step < len(strains)


class TestNonlinearAnalysisScenarios:
    """Test various nonlinear analysis scenarios"""

    def test_perfect_plasticity(self):
        """Test perfect plasticity (no hardening)"""
        elastic_modulus = 200e9
        yield_stress = 250e6

        yield_criterion = VonMisesYield(yield_stress)

        analyzer = NonlinearStressAnalyzer(
            elastic_modulus, 0.3, yield_criterion, None  # No hardening
        )

        # Apply strain beyond yield
        yield_strain = yield_stress / elastic_modulus
        applied_strain = yield_strain * 1.5

        total_strain = np.array([applied_strain, 0, 0, 0, 0, 0])
        solution = analyzer.solve_incremental_plasticity(total_strain, 5)

        # In perfect plasticity, stress should remain at yield
        final_stress = solution.final_stress[0]
        assert abs(final_stress - yield_stress) < 1e-3

    def test_different_hardening_models(self):
        """Test different hardening models"""
        elastic_modulus = 200e9
        yield_stress = 250e6

        yield_criterion = VonMisesYield(yield_stress)

        # Linear hardening
        linear_hardening = LinearHardening(2e9)
        analyzer_linear = NonlinearStressAnalyzer(
            elastic_modulus, 0.3, yield_criterion, linear_hardening
        )

        # Isotropic hardening
        iso_hardening = IsotropicHardening(2e9, 0.5)
        analyzer_iso = NonlinearStressAnalyzer(
            elastic_modulus, 0.3, yield_criterion, iso_hardening
        )

        # Apply same strain to both
        yield_strain = yield_stress / elastic_modulus
        applied_strain = yield_strain * 2.0
        total_strain = np.array([applied_strain, 0, 0, 0, 0, 0])

        solution_linear = analyzer_linear.solve_incremental_plasticity(total_strain, 10)
        solution_iso = analyzer_iso.solve_incremental_plasticity(total_strain, 10)

        # Both should give different final stresses due to different hardening
        final_stress_linear = solution_linear.final_stress[0]
        final_stress_iso = solution_iso.final_stress[0]

        assert abs(final_stress_linear - final_stress_iso) > 1e6  # Should differ significantly


class TestConvenienceFunctions:
    """Test convenience functions"""

    def test_calculate_nonlinear_response(self):
        """Test calculate_nonlinear_response function"""
        # Create stress history
        stress_history = [
            np.array([100e6, 0, 0, 0, 0, 0]),
            np.array([200e6, 0, 0, 0, 0, 0]),
            np.array([300e6, 0, 0, 0, 0, 0])
        ]

        elastic_modulus = 200e9
        yield_stress = 250e6
        hardening_modulus = 2e9

        result = calculate_nonlinear_response(
            stress_history, elastic_modulus, yield_stress, hardening_modulus
        )

        assert 'strains' in result
        assert 'stresses' in result
        assert 'plastic_strains' in result

        assert len(result['strains']) == len(stress_history)

        # First two should be elastic, third should have plastic component
        assert result['plastic_strains'][0] == 0
        assert result['plastic_strains'][1] == 0
        assert result['plastic_strains'][2] > 0

    def test_solve_nonlinear_stress(self):
        """Test solve_nonlinear_stress function"""
        # Test with elastic strain
        elastic_strain = 0.001
        elastic_modulus = 200e9
        yield_stress = 250e6

        result_elastic = solve_nonlinear_stress(
            elastic_strain, elastic_modulus, yield_stress
        )

        assert result_elastic['converged']
        assert abs(result_elastic['plastic_strain']) < 1e-12
        assert abs(result_elastic['final_stress'] - elastic_modulus * elastic_strain) < 1e3

        # Test with plastic strain
        plastic_strain = 0.003
        hardening_modulus = 2e9

        result_plastic = solve_nonlinear_stress(
            plastic_strain, elastic_modulus, yield_stress, hardening_modulus
        )

        assert result_plastic['converged']
        assert result_plastic['plastic_strain'] > 0
        assert result_plastic['final_stress'] > yield_stress


class TestEdgeCases:
    """Test edge cases and error conditions"""

    def test_zero_yield_stress(self):
        """Test behavior with very small yield stress"""
        # This should handle gracefully
        small_yield = 1e3  # 1 kPa
        criterion = VonMisesYield(small_yield)

        stress = np.array([small_yield, 0, 0, 0, 0, 0])
        yield_value = criterion.evaluate(stress)
        assert abs(yield_value) < 1e-6

    def test_large_strain_increment(self):
        """Test with very large strain increment"""
        elastic_modulus = 200e9
        yield_stress = 250e6
        yield_criterion = VonMisesYield(yield_stress)
        hardening_model = LinearHardening(2e9)

        analyzer = NonlinearStressAnalyzer(
            elastic_modulus, 0.3, yield_criterion, hardening_model
        )

        # Very large strain
        large_strain = 0.1  # 10% strain
        total_strain = np.array([large_strain, 0, 0, 0, 0, 0])

        solution = analyzer.solve_incremental_plasticity(total_strain, 20)

        # Should still provide a solution
        assert solution.final_stress is not None
        assert solution.final_strain is not None

    def test_invalid_stress_input(self):
        """Test with invalid stress input"""
        criterion = VonMisesYield(250e6)

        # Wrong size stress vector
        with pytest.raises(ValueError, match="6-component vector"):
            criterion.evaluate(np.array([100e6, 50e6, 25e6]))  # Only 3 components

    def test_convergence_failure_handling(self):
        """Test handling of convergence failure"""
        # Create analyzer with very tight tolerance
        elastic_modulus = 200e9
        yield_criterion = VonMisesYield(250e6)
        hardening_model = LinearHardening(2e9)

        analyzer = NonlinearStressAnalyzer(
            elastic_modulus, 0.3, yield_criterion, hardening_model
        )

        # Set very tight tolerance and low iteration limit
        analyzer.tolerance = 1e-15
        analyzer.max_iterations = 3

        # Apply strain that requires plastic response
        yield_strain = 250e6 / elastic_modulus
        total_strain = np.array([yield_strain * 2, 0, 0, 0, 0, 0])

        solution = analyzer.solve_incremental_plasticity(total_strain, 5)

        # Some steps may not converge, but solution should still be provided
        assert solution.final_stress is not None


if __name__ == "__main__":
    pytest.main([__file__])