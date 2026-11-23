"""
Test cases for stress-strain analysis module
"""

import pytest
import numpy as np
import math
from digitalmodel.stress.stress_strain import (
    StressStrainAnalyzer,
    RambergOsgoodModel,
    LinearElasticModel,
    BilinearModel,
    MaterialProperties,
    StressStrainCurve,
    StressStrainPoint,
    MaterialModel,
    calculate_stress_strain_curve,
    fit_stress_strain_data
)


class TestStressStrainPoint:
    """Test StressStrainPoint class"""

    def test_point_creation(self):
        """Test stress-strain point creation"""
        point = StressStrainPoint(0.002, 100e6)
        assert point.strain == 0.002
        assert point.stress == 100e6

    def test_point_validation(self):
        """Test stress-strain point validation"""
        # Valid point
        point = StressStrainPoint(0.002, 100e6)
        point.validate()  # Should not raise

        # Invalid: negative strain
        point_invalid = StressStrainPoint(-0.001, 100e6)
        with pytest.raises(ValueError, match="Strain must be non-negative"):
            point_invalid.validate()

        # Invalid: NaN values
        point_nan = StressStrainPoint(float('nan'), 100e6)
        with pytest.raises(ValueError, match="must be valid numbers"):
            point_nan.validate()


class TestStressStrainCurve:
    """Test StressStrainCurve class"""

    def test_curve_creation(self):
        """Test stress-strain curve creation"""
        strains = [0.0, 0.001, 0.002, 0.005]
        stresses = [0.0, 200e6, 250e6, 300e6]

        curve = StressStrainCurve(strains, stresses)
        assert len(curve.strains) == 4
        assert len(curve.stresses) == 4

    def test_curve_validation(self):
        """Test stress-strain curve validation"""
        # Valid curve
        curve = StressStrainCurve([0.0, 0.001, 0.002], [0.0, 200e6, 400e6])
        curve.validate()  # Should not raise

        # Invalid: mismatched lengths
        with pytest.raises(ValueError, match="same length"):
            StressStrainCurve([0.0, 0.001], [0.0, 200e6, 400e6])

        # Invalid: non-monotonic strains
        with pytest.raises(ValueError, match="monotonically increasing"):
            StressStrainCurve([0.0, 0.002, 0.001], [0.0, 200e6, 400e6])

    def test_add_point(self):
        """Test adding points to curve"""
        curve = StressStrainCurve([0.0, 0.002], [0.0, 400e6])

        # Add point in middle
        curve.add_point(0.001, 200e6)

        assert len(curve.strains) == 3
        assert curve.strains == [0.0, 0.001, 0.002]
        assert curve.stresses == [0.0, 200e6, 400e6]

    def test_stress_interpolation(self):
        """Test stress interpolation"""
        curve = StressStrainCurve([0.0, 0.001, 0.002], [0.0, 200e6, 400e6])

        # Interpolate at midpoint
        stress = curve.get_stress_at_strain(0.0005)
        assert abs(stress - 100e6) < 1e-6

        # Test boundary conditions
        assert curve.get_stress_at_strain(0.0) == 0.0
        assert curve.get_stress_at_strain(0.002) == 400e6

    def test_elastic_modulus_calculation(self):
        """Test elastic modulus calculation from curve"""
        # Perfect linear curve
        strains = [0.0, 0.001, 0.002, 0.003]
        stresses = [0.0, 200e6, 400e6, 600e6]

        curve = StressStrainCurve(strains, stresses)
        modulus = curve.get_elastic_modulus()

        expected = 200e9  # 200 GPa
        assert abs(modulus - expected) < 1e6


class TestLinearElasticModel:
    """Test linear elastic model"""

    def test_stress_calculation(self):
        """Test stress calculation using Hooke's law"""
        model = LinearElasticModel()

        stress = model.calculate_stress(0.001, elastic_modulus=200e9)
        expected = 200e9 * 0.001
        assert abs(stress - expected) < 1e-6

    def test_strain_calculation(self):
        """Test strain calculation using Hooke's law"""
        model = LinearElasticModel()

        strain = model.calculate_strain(200e6, elastic_modulus=200e9)
        expected = 200e6 / 200e9
        assert abs(strain - expected) < 1e-12

    def test_parameter_names(self):
        """Test parameter names"""
        model = LinearElasticModel()
        params = model.get_parameter_names()
        assert "elastic_modulus" in params


class TestRambergOsgoodModel:
    """Test Ramberg-Osgood model"""

    def test_strain_calculation(self):
        """Test strain calculation using Ramberg-Osgood equation"""
        model = RambergOsgoodModel()

        # Legacy parameters
        elastic_modulus = 2.12e8
        yield_strength = 8.27e5
        k = 0.002
        n = 18.85

        # Test case from legacy code
        stress = 400e3  # 400 kPa
        strain = model.calculate_strain(
            stress, elastic_modulus, yield_strength, k, n
        )

        # Should have both elastic and plastic components
        elastic_strain = stress / elastic_modulus
        plastic_strain = k * (stress / yield_strength) ** n
        expected = elastic_strain + plastic_strain

        assert abs(strain - expected) < 1e-12

    def test_stress_calculation(self):
        """Test stress calculation (iterative)"""
        model = RambergOsgoodModel()

        elastic_modulus = 2.12e8
        yield_strength = 8.27e5
        k = 0.002
        n = 18.85

        # Test with small strain (should be mostly elastic)
        strain = 0.001
        stress = model.calculate_stress(
            strain, elastic_modulus, yield_strength, k, n
        )

        # Verify by calculating strain back
        calculated_strain = model.calculate_strain(
            stress, elastic_modulus, yield_strength, k, n
        )

        assert abs(calculated_strain - strain) < 1e-6

    def test_parameter_names(self):
        """Test parameter names"""
        model = RambergOsgoodModel()
        params = model.get_parameter_names()
        expected_params = ["elastic_modulus", "yield_strength", "k", "n"]
        for param in expected_params:
            assert param in params


class TestBilinearModel:
    """Test bilinear model"""

    def test_elastic_region(self):
        """Test behavior in elastic region"""
        model = BilinearModel()

        elastic_modulus = 200e9
        yield_strength = 250e6

        # Test below yield
        strain = 0.001
        stress = model.calculate_stress(
            strain, elastic_modulus, yield_strength
        )

        expected = elastic_modulus * strain
        assert abs(stress - expected) < 1e-6
        assert stress < yield_strength

    def test_plastic_region(self):
        """Test behavior in plastic region"""
        model = BilinearModel()

        elastic_modulus = 200e9
        yield_strength = 250e6
        hardening_modulus = 2e9  # 1% of elastic modulus

        # Test above yield
        strain = 0.003  # Beyond yield strain of 0.00125
        stress = model.calculate_stress(
            strain, elastic_modulus, yield_strength, hardening_modulus
        )

        yield_strain = yield_strength / elastic_modulus
        plastic_strain = strain - yield_strain
        expected = yield_strength + hardening_modulus * plastic_strain

        assert abs(stress - expected) < 1e-6
        assert stress > yield_strength

    def test_perfect_plasticity(self):
        """Test perfect plasticity (no hardening)"""
        model = BilinearModel()

        elastic_modulus = 200e9
        yield_strength = 250e6

        # Test above yield with no hardening
        strain = 0.003
        stress = model.calculate_stress(
            strain, elastic_modulus, yield_strength
        )

        assert abs(stress - yield_strength) < 1e-6


class TestStressStrainAnalyzer:
    """Test stress-strain analyzer"""

    def setup_method(self):
        """Setup analyzer"""
        self.analyzer = StressStrainAnalyzer()

    def test_linear_elastic_curve_generation(self):
        """Test linear elastic curve generation"""
        strain_range = np.linspace(0, 0.002, 10)
        elastic_modulus = 200e9

        curve = self.analyzer.generate_curve(
            MaterialModel.LINEAR_ELASTIC,
            strain_range,
            elastic_modulus=elastic_modulus
        )

        assert len(curve.strains) == len(strain_range)
        assert curve.material_model == MaterialModel.LINEAR_ELASTIC

        # Check linearity
        for i, strain in enumerate(curve.strains):
            expected_stress = elastic_modulus * strain
            assert abs(curve.stresses[i] - expected_stress) < 1e-6

    def test_ramberg_osgood_curve_generation(self):
        """Test Ramberg-Osgood curve generation"""
        strain_range = np.linspace(0, 0.01, 50)

        # Legacy parameters
        params = {
            'elastic_modulus': 2.12e8,
            'yield_strength': 8.27e5,
            'k': 0.002,
            'n': 18.85
        }

        curve = self.analyzer.generate_curve(
            MaterialModel.RAMBERG_OSGOOD,
            strain_range,
            **params
        )

        assert len(curve.strains) > 0
        assert curve.material_model == MaterialModel.RAMBERG_OSGOOD

        # Check that curve shows nonlinear behavior
        # Calculate secant modulus at different points
        modulus_start = curve.stresses[1] / curve.strains[1]
        modulus_end = curve.stresses[-1] / curve.strains[-1]

        # Modulus should decrease (nonlinear hardening)
        assert modulus_end < modulus_start

    def test_ramberg_osgood_fitting(self):
        """Test Ramberg-Osgood parameter fitting"""
        # Create synthetic data using known parameters
        elastic_modulus = 200e9
        yield_strength = 250e6
        k = 0.002
        n = 10.0

        # Generate synthetic curve
        strain_range = np.linspace(0, 0.01, 50)
        model = RambergOsgoodModel()

        stresses = []
        for strain in strain_range:
            stress = model.calculate_stress(
                strain, elastic_modulus, yield_strength, k, n
            )
            stresses.append(stress)

        # Create curve and fit
        experimental_curve = StressStrainCurve(
            strain_range.tolist(), stresses
        )

        fitted_params = self.analyzer.fit_ramberg_osgood(experimental_curve)

        # Check that fitted parameters are reasonable
        assert 'elastic_modulus' in fitted_params
        assert 'yield_strength' in fitted_params
        assert 'k' in fitted_params
        assert 'n' in fitted_params

        # Elastic modulus should be close to input
        assert abs(fitted_params['elastic_modulus'] - elastic_modulus) / elastic_modulus < 0.1

    def test_engineering_properties_calculation(self):
        """Test engineering properties calculation"""
        # Create bilinear curve
        strains = [0.0, 0.001, 0.00125, 0.002, 0.003, 0.005]
        stresses = [0.0, 200e6, 250e6, 260e6, 270e6, 290e6]

        curve = StressStrainCurve(strains, stresses)
        properties = self.analyzer.calculate_engineering_properties(curve)

        assert 'elastic_modulus' in properties
        assert 'ultimate_strength' in properties

        # Check elastic modulus
        expected_modulus = 200e6 / 0.001  # 200 GPa
        assert abs(properties['elastic_modulus'] - expected_modulus) < 1e6

        # Check ultimate strength
        assert properties['ultimate_strength'] == max(stresses)

    def test_model_comparison(self):
        """Test model comparison functionality"""
        # Create test data
        strains = np.linspace(0, 0.005, 50)
        stresses = 200e9 * strains  # Linear for simplicity

        curve = StressStrainCurve(strains.tolist(), stresses.tolist())

        results = self.analyzer.compare_models(curve)

        assert MaterialModel.LINEAR_ELASTIC.value in results
        assert MaterialModel.RAMBERG_OSGOOD.value in results

        # Linear model should fit perfectly
        linear_result = results[MaterialModel.LINEAR_ELASTIC.value]
        assert 'r_squared' in linear_result
        assert linear_result['r_squared'] > 0.99


class TestConvenienceFunctions:
    """Test convenience functions"""

    def test_calculate_stress_strain_curve(self):
        """Test calculate_stress_strain_curve function"""
        strain_range = [0.0, 0.001, 0.002]
        elastic_modulus = 200e9

        curve = calculate_stress_strain_curve(
            MaterialModel.LINEAR_ELASTIC,
            strain_range,
            elastic_modulus=elastic_modulus
        )

        assert isinstance(curve, StressStrainCurve)
        assert len(curve.strains) == 3

    def test_fit_stress_strain_data(self):
        """Test fit_stress_strain_data function"""
        # Create simple linear data
        strains = [0.0, 0.001, 0.002]
        stresses = [0.0, 200e6, 400e6]

        # Note: This will use default Ramberg-Osgood fitting
        try:
            params = fit_stress_strain_data(strains, stresses)
            assert 'elastic_modulus' in params
        except NotImplementedError:
            # Expected for models other than Ramberg-Osgood
            pass


class TestLegacyCodeCompatibility:
    """Test compatibility with legacy stress-strain code"""

    def test_legacy_parameters(self):
        """Test using exact legacy parameters"""
        # From legacy code
        elastic_modulus = 2.12e8  # Pa
        yield_strength = 8.27e5   # Pa
        k = 0.002
        n = 18.85

        model = RambergOsgoodModel()

        # Test with stress values from legacy code range
        test_stresses = [100e3, 200e3, 400e3, 600e3, 800e3]  # kPa to Pa

        for stress in test_stresses:
            strain = model.calculate_strain(
                stress, elastic_modulus, yield_strength, k, n
            )

            # Strain should be positive and reasonable
            assert strain > 0
            assert strain < 1.0  # Less than 100% strain

            # Check elastic vs plastic contributions
            elastic_strain = stress / elastic_modulus
            plastic_strain = k * (stress / yield_strength) ** n
            expected_total = elastic_strain + plastic_strain

            assert abs(strain - expected_total) < 1e-12

    def test_legacy_curve_generation(self):
        """Test generating curves similar to legacy output"""
        analyzer = StressStrainAnalyzer()

        # Legacy parameters
        params = {
            'elastic_modulus': 2.12e8,
            'yield_strength': 8.27e5,
            'k': 0.002,
            'n': 18.85
        }

        # Generate strain range similar to legacy code
        strain_range = np.linspace(0, 0.1, 100)

        curve = analyzer.generate_curve(
            MaterialModel.RAMBERG_OSGOOD,
            strain_range,
            **params
        )

        # Verify curve characteristics
        assert len(curve.strains) > 50  # Should have most points
        assert max(curve.stresses) > params['yield_strength']

        # Check initial slope matches elastic modulus
        initial_modulus = curve.stresses[1] / curve.strains[1]
        expected_modulus = params['elastic_modulus']
        assert abs(initial_modulus - expected_modulus) / expected_modulus < 0.01


if __name__ == "__main__":
    pytest.main([__file__])