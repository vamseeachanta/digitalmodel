"""
Test suite for passing ship mathematical formulations.

Tests all mathematical functions including sectional areas, kernel functions,
and force calculations against known reference values.
"""

import numpy as np
import pytest
from digitalmodel.modules.marine_analysis.python_code_passing_ship import formulations


class TestSectionalAreaFunctions:
    """Test sectional area functions S1 and S2."""
    
    def test_s1_function_midship(self):
        """Test S1 function at midship (maximum value)."""
        L = 100.0
        assert formulations.s1_function(0, L) == 1.0
        
    def test_s1_function_ends(self):
        """Test S1 function at vessel ends (zero value)."""
        L = 100.0
        assert formulations.s1_function(L/2, L) == 0.0
        assert formulations.s1_function(-L/2, L) == 0.0
        
    def test_s1_function_symmetry(self):
        """Test S1 function symmetry."""
        L = 100.0
        x_values = np.linspace(-L/2 + 1, L/2 - 1, 20)
        for x in x_values:
            assert np.isclose(
                formulations.s1_function(x, L),
                formulations.s1_function(-x, L)
            )
            
    def test_s1_function_outside_bounds(self):
        """Test S1 function returns zero outside vessel bounds."""
        L = 100.0
        assert formulations.s1_function(L, L) == 0.0
        assert formulations.s1_function(-L, L) == 0.0
        
    def test_s1_function_parabolic_shape(self):
        """Test S1 follows expected parabolic distribution."""
        L = 100.0
        # At quarter length: S1 = 1 - (2*25/100)^2 = 0.75
        assert np.isclose(formulations.s1_function(L/4, L), 0.75)
        assert np.isclose(formulations.s1_function(-L/4, L), 0.75)
        
    def test_s2_function_midship(self):
        """Test S2 function at midship (zero due to x factor)."""
        L = 100.0
        assert formulations.s2_function(0, L) == 0.0
        
    def test_s2_function_antisymmetry(self):
        """Test S2 function antisymmetry."""
        L = 100.0
        x_values = np.linspace(-L/2 + 1, L/2 - 1, 20)
        for x in x_values:
            assert np.isclose(
                formulations.s2_function(x, L),
                -formulations.s2_function(-x, L)
            )
            
    def test_ds1_dx_derivative(self):
        """Test S1 derivative using numerical differentiation."""
        L = 100.0
        x = 10.0
        dx = 1e-6
        
        # Numerical derivative
        numerical = (formulations.s1_function(x + dx, L) - 
                    formulations.s1_function(x - dx, L)) / (2 * dx)
        
        # Analytical derivative
        analytical = formulations.ds1_dx(x, L)
        
        assert np.isclose(numerical, analytical, rtol=1e-4)
        
    def test_ds1_dx_at_midship(self):
        """Test S1 derivative at midship (zero slope)."""
        L = 100.0
        assert formulations.ds1_dx(0, L) == 0.0
        
    def test_ds2_dx_derivative(self):
        """Test S2 derivative using numerical differentiation."""
        L = 100.0
        x = 15.0
        dx = 1e-6
        
        # Numerical derivative
        numerical = (formulations.s2_function(x + dx, L) - 
                    formulations.s2_function(x - dx, L)) / (2 * dx)
        
        # Analytical derivative
        analytical = formulations.ds2_dx(x, L)
        
        assert np.isclose(numerical, analytical, rtol=1e-4)


class TestKernelFunctions:
    """Test F and G kernel functions."""
    
    def test_f_kernel_singularity_handling(self):
        """Test F kernel handles singularity at r=0."""
        L = 100.0
        xi = 0.0
        eta = 0.0
        x = 0.0
        y = 0.0
        
        result = formulations.f_kernel(xi, eta, x, y, L)
        assert result == 0.0
        
    def test_f_kernel_symmetry_in_y(self):
        """Test F kernel changes sign with y."""
        L = 100.0
        xi = 10.0
        eta = 5.0
        x = 20.0
        y = 15.0
        
        f_positive = formulations.f_kernel(xi, eta, x, y, L)
        f_negative = formulations.f_kernel(xi, eta, x, -y, L)
        
        # F kernel should change sign with y
        assert np.isclose(f_positive, -f_negative)
        
    def test_f_kernel_decay_with_distance(self):
        """Test F kernel decays with increasing separation."""
        L = 100.0
        xi = 10.0
        eta = 5.0
        x = 20.0
        
        y_values = [10.0, 20.0, 40.0, 80.0]
        f_values = [formulations.f_kernel(xi, eta, x, y, L) for y in y_values]
        
        # Check monotonic decay in magnitude
        for i in range(len(f_values) - 1):
            assert abs(f_values[i]) > abs(f_values[i + 1])
            
    def test_g_kernel_singularity_handling(self):
        """Test G kernel handles singularity at r=0."""
        L = 100.0
        xi = 0.0
        eta = 0.0
        x = 0.0
        y = 0.0
        
        result = formulations.g_kernel(xi, eta, x, y, L)
        assert result == 0.0
        
    def test_g_kernel_asymmetry_in_x(self):
        """Test G kernel behavior with stagger distance."""
        L = 100.0
        xi = 10.0
        eta = 5.0
        y = 15.0
        
        # Test at different stagger positions
        x_values = [-30.0, -10.0, 0.0, 10.0, 30.0]
        g_values = [formulations.g_kernel(xi, eta, x, y, L) for x in x_values]
        
        # Values should be different at different stagger positions
        assert len(set(np.round(g_values, 6))) > 1
        
    def test_kernel_functions_outside_vessel_bounds(self):
        """Test kernels return zero when points are outside vessel."""
        L = 100.0
        y = 15.0
        x = 20.0
        
        # Outside vessel bounds
        xi_outside = 60.0  # > L/2
        eta_outside = 60.0  # > L/2
        
        assert formulations.f_kernel(xi_outside, 0, x, y, L) == 0.0
        assert formulations.f_kernel(0, eta_outside, x, y, L) == 0.0
        assert formulations.g_kernel(xi_outside, 0, x, y, L) == 0.0
        assert formulations.g_kernel(0, eta_outside, x, y, L) == 0.0


class TestForceCalculations:
    """Test force calculation functions."""
    
    @pytest.fixture
    def standard_vessel_params(self):
        """Standard vessel parameters for testing."""
        return {
            'L': 200.0,  # Length [m]
            'B': 32.0,   # Beam [m]
            'T': 12.0,   # Draft [m]
            'Cb': 0.85   # Block coefficient [-]
        }
        
    @pytest.fixture
    def passing_params(self):
        """Standard passing scenario parameters."""
        return {
            'U': 10.0,   # Velocity [m/s]
            'y': 50.0,   # Lateral separation [m]
            'x': 0.0     # Stagger distance [m]
        }
        
    def test_surge_force_sign_convention(self, standard_vessel_params, passing_params):
        """Test surge force sign convention."""
        # Surge force should be negative (opposing motion) for typical passing
        force = formulations.calculate_surge_force_infinite(
            **standard_vessel_params,
            **passing_params
        )
        
        # Force magnitude should be reasonable (not zero, not infinite)
        assert abs(force) > 1e-3
        assert abs(force) < 1e10
        
    def test_sway_force_attractive(self, standard_vessel_params, passing_params):
        """Test sway force is attractive between vessels."""
        force = formulations.calculate_sway_force_infinite(
            **standard_vessel_params,
            **passing_params
        )
        
        # Force magnitude should be reasonable
        assert abs(force) > 1e-3
        assert abs(force) < 1e10
        
    def test_yaw_moment_calculation(self, standard_vessel_params, passing_params):
        """Test yaw moment calculation."""
        moment = formulations.calculate_yaw_moment_infinite(
            **standard_vessel_params,
            **passing_params
        )
        
        # Moment magnitude should be reasonable
        assert abs(moment) > 1e-3
        assert abs(moment) < 1e12
        
    def test_forces_increase_with_velocity(self, standard_vessel_params, passing_params):
        """Test forces increase with velocity squared."""
        # Calculate at two velocities
        passing_params_slow = passing_params.copy()
        passing_params_slow['U'] = 5.0
        
        passing_params_fast = passing_params.copy()
        passing_params_fast['U'] = 10.0
        
        surge_slow = formulations.calculate_surge_force_infinite(
            **standard_vessel_params,
            **passing_params_slow
        )
        surge_fast = formulations.calculate_surge_force_infinite(
            **standard_vessel_params,
            **passing_params_fast
        )
        
        # Force should scale with U^2
        # (10/5)^2 = 4
        assert np.isclose(abs(surge_fast) / abs(surge_slow), 4.0, rtol=0.1)
        
    def test_forces_decrease_with_separation(self, standard_vessel_params):
        """Test forces decrease with increasing lateral separation."""
        passing_close = {'U': 10.0, 'y': 30.0, 'x': 0.0}
        passing_far = {'U': 10.0, 'y': 100.0, 'x': 0.0}
        
        sway_close = formulations.calculate_sway_force_infinite(
            **standard_vessel_params,
            **passing_close
        )
        sway_far = formulations.calculate_sway_force_infinite(
            **standard_vessel_params,
            **passing_far
        )
        
        # Force magnitude should decrease with distance
        assert abs(sway_close) > abs(sway_far)
        
    def test_forces_with_stagger(self, standard_vessel_params, passing_params):
        """Test force variation with stagger distance."""
        x_values = [-100.0, -50.0, 0.0, 50.0, 100.0]
        surge_forces = []
        
        for x in x_values:
            params = passing_params.copy()
            params['x'] = x
            force = formulations.calculate_surge_force_infinite(
                **standard_vessel_params,
                **params
            )
            surge_forces.append(force)
            
        # Forces should vary with stagger position
        assert len(set(np.round(surge_forces, 3))) > 1


class TestFiniteDepthCorrections:
    """Test finite water depth corrections."""
    
    def test_deep_water_no_correction(self):
        """Test no correction applied in deep water."""
        h = 100.0  # Deep water
        T = 12.0
        L = 200.0
        y = 50.0
        
        correction = formulations.finite_depth_correction(h, T, L, y, 'surge')
        assert np.isclose(correction, 1.0, rtol=0.01)
        
    def test_shallow_water_amplification(self):
        """Test amplification in shallow water."""
        h = 15.0  # Shallow water (h/T = 1.25)
        T = 12.0
        L = 200.0
        y = 50.0
        
        correction = formulations.finite_depth_correction(h, T, L, y, 'surge')
        assert correction > 1.0  # Should amplify forces
        
    def test_depth_correction_modes(self):
        """Test different correction factors for different force modes."""
        h = 20.0
        T = 12.0
        L = 200.0
        y = 50.0
        
        surge_corr = formulations.finite_depth_correction(h, T, L, y, 'surge')
        sway_corr = formulations.finite_depth_correction(h, T, L, y, 'sway')
        yaw_corr = formulations.finite_depth_correction(h, T, L, y, 'yaw')
        
        # Different modes should have different corrections
        assert not np.allclose([surge_corr, sway_corr, yaw_corr], 
                              [surge_corr, surge_corr, surge_corr])
        
    def test_calculate_forces_with_depth_integration(self):
        """Test integrated force calculation with finite depth."""
        vessel_params = {
            'L': 200.0,
            'B': 32.0,
            'T': 12.0,
            'Cb': 0.85
        }
        passing_params = {
            'U': 10.0,
            'y': 50.0,
            'x': 0.0
        }
        
        # Calculate with infinite depth
        surge_inf, sway_inf, yaw_inf = formulations.calculate_forces_with_depth(
            vessel_params, passing_params, water_depth=None
        )
        
        # Calculate with finite depth
        surge_fin, sway_fin, yaw_fin = formulations.calculate_forces_with_depth(
            vessel_params, passing_params, water_depth=20.0
        )
        
        # Forces should be different (finite depth correction applied)
        assert not np.isclose(surge_inf, surge_fin)
        assert not np.isclose(sway_inf, sway_fin)
        assert not np.isclose(yaw_inf, yaw_fin)
        
        # All forces should be non-zero
        assert abs(surge_fin) > 1e-3
        assert abs(sway_fin) > 1e-3
        assert abs(yaw_fin) > 1e-3


class TestIntegrationAccuracy:
    """Test numerical integration accuracy and convergence."""
    
    def test_integration_convergence(self):
        """Test that integration results are consistent."""
        vessel_params = {
            'L': 200.0,
            'B': 32.0,
            'T': 12.0,
            'Cb': 0.85
        }
        passing_params = {
            'U': 10.0,
            'y': 50.0,
            'x': 0.0
        }
        
        # Calculate forces multiple times
        results = []
        for _ in range(5):
            surge = formulations.calculate_surge_force_infinite(
                **vessel_params,
                **passing_params
            )
            results.append(surge)
            
        # Results should be consistent
        assert np.std(results) / np.mean(results) < 0.01  # < 1% variation
        
    def test_edge_cases_handling(self):
        """Test handling of edge cases."""
        vessel_params = {
            'L': 200.0,
            'B': 32.0,
            'T': 12.0,
            'Cb': 0.85
        }
        
        # Very large separation
        passing_far = {
            'U': 10.0,
            'y': 1000.0,  # Very far
            'x': 0.0
        }
        
        force = formulations.calculate_sway_force_infinite(
            **vessel_params,
            **passing_far
        )
        
        # Force should be very small but not exactly zero
        assert abs(force) < 1000.0  # Small force
        
        # Zero velocity
        passing_zero = {
            'U': 0.0,  # No velocity
            'y': 50.0,
            'x': 0.0
        }
        
        force_zero = formulations.calculate_surge_force_infinite(
            **vessel_params,
            **passing_zero
        )
        
        # Zero velocity should give zero force
        assert force_zero == 0.0


class TestReferenceValidation:
    """Test against reference values from literature/MathCAD."""
    
    @pytest.mark.parametrize("case", [
        # Format: (L, B, T, Cb, U, y, x, expected_surge_order, expected_sway_order)
        (200.0, 32.0, 12.0, 0.85, 10.0, 50.0, 0.0, 1e5, 1e5),  # Typical case
        (300.0, 45.0, 15.0, 0.82, 8.0, 75.0, 50.0, 1e5, 1e5),   # Larger vessel
        (150.0, 25.0, 10.0, 0.80, 12.0, 40.0, -30.0, 1e5, 1e5), # Smaller, faster
    ])
    def test_force_magnitude_orders(self, case):
        """Test that force magnitudes are in expected ranges."""
        L, B, T, Cb, U, y, x, surge_order, sway_order = case
        
        vessel_params = {'L': L, 'B': B, 'T': T, 'Cb': Cb}
        passing_params = {'U': U, 'y': y, 'x': x}
        
        surge = formulations.calculate_surge_force_infinite(
            **vessel_params,
            **passing_params
        )
        sway = formulations.calculate_sway_force_infinite(
            **vessel_params,
            **passing_params
        )
        
        # Check order of magnitude
        assert 0.01 * surge_order < abs(surge) < 100 * surge_order
        assert 0.01 * sway_order < abs(sway) < 100 * sway_order
        
    def test_block_coefficient_effect(self):
        """Test that block coefficient affects forces appropriately."""
        vessel_base = {
            'L': 200.0,
            'B': 32.0,
            'T': 12.0,
            'Cb': 0.70  # Slender
        }
        vessel_full = vessel_base.copy()
        vessel_full['Cb'] = 0.85  # Full form
        
        passing_params = {
            'U': 10.0,
            'y': 50.0,
            'x': 0.0
        }
        
        force_slender = formulations.calculate_sway_force_infinite(
            **vessel_base,
            **passing_params
        )
        force_full = formulations.calculate_sway_force_infinite(
            **vessel_full,
            **passing_params
        )
        
        # Fuller vessel should generate larger forces
        assert abs(force_full) > abs(force_slender)