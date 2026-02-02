#!/usr/bin/env python3
"""
ABOUTME: Unit tests for hydrodynamics module covering wave spectra, coefficient
database, OCIMF loading, and interpolation utilities.
"""

import pytest
import numpy as np
import tempfile
from pathlib import Path

from digitalmodel.hydrodynamics.hydrodynamics.models import (
    WaveParameters,
    WaveSpectrumType,
    VesselProperties,
    EnvironmentalConditions,
    HydrodynamicMatrix,
    MatrixDOF,
    RAOData,
    get_vessel_type,
)
from digitalmodel.hydrodynamics.hydrodynamics.wave_spectra import WaveSpectra
from digitalmodel.hydrodynamics.hydrodynamics.coefficient_database import CoefficientDatabase
from digitalmodel.hydrodynamics.hydrodynamics.ocimf_loading import OCIMFLoading
from digitalmodel.hydrodynamics.hydrodynamics.interpolator import CoefficientsInterpolator


# ============================================================================
# Models Tests
# ============================================================================

class TestWaveParameters:
    """Test wave parameter model"""

    def test_jonswap_parameters(self):
        """Test JONSWAP parameters"""
        params = WaveParameters(
            spectrum_type=WaveSpectrumType.JONSWAP,
            significant_height=3.5,
            peak_period=10.0,
            gamma=3.3
        )

        assert params.spectrum_type == WaveSpectrumType.JONSWAP
        assert params.significant_height == 3.5
        assert params.peak_period == 10.0
        assert abs(params.peak_frequency - 2*np.pi/10.0) < 1e-6
        assert abs(params.zero_crossing_period - 0.711 * 10.0) < 0.1

    def test_frequency_array(self):
        """Test frequency array generation"""
        params = WaveParameters(
            spectrum_type=WaveSpectrumType.JONSWAP,
            significant_height=3.0,
            peak_period=8.0,
            n_frequencies=50
        )

        freqs = params.frequency_array()
        assert len(freqs) == 50
        assert freqs[0] == params.freq_min
        assert freqs[-1] == params.freq_max


class TestVesselProperties:
    """Test vessel properties model"""

    def test_vessel_creation(self):
        """Test vessel creation"""
        vessel = VesselProperties(
            name="Test FPSO",
            length_overall=300.0,
            beam=50.0,
            draft=20.0,
            displacement=200000.0
        )

        assert vessel.name == "Test FPSO"
        assert vessel.length_overall == 300.0
        assert abs(vessel.length_to_beam_ratio - 6.0) < 1e-6
        assert abs(vessel.beam_to_draft_ratio - 2.5) < 1e-6

    def test_get_standard_vessel(self):
        """Test getting standard vessel types"""
        fpso = get_vessel_type('fpso')
        assert fpso.name == "Standard FPSO"
        assert fpso.length_overall == 300.0

        semi = get_vessel_type('semisubmersible')
        assert semi.beam == 80.0

    def test_invalid_vessel_type(self):
        """Test error for invalid vessel type"""
        with pytest.raises(KeyError):
            get_vessel_type('invalid')


class TestHydrodynamicMatrix:
    """Test hydrodynamic matrix model"""

    def test_matrix_creation(self):
        """Test matrix creation and validation"""
        matrix = np.random.rand(6, 6)
        hydro_matrix = HydrodynamicMatrix(
            frequency=0.5,
            matrix=matrix,
            matrix_type='added_mass'
        )

        assert hydro_matrix.frequency == 0.5
        assert hydro_matrix.matrix_type == 'added_mass'
        assert hydro_matrix.matrix.shape == (6, 6)

    def test_symmetric_matrix(self):
        """Test symmetry check"""
        matrix = np.eye(6)
        hydro_matrix = HydrodynamicMatrix(
            frequency=0.5,
            matrix=matrix,
            matrix_type='added_mass'
        )

        assert hydro_matrix.is_symmetric

    def test_get_component(self):
        """Test getting matrix components"""
        matrix = np.arange(36).reshape(6, 6)
        hydro_matrix = HydrodynamicMatrix(
            frequency=0.5,
            matrix=matrix,
            matrix_type='added_mass'
        )

        surge_surge = hydro_matrix.get_component(MatrixDOF.SURGE, MatrixDOF.SURGE)
        assert surge_surge == 0

        heave_heave = hydro_matrix.get_component(MatrixDOF.HEAVE, MatrixDOF.HEAVE)
        assert heave_heave == 14


# ============================================================================
# Wave Spectra Tests
# ============================================================================

class TestWaveSpectra:
    """Test wave spectrum generation"""

    def test_jonswap_spectrum(self):
        """Test JONSWAP spectrum generation"""
        generator = WaveSpectra()
        omega, S = generator.jonswap(hs=3.5, tp=10.0, gamma=3.3)

        assert len(omega) == 100
        assert len(S) == 100
        assert np.all(S >= 0)
        assert np.max(S) > 0

        # Peak should be near peak frequency
        omega_p = 2 * np.pi / 10.0
        peak_idx = np.argmax(S)
        assert abs(omega[peak_idx] - omega_p) < 0.1

    def test_pierson_moskowitz_spectrum(self):
        """Test Pierson-Moskowitz spectrum"""
        generator = WaveSpectra()
        omega, S = generator.pierson_moskowitz(hs=3.0, tp=8.0)

        assert len(omega) == 100
        assert np.all(S >= 0)

    def test_bretschneider_spectrum(self):
        """Test Bretschneider spectrum"""
        generator = WaveSpectra()
        omega, S = generator.bretschneider(hs=2.5, tp=7.0)

        assert len(omega) == 100
        assert np.all(S >= 0)

    def test_spectral_moments(self):
        """Test spectral moment calculation"""
        generator = WaveSpectra()
        omega, S = generator.jonswap(hs=3.0, tp=10.0)

        m0 = generator.spectral_moment(omega, S, n=0)
        m2 = generator.spectral_moment(omega, S, n=2)

        assert m0 > 0
        assert m2 > 0

        # Check Hs calculation
        hs_calc = generator.significant_height_from_spectrum(omega, S)
        assert abs(hs_calc - 3.0) < 0.5  # Should be close to input

    def test_spectrum_statistics(self):
        """Test comprehensive spectrum statistics"""
        generator = WaveSpectra()
        omega, S = generator.jonswap(hs=3.5, tp=10.0, gamma=3.3)

        stats = generator.spectrum_statistics(omega, S)

        assert 'Hs_m' in stats
        assert 'Tp_s' in stats
        assert 'Tz_s' in stats
        assert abs(stats['Hs_m'] - 3.5) < 0.5
        assert abs(stats['Tp_s'] - 10.0) < 1.0


# ============================================================================
# Coefficient Database Tests
# ============================================================================

class TestCoefficientDatabase:
    """Test hydrodynamic coefficient database"""

    def test_store_and_retrieve(self):
        """Test storing and retrieving matrices"""
        db = CoefficientDatabase()

        # Create test matrices
        added_mass = np.eye(6) * 1e6
        damping = np.eye(6) * 1e5

        # Store
        db.store("FPSO", frequency=0.5, added_mass=added_mass, damping=damping)

        # Retrieve
        A, B = db.get_matrices("FPSO", frequency=0.5, interpolate=False)

        np.testing.assert_array_almost_equal(A, added_mass)
        np.testing.assert_array_almost_equal(B, damping)

    def test_multiple_frequencies(self):
        """Test multiple frequency storage"""
        db = CoefficientDatabase()

        freqs = [0.3, 0.5, 0.7]
        for freq in freqs:
            A = np.eye(6) * freq * 1e6
            B = np.eye(6) * freq * 1e5
            db.store("FPSO", frequency=freq, added_mass=A, damping=B)

        retrieved_freqs = db.get_frequencies("FPSO")
        np.testing.assert_array_almost_equal(retrieved_freqs, freqs)

    def test_interpolation(self):
        """Test frequency interpolation"""
        db = CoefficientDatabase()

        # Store at 0.4 and 0.6 rad/s
        A1 = np.eye(6) * 100.0
        A2 = np.eye(6) * 200.0

        db.store("FPSO", frequency=0.4, added_mass=A1)
        db.store("FPSO", frequency=0.6, added_mass=A2)

        # Interpolate at 0.5 (midpoint)
        A_interp = db.get_matrix("FPSO", frequency=0.5, matrix_type='added_mass')

        expected = np.eye(6) * 150.0
        np.testing.assert_array_almost_equal(A_interp, expected, decimal=2)

    def test_save_and_load(self):
        """Test database persistence"""
        db = CoefficientDatabase()

        # Store data
        A = np.eye(6) * 1e6
        B = np.eye(6) * 1e5
        db.store("FPSO", frequency=0.5, added_mass=A, damping=B)

        # Save to file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
            filepath = f.name

        try:
            db.save_to_file(filepath)

            # Load in new database
            db2 = CoefficientDatabase()
            db2.load_from_file(filepath)

            # Verify
            A2, B2 = db2.get_matrices("FPSO", frequency=0.5, interpolate=False)
            np.testing.assert_array_almost_equal(A, A2)
            np.testing.assert_array_almost_equal(B, B2)
        finally:
            Path(filepath).unlink(missing_ok=True)


# ============================================================================
# OCIMF Loading Tests
# ============================================================================

class TestOCIMFLoading:
    """Test OCIMF environmental loading calculations"""

    def test_wind_load_head_wind(self):
        """Test wind load calculation for head wind"""
        ocimf = OCIMFLoading()

        vessel = VesselProperties(
            name="FPSO",
            length_overall=300.0,
            beam=50.0,
            draft=20.0,
            displacement=200000.0,
            freeboard=15.0
        )

        loads = ocimf.wind_load(vessel, wind_speed=25.0, wind_direction=0.0)

        assert 'Fx_surge_N' in loads
        assert 'Fy_sway_N' in loads
        assert 'Mz_yaw_Nm' in loads

        # Head wind should have surge force
        assert abs(loads['Fx_surge_N']) > 0
        # Sway force should be small for head wind
        assert abs(loads['Fy_sway_N']) < abs(loads['Fx_surge_N'])

    def test_wind_load_beam_wind(self):
        """Test wind load for beam wind"""
        ocimf = OCIMFLoading()

        vessel = get_vessel_type('fpso')

        loads = ocimf.wind_load(vessel, wind_speed=20.0, wind_direction=90.0)

        # Beam wind should have large sway force
        assert abs(loads['Fy_sway_N']) > abs(loads['Fx_surge_N'])

    def test_current_load_head_current(self):
        """Test current load for head current"""
        ocimf = OCIMFLoading()

        vessel = get_vessel_type('fpso')

        loads = ocimf.current_load(vessel, current_speed=1.5, current_direction=0.0)

        assert 'Fx_surge_N' in loads
        assert 'Fy_sway_N' in loads
        assert loads['Fx_surge_N'] != 0

    def test_combined_loads(self):
        """Test combined wind and current loading"""
        ocimf = OCIMFLoading()

        vessel = get_vessel_type('fpso')

        env = EnvironmentalConditions(
            wind_speed=25.0,
            wind_direction=45.0,
            current_speed=1.5,
            current_direction=90.0
        )

        result = ocimf.combined_environmental_load(vessel, env)

        assert 'wind_loads' in result
        assert 'current_loads' in result
        assert 'combined_loads' in result

        combined = result['combined_loads']
        assert 'F_resultant_N' in combined
        assert combined['F_resultant_N'] > 0


# ============================================================================
# Interpolator Tests
# ============================================================================

class TestCoefficientsInterpolator:
    """Test RAO and coefficient interpolation"""

    def test_rao_1d_interpolation(self):
        """Test 1D RAO interpolation"""
        # Create test RAO data
        freqs = np.array([0.3, 0.5, 0.7])
        dirs = np.array([0, 45, 90])

        amplitudes = np.zeros((3, 3, 6))
        phases = np.zeros((3, 3, 6))

        # Simple linear pattern for testing
        for i, freq in enumerate(freqs):
            amplitudes[i, :, 0] = freq * 10  # Surge amplitude increases with frequency

        rao_data = RAOData(
            frequencies=freqs,
            directions=dirs,
            amplitudes=amplitudes,
            phases=phases
        )

        interp = CoefficientsInterpolator()
        interp.load_raos(rao_data)

        # Interpolate at frequency 0.4 (between 0.3 and 0.5)
        target_freqs = np.array([0.4])
        amp, phase = interp.interpolate_rao_1d(target_freqs, direction=0.0, dof=MatrixDOF.SURGE)

        # Should interpolate to 4.0
        assert abs(amp[0] - 4.0) < 0.1

    def test_extract_at_frequency(self):
        """Test extracting RAO at specific frequency"""
        freqs = np.array([0.4, 0.5, 0.6])
        dirs = np.array([0, 90, 180])

        amplitudes = np.random.rand(3, 3, 6)
        phases = np.random.rand(3, 3, 6) * 180

        rao_data = RAOData(
            frequencies=freqs,
            directions=dirs,
            amplitudes=amplitudes,
            phases=phases
        )

        interp = CoefficientsInterpolator()
        interp.load_raos(rao_data)

        result = interp.extract_rao_at_frequency(0.5, tolerance=0.01)

        assert result['frequency_rad_s'] == 0.5
        assert len(result['directions_deg']) == 3
        assert result['amplitudes'].shape == (3, 6)


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests combining multiple components"""

    def test_wave_spectrum_to_stats(self):
        """Test complete workflow: parameters → spectrum → statistics"""
        params = WaveParameters(
            spectrum_type=WaveSpectrumType.JONSWAP,
            significant_height=4.0,
            peak_period=12.0,
            gamma=3.3
        )

        generator = WaveSpectra()
        omega, S = generator.generate_spectrum(params)
        stats = generator.spectrum_statistics(omega, S)

        assert abs(stats['Hs_m'] - 4.0) < 0.5
        assert abs(stats['Tp_s'] - 12.0) < 1.5

    def test_vessel_loading_workflow(self):
        """Test vessel → environment → loads workflow"""
        vessel = get_vessel_type('fpso')

        env = EnvironmentalConditions(
            wind_speed=30.0,
            wind_direction=60.0,
            current_speed=2.0,
            current_direction=90.0
        )

        ocimf = OCIMFLoading()
        result = ocimf.combined_environmental_load(vessel, env)

        # Verify result structure
        assert all(key in result for key in ['wind_loads', 'current_loads', 'combined_loads'])

        # Combined should be different from individual
        combined_fx = result['combined_loads']['Fx_surge_N']
        wind_fx = result['wind_loads']['Fx_surge_N']
        current_fx = result['current_loads']['Fx_surge_N']

        # Combined should be approximately sum (vector addition)
        assert abs(combined_fx - (wind_fx + current_fx)) < 1e-3
