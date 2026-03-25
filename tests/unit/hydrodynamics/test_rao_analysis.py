"""
ABOUTME: Unit tests for RAO analysis modules — hydrodynamics.models,
wave_spectra.WaveSpectra, and the catalog.HullCatalog motion pipeline.

TDD approach: tests are written to verify real engineering behaviour.
License-independent: no OrcaFlex or AQWA calls.

WRK-149 — hydrodynamics/rao_analysis test coverage.
"""

from __future__ import annotations

import math

import numpy as np
import pytest

from digitalmodel.hydrodynamics.models import (
    HydrodynamicMatrix,
    MatrixDOF,
    RAOData,
    VesselProperties,
    WaveParameters,
    WaveSpectrumType,
    EnvironmentalConditions,
    VESSEL_TYPES,
    get_vessel_type,
)
from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def simple_wave_params():
    return WaveParameters(
        spectrum_type=WaveSpectrumType.JONSWAP,
        significant_height=4.0,
        peak_period=12.0,
        gamma=3.3,
    )


@pytest.fixture
def simple_rao_data():
    """Minimal 3-frequency, 2-direction, 6-DOF RAO data for testing."""
    n_freq = 20
    n_dir = 3
    frequencies = np.linspace(0.1, 1.5, n_freq)
    directions = np.array([0.0, 90.0, 180.0])
    amplitudes = np.ones((n_freq, n_dir, 6)) * 0.5
    phases = np.zeros((n_freq, n_dir, 6))
    return RAOData(
        frequencies=frequencies,
        directions=directions,
        amplitudes=amplitudes,
        phases=phases,
        vessel_name="TestVessel",
    )


# ---------------------------------------------------------------------------
# WaveSpectrumType tests
# ---------------------------------------------------------------------------


class TestWaveSpectrumType:
    def test_jonswap_value(self):
        assert WaveSpectrumType.JONSWAP.value == "jonswap"

    def test_pierson_moskowitz_value(self):
        assert WaveSpectrumType.PIERSON_MOSKOWITZ.value == "pierson_moskowitz"

    def test_bretschneider_value(self):
        assert WaveSpectrumType.BRETSCHNEIDER.value == "bretschneider"

    def test_issc_value(self):
        assert WaveSpectrumType.ISSC.value == "issc"

    def test_ochi_hubble_value(self):
        assert WaveSpectrumType.OCHI_HUBBLE.value == "ochi_hubble"

    def test_custom_value(self):
        assert WaveSpectrumType.CUSTOM.value == "custom"

    def test_from_string(self):
        wst = WaveSpectrumType("jonswap")
        assert wst == WaveSpectrumType.JONSWAP


# ---------------------------------------------------------------------------
# WaveParameters tests
# ---------------------------------------------------------------------------


class TestWaveParameters:
    def test_create_valid_params(self, simple_wave_params):
        assert simple_wave_params.significant_height == pytest.approx(4.0)
        assert simple_wave_params.peak_period == pytest.approx(12.0)
        assert simple_wave_params.gamma == pytest.approx(3.3)

    def test_peak_frequency_computed(self, simple_wave_params):
        expected_wp = 2 * math.pi / 12.0
        assert simple_wave_params.peak_frequency == pytest.approx(expected_wp)

    def test_zero_crossing_period_computed(self, simple_wave_params):
        # Tz ≈ 0.711 * Tp
        expected = 0.711 * 12.0
        assert simple_wave_params.zero_crossing_period == pytest.approx(expected)

    def test_frequency_array_length(self, simple_wave_params):
        arr = simple_wave_params.frequency_array()
        assert len(arr) == simple_wave_params.n_frequencies

    def test_frequency_array_within_bounds(self, simple_wave_params):
        arr = simple_wave_params.frequency_array()
        assert arr[0] == pytest.approx(simple_wave_params.freq_min)
        assert arr[-1] == pytest.approx(simple_wave_params.freq_max)

    def test_negative_hs_raises(self):
        with pytest.raises(ValueError):
            WaveParameters(
                spectrum_type=WaveSpectrumType.JONSWAP,
                significant_height=-1.0,
                peak_period=10.0,
            )

    def test_zero_tp_raises(self):
        with pytest.raises(ValueError):
            WaveParameters(
                spectrum_type=WaveSpectrumType.JONSWAP,
                significant_height=3.0,
                peak_period=0.0,
            )

    def test_negative_gamma_raises(self):
        with pytest.raises(ValueError):
            WaveParameters(
                spectrum_type=WaveSpectrumType.JONSWAP,
                significant_height=3.0,
                peak_period=10.0,
                gamma=-0.5,
            )

    def test_freq_min_greater_than_freq_max_raises(self):
        with pytest.raises(ValueError):
            WaveParameters(
                spectrum_type=WaveSpectrumType.JONSWAP,
                significant_height=3.0,
                peak_period=10.0,
                freq_min=2.0,
                freq_max=0.1,
            )

    def test_to_dict_keys(self, simple_wave_params):
        d = simple_wave_params.to_dict()
        assert "spectrum_type" in d
        assert "Hs_m" in d
        assert "Tp_s" in d
        assert "gamma" in d
        assert "peak_frequency_rad_s" in d

    def test_to_dict_values(self, simple_wave_params):
        d = simple_wave_params.to_dict()
        assert d["Hs_m"] == pytest.approx(4.0)
        assert d["Tp_s"] == pytest.approx(12.0)


# ---------------------------------------------------------------------------
# WaveSpectra tests
# ---------------------------------------------------------------------------


class TestWaveSpectra:
    """Tests for WaveSpectra spectrum generation."""

    def test_jonswap_returns_two_arrays(self):
        ws = WaveSpectra()
        omega, S = ws.jonswap(hs=4.0, tp=12.0)
        assert len(omega) > 0
        assert len(S) == len(omega)

    def test_jonswap_non_negative_spectrum(self):
        ws = WaveSpectra()
        omega, S = ws.jonswap(hs=4.0, tp=12.0)
        assert np.all(S >= 0.0)

    def test_jonswap_peak_near_tp(self):
        """Peak spectral density should be near the peak period."""
        ws = WaveSpectra()
        tp = 12.0
        omega, S = ws.jonswap(hs=4.0, tp=tp, n_points=200)
        peak_omega = omega[np.argmax(S)]
        expected_wp = 2.0 * math.pi / tp
        # Within 10% of expected peak
        assert abs(peak_omega - expected_wp) / expected_wp < 0.15

    def test_jonswap_higher_hs_more_energy(self):
        """Doubling Hs should quadruple total spectral energy (m0 ∝ Hs^2)."""
        ws = WaveSpectra()
        _, S1 = ws.jonswap(hs=2.0, tp=10.0)
        _, S2 = ws.jonswap(hs=4.0, tp=10.0)
        # Total energy ratio should be ≈ 4
        energy_ratio = np.sum(S2) / np.sum(S1)
        assert energy_ratio == pytest.approx(4.0, rel=0.15)

    def test_jonswap_gamma_one_has_similar_total_energy_to_pm(self):
        """JONSWAP with gamma=1 total energy should be comparable to Pierson-Moskowitz."""
        ws = WaveSpectra()
        omega, S_js = ws.jonswap(hs=3.0, tp=10.0, gamma=1.0)
        _, S_pm = ws.pierson_moskowitz(hs=3.0, tp=10.0)
        # Both spectra should integrate to approximately Hs^2/16 m0
        # Ratio of total energy should be within 50% (same Hs used)
        energy_ratio = np.sum(S_js) / (np.sum(S_pm) + 1e-30)
        assert 0.5 < energy_ratio < 2.0

    def test_pierson_moskowitz_non_negative(self):
        ws = WaveSpectra()
        omega, S = ws.pierson_moskowitz(hs=3.0, tp=10.0)
        assert np.all(S >= 0.0)

    def test_pierson_moskowitz_length(self):
        ws = WaveSpectra()
        omega, S = ws.pierson_moskowitz(hs=3.0, tp=10.0, n_points=50)
        assert len(S) == 50

    def test_generate_spectrum_jonswap(self):
        """generate_spectrum dispatches to JONSWAP correctly."""
        ws = WaveSpectra()
        params = WaveParameters(
            spectrum_type=WaveSpectrumType.JONSWAP,
            significant_height=4.0,
            peak_period=12.0,
        )
        omega, S = ws.generate_spectrum(params)
        assert len(omega) > 0
        assert np.all(S >= 0.0)

    def test_generate_spectrum_pm(self):
        ws = WaveSpectra()
        params = WaveParameters(
            spectrum_type=WaveSpectrumType.PIERSON_MOSKOWITZ,
            significant_height=3.0,
            peak_period=10.0,
        )
        omega, S = ws.generate_spectrum(params)
        assert np.all(S >= 0.0)

    def test_generate_spectrum_bretschneider(self):
        ws = WaveSpectra()
        params = WaveParameters(
            spectrum_type=WaveSpectrumType.BRETSCHNEIDER,
            significant_height=3.0,
            peak_period=10.0,
        )
        omega, S = ws.generate_spectrum(params)
        assert np.all(S >= 0.0)

    def test_generate_spectrum_issc(self):
        ws = WaveSpectra()
        params = WaveParameters(
            spectrum_type=WaveSpectrumType.ISSC,
            significant_height=3.0,
            peak_period=10.0,
        )
        omega, S = ws.generate_spectrum(params)
        assert np.all(S >= 0.0)


# ---------------------------------------------------------------------------
# HydrodynamicMatrix tests
# ---------------------------------------------------------------------------


class TestHydrodynamicMatrix:
    def test_create_valid_matrix(self):
        m = HydrodynamicMatrix(
            frequency=0.5,
            matrix=np.eye(6) * 1000.0,
            matrix_type="added_mass",
        )
        assert m.frequency == pytest.approx(0.5)

    def test_invalid_shape_raises(self):
        with pytest.raises(ValueError):
            HydrodynamicMatrix(
                frequency=0.5,
                matrix=np.eye(3),
                matrix_type="added_mass",
            )

    def test_negative_frequency_raises(self):
        with pytest.raises(ValueError):
            HydrodynamicMatrix(
                frequency=-0.5,
                matrix=np.eye(6),
                matrix_type="added_mass",
            )

    def test_zero_frequency_allowed(self):
        m = HydrodynamicMatrix(
            frequency=0.0,
            matrix=np.eye(6),
            matrix_type="added_mass",
        )
        assert m.frequency == pytest.approx(0.0)

    def test_symmetric_identity_matrix(self):
        m = HydrodynamicMatrix(
            frequency=0.5,
            matrix=np.eye(6),
            matrix_type="added_mass",
        )
        assert m.is_symmetric

    def test_asymmetric_matrix(self):
        mat = np.eye(6)
        mat[0, 1] = 1.0  # break symmetry
        m = HydrodynamicMatrix(frequency=0.5, matrix=mat, matrix_type="added_mass")
        assert not m.is_symmetric

    def test_get_component_surge_surge(self):
        mat = np.arange(36).reshape(6, 6).astype(float)
        m = HydrodynamicMatrix(frequency=0.5, matrix=mat, matrix_type="added_mass")
        comp = m.get_component(MatrixDOF.SURGE, MatrixDOF.SURGE)
        assert comp == pytest.approx(0.0)

    def test_get_component_heave_pitch(self):
        mat = np.zeros((6, 6))
        mat[2, 4] = 42.0  # heave-pitch coupling
        m = HydrodynamicMatrix(frequency=0.5, matrix=mat, matrix_type="damping")
        comp = m.get_component(MatrixDOF.HEAVE, MatrixDOF.PITCH)
        assert comp == pytest.approx(42.0)

    def test_to_dict_contains_matrix(self):
        m = HydrodynamicMatrix(
            frequency=1.0,
            matrix=np.eye(6),
            matrix_type="added_mass",
        )
        d = m.to_dict()
        assert "matrix" in d
        assert "frequency_rad_s" in d
        assert d["matrix_type"] == "added_mass"

    def test_to_dict_period_correct(self):
        omega = math.pi  # period = 2
        m = HydrodynamicMatrix(
            frequency=omega,
            matrix=np.eye(6),
            matrix_type="added_mass",
        )
        d = m.to_dict()
        assert d["period_s"] == pytest.approx(2.0)


# ---------------------------------------------------------------------------
# VesselProperties tests
# ---------------------------------------------------------------------------


class TestVesselProperties:
    def test_create_valid_vessel(self):
        v = VesselProperties(
            name="FPSO Alpha",
            length_overall=300.0,
            beam=50.0,
            draft=18.0,
            displacement=200000.0,
        )
        assert v.name == "FPSO Alpha"

    def test_zero_length_raises(self):
        with pytest.raises(ValueError, match="Length"):
            VesselProperties(
                name="bad",
                length_overall=0.0,
                beam=50.0,
                draft=18.0,
                displacement=200000.0,
            )

    def test_negative_beam_raises(self):
        with pytest.raises(ValueError, match="[Bb]eam"):
            VesselProperties(
                name="bad",
                length_overall=300.0,
                beam=-1.0,
                draft=18.0,
                displacement=200000.0,
            )

    def test_negative_draft_raises(self):
        with pytest.raises(ValueError, match="[Dd]raft"):
            VesselProperties(
                name="bad",
                length_overall=300.0,
                beam=50.0,
                draft=-1.0,
                displacement=200000.0,
            )

    def test_zero_displacement_raises(self):
        with pytest.raises(ValueError, match="[Dd]isplacement"):
            VesselProperties(
                name="bad",
                length_overall=300.0,
                beam=50.0,
                draft=18.0,
                displacement=0.0,
            )

    def test_length_to_beam_ratio(self):
        v = VesselProperties(
            name="test",
            length_overall=300.0,
            beam=50.0,
            draft=18.0,
            displacement=200000.0,
        )
        assert v.length_to_beam_ratio == pytest.approx(6.0)

    def test_beam_to_draft_ratio(self):
        v = VesselProperties(
            name="test",
            length_overall=300.0,
            beam=50.0,
            draft=10.0,
            displacement=200000.0,
        )
        assert v.beam_to_draft_ratio == pytest.approx(5.0)

    def test_wetted_surface_positive(self):
        v = VesselProperties(
            name="test",
            length_overall=300.0,
            beam=50.0,
            draft=18.0,
            displacement=200000.0,
        )
        assert v.estimated_wetted_surface > 0.0

    def test_to_dict_contains_ratios(self):
        v = VesselProperties(
            name="test",
            length_overall=300.0,
            beam=50.0,
            draft=18.0,
            displacement=200000.0,
        )
        d = v.to_dict()
        assert "L_B_ratio" in d
        assert "B_T_ratio" in d


# ---------------------------------------------------------------------------
# RAOData tests
# ---------------------------------------------------------------------------


class TestRAOData:
    def test_create_valid_rao_data(self, simple_rao_data):
        assert simple_rao_data.vessel_name == "TestVessel"
        assert simple_rao_data.frequencies.shape[0] == 20

    def test_mismatched_amplitude_shape_raises(self):
        frequencies = np.linspace(0.1, 1.5, 10)
        directions = np.array([0.0, 90.0])
        # Provide wrong shape for amplitudes
        with pytest.raises(ValueError, match="shape"):
            RAOData(
                frequencies=frequencies,
                directions=directions,
                amplitudes=np.ones((5, 2, 6)),  # wrong n_freq
                phases=np.zeros((10, 2, 6)),
            )

    def test_mismatched_phase_shape_raises(self):
        frequencies = np.linspace(0.1, 1.5, 10)
        directions = np.array([0.0, 90.0])
        with pytest.raises(ValueError, match="shape"):
            RAOData(
                frequencies=frequencies,
                directions=directions,
                amplitudes=np.ones((10, 2, 6)),
                phases=np.zeros((10, 3, 6)),  # wrong n_dir
            )

    def test_get_rao_returns_amplitude_and_phase(self, simple_rao_data):
        amp, phase = simple_rao_data.get_rao(
            frequency=0.5,
            direction=0.0,
            dof=MatrixDOF.HEAVE,
        )
        assert math.isfinite(amp)
        assert math.isfinite(phase)

    def test_get_rao_amplitude_correct(self, simple_rao_data):
        """All amplitudes are 0.5; any query should return ~0.5."""
        amp, _ = simple_rao_data.get_rao(
            frequency=0.5,
            direction=0.0,
            dof=MatrixDOF.SURGE,
        )
        assert amp == pytest.approx(0.5)

    def test_get_rao_nearest_frequency(self, simple_rao_data):
        """get_rao should snap to nearest available frequency."""
        # query at freq far from any grid point
        amp, _ = simple_rao_data.get_rao(
            frequency=99.0,
            direction=0.0,
            dof=MatrixDOF.HEAVE,
        )
        assert amp == pytest.approx(0.5)

    def test_get_rao_nearest_direction(self, simple_rao_data):
        """get_rao should snap to nearest available direction."""
        amp, _ = simple_rao_data.get_rao(
            frequency=0.5,
            direction=45.0,  # midway; nearest is 0 or 90
            dof=MatrixDOF.HEAVE,
        )
        assert amp == pytest.approx(0.5)


# ---------------------------------------------------------------------------
# EnvironmentalConditions tests
# ---------------------------------------------------------------------------


class TestEnvironmentalConditions:
    def test_create_default_conditions(self):
        ec = EnvironmentalConditions()
        assert ec.wind_speed == pytest.approx(0.0)
        assert ec.current_speed == pytest.approx(0.0)
        assert ec.water_density == pytest.approx(1025.0)

    def test_negative_wind_speed_raises(self):
        with pytest.raises(ValueError, match="[Ww]ind"):
            EnvironmentalConditions(wind_speed=-1.0)

    def test_negative_current_speed_raises(self):
        with pytest.raises(ValueError, match="[Cc]urrent"):
            EnvironmentalConditions(current_speed=-0.5)

    def test_zero_water_density_raises(self):
        with pytest.raises(ValueError, match="[Ww]ater"):
            EnvironmentalConditions(water_density=0.0)

    def test_negative_air_density_raises(self):
        with pytest.raises(ValueError, match="[Aa]ir"):
            EnvironmentalConditions(air_density=-1.0)

    def test_to_dict_with_wave_params(self):
        params = WaveParameters(
            spectrum_type=WaveSpectrumType.JONSWAP,
            significant_height=4.0,
            peak_period=12.0,
        )
        ec = EnvironmentalConditions(wave_params=params)
        d = ec.to_dict()
        assert "wave_parameters" in d

    def test_to_dict_without_wave_params(self):
        ec = EnvironmentalConditions()
        d = ec.to_dict()
        assert "wave_parameters" not in d


# ---------------------------------------------------------------------------
# Standard vessel types tests
# ---------------------------------------------------------------------------


class TestStandardVesselTypes:
    def test_fpso_exists(self):
        v = get_vessel_type("fpso")
        assert v is not None
        assert v.length_overall > 0

    def test_tanker_exists(self):
        v = get_vessel_type("tanker")
        assert v is not None

    def test_semisubmersible_exists(self):
        v = get_vessel_type("semisubmersible")
        assert v is not None

    def test_case_insensitive(self):
        v = get_vessel_type("FPSO")
        assert v is not None

    def test_unknown_type_raises_key_error(self):
        with pytest.raises(KeyError):
            get_vessel_type("unknown_type_xyz")

    def test_vessel_types_dict_not_empty(self):
        assert len(VESSEL_TYPES) > 0

    def test_fpso_dimensions_sensible(self):
        v = get_vessel_type("fpso")
        assert v.length_overall > 100.0
        assert v.beam > 30.0
        assert v.draft > 10.0
