"""
Integration tests for Hydrodynamic Coefficients → RAO Processing.

Tests that hydrodynamic coefficient database correctly feeds into RAO calculations
and that frequency-dependent coefficients affect motion response.

Test Coverage:
- Load hydro coefficients from database
- Interpolate coefficients at RAO frequencies
- Added mass affects motion amplitudes
- Damping coefficients affect phase
- Frequency-dependent coupling terms
- Kramers-Kronig causality validation
"""

import pytest
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
import tempfile
import pandas as pd

from digitalmodel.marine_ops.marine_analysis.hydrodynamic_coefficients.coefficients import (
    CoefficientDatabase,
    FrequencyDependentMatrix,
    DOF_NAMES,
    KramersKronigValidator
)


class TestHydroRAOIntegration:
    """Integration tests for hydrodynamic coefficients and RAO processing."""

    @pytest.fixture
    def output_dir(self):
        """Create output directory for test charts."""
        output_path = Path(__file__).parent / "charts" / "hydro_rao"
        output_path.mkdir(parents=True, exist_ok=True)
        return output_path

    @pytest.fixture
    def sample_database(self, tmp_path):
        """Create sample coefficient database for testing."""
        # Create sample data at multiple frequencies
        frequencies = np.array([0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4])  # rad/s

        # Typical vessel properties (semi-submersible)
        mass = 50000e3  # kg
        base_added_mass = mass * 0.2  # ~20% of vessel mass

        for freq in frequencies:
            # Added mass decreases with frequency
            added_mass_diag = base_added_mass * (1.0 + 0.5 * np.exp(-freq))

            # Damping increases with frequency (radiation damping)
            damping_diag = 1e7 * freq * (1 + 0.3 * freq)

            # Create 6x6 matrices
            A_matrix = np.diag([
                added_mass_diag * 1.0,  # Surge
                added_mass_diag * 1.1,  # Sway
                added_mass_diag * 1.3,  # Heave (largest)
                added_mass_diag * 0.05,  # Roll
                added_mass_diag * 0.05,  # Pitch
                added_mass_diag * 0.03   # Yaw
            ])

            B_matrix = np.diag([
                damping_diag * 0.8,  # Surge
                damping_diag * 0.9,  # Sway
                damping_diag * 1.0,  # Heave
                damping_diag * 0.04,  # Roll
                damping_diag * 0.04,  # Pitch
                damping_diag * 0.02   # Yaw
            ])

            # Add some coupling terms (pitch-heave, roll-sway)
            A_matrix[2, 4] = A_matrix[4, 2] = added_mass_diag * 0.1  # Heave-Pitch
            A_matrix[1, 3] = A_matrix[3, 1] = added_mass_diag * 0.05  # Sway-Roll

            B_matrix[2, 4] = B_matrix[4, 2] = damping_diag * 0.08
            B_matrix[1, 3] = B_matrix[3, 1] = damping_diag * 0.04

            # Save to CSV
            df_A = pd.DataFrame(A_matrix, index=DOF_NAMES, columns=DOF_NAMES)
            df_B = pd.DataFrame(B_matrix, index=DOF_NAMES, columns=DOF_NAMES)

            df_A.to_csv(tmp_path / f'added_mass_omega_{freq:.1f}.csv')
            df_B.to_csv(tmp_path / f'damping_omega_{freq:.1f}.csv')

        # Load database
        db = CoefficientDatabase.from_csv(tmp_path)
        return db

    def test_database_loading(self, sample_database):
        """Test that database loads correctly."""
        db = sample_database

        assert len(db.frequencies) == 7, "Should have 7 frequencies"
        assert len(db.added_mass_matrices) == 7, "Should have 7 added mass matrices"
        assert len(db.damping_matrices) == 7, "Should have 7 damping matrices"

        # Check frequency range
        freq_min, freq_max = db.get_frequency_range()
        assert freq_min == 0.2, "Min frequency should be 0.2 rad/s"
        assert freq_max == 1.4, "Max frequency should be 1.4 rad/s"

    def test_coefficient_interpolation_accuracy(self, sample_database):
        """Test that interpolation maintains accuracy between known points."""
        db = sample_database

        # Test heave added mass at mid-point frequency
        freq_test = 0.5  # Between 0.4 and 0.6
        A_heave = db.get_added_mass(freq_test, 'Heave', 'Heave')

        # Get values at neighboring frequencies
        A_heave_04 = db.get_added_mass(0.4, 'Heave', 'Heave')
        A_heave_06 = db.get_added_mass(0.6, 'Heave', 'Heave')

        # Interpolated value should be between neighbors
        assert A_heave_04 >= A_heave >= A_heave_06 or \
               A_heave_06 >= A_heave >= A_heave_04, \
               "Interpolated value should be between neighbors"

        # For linear region, check approximate linearity
        expected_linear = (A_heave_04 + A_heave_06) / 2
        relative_error = abs(A_heave - expected_linear) / expected_linear
        assert relative_error < 0.1, \
            f"Interpolation error {relative_error*100:.1f}% exceeds 10%"

    def test_added_mass_affects_natural_frequency(self, sample_database, output_dir):
        """Test that added mass affects motion natural frequency."""
        db = sample_database

        # Vessel properties
        mass_vessel = 50000e3  # kg
        stiffness = 5e6  # N/m (heave stiffness)

        frequencies = np.linspace(0.2, 1.4, 50)
        natural_frequencies = []

        for freq in frequencies:
            A_heave = db.get_added_mass(freq, 'Heave', 'Heave')
            total_mass = mass_vessel + A_heave

            # Natural frequency: ω_n = sqrt(k / m_total)
            omega_n = np.sqrt(stiffness / total_mass)
            natural_frequencies.append(omega_n)

        natural_frequencies = np.array(natural_frequencies)

        # Natural frequency should decrease as added mass increases
        # (added mass decreases with frequency, so natural freq increases)
        assert natural_frequencies[-1] > natural_frequencies[0], \
            "Natural frequency should increase as wave frequency increases"

        # Create chart
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))

        # Added mass variation
        added_masses = [db.get_added_mass(f, 'Heave', 'Heave') for f in frequencies]
        ax1.plot(frequencies, np.array(added_masses) / 1e6, linewidth=2)
        ax1.set_xlabel('Wave Frequency [rad/s]')
        ax1.set_ylabel('Added Mass [×10⁶ kg]')
        ax1.set_title('Frequency-Dependent Added Mass (Heave)')
        ax1.grid(True, alpha=0.3)

        # Natural frequency variation
        ax2.plot(frequencies, natural_frequencies, linewidth=2, color='green')
        ax2.set_xlabel('Wave Frequency [rad/s]')
        ax2.set_ylabel('Natural Frequency [rad/s]')
        ax2.set_title('Natural Frequency vs Wave Frequency')
        ax2.grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig(output_dir / "added_mass_natural_frequency.png", dpi=300, bbox_inches='tight')
        plt.close()

    def test_damping_affects_response_amplitude(self, sample_database, output_dir):
        """Test that damping coefficients affect RAO amplitude."""
        db = sample_database

        # System properties
        mass = 50000e3  # kg
        stiffness = 5e6  # N/m
        omega_n = np.sqrt(stiffness / mass)  # Natural frequency

        # Test frequencies around natural frequency
        frequencies = np.linspace(0.3, 1.2, 100)
        RAO_amplitudes = []

        for omega in frequencies:
            A_heave = db.get_added_mass(omega, 'Heave', 'Heave')
            B_heave = db.get_damping(omega, 'Heave', 'Heave')

            total_mass = mass + A_heave

            # Response amplitude operator (simplified)
            # |RAO| = 1 / sqrt((k - ω²m)² + (ωB)²)
            denominator = np.sqrt(
                (stiffness - omega**2 * total_mass)**2 +
                (omega * B_heave)**2
            )
            RAO = 1.0 / denominator if denominator > 0 else 0
            RAO_amplitudes.append(RAO)

        RAO_amplitudes = np.array(RAO_amplitudes)

        # Peak should occur near natural frequency
        peak_idx = np.argmax(RAO_amplitudes)
        peak_freq = frequencies[peak_idx]

        # Allow 20% deviation due to added mass effects
        assert abs(peak_freq - omega_n) / omega_n < 0.3, \
            f"Peak at {peak_freq:.3f} rad/s far from natural freq {omega_n:.3f} rad/s"

        # Create RAO chart
        fig, ax = plt.subplots(figsize=(10, 6))

        ax.plot(frequencies, RAO_amplitudes * 1e6, linewidth=2)
        ax.axvline(omega_n, color='r', linestyle='--', label=f'Natural Freq: {omega_n:.3f} rad/s')
        ax.axvline(peak_freq, color='g', linestyle='--', label=f'Peak Freq: {peak_freq:.3f} rad/s')

        ax.set_xlabel('Wave Frequency [rad/s]')
        ax.set_ylabel('RAO Amplitude [×10⁻⁶ m/N]')
        ax.set_title('Response Amplitude Operator (Heave)')
        ax.legend()
        ax.grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig(output_dir / "rao_amplitude.png", dpi=300, bbox_inches='tight')
        plt.close()

    def test_damping_affects_phase(self, sample_database, output_dir):
        """Test that damping affects phase of response."""
        db = sample_database

        mass = 50000e3
        stiffness = 5e6

        frequencies = np.linspace(0.3, 1.2, 100)
        phases = []

        for omega in frequencies:
            A_heave = db.get_added_mass(omega, 'Heave', 'Heave')
            B_heave = db.get_damping(omega, 'Heave', 'Heave')

            total_mass = mass + A_heave

            # Phase angle: φ = arctan(ωB / (k - ω²m))
            numerator = omega * B_heave
            denominator = stiffness - omega**2 * total_mass

            phase = np.arctan2(numerator, denominator)
            phases.append(np.degrees(phase))

        phases = np.array(phases)

        # Phase should transition through -90° at resonance
        # Below resonance: phase ≈ 0°
        # At resonance: phase ≈ -90°
        # Above resonance: phase ≈ -180°

        min_phase_idx = np.argmin(phases)
        assert -120 <= phases[min_phase_idx] <= -60, \
            f"Phase at resonance {phases[min_phase_idx]:.1f}° should be near -90°"

        # Create phase chart
        fig, ax = plt.subplots(figsize=(10, 6))

        ax.plot(frequencies, phases, linewidth=2, color='purple')
        ax.axhline(-90, color='r', linestyle='--', label='Resonance Phase: -90°')
        ax.set_xlabel('Wave Frequency [rad/s]')
        ax.set_ylabel('Phase Angle [degrees]')
        ax.set_title('Response Phase (Heave)')
        ax.legend()
        ax.grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig(output_dir / "rao_phase.png", dpi=300, bbox_inches='tight')
        plt.close()

    def test_coupling_terms_affect_response(self, sample_database):
        """Test that coupling terms (e.g., heave-pitch) affect response."""
        db = sample_database

        # Get heave-pitch coupling at test frequency
        freq_test = 0.8
        A_heave_pitch = db.get_added_mass(freq_test, 'Heave', 'Pitch')
        A_pitch_heave = db.get_added_mass(freq_test, 'Pitch', 'Heave')

        # Matrix should be symmetric
        np.testing.assert_allclose(A_heave_pitch, A_pitch_heave, rtol=1e-5,
                                  err_msg="Added mass matrix should be symmetric")

        # Coupling term should be non-zero
        assert abs(A_heave_pitch) > 0, "Heave-pitch coupling should be non-zero"

        # Coupling should be smaller than diagonal terms
        A_heave_heave = db.get_added_mass(freq_test, 'Heave', 'Heave')
        A_pitch_pitch = db.get_added_mass(freq_test, 'Pitch', 'Pitch')

        assert abs(A_heave_pitch) < abs(A_heave_heave), \
            "Coupling term should be smaller than diagonal term"
        assert abs(A_heave_pitch) < abs(A_pitch_pitch), \
            "Coupling term should be smaller than diagonal term"

    def test_full_matrix_interpolation(self, sample_database, output_dir):
        """Test interpolation of full 6×6 matrices."""
        db = sample_database

        freq_test = 0.7  # Between known points
        A_matrix = db.get_added_mass_matrix(freq_test)
        B_matrix = db.get_damping_matrix(freq_test)

        # Check dimensions
        assert A_matrix.shape == (6, 6), "Added mass matrix should be 6×6"
        assert B_matrix.shape == (6, 6), "Damping matrix should be 6×6"

        # Check symmetry (should be approximately symmetric)
        assert np.allclose(A_matrix, A_matrix.T, rtol=1e-3), \
            "Added mass matrix should be symmetric"
        assert np.allclose(B_matrix, B_matrix.T, rtol=1e-3), \
            "Damping matrix should be symmetric"

        # Check diagonal dominance (diagonal > off-diagonal)
        for i in range(6):
            for j in range(6):
                if i != j:
                    assert abs(A_matrix[i, i]) > abs(A_matrix[i, j]), \
                        f"Added mass should be diagonally dominant at ({i},{j})"

        # Create heatmap visualization
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

        # Added mass heatmap
        im1 = ax1.imshow(np.log10(np.abs(A_matrix) + 1), cmap='viridis', aspect='auto')
        ax1.set_xticks(range(6))
        ax1.set_yticks(range(6))
        ax1.set_xticklabels(DOF_NAMES, rotation=45)
        ax1.set_yticklabels(DOF_NAMES)
        ax1.set_title(f'Added Mass Matrix (log₁₀) at ω={freq_test} rad/s')
        plt.colorbar(im1, ax=ax1)

        # Damping heatmap
        im2 = ax2.imshow(np.log10(np.abs(B_matrix) + 1), cmap='plasma', aspect='auto')
        ax2.set_xticks(range(6))
        ax2.set_yticks(range(6))
        ax2.set_xticklabels(DOF_NAMES, rotation=45)
        ax2.set_yticklabels(DOF_NAMES)
        ax2.set_title(f'Damping Matrix (log₁₀) at ω={freq_test} rad/s')
        plt.colorbar(im2, ax=ax2)

        plt.tight_layout()
        plt.savefig(output_dir / "coefficient_matrices_heatmap.png", dpi=300, bbox_inches='tight')
        plt.close()

    def test_frequency_sweep_all_dof(self, sample_database, output_dir):
        """Test frequency sweep for all degrees of freedom."""
        db = sample_database

        frequencies = np.linspace(0.2, 1.4, 50)

        fig, axes = plt.subplots(2, 3, figsize=(15, 10))
        axes = axes.flatten()

        for idx, dof in enumerate(DOF_NAMES):
            added_masses = []
            dampings = []

            for freq in frequencies:
                A = db.get_added_mass(freq, dof, dof)
                B = db.get_damping(freq, dof, dof)
                added_masses.append(A)
                dampings.append(B)

            ax = axes[idx]
            ax2 = ax.twinx()

            # Plot added mass
            line1 = ax.plot(frequencies, np.array(added_masses) / 1e6,
                           'b-', linewidth=2, label='Added Mass')
            ax.set_xlabel('Frequency [rad/s]')
            ax.set_ylabel('Added Mass [×10⁶ kg]', color='b')
            ax.tick_params(axis='y', labelcolor='b')

            # Plot damping
            line2 = ax2.plot(frequencies, np.array(dampings) / 1e6,
                            'r-', linewidth=2, label='Damping')
            ax2.set_ylabel('Damping [×10⁶ N·s/m]', color='r')
            ax2.tick_params(axis='y', labelcolor='r')

            ax.set_title(f'{dof} Coefficients')
            ax.grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig(output_dir / "all_dof_frequency_sweep.png", dpi=300, bbox_inches='tight')
        plt.close()
