#!/usr/bin/env python3
"""
ABOUTME: Coefficient interpolation utilities for RAOs and hydrodynamic coefficients
with 2D interpolation across frequency and direction.
"""

import numpy as np
from typing import Dict, List, Tuple, Optional
from scipy.interpolate import interp1d, interp2d, RegularGridInterpolator
from .models import RAOData, MatrixDOF


class CoefficientsInterpolator:
    """
    Interpolate RAOs and hydrodynamic coefficients

    Provides 1D (frequency) and 2D (frequency × direction) interpolation
    for Response Amplitude Operators and other frequency/direction-dependent
    coefficients.
    """

    def __init__(self):
        """Initialize interpolator"""
        self.rao_data: Optional[RAOData] = None

    def load_raos(self, rao_data: RAOData) -> None:
        """
        Load RAO data for interpolation

        Args:
            rao_data: RAOData object with frequencies, directions, amplitudes, phases
        """
        self.rao_data = rao_data

    def interpolate_rao_1d(
        self,
        target_frequencies: np.ndarray,
        direction: float,
        dof: MatrixDOF,
        method: str = 'cubic'
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Interpolate RAO at fixed direction across frequencies

        Args:
            target_frequencies: Target frequencies (rad/s)
            direction: Fixed direction (degrees)
            dof: Degree of freedom
            method: 'linear', 'cubic', or 'spline'

        Returns:
            (amplitudes, phases) at target frequencies
        """
        if self.rao_data is None:
            raise ValueError("No RAO data loaded. Call load_raos() first.")

        # Find nearest direction
        dir_idx = np.argmin(np.abs(self.rao_data.directions - direction))

        # Extract RAO at this direction
        rao_amp = self.rao_data.amplitudes[:, dir_idx, dof.value]
        rao_phase = self.rao_data.phases[:, dir_idx, dof.value]

        # Interpolate amplitudes
        if method in ['linear', 'cubic']:
            f_amp = interp1d(
                self.rao_data.frequencies,
                rao_amp,
                kind=method,
                bounds_error=False,
                fill_value='extrapolate'
            )
            amp_interp = f_amp(target_frequencies)

            # Interpolate phases
            f_phase = interp1d(
                self.rao_data.frequencies,
                rao_phase,
                kind=method,
                bounds_error=False,
                fill_value='extrapolate'
            )
            phase_interp = f_phase(target_frequencies)

        else:
            raise ValueError(f"Method '{method}' not supported. Use 'linear' or 'cubic'.")

        return amp_interp, phase_interp

    def interpolate_rao_2d(
        self,
        target_frequencies: np.ndarray,
        target_directions: np.ndarray,
        dof: MatrixDOF,
        method: str = 'linear'
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Interpolate RAO across frequencies and directions

        Args:
            target_frequencies: Target frequencies (rad/s)
            target_directions: Target directions (degrees)
            dof: Degree of freedom
            method: 'linear' or 'nearest'

        Returns:
            (amplitudes, phases) on grid [n_freq × n_dir]
        """
        if self.rao_data is None:
            raise ValueError("No RAO data loaded. Call load_raos() first.")

        # Extract RAO data for this DOF
        rao_amp = self.rao_data.amplitudes[:, :, dof.value]
        rao_phase = self.rao_data.phases[:, :, dof.value]

        # Create 2D interpolators
        interp_amp = RegularGridInterpolator(
            (self.rao_data.frequencies, self.rao_data.directions),
            rao_amp,
            method=method,
            bounds_error=False,
            fill_value=None
        )

        interp_phase = RegularGridInterpolator(
            (self.rao_data.frequencies, self.rao_data.directions),
            rao_phase,
            method=method,
            bounds_error=False,
            fill_value=None
        )

        # Create target grid
        freq_grid, dir_grid = np.meshgrid(target_frequencies, target_directions, indexing='ij')
        points = np.column_stack([freq_grid.ravel(), dir_grid.ravel()])

        # Interpolate
        amp_interp = interp_amp(points).reshape(freq_grid.shape)
        phase_interp = interp_phase(points).reshape(freq_grid.shape)

        return amp_interp, phase_interp

    def interpolate_all_dofs(
        self,
        target_frequencies: np.ndarray,
        target_directions: np.ndarray,
        method: str = 'linear'
    ) -> RAOData:
        """
        Interpolate RAOs for all 6 DOFs

        Args:
            target_frequencies: Target frequencies (rad/s)
            target_directions: Target directions (degrees)
            method: 'linear' or 'nearest'

        Returns:
            New RAOData with interpolated values
        """
        if self.rao_data is None:
            raise ValueError("No RAO data loaded. Call load_raos() first.")

        n_freq = len(target_frequencies)
        n_dir = len(target_directions)

        amplitudes_interp = np.zeros((n_freq, n_dir, 6))
        phases_interp = np.zeros((n_freq, n_dir, 6))

        for dof in MatrixDOF:
            amp, phase = self.interpolate_rao_2d(
                target_frequencies,
                target_directions,
                dof,
                method=method
            )
            amplitudes_interp[:, :, dof.value] = amp
            phases_interp[:, :, dof.value] = phase

        return RAOData(
            frequencies=target_frequencies,
            directions=target_directions,
            amplitudes=amplitudes_interp,
            phases=phases_interp,
            vessel_name=self.rao_data.vessel_name
        )

    def frequency_interpolation(
        self,
        source_freqs: np.ndarray,
        source_values: np.ndarray,
        target_freqs: np.ndarray,
        method: str = 'cubic'
    ) -> np.ndarray:
        """
        Generic 1D frequency interpolation

        Args:
            source_freqs: Source frequencies (rad/s)
            source_values: Values at source frequencies
            target_freqs: Target frequencies (rad/s)
            method: 'linear', 'cubic', or 'spline'

        Returns:
            Interpolated values at target frequencies
        """
        if method in ['linear', 'cubic']:
            f_interp = interp1d(
                source_freqs,
                source_values,
                kind=method,
                bounds_error=False,
                fill_value='extrapolate'
            )
            return f_interp(target_freqs)
        else:
            raise ValueError(f"Method '{method}' not supported")

    def extract_rao_at_frequency(
        self,
        frequency: float,
        tolerance: float = 0.01
    ) -> Dict[str, np.ndarray]:
        """
        Extract RAO data at a specific frequency

        Args:
            frequency: Target frequency (rad/s)
            tolerance: Frequency matching tolerance (rad/s)

        Returns:
            Dictionary with RAO amplitudes and phases vs direction for all DOFs
        """
        if self.rao_data is None:
            raise ValueError("No RAO data loaded")

        # Find closest frequency
        freq_idx = np.argmin(np.abs(self.rao_data.frequencies - frequency))
        actual_freq = self.rao_data.frequencies[freq_idx]

        if abs(actual_freq - frequency) > tolerance:
            raise ValueError(
                f"No frequency within tolerance {tolerance} rad/s. "
                f"Closest is {actual_freq:.4f} rad/s"
            )

        # Extract RAO at this frequency
        amplitudes = self.rao_data.amplitudes[freq_idx, :, :]  # [n_dir × 6]
        phases = self.rao_data.phases[freq_idx, :, :]  # [n_dir × 6]

        return {
            'frequency_rad_s': actual_freq,
            'directions_deg': self.rao_data.directions,
            'amplitudes': amplitudes,
            'phases': phases,
        }

    def extract_rao_at_direction(
        self,
        direction: float,
        tolerance: float = 1.0
    ) -> Dict[str, np.ndarray]:
        """
        Extract RAO data at a specific direction

        Args:
            direction: Target direction (degrees)
            tolerance: Direction matching tolerance (degrees)

        Returns:
            Dictionary with RAO amplitudes and phases vs frequency for all DOFs
        """
        if self.rao_data is None:
            raise ValueError("No RAO data loaded")

        # Find closest direction
        dir_idx = np.argmin(np.abs(self.rao_data.directions - direction))
        actual_dir = self.rao_data.directions[dir_idx]

        if abs(actual_dir - direction) > tolerance:
            raise ValueError(
                f"No direction within tolerance {tolerance}°. "
                f"Closest is {actual_dir:.1f}°"
            )

        # Extract RAO at this direction
        amplitudes = self.rao_data.amplitudes[:, dir_idx, :]  # [n_freq × 6]
        phases = self.rao_data.phases[:, dir_idx, :]  # [n_freq × 6]

        return {
            'direction_deg': actual_dir,
            'frequencies_rad_s': self.rao_data.frequencies,
            'amplitudes': amplitudes,
            'phases': phases,
        }
