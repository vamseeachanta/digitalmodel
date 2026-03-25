#!/usr/bin/env python3
"""
ABOUTME: Hydrodynamic coefficient database for storing and retrieving frequency-dependent
6×6 added mass and damping matrices from diffraction analysis.
"""

import numpy as np
import json
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from .models import HydrodynamicMatrix


class CoefficientDatabase:
    """
    Storage and retrieval system for frequency-dependent hydrodynamic coefficients

    Manages 6×6 added mass and damping matrices from diffraction analysis
    (AQWA, WAMIT, OrcaWave, etc.) with JSON serialization.

    Reference: DNV-RP-H103 Modelling and Analysis of Marine Operations
    """

    def __init__(self):
        """Initialize empty coefficient database"""
        self.data: Dict[str, Dict[str, any]] = {}

    def store_matrix(
        self,
        vessel_name: str,
        frequency: float,
        matrix: np.ndarray,
        matrix_type: str
    ) -> None:
        """
        Store hydrodynamic coefficient matrix

        Args:
            vessel_name: Vessel identifier
            frequency: Frequency in rad/s
            matrix: 6×6 coefficient matrix
            matrix_type: "added_mass" or "damping"

        Raises:
            ValueError: If matrix dimensions incorrect or type invalid
        """
        if matrix_type not in ['added_mass', 'damping']:
            raise ValueError(
                f"matrix_type must be 'added_mass' or 'damping', got '{matrix_type}'"
            )

        # Create HydrodynamicMatrix for validation
        hydro_matrix = HydrodynamicMatrix(
            frequency=frequency,
            matrix=matrix,
            matrix_type=matrix_type
        )

        # Initialize vessel if not exists
        if vessel_name not in self.data:
            self.data[vessel_name] = {
                'added_mass': {},
                'damping': {},
                'frequencies': []
            }

        # Store matrix
        freq_key = f"{frequency:.6f}"
        self.data[vessel_name][matrix_type][freq_key] = matrix.tolist()

        # Update frequency list
        if frequency not in self.data[vessel_name]['frequencies']:
            self.data[vessel_name]['frequencies'].append(frequency)
            self.data[vessel_name]['frequencies'].sort()

    def store(
        self,
        vessel_name: str,
        frequency: float,
        added_mass: Optional[np.ndarray] = None,
        damping: Optional[np.ndarray] = None
    ) -> None:
        """
        Store both added mass and damping at a frequency

        Args:
            vessel_name: Vessel identifier
            frequency: Frequency in rad/s
            added_mass: 6×6 added mass matrix (optional)
            damping: 6×6 damping matrix (optional)
        """
        if added_mass is not None:
            self.store_matrix(vessel_name, frequency, added_mass, 'added_mass')

        if damping is not None:
            self.store_matrix(vessel_name, frequency, damping, 'damping')

    def get_matrix(
        self,
        vessel_name: str,
        frequency: float,
        matrix_type: str,
        interpolate: bool = True
    ) -> np.ndarray:
        """
        Retrieve hydrodynamic coefficient matrix

        Args:
            vessel_name: Vessel identifier
            frequency: Frequency in rad/s
            matrix_type: "added_mass" or "damping"
            interpolate: If True, interpolate between frequencies

        Returns:
            6×6 coefficient matrix

        Raises:
            KeyError: If vessel not in database
            ValueError: If no data for interpolation
        """
        if vessel_name not in self.data:
            raise KeyError(f"Vessel '{vessel_name}' not in database")

        vessel_data = self.data[vessel_name]
        available_freqs = vessel_data['frequencies']

        if not available_freqs:
            raise ValueError(f"No frequency data for vessel '{vessel_name}'")

        # Exact match
        freq_key = f"{frequency:.6f}"
        if freq_key in vessel_data[matrix_type]:
            return np.array(vessel_data[matrix_type][freq_key])

        if not interpolate:
            raise ValueError(f"No exact match for frequency {frequency} rad/s")

        # Interpolate
        return self._interpolate_matrix(
            vessel_name, frequency, matrix_type, available_freqs
        )

    def _interpolate_matrix(
        self,
        vessel_name: str,
        frequency: float,
        matrix_type: str,
        available_freqs: List[float]
    ) -> np.ndarray:
        """
        Interpolate matrix at target frequency

        Uses linear interpolation between nearest frequencies.

        Args:
            vessel_name: Vessel identifier
            frequency: Target frequency (rad/s)
            matrix_type: "added_mass" or "damping"
            available_freqs: Available frequencies

        Returns:
            Interpolated 6×6 matrix
        """
        vessel_data = self.data[vessel_name]

        # Find bounding frequencies
        available_freqs = np.array(available_freqs)

        if frequency <= available_freqs[0]:
            # Use first frequency
            freq_key = f"{available_freqs[0]:.6f}"
            return np.array(vessel_data[matrix_type][freq_key])

        if frequency >= available_freqs[-1]:
            # Use last frequency
            freq_key = f"{available_freqs[-1]:.6f}"
            return np.array(vessel_data[matrix_type][freq_key])

        # Linear interpolation between two nearest frequencies
        idx_upper = np.searchsorted(available_freqs, frequency)
        idx_lower = idx_upper - 1

        freq_lower = available_freqs[idx_lower]
        freq_upper = available_freqs[idx_upper]

        # Get matrices
        key_lower = f"{freq_lower:.6f}"
        key_upper = f"{freq_upper:.6f}"

        matrix_lower = np.array(vessel_data[matrix_type][key_lower])
        matrix_upper = np.array(vessel_data[matrix_type][key_upper])

        # Linear interpolation factor
        alpha = (frequency - freq_lower) / (freq_upper - freq_lower)

        # Interpolate
        matrix_interp = (1 - alpha) * matrix_lower + alpha * matrix_upper

        return matrix_interp

    def get_matrices(
        self,
        vessel_name: str,
        frequency: float,
        interpolate: bool = True
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Get both added mass and damping matrices

        Args:
            vessel_name: Vessel identifier
            frequency: Frequency in rad/s
            interpolate: If True, interpolate between frequencies

        Returns:
            (added_mass, damping) as 6×6 matrices
        """
        added_mass = self.get_matrix(
            vessel_name, frequency, 'added_mass', interpolate
        )
        damping = self.get_matrix(
            vessel_name, frequency, 'damping', interpolate
        )

        return added_mass, damping

    def get_frequencies(self, vessel_name: str) -> np.ndarray:
        """
        Get all available frequencies for a vessel

        Args:
            vessel_name: Vessel identifier

        Returns:
            Array of frequencies (rad/s)
        """
        if vessel_name not in self.data:
            raise KeyError(f"Vessel '{vessel_name}' not in database")

        return np.array(self.data[vessel_name]['frequencies'])

    def get_infinite_frequency_added_mass(
        self,
        vessel_name: str
    ) -> np.ndarray:
        """
        Get infinite frequency added mass (A_inf)

        Uses highest frequency as approximation to infinite frequency.

        Args:
            vessel_name: Vessel identifier

        Returns:
            6×6 added mass matrix at infinite frequency
        """
        freqs = self.get_frequencies(vessel_name)

        if len(freqs) == 0:
            raise ValueError(f"No frequency data for '{vessel_name}'")

        # Use highest frequency
        freq_max = freqs[-1]
        return self.get_matrix(vessel_name, freq_max, 'added_mass', interpolate=False)

    def save_to_file(self, filepath: str) -> None:
        """
        Save database to JSON file

        Args:
            filepath: Output JSON file path
        """
        output = {}

        for vessel_name, vessel_data in self.data.items():
            output[vessel_name] = {
                'frequencies_rad_s': vessel_data['frequencies'],
                'added_mass': vessel_data['added_mass'],
                'damping': vessel_data['damping'],
            }

        with open(filepath, 'w') as f:
            json.dump(output, f, indent=2)

    def load_from_file(self, filepath: str) -> None:
        """
        Load database from JSON file

        Args:
            filepath: Input JSON file path
        """
        with open(filepath, 'r') as f:
            input_data = json.load(f)

        self.data = {}

        for vessel_name, vessel_data in input_data.items():
            self.data[vessel_name] = {
                'frequencies': vessel_data['frequencies_rad_s'],
                'added_mass': vessel_data['added_mass'],
                'damping': vessel_data['damping'],
            }

    def list_vessels(self) -> List[str]:
        """Get list of all vessels in database"""
        return list(self.data.keys())

    def check_symmetry(self, vessel_name: str, frequency: float) -> Dict[str, bool]:
        """
        Check matrix symmetry at a frequency

        Args:
            vessel_name: Vessel identifier
            frequency: Frequency in rad/s

        Returns:
            Dictionary with symmetry check results
        """
        added_mass, damping = self.get_matrices(vessel_name, frequency)

        am_symmetric = np.allclose(added_mass, added_mass.T, rtol=1e-5)
        damp_symmetric = np.allclose(damping, damping.T, rtol=1e-5)

        return {
            'added_mass_symmetric': am_symmetric,
            'damping_symmetric': damp_symmetric,
        }

    def check_positive_definite(
        self,
        vessel_name: str,
        frequency: float
    ) -> Dict[str, bool]:
        """
        Check if matrices are positive definite

        Args:
            vessel_name: Vessel identifier
            frequency: Frequency in rad/s

        Returns:
            Dictionary with positive definite check results
        """
        added_mass, damping = self.get_matrices(vessel_name, frequency)

        def is_pd(matrix):
            try:
                np.linalg.cholesky(matrix)
                return True
            except np.linalg.LinAlgError:
                return False

        return {
            'added_mass_positive_definite': is_pd(added_mass),
            'damping_positive_definite': is_pd(damping),
        }

    def summary(self, vessel_name: str) -> Dict:
        """
        Get summary statistics for vessel coefficients

        Args:
            vessel_name: Vessel identifier

        Returns:
            Summary dictionary
        """
        if vessel_name not in self.data:
            raise KeyError(f"Vessel '{vessel_name}' not in database")

        freqs = self.get_frequencies(vessel_name)

        return {
            'vessel_name': vessel_name,
            'n_frequencies': len(freqs),
            'frequency_range_rad_s': {
                'min': float(freqs[0]) if len(freqs) > 0 else None,
                'max': float(freqs[-1]) if len(freqs) > 0 else None,
            },
            'period_range_s': {
                'min': float(2*np.pi/freqs[-1]) if len(freqs) > 0 else None,
                'max': float(2*np.pi/freqs[0]) if len(freqs) > 0 else None,
            },
        }
