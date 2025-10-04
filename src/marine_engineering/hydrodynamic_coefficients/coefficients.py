"""
Hydrodynamic Coefficients Database and Interpolation Module

Provides comprehensive management of frequency-dependent hydrodynamic coefficients
including added mass and damping matrices with 2D interpolation capabilities.

Classes:
    CoefficientDatabase: Main database for hydrodynamic coefficients
    FrequencyDependentMatrix: Container for 6×6 matrices at specific frequencies
    KramersKronigValidator: Validates causality relationships
"""

import numpy as np
import pandas as pd
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Union
from dataclasses import dataclass
from scipy.interpolate import interp1d, interp2d, RectBivariateSpline
from scipy.signal import hilbert
import warnings


# DOF mapping for marine engineering
DOF_NAMES = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']
DOF_INDEX = {name: idx for idx, name in enumerate(DOF_NAMES)}


@dataclass
class FrequencyDependentMatrix:
    """Container for a 6×6 hydrodynamic coefficient matrix at a specific frequency.

    Attributes:
        frequency: Wave frequency in rad/s
        matrix: 6×6 numpy array of coefficient values
        matrix_type: Type of matrix ('added_mass' or 'damping')
    """
    frequency: float
    matrix: np.ndarray
    matrix_type: str

    def __post_init__(self):
        """Validate matrix dimensions and type."""
        if self.matrix.shape != (6, 6):
            raise ValueError(f"Matrix must be 6×6, got {self.matrix.shape}")
        if self.matrix_type not in ['added_mass', 'damping']:
            raise ValueError(f"Invalid matrix type: {self.matrix_type}")

    def get_coefficient(self, dof_i: Union[int, str], dof_j: Union[int, str]) -> float:
        """Get coefficient for specific DOF pair.

        Args:
            dof_i: First DOF (0-5 or 'Surge', 'Sway', etc.)
            dof_j: Second DOF (0-5 or 'Surge', 'Sway', etc.)

        Returns:
            Coefficient value at (i, j)
        """
        i = DOF_INDEX[dof_i] if isinstance(dof_i, str) else dof_i
        j = DOF_INDEX[dof_j] if isinstance(dof_j, str) else dof_j
        return self.matrix[i, j]

    def is_symmetric(self, rtol: float = 1e-5) -> bool:
        """Check if matrix is symmetric.

        Args:
            rtol: Relative tolerance for symmetry check

        Returns:
            True if matrix is symmetric within tolerance
        """
        return np.allclose(self.matrix, self.matrix.T, rtol=rtol)

    def get_diagonal(self) -> np.ndarray:
        """Get diagonal elements (uncoupled coefficients)."""
        return np.diag(self.matrix)

    def get_off_diagonal(self) -> np.ndarray:
        """Get off-diagonal elements (coupling coefficients)."""
        mask = ~np.eye(6, dtype=bool)
        return self.matrix[mask]


class KramersKronigValidator:
    """Validates causality relationships using Kramers-Kronig relations.

    The Kramers-Kronig relations connect the real (added mass) and imaginary
    (damping) parts of the frequency response function, ensuring physical causality.
    """

    @staticmethod
    def validate_pair(frequencies: np.ndarray,
                     added_mass: np.ndarray,
                     damping: np.ndarray,
                     tolerance: float = 0.1) -> Tuple[bool, float]:
        """Validate Kramers-Kronig relation for a DOF pair.

        Args:
            frequencies: Array of frequencies in rad/s
            added_mass: Added mass coefficients at each frequency
            damping: Damping coefficients at each frequency
            tolerance: Maximum allowed relative error

        Returns:
            Tuple of (is_valid, max_error)
        """
        # Sort by frequency
        sort_idx = np.argsort(frequencies)
        freq = frequencies[sort_idx]
        A = added_mass[sort_idx]
        B = damping[sort_idx]

        # Create complex frequency response
        # H(ω) = A(ω) + iB(ω)
        H = A + 1j * B

        # Use Hilbert transform to check causality
        # For causal systems: Im[H(ω)] = Hilbert[Re[H(ω)]]
        damping_reconstructed = np.imag(hilbert(A))

        # Calculate relative error
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            relative_error = np.abs((B - damping_reconstructed) / (B + 1e-10))

        max_error = np.max(relative_error[~np.isnan(relative_error)])
        is_valid = max_error < tolerance

        return is_valid, max_error


class CoefficientDatabase:
    """Main database for frequency-dependent hydrodynamic coefficients.

    Manages added mass and damping matrices across frequencies with interpolation
    capabilities and validation methods.

    Attributes:
        frequencies: Array of available frequencies in rad/s
        added_mass_matrices: Dictionary of FrequencyDependentMatrix objects
        damping_matrices: Dictionary of FrequencyDependentMatrix objects
    """

    def __init__(self):
        """Initialize empty coefficient database."""
        self.frequencies: np.ndarray = np.array([])
        self.added_mass_matrices: Dict[float, FrequencyDependentMatrix] = {}
        self.damping_matrices: Dict[float, FrequencyDependentMatrix] = {}
        self._interpolators_added_mass: Dict[Tuple[int, int], interp1d] = {}
        self._interpolators_damping: Dict[Tuple[int, int], interp1d] = {}
        self._interpolators_built = False

    @classmethod
    def from_csv(cls, data_dir: Union[str, Path]) -> 'CoefficientDatabase':
        """Load coefficient database from CSV files.

        Expected file naming convention:
        - added_mass_omega_{frequency}.csv
        - damping_omega_{frequency}.csv

        Args:
            data_dir: Directory containing CSV files

        Returns:
            Populated CoefficientDatabase instance
        """
        db = cls()
        data_path = Path(data_dir)

        # Load added mass matrices
        added_mass_files = sorted(data_path.glob('added_mass_omega_*.csv'))
        for file in added_mass_files:
            # Extract frequency from filename
            freq_str = file.stem.replace('added_mass_omega_', '')
            frequency = float(freq_str)

            # Load matrix
            df = pd.read_csv(file, index_col=0)
            matrix = df.values

            # Store matrix
            db.added_mass_matrices[frequency] = FrequencyDependentMatrix(
                frequency=frequency,
                matrix=matrix,
                matrix_type='added_mass'
            )

        # Load damping matrices
        damping_files = sorted(data_path.glob('damping_omega_*.csv'))
        for file in damping_files:
            # Extract frequency from filename
            freq_str = file.stem.replace('damping_omega_', '')
            frequency = float(freq_str)

            # Load matrix
            df = pd.read_csv(file, index_col=0)
            matrix = df.values

            # Store matrix
            db.damping_matrices[frequency] = FrequencyDependentMatrix(
                frequency=frequency,
                matrix=matrix,
                matrix_type='damping'
            )

        # Build frequency array
        db.frequencies = np.array(sorted(db.added_mass_matrices.keys()))

        # Validate matching frequencies
        damping_freqs = np.array(sorted(db.damping_matrices.keys()))
        if not np.allclose(db.frequencies, damping_freqs):
            warnings.warn("Added mass and damping frequencies do not match")

        return db

    def _build_interpolators(self):
        """Build interpolation functions for all DOF pairs."""
        if self._interpolators_built:
            return

        # Build interpolators for each DOF pair
        for i in range(6):
            for j in range(6):
                # Added mass interpolator
                added_mass_values = np.array([
                    self.added_mass_matrices[freq].matrix[i, j]
                    for freq in self.frequencies
                ])
                self._interpolators_added_mass[(i, j)] = interp1d(
                    self.frequencies,
                    added_mass_values,
                    kind='cubic',
                    bounds_error=False,
                    fill_value='extrapolate'
                )

                # Damping interpolator
                damping_values = np.array([
                    self.damping_matrices[freq].matrix[i, j]
                    for freq in self.frequencies
                ])
                self._interpolators_damping[(i, j)] = interp1d(
                    self.frequencies,
                    damping_values,
                    kind='cubic',
                    bounds_error=False,
                    fill_value='extrapolate'
                )

        self._interpolators_built = True

    def get_added_mass(self, frequency: float,
                       dof_i: Union[int, str],
                       dof_j: Union[int, str]) -> float:
        """Get added mass coefficient at specified frequency.

        Args:
            frequency: Wave frequency in rad/s
            dof_i: First DOF (0-5 or 'Surge', 'Sway', etc.)
            dof_j: Second DOF (0-5 or 'Surge', 'Sway', etc.)

        Returns:
            Interpolated added mass coefficient
        """
        self._build_interpolators()

        i = DOF_INDEX[dof_i] if isinstance(dof_i, str) else dof_i
        j = DOF_INDEX[dof_j] if isinstance(dof_j, str) else dof_j

        return float(self._interpolators_added_mass[(i, j)](frequency))

    def get_damping(self, frequency: float,
                    dof_i: Union[int, str],
                    dof_j: Union[int, str]) -> float:
        """Get damping coefficient at specified frequency.

        Args:
            frequency: Wave frequency in rad/s
            dof_i: First DOF (0-5 or 'Surge', 'Sway', etc.)
            dof_j: Second DOF (0-5 or 'Surge', 'Sway', etc.)

        Returns:
            Interpolated damping coefficient
        """
        self._build_interpolators()

        i = DOF_INDEX[dof_i] if isinstance(dof_i, str) else dof_i
        j = DOF_INDEX[dof_j] if isinstance(dof_j, str) else dof_j

        return float(self._interpolators_damping[(i, j)](frequency))

    def get_added_mass_matrix(self, frequency: float) -> np.ndarray:
        """Get full 6×6 added mass matrix at specified frequency.

        Args:
            frequency: Wave frequency in rad/s

        Returns:
            6×6 added mass matrix
        """
        self._build_interpolators()

        matrix = np.zeros((6, 6))
        for i in range(6):
            for j in range(6):
                matrix[i, j] = self._interpolators_added_mass[(i, j)](frequency)

        return matrix

    def get_damping_matrix(self, frequency: float) -> np.ndarray:
        """Get full 6×6 damping matrix at specified frequency.

        Args:
            frequency: Wave frequency in rad/s

        Returns:
            6×6 damping matrix
        """
        self._build_interpolators()

        matrix = np.zeros((6, 6))
        for i in range(6):
            for j in range(6):
                matrix[i, j] = self._interpolators_damping[(i, j)](frequency)

        return matrix

    def calculate_critical_damping_ratio(self, mass: float,
                                        stiffness: float,
                                        frequency: float,
                                        dof: Union[int, str]) -> float:
        """Calculate critical damping ratio for a DOF.

        Args:
            mass: System mass (including added mass)
            stiffness: System stiffness
            frequency: Wave frequency in rad/s
            dof: Degree of freedom (0-5 or 'Surge', 'Sway', etc.)

        Returns:
            Critical damping ratio (dimensionless)
        """
        dof_idx = DOF_INDEX[dof] if isinstance(dof, str) else dof

        # Get damping coefficient
        B = self.get_damping(frequency, dof_idx, dof_idx)

        # Critical damping: C_critical = 2 * sqrt(k * m)
        # Damping ratio: ζ = C / C_critical = B / (2 * sqrt(k * m))
        critical_damping = 2.0 * np.sqrt(stiffness * mass)

        if critical_damping > 0:
            return B / critical_damping
        else:
            return 0.0

    def validate_causality(self, dof_i: Union[int, str],
                          dof_j: Union[int, str],
                          tolerance: float = 0.1) -> Tuple[bool, float]:
        """Validate Kramers-Kronig causality for a DOF pair.

        Args:
            dof_i: First DOF (0-5 or 'Surge', 'Sway', etc.)
            dof_j: Second DOF (0-5 or 'Surge', 'Sway', etc.)
            tolerance: Maximum allowed relative error

        Returns:
            Tuple of (is_valid, max_error)
        """
        i = DOF_INDEX[dof_i] if isinstance(dof_i, str) else dof_i
        j = DOF_INDEX[dof_j] if isinstance(dof_j, str) else dof_j

        # Extract coefficient arrays
        added_mass = np.array([
            self.added_mass_matrices[freq].matrix[i, j]
            for freq in self.frequencies
        ])
        damping = np.array([
            self.damping_matrices[freq].matrix[i, j]
            for freq in self.frequencies
        ])

        return KramersKronigValidator.validate_pair(
            self.frequencies, added_mass, damping, tolerance
        )

    def get_frequency_range(self) -> Tuple[float, float]:
        """Get minimum and maximum frequencies in database.

        Returns:
            Tuple of (min_frequency, max_frequency)
        """
        return float(self.frequencies.min()), float(self.frequencies.max())

    def get_coefficient_statistics(self, coefficient_type: str) -> pd.DataFrame:
        """Get statistical summary of coefficients.

        Args:
            coefficient_type: 'added_mass' or 'damping'

        Returns:
            DataFrame with statistics for each DOF pair
        """
        stats = []

        matrices = (self.added_mass_matrices if coefficient_type == 'added_mass'
                   else self.damping_matrices)

        for i, dof_i in enumerate(DOF_NAMES):
            for j, dof_j in enumerate(DOF_NAMES):
                values = np.array([m.matrix[i, j] for m in matrices.values()])

                stats.append({
                    'DOF_i': dof_i,
                    'DOF_j': dof_j,
                    'Mean': np.mean(values),
                    'Std': np.std(values),
                    'Min': np.min(values),
                    'Max': np.max(values),
                    'Range': np.max(values) - np.min(values)
                })

        return pd.DataFrame(stats)

    def export_to_csv(self, output_dir: Union[str, Path]):
        """Export all matrices to CSV files.

        Args:
            output_dir: Directory to save CSV files
        """
        output_path = Path(output_dir)
        output_path.mkdir(parents=True, exist_ok=True)

        # Export added mass matrices
        for freq, matrix_obj in self.added_mass_matrices.items():
            filename = f'added_mass_omega_{freq:.4f}.csv'
            df = pd.DataFrame(matrix_obj.matrix,
                            index=DOF_NAMES,
                            columns=DOF_NAMES)
            df.to_csv(output_path / filename)

        # Export damping matrices
        for freq, matrix_obj in self.damping_matrices.items():
            filename = f'damping_omega_{freq:.4f}.csv'
            df = pd.DataFrame(matrix_obj.matrix,
                            index=DOF_NAMES,
                            columns=DOF_NAMES)
            df.to_csv(output_path / filename)

    def __repr__(self) -> str:
        """String representation of database."""
        freq_range = self.get_frequency_range()
        return (f"CoefficientDatabase("
                f"frequencies={len(self.frequencies)}, "
                f"range={freq_range[0]:.3f}-{freq_range[1]:.3f} rad/s)")
