#!/usr/bin/env python3
"""
OrcaWave Data Extraction Utilities

ABOUTME: Helper functions for extracting diffraction data from OrcaFlex models using OrcFxAPI.

Provides low-level data extraction methods for:
- RAO data (Response Amplitude Operators)
- Added mass matrices
- Damping matrices
- Frequency and heading discretization

Version: 3.0.0 (Phase 3.2)
Status: Data extraction implementation
"""

import numpy as np
from typing import Dict, List, Tuple, Optional

from digitalmodel.hydrodynamics.diffraction.diffraction_units import complex_phase_degrees

try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
except ImportError:
    ORCAFLEX_AVAILABLE = False


class OrcaWaveDataExtractor:
    """Extract diffraction data from OrcaFlex vessel type using OrcFxAPI"""

    def __init__(self, vessel):
        """
        Initialize extractor with vessel object

        Args:
            vessel: OrcFxAPI vessel object
        """
        if not ORCAFLEX_AVAILABLE:
            raise ImportError("OrcFxAPI required for data extraction")

        self.vessel = vessel
        self.vessel_type = vessel.VesselType

    def extract_rao_frequencies(self) -> np.ndarray:
        """
        Extract unique RAO frequencies from vessel type

        Returns:
            Array of frequencies in rad/s

        Raises:
            ValueError: If no valid frequencies found
        """
        # Access LoadRAOs data
        load_raos = self.vessel_type.LoadRAOs

        if load_raos.Size == 0:
            raise ValueError("No RAO data found in vessel type")

        # Extract frequency column
        # OrcaFlex stores RAOs in tables with Period/Frequency columns
        frequencies = []

        # Get all unique periods from RAO table
        for i in range(load_raos.Size):
            period = load_raos[i].Period
            if period > 0:  # Valid period
                freq = 2.0 * np.pi / period
                if freq not in frequencies:
                    frequencies.append(freq)

        if not frequencies:
            raise ValueError("No valid frequencies found in RAO data")

        return np.array(sorted(frequencies))

    def extract_rao_headings(self) -> np.ndarray:
        """
        Extract unique RAO heading angles from vessel type

        Returns:
            Array of headings in degrees

        Raises:
            ValueError: If no valid headings found
        """
        load_raos = self.vessel_type.LoadRAOs

        if load_raos.Size == 0:
            raise ValueError("No RAO data found in vessel type")

        headings = []

        # Get all unique directions from RAO table
        for i in range(load_raos.Size):
            direction = load_raos[i].Direction
            if direction not in headings:
                headings.append(direction)

        if not headings:
            raise ValueError("No valid headings found in RAO data")

        return np.array(sorted(headings))

    def extract_rao_for_dof(
        self,
        dof: str,
        frequencies: np.ndarray,
        headings: np.ndarray
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Extract RAO magnitude and phase for specific DOF

        Args:
            dof: DOF name ('Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw')
            frequencies: Array of frequencies
            headings: Array of headings

        Returns:
            (magnitude, phase) arrays of shape (nfreq, nheading)
        """
        load_raos = self.vessel_type.LoadRAOs

        nfreq = len(frequencies)
        nhead = len(headings)

        magnitude = np.zeros((nfreq, nhead))
        phase = np.zeros((nfreq, nhead))

        # Map to OrcaFlex variable names
        dof_map = {
            'Surge': 'LoadRAOSurge',
            'Sway': 'LoadRAOSway',
            'Heave': 'LoadRAOHeave',
            'Roll': 'LoadRAORoll',
            'Pitch': 'LoadRAOPitch',
            'Yaw': 'LoadRAOYaw'
        }

        if dof not in dof_map:
            raise ValueError(f"Invalid DOF: {dof}")

        orcaflex_var = dof_map[dof]

        # Extract data for each frequency and heading
        for i, freq in enumerate(frequencies):
            period = 2.0 * np.pi / freq

            for j, heading in enumerate(headings):
                # Find RAO entry for this period and heading
                for k in range(load_raos.Size):
                    entry = load_raos[k]

                    # Match period and direction (with tolerance)
                    if (abs(entry.Period - period) < 0.001 and
                        abs(entry.Direction - heading) < 0.1):

                        # Get RAO value (complex number)
                        rao_value = getattr(entry, orcaflex_var)

                        # Convert to magnitude and phase
                        mag = abs(rao_value)
                        ph = complex_phase_degrees(rao_value)

                        magnitude[i, j] = mag
                        phase[i, j] = ph
                        break

        return magnitude, phase

    def extract_added_mass_frequencies(self) -> np.ndarray:
        """
        Extract frequencies for added mass/damping data

        Returns:
            Array of frequencies in rad/s

        Raises:
            ValueError: If no valid frequencies found
        """
        # Access added mass and damping data
        am_damp = self.vessel_type.AddedMassAndDamping

        if am_damp.Size == 0:
            raise ValueError("No added mass/damping data found in vessel type")

        frequencies = []

        for i in range(am_damp.Size):
            period = am_damp[i].Period
            if period > 0:
                freq = 2.0 * np.pi / period
                if freq not in frequencies:
                    frequencies.append(freq)

        if not frequencies:
            raise ValueError("No valid frequencies found in added mass/damping data")

        return np.array(sorted(frequencies))

    def extract_added_mass_at_frequency(self, frequency: float) -> np.ndarray:
        """
        Extract 6×6 added mass matrix at specific frequency

        Args:
            frequency: Frequency in rad/s

        Returns:
            6×6 added mass matrix
        """
        am_damp = self.vessel_type.AddedMassAndDamping
        period = 2.0 * np.pi / frequency

        # Find entry for this frequency
        for i in range(am_damp.Size):
            if abs(am_damp[i].Period - period) < 0.001:
                entry = am_damp[i]

                # Extract 6×6 matrix
                # OrcaFlex stores as 21 independent values (symmetric matrix)
                matrix = np.zeros((6, 6))

                # Diagonal terms
                matrix[0, 0] = entry.AddedMassX
                matrix[1, 1] = entry.AddedMassY
                matrix[2, 2] = entry.AddedMassZ
                matrix[3, 3] = entry.AddedMassRx
                matrix[4, 4] = entry.AddedMassRy
                matrix[5, 5] = entry.AddedMassRz

                # Off-diagonal terms (symmetric)
                matrix[0, 1] = matrix[1, 0] = entry.AddedMassXY
                matrix[0, 2] = matrix[2, 0] = entry.AddedMassXZ
                matrix[0, 3] = matrix[3, 0] = entry.AddedMassXRx
                matrix[0, 4] = matrix[4, 0] = entry.AddedMassXRy
                matrix[0, 5] = matrix[5, 0] = entry.AddedMassXRz

                matrix[1, 2] = matrix[2, 1] = entry.AddedMassYZ
                matrix[1, 3] = matrix[3, 1] = entry.AddedMassYRx
                matrix[1, 4] = matrix[4, 1] = entry.AddedMassYRy
                matrix[1, 5] = matrix[5, 1] = entry.AddedMassYRz

                matrix[2, 3] = matrix[3, 2] = entry.AddedMassZRx
                matrix[2, 4] = matrix[4, 2] = entry.AddedMassZRy
                matrix[2, 5] = matrix[5, 2] = entry.AddedMassZRz

                matrix[3, 4] = matrix[4, 3] = entry.AddedMassRxRy
                matrix[3, 5] = matrix[5, 3] = entry.AddedMassRxRz

                matrix[4, 5] = matrix[5, 4] = entry.AddedMassRyRz

                return matrix

        # If not found, warn and return zero matrix
        import warnings
        warnings.warn(
            f"No added mass data found for frequency {frequency:.4f} rad/s. "
            f"Returning zero matrix.",
            UserWarning
        )
        return np.zeros((6, 6))

    def extract_damping_at_frequency(self, frequency: float) -> np.ndarray:
        """
        Extract 6×6 damping matrix at specific frequency

        Args:
            frequency: Frequency in rad/s

        Returns:
            6×6 damping matrix
        """
        am_damp = self.vessel_type.AddedMassAndDamping
        period = 2.0 * np.pi / frequency

        # Find entry for this frequency
        for i in range(am_damp.Size):
            if abs(am_damp[i].Period - period) < 0.001:
                entry = am_damp[i]

                # Extract 6×6 damping matrix (same structure as added mass)
                matrix = np.zeros((6, 6))

                # Diagonal terms
                matrix[0, 0] = entry.DampingX
                matrix[1, 1] = entry.DampingY
                matrix[2, 2] = entry.DampingZ
                matrix[3, 3] = entry.DampingRx
                matrix[4, 4] = entry.DampingRy
                matrix[5, 5] = entry.DampingRz

                # Off-diagonal terms (symmetric)
                matrix[0, 1] = matrix[1, 0] = entry.DampingXY
                matrix[0, 2] = matrix[2, 0] = entry.DampingXZ
                matrix[0, 3] = matrix[3, 0] = entry.DampingXRx
                matrix[0, 4] = matrix[4, 0] = entry.DampingXRy
                matrix[0, 5] = matrix[5, 0] = entry.DampingXRz

                matrix[1, 2] = matrix[2, 1] = entry.DampingYZ
                matrix[1, 3] = matrix[3, 1] = entry.DampingYRx
                matrix[1, 4] = matrix[4, 1] = entry.DampingYRy
                matrix[1, 5] = matrix[5, 1] = entry.DampingYRz

                matrix[2, 3] = matrix[3, 2] = entry.DampingZRx
                matrix[2, 4] = matrix[4, 2] = entry.DampingZRy
                matrix[2, 5] = matrix[5, 2] = entry.DampingZRz

                matrix[3, 4] = matrix[4, 3] = entry.DampingRxRy
                matrix[3, 5] = matrix[5, 3] = entry.DampingRxRz

                matrix[4, 5] = matrix[5, 4] = entry.DampingRyRz

                return matrix

        # If not found, warn and return zero matrix
        import warnings
        warnings.warn(
            f"No damping data found for frequency {frequency:.4f} rad/s. "
            f"Returning zero matrix.",
            UserWarning
        )
        return np.zeros((6, 6))

    def validate_data_consistency(self) -> Dict[str, any]:
        """
        Validate consistency of extracted data

        Returns:
            Dictionary with validation results
        """
        results = {
            'has_rao_data': False,
            'has_added_mass': False,
            'has_damping': False,
            'rao_freq_count': 0,
            'rao_heading_count': 0,
            'am_freq_count': 0,
            'issues': []
        }

        try:
            # Check RAO data
            load_raos = self.vessel_type.LoadRAOs
            if load_raos.Size > 0:
                results['has_rao_data'] = True
                results['rao_freq_count'] = len(self.extract_rao_frequencies())
                results['rao_heading_count'] = len(self.extract_rao_headings())

            # Check added mass/damping
            am_damp = self.vessel_type.AddedMassAndDamping
            if am_damp.Size > 0:
                results['has_added_mass'] = True
                results['has_damping'] = True
                results['am_freq_count'] = len(self.extract_added_mass_frequencies())

            # Consistency checks
            if results['rao_freq_count'] != results['am_freq_count']:
                results['issues'].append(
                    f"Frequency mismatch: RAOs={results['rao_freq_count']}, "
                    f"AddedMass={results['am_freq_count']}"
                )

        except Exception as e:
            results['issues'].append(f"Validation error: {str(e)}")

        return results


# Convenience functions

def extract_all_rao_data(vessel) -> Dict:
    """
    Extract complete RAO dataset from vessel

    Args:
        vessel: OrcFxAPI vessel object

    Returns:
        Dictionary with RAO data for all DOFs
    """
    extractor = OrcaWaveDataExtractor(vessel)

    frequencies = extractor.extract_rao_frequencies()
    headings = extractor.extract_rao_headings()

    rao_data = {
        'frequencies': frequencies,
        'headings': headings
    }

    # Extract each DOF
    for dof in ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']:
        magnitude, phase = extractor.extract_rao_for_dof(dof, frequencies, headings)
        rao_data[dof.lower()] = {
            'magnitude': magnitude,
            'phase': phase
        }

    return rao_data


def extract_all_added_mass(vessel) -> Dict:
    """
    Extract complete added mass dataset from vessel

    Args:
        vessel: OrcFxAPI vessel object

    Returns:
        Dictionary with added mass matrices
    """
    extractor = OrcaWaveDataExtractor(vessel)

    frequencies = extractor.extract_added_mass_frequencies()

    matrices = []
    for freq in frequencies:
        matrix = extractor.extract_added_mass_at_frequency(freq)
        matrices.append(matrix)

    return {
        'frequencies': frequencies,
        'matrices': matrices
    }


def extract_all_damping(vessel) -> Dict:
    """
    Extract complete damping dataset from vessel

    Args:
        vessel: OrcFxAPI vessel object

    Returns:
        Dictionary with damping matrices
    """
    extractor = OrcaWaveDataExtractor(vessel)

    frequencies = extractor.extract_added_mass_frequencies()  # Same frequencies

    matrices = []
    for freq in frequencies:
        matrix = extractor.extract_damping_at_frequency(freq)
        matrices.append(matrix)

    return {
        'frequencies': frequencies,
        'matrices': matrices
    }
