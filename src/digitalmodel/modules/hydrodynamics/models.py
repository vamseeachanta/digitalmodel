#!/usr/bin/env python3
"""
ABOUTME: Data models for hydrodynamic analysis including coefficient matrices,
vessel properties, wave parameters, and environmental conditions.
"""

import numpy as np
from dataclasses import dataclass, field
from typing import Optional, List, Dict, Tuple
from enum import Enum


class WaveSpectrumType(Enum):
    """Wave spectrum types for offshore analysis"""
    JONSWAP = "jonswap"
    PIERSON_MOSKOWITZ = "pierson_moskowitz"
    BRETSCHNEIDER = "bretschneider"
    ISSC = "issc"
    OCHI_HUBBLE = "ochi_hubble"
    CUSTOM = "custom"


class MatrixDOF(Enum):
    """Degrees of freedom for 6×6 hydrodynamic matrices"""
    SURGE = 0    # X-translation
    SWAY = 1     # Y-translation
    HEAVE = 2    # Z-translation
    ROLL = 3     # Rotation about X
    PITCH = 4    # Rotation about Y
    YAW = 5      # Rotation about Z


@dataclass
class HydrodynamicMatrix:
    """
    6×6 frequency-dependent hydrodynamic coefficient matrix

    Represents added mass or damping matrices at a specific frequency.
    Follows DNV-RP-C205 convention.
    """
    frequency: float  # rad/s
    matrix: np.ndarray  # 6×6 array
    matrix_type: str  # "added_mass" or "damping"

    def __post_init__(self):
        """Validate matrix dimensions and properties"""
        if self.matrix.shape != (6, 6):
            raise ValueError(f"Matrix must be 6×6, got {self.matrix.shape}")

        if self.frequency < 0:
            raise ValueError(f"Frequency must be non-negative, got {self.frequency}")

    @property
    def is_symmetric(self) -> bool:
        """Check if matrix is symmetric (should be for physical consistency)"""
        return np.allclose(self.matrix, self.matrix.T, rtol=1e-5)

    @property
    def is_positive_definite(self) -> bool:
        """Check if matrix is positive definite"""
        try:
            np.linalg.cholesky(self.matrix)
            return True
        except np.linalg.LinAlgError:
            return False

    def get_component(self, dof_i: MatrixDOF, dof_j: MatrixDOF) -> float:
        """Get matrix component for specific DOF coupling"""
        return self.matrix[dof_i.value, dof_j.value]

    def to_dict(self) -> dict:
        """Convert to dictionary for JSON serialization"""
        return {
            'frequency_rad_s': float(self.frequency),
            'frequency_hz': float(self.frequency / (2 * np.pi)),
            'period_s': float(2 * np.pi / self.frequency) if self.frequency > 0 else np.inf,
            'matrix_type': self.matrix_type,
            'matrix': self.matrix.tolist(),
            'is_symmetric': self.is_symmetric,
            'is_positive_definite': self.is_positive_definite,
        }


@dataclass
class VesselProperties:
    """
    Vessel geometric and mass properties for environmental loading

    Used for OCIMF wind/current load calculations and general
    hydrodynamic analysis.
    """
    name: str
    length_overall: float  # m
    beam: float  # m
    draft: float  # m
    displacement: float  # tonnes

    # Optional additional properties
    freeboard: Optional[float] = None  # m
    depth: Optional[float] = None  # m
    block_coefficient: Optional[float] = None
    waterplane_area: Optional[float] = None  # m²

    # Wind area projections
    frontal_wind_area: Optional[float] = None  # m²
    lateral_wind_area: Optional[float] = None  # m²

    def __post_init__(self):
        """Validate vessel properties"""
        if self.length_overall <= 0:
            raise ValueError("Length must be positive")
        if self.beam <= 0:
            raise ValueError("Beam must be positive")
        if self.draft <= 0:
            raise ValueError("Draft must be positive")
        if self.displacement <= 0:
            raise ValueError("Displacement must be positive")

    @property
    def length_to_beam_ratio(self) -> float:
        """L/B ratio"""
        return self.length_overall / self.beam

    @property
    def beam_to_draft_ratio(self) -> float:
        """B/T ratio"""
        return self.beam / self.draft

    @property
    def estimated_wetted_surface(self) -> float:
        """Estimated wetted surface area (m²)"""
        # Simplified formula
        return 1.7 * self.length_overall * (2 * self.draft + self.beam)

    def to_dict(self) -> dict:
        """Convert to dictionary"""
        return {
            'name': self.name,
            'length_overall_m': self.length_overall,
            'beam_m': self.beam,
            'draft_m': self.draft,
            'displacement_tonnes': self.displacement,
            'freeboard_m': self.freeboard,
            'L_B_ratio': self.length_to_beam_ratio,
            'B_T_ratio': self.beam_to_draft_ratio,
        }


@dataclass
class WaveParameters:
    """
    Wave spectrum parameters for sea state definition

    Supports multiple spectrum types with their characteristic parameters.
    """
    spectrum_type: WaveSpectrumType
    significant_height: float  # Hs (m)
    peak_period: float  # Tp (s)

    # JONSWAP-specific
    gamma: float = 3.3  # Peakedness parameter

    # Frequency range for spectrum generation
    freq_min: float = 0.02  # rad/s
    freq_max: float = 2.0   # rad/s
    n_frequencies: int = 100

    # Ochi-Hubble bimodal parameters
    hs_swell: Optional[float] = None
    tp_swell: Optional[float] = None

    def __post_init__(self):
        """Validate wave parameters"""
        if self.significant_height <= 0:
            raise ValueError("Significant height must be positive")
        if self.peak_period <= 0:
            raise ValueError("Peak period must be positive")
        if self.gamma <= 0:
            raise ValueError("Gamma must be positive")
        if self.freq_min >= self.freq_max:
            raise ValueError("freq_min must be less than freq_max")

    @property
    def peak_frequency(self) -> float:
        """Peak frequency (rad/s)"""
        return 2 * np.pi / self.peak_period

    @property
    def zero_crossing_period(self) -> float:
        """Approximate zero-crossing period (s)"""
        # For JONSWAP: Tz ≈ 0.711 * Tp
        return 0.711 * self.peak_period

    def frequency_array(self) -> np.ndarray:
        """Generate frequency array for spectrum"""
        return np.linspace(self.freq_min, self.freq_max, self.n_frequencies)

    def to_dict(self) -> dict:
        """Convert to dictionary"""
        return {
            'spectrum_type': self.spectrum_type.value,
            'Hs_m': self.significant_height,
            'Tp_s': self.peak_period,
            'Tz_s': self.zero_crossing_period,
            'gamma': self.gamma,
            'peak_frequency_rad_s': self.peak_frequency,
            'frequency_range': {
                'min_rad_s': self.freq_min,
                'max_rad_s': self.freq_max,
                'n_points': self.n_frequencies,
            }
        }


@dataclass
class EnvironmentalConditions:
    """
    Complete environmental conditions for vessel loading analysis

    Includes wave, wind, and current parameters for OCIMF calculations
    and general response analysis.
    """
    # Wave conditions
    wave_params: Optional[WaveParameters] = None
    wave_direction: float = 0.0  # degrees, 0=head seas

    # Wind conditions
    wind_speed: float = 0.0  # m/s (1-hour mean at 10m height)
    wind_direction: float = 0.0  # degrees from bow

    # Current conditions
    current_speed: float = 0.0  # m/s
    current_direction: float = 0.0  # degrees from bow

    # Environmental parameters
    water_density: float = 1025.0  # kg/m³
    air_density: float = 1.225  # kg/m³

    def __post_init__(self):
        """Validate environmental conditions"""
        if self.wind_speed < 0:
            raise ValueError("Wind speed must be non-negative")
        if self.current_speed < 0:
            raise ValueError("Current speed must be non-negative")
        if self.water_density <= 0:
            raise ValueError("Water density must be positive")
        if self.air_density <= 0:
            raise ValueError("Air density must be positive")

    def to_dict(self) -> dict:
        """Convert to dictionary"""
        result = {
            'wave_direction_deg': self.wave_direction,
            'wind_speed_m_s': self.wind_speed,
            'wind_direction_deg': self.wind_direction,
            'current_speed_m_s': self.current_speed,
            'current_direction_deg': self.current_direction,
            'water_density_kg_m3': self.water_density,
            'air_density_kg_m3': self.air_density,
        }

        if self.wave_params:
            result['wave_parameters'] = self.wave_params.to_dict()

        return result


@dataclass
class RAOData:
    """
    Response Amplitude Operator data

    RAOs define vessel motion response per unit wave amplitude
    as function of frequency and direction.
    """
    frequencies: np.ndarray  # rad/s
    directions: np.ndarray   # degrees

    # RAO amplitudes for each DOF [n_freq × n_dir × 6]
    amplitudes: np.ndarray

    # RAO phases for each DOF [n_freq × n_dir × 6]
    phases: np.ndarray

    vessel_name: str = "Vessel"

    def __post_init__(self):
        """Validate RAO data structure"""
        n_freq = len(self.frequencies)
        n_dir = len(self.directions)

        expected_shape = (n_freq, n_dir, 6)

        if self.amplitudes.shape != expected_shape:
            raise ValueError(
                f"Amplitude shape {self.amplitudes.shape} doesn't match "
                f"expected {expected_shape}"
            )

        if self.phases.shape != expected_shape:
            raise ValueError(
                f"Phase shape {self.phases.shape} doesn't match "
                f"expected {expected_shape}"
            )

    def get_rao(self, frequency: float, direction: float, dof: MatrixDOF) -> Tuple[float, float]:
        """
        Get RAO amplitude and phase for specific conditions

        Returns:
            (amplitude, phase_deg)
        """
        # Find nearest frequency
        freq_idx = np.argmin(np.abs(self.frequencies - frequency))

        # Find nearest direction
        dir_idx = np.argmin(np.abs(self.directions - direction))

        amplitude = self.amplitudes[freq_idx, dir_idx, dof.value]
        phase = self.phases[freq_idx, dir_idx, dof.value]

        return amplitude, phase


# Standard vessel type definitions for quick reference
VESSEL_TYPES = {
    'fpso': VesselProperties(
        name="Standard FPSO",
        length_overall=300.0,
        beam=50.0,
        draft=20.0,
        displacement=200000.0,
        freeboard=15.0,
    ),
    'semisubmersible': VesselProperties(
        name="Standard Semi-submersible",
        length_overall=100.0,
        beam=80.0,
        draft=25.0,
        displacement=40000.0,
        freeboard=20.0,
    ),
    'tanker': VesselProperties(
        name="Standard Tanker",
        length_overall=250.0,
        beam=45.0,
        draft=16.0,
        displacement=150000.0,
        freeboard=12.0,
    ),
}


def get_vessel_type(vessel_type: str) -> VesselProperties:
    """
    Get standard vessel properties by type

    Args:
        vessel_type: One of 'fpso', 'semisubmersible', 'tanker'

    Returns:
        VesselProperties object

    Raises:
        KeyError: If vessel type not recognized
    """
    if vessel_type.lower() not in VESSEL_TYPES:
        raise KeyError(
            f"Unknown vessel type '{vessel_type}'. "
            f"Available: {list(VESSEL_TYPES.keys())}"
        )

    return VESSEL_TYPES[vessel_type.lower()]
