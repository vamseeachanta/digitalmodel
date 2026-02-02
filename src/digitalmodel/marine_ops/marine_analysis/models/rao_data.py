"""Unified RAO Data Models for Marine Analysis.

This module provides comprehensive data models for Response Amplitude Operators (RAOs)
including displacement, velocity, and acceleration RAOs from various sources (AQWA, OrcaFlex).
"""

from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, List, Optional, Any
import numpy as np
from datetime import datetime


class RAOType(Enum):
    """RAO types supported by the unified reader."""
    DISPLACEMENT = "displacement"
    VELOCITY = "velocity"
    ACCELERATION = "acceleration"


class SourceFormat(Enum):
    """Source file formats for RAO data."""
    AQWA_LIS = "aqwa_lis"
    AQWA_BINARY = "aqwa_binary"  # Via licensed reader
    ORCAFLEX_YML = "orcaflex_yml"
    CSV = "csv"


@dataclass
class RAOMetadata:
    """Metadata for RAO data."""
    source_file: str = ""
    source_format: SourceFormat = SourceFormat.AQWA_LIS
    vessel_name: str = ""
    analysis_date: str = field(default_factory=lambda: datetime.now().isoformat())
    parser_version: str = "2.0.0"
    license_required: bool = False
    additional_info: Dict[str, Any] = field(default_factory=dict)


@dataclass
class DOFData:
    """Data container for a single degree of freedom."""
    amplitude: np.ndarray  # Shape: (n_frequencies, n_headings)
    phase: np.ndarray      # Shape: (n_frequencies, n_headings), in degrees

    def __post_init__(self):
        """Validate array shapes."""
        if self.amplitude.shape != self.phase.shape:
            raise ValueError(
                f"Amplitude and phase arrays must have same shape. "
                f"Got amplitude: {self.amplitude.shape}, phase: {self.phase.shape}"
            )


@dataclass
class RAOData:
    """Base class for RAO data with standard 6-DOF structure.

    This represents RAO data for a single RAO type (displacement, velocity, or acceleration).
    """
    frequencies: np.ndarray  # rad/s, shape: (n_frequencies,)
    headings: np.ndarray     # degrees, shape: (n_headings,)

    # 6-DOF data
    surge: DOFData
    sway: DOFData
    heave: DOFData
    roll: DOFData
    pitch: DOFData
    yaw: DOFData

    # Units for each DOF
    units: Dict[str, str] = field(default_factory=dict)
    metadata: RAOMetadata = field(default_factory=RAOMetadata)

    def __post_init__(self):
        """Set default units if not provided."""
        if not self.units:
            self.units = self._get_default_units()

        # Validate consistency
        self._validate_dimensions()

    def _get_default_units(self) -> Dict[str, str]:
        """Get default units (to be overridden by subclasses)."""
        return {
            'frequency': 'rad/s',
            'heading': 'deg',
            'surge': 'unit/unit',
            'sway': 'unit/unit',
            'heave': 'unit/unit',
            'roll': 'unit/unit',
            'pitch': 'unit/unit',
            'yaw': 'unit/unit',
            'phase': 'deg'
        }

    def _validate_dimensions(self):
        """Validate that all DOF data has consistent dimensions."""
        expected_shape = (len(self.frequencies), len(self.headings))

        for dof_name in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']:
            dof_data = getattr(self, dof_name)
            if dof_data.amplitude.shape != expected_shape:
                raise ValueError(
                    f"{dof_name} amplitude shape {dof_data.amplitude.shape} "
                    f"does not match expected shape {expected_shape}"
                )
            if dof_data.phase.shape != expected_shape:
                raise ValueError(
                    f"{dof_name} phase shape {dof_data.phase.shape} "
                    f"does not match expected shape {expected_shape}"
                )

    def get_dof_names(self) -> List[str]:
        """Get list of DOF names."""
        return ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']

    def get_dof_data(self, dof_name: str) -> DOFData:
        """Get data for a specific DOF."""
        if dof_name not in self.get_dof_names():
            raise ValueError(f"Invalid DOF name: {dof_name}")
        return getattr(self, dof_name)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary format (compatible with legacy code)."""
        return {
            'frequencies': self.frequencies,
            'headings': self.headings,
            'raos': {
                dof: {
                    'amplitude': getattr(self, dof).amplitude,
                    'phase': getattr(self, dof).phase
                }
                for dof in self.get_dof_names()
            },
            'units': self.units,
            'metadata': {
                'source_file': self.metadata.source_file,
                'vessel_name': self.metadata.vessel_name,
                'analysis_date': self.metadata.analysis_date
            }
        }


@dataclass
class DisplacementRAO(RAOData):
    """Displacement RAO data.

    Units:
    - Translation (surge, sway, heave): m/m (motion per unit wave amplitude)
    - Rotation (roll, pitch, yaw): deg/m (rotation per unit wave amplitude)
    """

    def _get_default_units(self) -> Dict[str, str]:
        """Get displacement RAO units."""
        return {
            'frequency': 'rad/s',
            'heading': 'deg',
            'surge': 'm/m',
            'sway': 'm/m',
            'heave': 'm/m',
            'roll': 'deg/m',
            'pitch': 'deg/m',
            'yaw': 'deg/m',
            'phase': 'deg'
        }


@dataclass
class VelocityRAO(RAOData):
    """Velocity RAO data.

    Units:
    - Translation (surge, sway, heave): (m/s)/m (velocity per unit wave amplitude)
    - Rotation (roll, pitch, yaw): (deg/s)/m (angular velocity per unit wave amplitude)
    """

    def _get_default_units(self) -> Dict[str, str]:
        """Get velocity RAO units."""
        return {
            'frequency': 'rad/s',
            'heading': 'deg',
            'surge': '(m/s)/m',
            'sway': '(m/s)/m',
            'heave': '(m/s)/m',
            'roll': '(deg/s)/m',
            'pitch': '(deg/s)/m',
            'yaw': '(deg/s)/m',
            'phase': 'deg'
        }


@dataclass
class AccelerationRAO(RAOData):
    """Acceleration RAO data.

    Units:
    - Translation (surge, sway, heave): (m/s²)/m (acceleration per unit wave amplitude)
    - Rotation (roll, pitch, yaw): (deg/s²)/m (angular acceleration per unit wave amplitude)
    """

    def _get_default_units(self) -> Dict[str, str]:
        """Get acceleration RAO units."""
        return {
            'frequency': 'rad/s',
            'heading': 'deg',
            'surge': '(m/s²)/m',
            'sway': '(m/s²)/m',
            'heave': '(m/s²)/m',
            'roll': '(deg/s²)/m',
            'pitch': '(deg/s²)/m',
            'yaw': '(deg/s²)/m',
            'phase': 'deg'
        }


@dataclass
class UnifiedRAOData:
    """Unified container for all RAO types from a single analysis.

    This allows storing displacement, velocity, and acceleration RAOs together,
    which is common when parsing AQWA .lis files that contain all three types.
    """
    displacement: Optional[DisplacementRAO] = None
    velocity: Optional[VelocityRAO] = None
    acceleration: Optional[AccelerationRAO] = None

    metadata: RAOMetadata = field(default_factory=RAOMetadata)

    def has_displacement(self) -> bool:
        """Check if displacement RAOs are available."""
        return self.displacement is not None

    def has_velocity(self) -> bool:
        """Check if velocity RAOs are available."""
        return self.velocity is not None

    def has_acceleration(self) -> bool:
        """Check if acceleration RAOs are available."""
        return self.acceleration is not None

    def get_available_types(self) -> List[RAOType]:
        """Get list of available RAO types."""
        available = []
        if self.has_displacement():
            available.append(RAOType.DISPLACEMENT)
        if self.has_velocity():
            available.append(RAOType.VELOCITY)
        if self.has_acceleration():
            available.append(RAOType.ACCELERATION)
        return available

    def get_rao_data(self, rao_type: RAOType) -> Optional[RAOData]:
        """Get RAO data for a specific type."""
        if rao_type == RAOType.DISPLACEMENT:
            return self.displacement
        elif rao_type == RAOType.VELOCITY:
            return self.velocity
        elif rao_type == RAOType.ACCELERATION:
            return self.acceleration
        else:
            raise ValueError(f"Unknown RAO type: {rao_type}")

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary format."""
        result = {
            'metadata': {
                'source_file': self.metadata.source_file,
                'vessel_name': self.metadata.vessel_name,
                'analysis_date': self.metadata.analysis_date,
                'available_types': [t.value for t in self.get_available_types()]
            }
        }

        if self.displacement:
            result['displacement'] = self.displacement.to_dict()
        if self.velocity:
            result['velocity'] = self.velocity.to_dict()
        if self.acceleration:
            result['acceleration'] = self.acceleration.to_dict()

        return result


def create_dof_data(amplitude: np.ndarray, phase: np.ndarray) -> DOFData:
    """Helper function to create DOFData with validation."""
    return DOFData(amplitude=amplitude, phase=phase)


def create_empty_rao_arrays(n_freq: int, n_head: int) -> Dict[str, DOFData]:
    """Create empty RAO arrays for all 6 DOF.

    Args:
        n_freq: Number of frequencies
        n_head: Number of headings

    Returns:
        Dictionary with DOFData for each DOF
    """
    dof_names = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
    return {
        dof: DOFData(
            amplitude=np.zeros((n_freq, n_head)),
            phase=np.zeros((n_freq, n_head))
        )
        for dof in dof_names
    }
