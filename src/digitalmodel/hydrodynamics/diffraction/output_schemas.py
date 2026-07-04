#!/usr/bin/env python3
"""
Unified Output Schemas for Diffraction Analysis

Defines standardized data structures for RAO, added mass, and damping
coefficients from both AQWA and OrcaWave analyses.

Compatible with OrcaFlex import requirements.
"""

from dataclasses import dataclass, field
from typing import List, Dict, Optional, Union
import numpy as np
from enum import Enum


class DOF(Enum):
    """Degrees of Freedom for vessel motion"""
    SURGE = 1    # X-translation
    SWAY = 2     # Y-translation
    HEAVE = 3    # Z-translation
    ROLL = 4     # Rotation about X
    PITCH = 5    # Rotation about Y
    YAW = 6      # Rotation about Z


class Unit(Enum):
    """Standard units for hydrodynamic coefficients"""
    # RAO units
    RAO_TRANSLATION = "m/m"           # Translation per wave amplitude
    RAO_ROTATION = "deg/m"            # Rotation per wave amplitude

    # Added mass units
    ADDED_MASS_LINEAR = "kg"          # Linear-linear coupling
    ADDED_MASS_ANGULAR = "kg.m"       # Linear-angular coupling
    ADDED_MASS_ROTATIONAL = "kg.m^2"  # Angular-angular coupling

    # Damping units
    DAMPING_LINEAR = "N.s/m"          # Linear damping
    DAMPING_ANGULAR = "N.m.s/rad"     # Angular damping

    # Force units
    FORCE_LINEAR = "N/m"              # Force per wave amplitude
    FORCE_ANGULAR = "N.m/m"           # Moment per wave amplitude

    # Frequency/period
    FREQUENCY = "rad/s"
    PERIOD = "s"
    HEADING = "deg"


@dataclass
class FrequencyData:
    """Frequency discretization for diffraction analysis"""
    values: np.ndarray              # Frequency values (rad/s)
    periods: np.ndarray             # Corresponding periods (s)
    count: int                      # Number of frequencies
    min_freq: float                 # Minimum frequency (rad/s)
    max_freq: float                 # Maximum frequency (rad/s)
    unit: str = Unit.FREQUENCY.value

    def __post_init__(self):
        """Validate and compute derived quantities"""
        if self.values is not None:
            self.count = len(self.values)
            self.min_freq = float(np.min(self.values))
            self.max_freq = float(np.max(self.values))
            self.periods = 2.0 * np.pi / self.values


@dataclass
class HeadingData:
    """Heading angles for diffraction analysis"""
    values: np.ndarray              # Heading values (degrees)
    count: int                      # Number of headings
    min_heading: float              # Minimum heading (deg)
    max_heading: float              # Maximum heading (deg)
    unit: str = Unit.HEADING.value

    def __post_init__(self):
        """Validate and compute derived quantities"""
        if self.values is not None:
            self.count = len(self.values)
            self.min_heading = float(np.min(self.values))
            self.max_heading = float(np.max(self.values))


@dataclass
class RAOComponent:
    """Single RAO component (one DOF, all frequencies and headings)"""
    dof: DOF                        # Degree of freedom
    magnitude: np.ndarray           # RAO magnitude [nfreq x nheading]
    phase: np.ndarray               # RAO phase in degrees [nfreq x nheading]
    frequencies: FrequencyData      # Frequency discretization
    headings: HeadingData           # Heading discretization
    unit: str                       # RAO unit (depends on DOF type)

    def __post_init__(self):
        """Set appropriate units based on DOF"""
        if self.dof in [DOF.SURGE, DOF.SWAY, DOF.HEAVE]:
            self.unit = Unit.RAO_TRANSLATION.value
        else:
            self.unit = Unit.RAO_ROTATION.value


@dataclass
class RAOSet:
    """Complete set of RAOs for all 6 DOFs"""
    vessel_name: str
    analysis_tool: str              # "AQWA" or "OrcaWave"
    water_depth: float              # meters

    # RAO components for each DOF
    surge: RAOComponent
    sway: RAOComponent
    heave: RAOComponent
    roll: RAOComponent
    pitch: RAOComponent
    yaw: RAOComponent

    # Metadata
    created_date: str
    source_file: Optional[str] = None
    notes: Optional[str] = None

    def get_component(self, dof: DOF) -> RAOComponent:
        """Get RAO component by DOF"""
        mapping = {
            DOF.SURGE: self.surge,
            DOF.SWAY: self.sway,
            DOF.HEAVE: self.heave,
            DOF.ROLL: self.roll,
            DOF.PITCH: self.pitch,
            DOF.YAW: self.yaw
        }
        return mapping[dof]

    def to_dict(self) -> Dict:
        """Convert to dictionary for serialization"""
        return {
            'vessel_name': self.vessel_name,
            'analysis_tool': self.analysis_tool,
            'water_depth': self.water_depth,
            'created_date': self.created_date,
            'source_file': self.source_file,
            'notes': self.notes,
            'raos': {
                'surge': self._component_to_dict(self.surge),
                'sway': self._component_to_dict(self.sway),
                'heave': self._component_to_dict(self.heave),
                'roll': self._component_to_dict(self.roll),
                'pitch': self._component_to_dict(self.pitch),
                'yaw': self._component_to_dict(self.yaw)
            }
        }

    @classmethod
    def from_dict(cls, d: Dict) -> "RAOSet":
        comps = d['raos']
        components = {name: _rao_component_from_dict(comps[name]) for name in
                     ('surge', 'sway', 'heave', 'roll', 'pitch', 'yaw')}
        return cls(
            vessel_name=d['vessel_name'],
            analysis_tool=d['analysis_tool'],
            water_depth=float(d['water_depth']),
            created_date=d.get('created_date', ''),
            source_file=d.get('source_file'),
            notes=d.get('notes'),
            **components,
        )

    @staticmethod
    def _component_to_dict(component: RAOComponent) -> Dict:
        """Convert RAO component to dictionary"""
        return {
            'dof': component.dof.name,
            'magnitude': component.magnitude.tolist(),
            'phase': component.phase.tolist(),
            'unit': component.unit,
            'frequencies': {
                'values': component.frequencies.values.tolist(),
                'periods': component.frequencies.periods.tolist(),
                'count': component.frequencies.count,
                'unit': component.frequencies.unit
            },
            'headings': {
                'values': component.headings.values.tolist(),
                'count': component.headings.count,
                'unit': component.headings.unit
            }
        }


@dataclass
class HydrodynamicMatrix:
    """6x6 hydrodynamic coefficient matrix (added mass or damping)"""
    matrix: np.ndarray              # 6x6 matrix
    frequency: float                # Frequency at which computed (rad/s)
    matrix_type: str                # "added_mass" or "damping"
    units: Dict[str, str]           # Units for each coupling type

    def __post_init__(self):
        """Validate matrix dimensions"""
        assert self.matrix.shape == (6, 6), f"Matrix must be 6x6, got {self.matrix.shape}"

    def get_coupling(self, dof_i: DOF, dof_j: DOF) -> float:
        """Get coupling coefficient between two DOFs"""
        return self.matrix[dof_i.value - 1, dof_j.value - 1]

    def to_dict(self) -> Dict:
        """Convert to dictionary"""
        return {
            'matrix': self.matrix.tolist(),
            'frequency': self.frequency,
            'matrix_type': self.matrix_type,
            'units': self.units
        }


@dataclass
class AddedMassSet:
    """Frequency-dependent added mass matrices"""
    vessel_name: str
    analysis_tool: str
    water_depth: float

    matrices: List[HydrodynamicMatrix]  # One matrix per frequency
    frequencies: FrequencyData

    created_date: str
    source_file: Optional[str] = None
    notes: Optional[str] = None

    @classmethod
    def from_dict(cls, d: Dict) -> "AddedMassSet":
        return _matrix_set_from_dict(cls, d)

    def get_matrix_at_frequency(self, freq: float) -> Optional[HydrodynamicMatrix]:
        """Get added mass matrix at specific frequency"""
        for matrix in self.matrices:
            if np.isclose(matrix.frequency, freq):
                return matrix
        return None

    def to_dict(self) -> Dict:
        """Convert to dictionary"""
        return {
            'vessel_name': self.vessel_name,
            'analysis_tool': self.analysis_tool,
            'water_depth': self.water_depth,
            'created_date': self.created_date,
            'source_file': self.source_file,
            'notes': self.notes,
            'frequencies': {
                'values': self.frequencies.values.tolist(),
                'periods': self.frequencies.periods.tolist(),
                'count': self.frequencies.count
            },
            'matrices': [m.to_dict() for m in self.matrices]
        }


@dataclass
class DampingSet:
    """Frequency-dependent damping matrices"""
    vessel_name: str
    analysis_tool: str
    water_depth: float

    matrices: List[HydrodynamicMatrix]  # One matrix per frequency
    frequencies: FrequencyData

    created_date: str
    source_file: Optional[str] = None
    notes: Optional[str] = None

    @classmethod
    def from_dict(cls, d: Dict) -> "DampingSet":
        return _matrix_set_from_dict(cls, d)

    def get_matrix_at_frequency(self, freq: float) -> Optional[HydrodynamicMatrix]:
        """Get damping matrix at specific frequency"""
        for matrix in self.matrices:
            if np.isclose(matrix.frequency, freq):
                return matrix
        return None

    def to_dict(self) -> Dict:
        """Convert to dictionary"""
        return {
            'vessel_name': self.vessel_name,
            'analysis_tool': self.analysis_tool,
            'water_depth': self.water_depth,
            'created_date': self.created_date,
            'source_file': self.source_file,
            'notes': self.notes,
            'frequencies': {
                'values': self.frequencies.values.tolist(),
                'periods': self.frequencies.periods.tolist(),
                'count': self.frequencies.count
            },
            'matrices': [m.to_dict() for m in self.matrices]
        }


@dataclass
class HydrostaticResults:
    """Hydrostatic properties of a vessel."""
    vessel_name: str
    displacement_volume: float        # m^3
    mass: float                       # kg
    centre_of_gravity: List[float]    # [x, y, z] in meters
    centre_of_buoyancy: List[float]   # [x, y, z] in meters
    waterplane_area: float            # m^2
    stiffness_matrix: np.ndarray      # 6x6 matrix

    def to_dict(self) -> Dict:
        """Convert to dictionary for serialization"""
        return {
            'vessel_name': self.vessel_name,
            'displacement_volume': self.displacement_volume,
            'mass': self.mass,
            'centre_of_gravity': self.centre_of_gravity,
            'centre_of_buoyancy': self.centre_of_buoyancy,
            'waterplane_area': self.waterplane_area,
            'stiffness_matrix': self.stiffness_matrix.tolist()
        }

    @classmethod
    def from_dict(cls, d: Dict) -> "HydrostaticResults":
        return cls(
            vessel_name=d['vessel_name'],
            displacement_volume=float(d['displacement_volume']),
            mass=float(d['mass']),
            centre_of_gravity=list(d['centre_of_gravity']),
            centre_of_buoyancy=list(d['centre_of_buoyancy']),
            waterplane_area=float(d['waterplane_area']),
            stiffness_matrix=np.asarray(d['stiffness_matrix'], dtype=float),
        )


@dataclass
class DiffractionResults:
    """Complete diffraction analysis results"""
    vessel_name: str
    analysis_tool: str              # "AQWA" or "OrcaWave"
    water_depth: float

    # Main coefficient sets
    raos: RAOSet
    added_mass: AddedMassSet
    damping: DampingSet
    hydrostatics: Optional[HydrostaticResults] = None

    # Metadata
    created_date: str = ""
    analysis_date: Optional[str] = None
    source_files: Optional[List[str]] = None
    notes: Optional[str] = None
    phase_convention: str = "unknown"  # "orcina_lag" or "iso_lead"
    unit_system: str = "SI"            # "SI" (kg,m,s) or "orcaflex" (te,m,s)

    def to_dict(self) -> Dict:
        """Convert complete results to dictionary"""
        res = {
            'vessel_name': self.vessel_name,
            'analysis_tool': self.analysis_tool,
            'water_depth': self.water_depth,
            'created_date': self.created_date,
            'analysis_date': self.analysis_date,
            'source_files': self.source_files,
            'notes': self.notes,
            'phase_convention': self.phase_convention,
            'unit_system': self.unit_system,
            'raos': self.raos.to_dict(),
            'added_mass': self.added_mass.to_dict(),
            'damping': self.damping.to_dict()
        }
        if self.hydrostatics:
            res['hydrostatics'] = self.hydrostatics.to_dict()
        return res

    @classmethod
    def from_dict(cls, d: Dict) -> "DiffractionResults":
        """Reconstruct results from ``to_dict()`` output.

        License-free loader for Linux-side postprocessing: a licensed run on the
        host exports ``DiffractionResults.to_dict()`` as JSON, which this rebuilds
        into the full object (no OrcFxAPI, no heavy binaries). Round-trips
        ``to_dict()`` losslessly.
        """
        hydro = d.get('hydrostatics')
        return cls(
            vessel_name=d['vessel_name'],
            analysis_tool=d['analysis_tool'],
            water_depth=float(d['water_depth']),
            raos=RAOSet.from_dict(d['raos']),
            added_mass=AddedMassSet.from_dict(d['added_mass']),
            damping=DampingSet.from_dict(d['damping']),
            hydrostatics=HydrostaticResults.from_dict(hydro) if hydro else None,
            created_date=d.get('created_date', ''),
            analysis_date=d.get('analysis_date'),
            source_files=d.get('source_files'),
            notes=d.get('notes'),
            phase_convention=d.get('phase_convention', 'unknown'),
            unit_system=d.get('unit_system', 'SI'),
        )


# from_dict reconstruction helpers (reverse of the to_dict() serializers above) --

def _frequency_from_dict(d: Dict) -> "FrequencyData":
    values = np.asarray(d['values'], dtype=float)
    # __post_init__ recomputes count/min/max/periods from values.
    return FrequencyData(values=values, periods=None, count=0,
                         min_freq=0.0, max_freq=0.0,
                         unit=d.get('unit', Unit.FREQUENCY.value))


def _heading_from_dict(d: Dict) -> "HeadingData":
    values = np.asarray(d['values'], dtype=float)
    return HeadingData(values=values, count=0, min_heading=0.0, max_heading=0.0,
                       unit=d.get('unit', Unit.HEADING.value))


def _rao_component_from_dict(d: Dict) -> "RAOComponent":
    return RAOComponent(
        dof=DOF[d['dof']],
        magnitude=np.asarray(d['magnitude'], dtype=float),
        phase=np.asarray(d['phase'], dtype=float),
        frequencies=_frequency_from_dict(d['frequencies']),
        headings=_heading_from_dict(d['headings']),
        unit=d.get('unit', ''),  # __post_init__ resets from dof
    )


def _matrix_from_dict(d: Dict) -> "HydrodynamicMatrix":
    return HydrodynamicMatrix(
        matrix=np.asarray(d['matrix'], dtype=float),
        frequency=float(d['frequency']),
        matrix_type=d['matrix_type'],
        units=d.get('units', {}),
    )


def _matrix_set_from_dict(cls, d: Dict):
    """Shared from_dict for AddedMassSet / DampingSet (identical layout)."""
    return cls(
        vessel_name=d['vessel_name'],
        analysis_tool=d['analysis_tool'],
        water_depth=float(d['water_depth']),
        matrices=[_matrix_from_dict(m) for m in d['matrices']],
        frequencies=_frequency_from_dict(
            {'values': d['frequencies']['values'], 'unit': Unit.FREQUENCY.value}),
        created_date=d.get('created_date', ''),
        source_file=d.get('source_file'),
        notes=d.get('notes'),
    )


# Schema validation utilities

def validate_rao_completeness(rao_set: RAOSet) -> List[str]:
    """
    Validate RAO set for completeness

    Returns:
        List of validation warnings/errors
    """
    issues = []

    # Check all DOFs present
    for dof in DOF:
        component = rao_set.get_component(dof)
        if component is None:
            issues.append(f"Missing RAO component for {dof.name}")
            continue

        # Check data dimensions
        nfreq = component.frequencies.count
        nhead = component.headings.count

        if component.magnitude.shape != (nfreq, nhead):
            issues.append(
                f"{dof.name}: Magnitude shape {component.magnitude.shape} "
                f"doesn't match expected ({nfreq}, {nhead})"
            )

        if component.phase.shape != (nfreq, nhead):
            issues.append(
                f"{dof.name}: Phase shape {component.phase.shape} "
                f"doesn't match expected ({nfreq}, {nhead})"
            )

        # Check for NaN or Inf values
        if np.any(np.isnan(component.magnitude)) or np.any(np.isinf(component.magnitude)):
            issues.append(f"{dof.name}: Magnitude contains NaN or Inf values")

        if np.any(np.isnan(component.phase)) or np.any(np.isinf(component.phase)):
            issues.append(f"{dof.name}: Phase contains NaN or Inf values")

    # Check frequency range
    if rao_set.surge.frequencies.min_freq <= 0:
        issues.append(f"Invalid minimum frequency: {rao_set.surge.frequencies.min_freq}")

    return issues


def validate_matrix_set(matrix_set: Union[AddedMassSet, DampingSet]) -> List[str]:
    """
    Validate added mass or damping matrix set

    Returns:
        List of validation warnings/errors
    """
    issues = []

    # Check number of matrices matches frequencies
    if len(matrix_set.matrices) != matrix_set.frequencies.count:
        issues.append(
            f"Number of matrices ({len(matrix_set.matrices)}) doesn't match "
            f"frequency count ({matrix_set.frequencies.count})"
        )

    # Validate each matrix
    for i, matrix in enumerate(matrix_set.matrices):
        # Check symmetry (should be symmetric for physical validity)
        if not np.allclose(matrix.matrix, matrix.matrix.T, rtol=1e-3):
            issues.append(f"Matrix at frequency {matrix.frequency:.4f} is not symmetric")

        # Check for NaN or Inf
        if np.any(np.isnan(matrix.matrix)) or np.any(np.isinf(matrix.matrix)):
            issues.append(f"Matrix at frequency {matrix.frequency:.4f} contains NaN or Inf")

        # Check frequency matches expected
        expected_freq = matrix_set.frequencies.values[i]
        if not np.isclose(matrix.frequency, expected_freq):
            issues.append(
                f"Matrix {i}: frequency {matrix.frequency:.4f} doesn't match "
                f"expected {expected_freq:.4f}"
            )

    return issues


def validate_diffraction_results(results: DiffractionResults) -> Dict[str, List[str]]:
    """
    Comprehensive validation of diffraction results

    Returns:
        Dictionary with validation issues by category
    """
    validation_report = {
        'raos': validate_rao_completeness(results.raos),
        'added_mass': validate_matrix_set(results.added_mass),
        'damping': validate_matrix_set(results.damping),
        'consistency': []
    }

    # Cross-check consistency between data sets
    rao_freqs = results.raos.surge.frequencies.values
    am_freqs = results.added_mass.frequencies.values
    damp_freqs = results.damping.frequencies.values

    if not np.allclose(rao_freqs, am_freqs):
        validation_report['consistency'].append("RAO and added mass frequencies don't match")

    if not np.allclose(rao_freqs, damp_freqs):
        validation_report['consistency'].append("RAO and damping frequencies don't match")

    # Check water depth consistency
    if results.raos.water_depth != results.water_depth:
        validation_report['consistency'].append("RAO water depth doesn't match results water depth")

    return validation_report
