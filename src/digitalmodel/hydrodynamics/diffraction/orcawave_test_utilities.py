#!/usr/bin/env python3
"""
OrcaWave Test Utilities

ABOUTME: Mock data generators and test utilities for OrcaWave converter testing without requiring actual OrcaFlex installation.

Provides mock vessel objects, RAO data, and hydrodynamic matrices for testing
the OrcaWave converter in environments where OrcFxAPI is not available.

Version: 3.0.0 (Phase 3)
Status: Test utilities for converter validation
"""

import numpy as np
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass


@dataclass
class MockRAOEntry:
    """Mock OrcaFlex RAO table entry"""
    Period: float
    Direction: float
    LoadRAOSurge: complex
    LoadRAOSway: complex
    LoadRAOHeave: complex
    LoadRAORoll: complex
    LoadRAOPitch: complex
    LoadRAOYaw: complex


@dataclass
class MockRAOTable:
    """Mock OrcaFlex LoadRAOs table"""
    Size: int
    _entries: List[MockRAOEntry]

    def __getitem__(self, index: int) -> MockRAOEntry:
        return self._entries[index]


@dataclass
class MockAddedMassDampingEntry:
    """Mock OrcaFlex AddedMassAndDamping entry"""
    Period: float

    # Added mass diagonal terms
    AddedMassX: float
    AddedMassY: float
    AddedMassZ: float
    AddedMassRx: float
    AddedMassRy: float
    AddedMassRz: float

    # Added mass off-diagonal terms
    AddedMassXY: float
    AddedMassXZ: float
    AddedMassXRx: float
    AddedMassXRy: float
    AddedMassXRz: float
    AddedMassYZ: float
    AddedMassYRx: float
    AddedMassYRy: float
    AddedMassYRz: float
    AddedMassZRx: float
    AddedMassZRy: float
    AddedMassZRz: float
    AddedMassRxRy: float
    AddedMassRxRz: float
    AddedMassRyRz: float

    # Damping diagonal terms
    DampingX: float
    DampingY: float
    DampingZ: float
    DampingRx: float
    DampingRy: float
    DampingRz: float

    # Damping off-diagonal terms
    DampingXY: float
    DampingXZ: float
    DampingXRx: float
    DampingXRy: float
    DampingXRz: float
    DampingYZ: float
    DampingYRx: float
    DampingYRy: float
    DampingYRz: float
    DampingZRx: float
    DampingZRy: float
    DampingZRz: float
    DampingRxRy: float
    DampingRxRz: float
    DampingRyRz: float


@dataclass
class MockAddedMassDampingTable:
    """Mock OrcaFlex AddedMassAndDamping table"""
    Size: int
    _entries: List[MockAddedMassDampingEntry]

    def __getitem__(self, index: int) -> MockAddedMassDampingEntry:
        return self._entries[index]


@dataclass
class MockVesselType:
    """Mock OrcaFlex vessel type"""
    LoadRAOs: MockRAOTable
    AddedMassAndDamping: MockAddedMassDampingTable


@dataclass
class MockVessel:
    """Mock OrcaFlex vessel object"""
    Name: str
    VesselType: MockVesselType


class MockDataGenerator:
    """Generate realistic mock diffraction data for testing"""

    def __init__(
        self,
        vessel_name: str = "MockVessel",
        frequencies: Optional[np.ndarray] = None,
        headings: Optional[np.ndarray] = None
    ):
        """
        Initialize mock data generator

        Args:
            vessel_name: Name for mock vessel
            frequencies: Array of frequencies in rad/s (default: 17 frequencies)
            headings: Array of headings in degrees (default: 12 headings)
        """
        self.vessel_name = vessel_name

        # Default frequencies: 0.2 to 2.0 rad/s
        if frequencies is None:
            self.frequencies = np.linspace(0.2, 2.0, 17)
        else:
            self.frequencies = frequencies

        # Default headings: 0 to 330 degrees in 30 deg increments
        if headings is None:
            self.headings = np.arange(0, 360, 30)
        else:
            self.headings = headings

    def generate_rao_magnitude(
        self,
        dof: str,
        freq: float,
        heading: float
    ) -> float:
        """
        Generate realistic RAO magnitude for testing

        Args:
            dof: DOF name ('Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw')
            freq: Frequency in rad/s
            heading: Heading in degrees

        Returns:
            RAO magnitude
        """
        # Realistic RAO magnitude models
        period = 2.0 * np.pi / freq

        if dof == 'Surge':
            # Peak around 10-15 second period
            base = 1.2 * np.exp(-((period - 12.0)**2) / 20.0)
            heading_effect = 0.5 + 0.5 * np.cos(np.radians(heading))
            return base * heading_effect

        elif dof == 'Sway':
            # Peak around 8-12 second period
            base = 1.0 * np.exp(-((period - 10.0)**2) / 15.0)
            heading_effect = np.abs(np.sin(np.radians(heading)))
            return base * heading_effect

        elif dof == 'Heave':
            # Relatively constant, small peak
            base = 0.8 + 0.3 * np.exp(-((period - 10.0)**2) / 25.0)
            return base

        elif dof == 'Roll':
            # Peak around 12-16 second period
            base = 20.0 * np.exp(-((period - 14.0)**2) / 30.0)  # deg/m
            heading_effect = np.abs(np.sin(np.radians(heading)))
            return base * heading_effect

        elif dof == 'Pitch':
            # Peak around 8-12 second period
            base = 15.0 * np.exp(-((period - 10.0)**2) / 20.0)  # deg/m
            heading_effect = 0.5 + 0.5 * np.cos(np.radians(heading))
            return base * heading_effect

        elif dof == 'Yaw':
            # Smaller magnitude
            base = 8.0 * np.exp(-((period - 11.0)**2) / 25.0)  # deg/m
            heading_effect = 0.3 + 0.7 * np.abs(np.sin(np.radians(heading)))
            return base * heading_effect

        return 0.0

    def generate_rao_phase(
        self,
        dof: str,
        freq: float,
        heading: float
    ) -> float:
        """
        Generate realistic RAO phase for testing

        Returns:
            Phase in degrees
        """
        # Simple phase model - typically between -180 and 180
        period = 2.0 * np.pi / freq

        # Phase varies with period and heading
        base_phase = -90.0 + 30.0 * np.sin(period / 5.0)
        heading_variation = 20.0 * np.sin(np.radians(heading / 2.0))

        phase = base_phase + heading_variation

        # Keep in -180 to 180 range
        while phase > 180:
            phase -= 360
        while phase < -180:
            phase += 360

        return phase

    def generate_added_mass_matrix(self, freq: float) -> np.ndarray:
        """
        Generate realistic 6x6 added mass matrix

        Args:
            freq: Frequency in rad/s

        Returns:
            6x6 added mass matrix
        """
        matrix = np.zeros((6, 6))

        # Frequency-dependent scaling
        freq_factor = 1.0 + 0.1 * freq

        # Diagonal terms (typical FPSO values)
        matrix[0, 0] = 5e6 * freq_factor    # Surge (kg)
        matrix[1, 1] = 6e6 * freq_factor    # Sway (kg)
        matrix[2, 2] = 8e6 * freq_factor    # Heave (kg)
        matrix[3, 3] = 5e9 * freq_factor    # Roll (kg·m²)
        matrix[4, 4] = 8e10 * freq_factor   # Pitch (kg·m²)
        matrix[5, 5] = 8e10 * freq_factor   # Yaw (kg·m²)

        # Off-diagonal coupling terms (smaller)
        matrix[0, 1] = matrix[1, 0] = 1e5 * freq_factor   # Surge-Sway
        matrix[0, 4] = matrix[4, 0] = -5e7 * freq_factor  # Surge-Pitch
        matrix[1, 3] = matrix[3, 1] = -3e7 * freq_factor  # Sway-Roll
        matrix[2, 4] = matrix[4, 2] = 2e7 * freq_factor   # Heave-Pitch

        return matrix

    def generate_damping_matrix(self, freq: float) -> np.ndarray:
        """
        Generate realistic 6x6 damping matrix

        Args:
            freq: Frequency in rad/s

        Returns:
            6x6 damping matrix
        """
        matrix = np.zeros((6, 6))

        # Damping increases with frequency
        freq_factor = freq

        # Diagonal terms (typical FPSO values)
        matrix[0, 0] = 2e5 * freq_factor    # Surge (N·s/m)
        matrix[1, 1] = 2.5e5 * freq_factor  # Sway (N·s/m)
        matrix[2, 2] = 3e5 * freq_factor    # Heave (N·s/m)
        matrix[3, 3] = 1e8 * freq_factor    # Roll (N·m·s/rad)
        matrix[4, 4] = 2e9 * freq_factor    # Pitch (N·m·s/rad)
        matrix[5, 5] = 2e9 * freq_factor    # Yaw (N·m·s/rad)

        # Off-diagonal coupling terms (smaller)
        matrix[0, 1] = matrix[1, 0] = 5e3 * freq_factor    # Surge-Sway
        matrix[0, 4] = matrix[4, 0] = -2e6 * freq_factor   # Surge-Pitch
        matrix[1, 3] = matrix[3, 1] = -1e6 * freq_factor   # Sway-Roll
        matrix[2, 4] = matrix[4, 2] = 5e5 * freq_factor    # Heave-Pitch

        return matrix

    def create_mock_vessel(self) -> MockVessel:
        """
        Create complete mock vessel with RAO and matrix data

        Returns:
            Mock vessel object compatible with OrcaWaveDataExtractor
        """
        # Generate RAO table entries
        rao_entries = []

        for freq in self.frequencies:
            period = 2.0 * np.pi / freq

            for heading in self.headings:
                # Generate RAO values for all DOFs
                raos = {}
                for dof in ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']:
                    mag = self.generate_rao_magnitude(dof, freq, heading)
                    phase = self.generate_rao_phase(dof, freq, heading)

                    # Convert to complex number
                    raos[f'LoadRAO{dof}'] = mag * np.exp(1j * np.radians(phase))

                entry = MockRAOEntry(
                    Period=period,
                    Direction=heading,
                    LoadRAOSurge=raos['LoadRAOSurge'],
                    LoadRAOSway=raos['LoadRAOSway'],
                    LoadRAOHeave=raos['LoadRAOHeave'],
                    LoadRAORoll=raos['LoadRAORoll'],
                    LoadRAOPitch=raos['LoadRAOPitch'],
                    LoadRAOYaw=raos['LoadRAOYaw']
                )
                rao_entries.append(entry)

        rao_table = MockRAOTable(
            Size=len(rao_entries),
            _entries=rao_entries
        )

        # Generate added mass and damping entries
        am_damp_entries = []

        for freq in self.frequencies:
            period = 2.0 * np.pi / freq

            # Generate matrices
            am_matrix = self.generate_added_mass_matrix(freq)
            damp_matrix = self.generate_damping_matrix(freq)

            entry = MockAddedMassDampingEntry(
                Period=period,
                # Added mass
                AddedMassX=am_matrix[0, 0],
                AddedMassY=am_matrix[1, 1],
                AddedMassZ=am_matrix[2, 2],
                AddedMassRx=am_matrix[3, 3],
                AddedMassRy=am_matrix[4, 4],
                AddedMassRz=am_matrix[5, 5],
                AddedMassXY=am_matrix[0, 1],
                AddedMassXZ=am_matrix[0, 2],
                AddedMassXRx=am_matrix[0, 3],
                AddedMassXRy=am_matrix[0, 4],
                AddedMassXRz=am_matrix[0, 5],
                AddedMassYZ=am_matrix[1, 2],
                AddedMassYRx=am_matrix[1, 3],
                AddedMassYRy=am_matrix[1, 4],
                AddedMassYRz=am_matrix[1, 5],
                AddedMassZRx=am_matrix[2, 3],
                AddedMassZRy=am_matrix[2, 4],
                AddedMassZRz=am_matrix[2, 5],
                AddedMassRxRy=am_matrix[3, 4],
                AddedMassRxRz=am_matrix[3, 5],
                AddedMassRyRz=am_matrix[4, 5],
                # Damping
                DampingX=damp_matrix[0, 0],
                DampingY=damp_matrix[1, 1],
                DampingZ=damp_matrix[2, 2],
                DampingRx=damp_matrix[3, 3],
                DampingRy=damp_matrix[4, 4],
                DampingRz=damp_matrix[5, 5],
                DampingXY=damp_matrix[0, 1],
                DampingXZ=damp_matrix[0, 2],
                DampingXRx=damp_matrix[0, 3],
                DampingXRy=damp_matrix[0, 4],
                DampingXRz=damp_matrix[0, 5],
                DampingYZ=damp_matrix[1, 2],
                DampingYRx=damp_matrix[1, 3],
                DampingYRy=damp_matrix[1, 4],
                DampingYRz=damp_matrix[1, 5],
                DampingZRx=damp_matrix[2, 3],
                DampingZRy=damp_matrix[2, 4],
                DampingZRz=damp_matrix[2, 5],
                DampingRxRy=damp_matrix[3, 4],
                DampingRxRz=damp_matrix[3, 5],
                DampingRyRz=damp_matrix[4, 5]
            )
            am_damp_entries.append(entry)

        am_damp_table = MockAddedMassDampingTable(
            Size=len(am_damp_entries),
            _entries=am_damp_entries
        )

        # Create vessel type
        vessel_type = MockVesselType(
            LoadRAOs=rao_table,
            AddedMassAndDamping=am_damp_table
        )

        # Create vessel
        vessel = MockVessel(
            Name=self.vessel_name,
            VesselType=vessel_type
        )

        return vessel


def create_test_vessel(
    vessel_name: str = "TestFPSO",
    nfreq: int = 17,
    nhead: int = 12
) -> MockVessel:
    """
    Quick helper to create a test vessel with default parameters

    Args:
        vessel_name: Name for test vessel
        nfreq: Number of frequencies
        nhead: Number of headings

    Returns:
        Mock vessel object
    """
    frequencies = np.linspace(0.2, 2.0, nfreq)
    headings = np.linspace(0, 330, nhead)

    generator = MockDataGenerator(
        vessel_name=vessel_name,
        frequencies=frequencies,
        headings=headings
    )

    return generator.create_mock_vessel()
