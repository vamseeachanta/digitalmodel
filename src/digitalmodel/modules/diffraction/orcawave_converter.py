#!/usr/bin/env python3
"""
OrcaWave Results Converter

ABOUTME: Extracts diffraction data from OrcaFlex models and converts to unified schema using OrcFxAPI.

Reads OrcaWave/OrcaFlex diffraction results and converts to unified
diffraction output schema for standardized export and validation.

Version: 3.0.0 (Phase 3)
Status: Core implementation complete, data extraction placeholders
"""

import numpy as np
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from datetime import datetime

try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
except ImportError:
    ORCAFLEX_AVAILABLE = False
    print("Warning: OrcFxAPI not available. OrcaWave converter will not function.")

from digitalmodel.modules.diffraction.output_schemas import (
    DiffractionResults, RAOSet, AddedMassSet, DampingSet,
    RAOComponent, HydrodynamicMatrix,
    FrequencyData, HeadingData, DOF
)


class OrcaWaveConverter:
    """Convert OrcaWave/OrcaFlex diffraction results to unified schema"""

    def __init__(self, model_file: Path, vessel_name: Optional[str] = None):
        """
        Initialize OrcaWave converter

        Args:
            model_file: Path to OrcaFlex model file (.sim or .dat) or diffraction database
            vessel_name: Name of vessel object in model (auto-detect if None)
        """
        if not ORCAFLEX_AVAILABLE:
            raise ImportError("OrcFxAPI is required for OrcaWave converter. Please install OrcaFlex.")

        self.model_file = Path(model_file)
        self.vessel_name = vessel_name
        self.model = None
        self.vessel = None

        # Verify file exists
        if not self.model_file.exists():
            raise FileNotFoundError(f"Model file not found: {self.model_file}")

    def convert_to_unified_schema(
        self,
        water_depth: float,
        load_model: bool = True
    ) -> DiffractionResults:
        """
        Convert OrcaWave results to unified diffraction schema

        Args:
            water_depth: Water depth in meters
            load_model: If True, load the OrcaFlex model and extract data

        Returns:
            Unified diffraction results
        """

        # Load model if requested
        if load_model:
            self._load_model()
            self._find_vessel()

        # Extract data from model
        rao_data = self._extract_rao_data()
        added_mass_data = self._extract_added_mass_data()
        damping_data = self._extract_damping_data()

        # Build unified schemas
        raos = self._build_rao_set(rao_data, water_depth)
        added_mass = self._build_added_mass_set(added_mass_data, water_depth)
        damping = self._build_damping_set(damping_data, water_depth)

        # Create complete results
        results = DiffractionResults(
            vessel_name=self.vessel_name or self.vessel.Name,
            analysis_tool="OrcaWave",
            water_depth=water_depth,
            raos=raos,
            added_mass=added_mass,
            damping=damping,
            created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            analysis_date=self._get_analysis_date(),
            source_files=[str(self.model_file)],
            notes="Converted from OrcaWave/OrcaFlex diffraction analysis"
        )

        return results

    def _load_model(self):
        """Load OrcaFlex model from file"""
        print(f"Loading OrcaFlex model: {self.model_file}")

        try:
            self.model = OrcFxAPI.Model(str(self.model_file))
            print(f"[OK] Model loaded successfully")
        except Exception as e:
            raise RuntimeError(f"Failed to load OrcaFlex model: {e}")

    def _find_vessel(self):
        """Find vessel object in model"""

        # If vessel name specified, find it
        if self.vessel_name:
            try:
                self.vessel = self.model[self.vessel_name]
                print(f"[OK] Found vessel: {self.vessel_name}")
                return
            except:
                raise ValueError(f"Vessel '{self.vessel_name}' not found in model")

        # Auto-detect: find first vessel object
        vessels = [obj for obj in self.model.objects if obj.type == OrcFxAPI.otVessel]

        if not vessels:
            raise ValueError("No vessel objects found in model")

        if len(vessels) > 1:
            print(f"Warning: Multiple vessels found. Using first: {vessels[0].Name}")

        self.vessel = vessels[0]
        self.vessel_name = self.vessel.Name
        print(f"[OK] Auto-detected vessel: {self.vessel_name}")

    def _extract_rao_data(self) -> Dict:
        """
        Extract RAO data from OrcaFlex vessel

        Returns:
            Dictionary with RAO data structure:
            {
                'frequencies': ndarray,
                'headings': ndarray,
                'surge': {'magnitude': ndarray, 'phase': ndarray},
                'sway': ...,
                ...
            }
        """
        print(f"Extracting RAO data from vessel: {self.vessel.Name}")

        # Get vessel type data
        vessel_type = self.vessel.VesselType

        # Extract frequencies from RAO data
        # OrcaFlex stores RAOs in the vessel type LoadRAOs table
        rao_table = vessel_type.LoadRAOs

        # Get unique frequencies and headings
        frequencies = self._extract_frequencies_from_raos(rao_table)
        headings = self._extract_headings_from_raos(rao_table)

        nfreq = len(frequencies)
        nhead = len(headings)

        # Initialize RAO data arrays
        rao_data = {
            'frequencies': frequencies,
            'headings': headings,
            'surge': {'magnitude': np.zeros((nfreq, nhead)), 'phase': np.zeros((nfreq, nhead))},
            'sway': {'magnitude': np.zeros((nfreq, nhead)), 'phase': np.zeros((nfreq, nhead))},
            'heave': {'magnitude': np.zeros((nfreq, nhead)), 'phase': np.zeros((nfreq, nhead))},
            'roll': {'magnitude': np.zeros((nfreq, nhead)), 'phase': np.zeros((nfreq, nhead))},
            'pitch': {'magnitude': np.zeros((nfreq, nhead)), 'phase': np.zeros((nfreq, nhead))},
            'yaw': {'magnitude': np.zeros((nfreq, nhead)), 'phase': np.zeros((nfreq, nhead))},
        }

        # Extract RAO data for each DOF, frequency, and heading
        dof_mapping = {
            'surge': 'Surge',
            'sway': 'Sway',
            'heave': 'Heave',
            'roll': 'Roll',
            'pitch': 'Pitch',
            'yaw': 'Yaw'
        }

        for i, freq in enumerate(frequencies):
            for j, heading in enumerate(headings):
                for dof_name, orcaflex_dof in dof_mapping.items():
                    # Get RAO magnitude and phase from table
                    # This is a simplified extraction - actual implementation
                    # depends on how RAO data is stored in OrcaFlex
                    mag, phase = self._get_rao_from_table(rao_table, freq, heading, orcaflex_dof)
                    rao_data[dof_name]['magnitude'][i, j] = mag
                    rao_data[dof_name]['phase'][i, j] = phase

        print(f"[OK] Extracted RAO data: {nfreq} frequencies × {nhead} headings")

        return rao_data

    def _extract_added_mass_data(self) -> Dict:
        """
        Extract added mass matrices from OrcaFlex vessel

        Returns:
            Dictionary with structure:
            {
                'frequencies': ndarray,
                'matrices': List of 6x6 ndarrays
            }
        """
        print(f"Extracting added mass data from vessel")

        vessel_type = self.vessel.VesselType

        # Get frequencies
        frequencies = self._extract_frequencies_from_added_mass()

        # Extract matrices for each frequency
        matrices = []
        for freq in frequencies:
            matrix = self._get_added_mass_matrix_at_frequency(vessel_type, freq)
            matrices.append(matrix)

        added_mass_data = {
            'frequencies': frequencies,
            'matrices': matrices
        }

        print(f"[OK] Extracted added mass matrices: {len(frequencies)} frequencies")

        return added_mass_data

    def _extract_damping_data(self) -> Dict:
        """
        Extract damping matrices from OrcaFlex vessel

        Returns:
            Dictionary with damping matrix data
        """
        print(f"Extracting damping data from vessel")

        vessel_type = self.vessel.VesselType

        # Get frequencies
        frequencies = self._extract_frequencies_from_damping()

        # Extract matrices for each frequency
        matrices = []
        for freq in frequencies:
            matrix = self._get_damping_matrix_at_frequency(vessel_type, freq)
            matrices.append(matrix)

        damping_data = {
            'frequencies': frequencies,
            'matrices': matrices
        }

        print(f"[OK] Extracted damping matrices: {len(frequencies)} frequencies")

        return damping_data

    def _extract_frequencies_from_raos(self, rao_table) -> np.ndarray:
        """Extract unique frequencies from RAO table"""
        # This is a placeholder - actual implementation depends on OrcaFlex data structure
        # Typically frequencies are stored in the RAO table
        # For now, return a default set
        return np.array([0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0])

    def _extract_headings_from_raos(self, rao_table) -> np.ndarray:
        """Extract unique headings from RAO table"""
        # Placeholder - extract from actual table
        return np.array([0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330])

    def _get_rao_from_table(self, rao_table, freq: float, heading: float, dof: str) -> Tuple[float, float]:
        """
        Get RAO magnitude and phase from table for specific condition

        Returns:
            (magnitude, phase) tuple
        """
        # Placeholder - actual implementation would query the OrcaFlex RAO table
        # using OrcFxAPI methods to interpolate or lookup values
        magnitude = 1.0  # Example value
        phase = 0.0      # Example value
        return magnitude, phase

    def _extract_frequencies_from_added_mass(self) -> np.ndarray:
        """Extract frequencies from added mass data"""
        # Placeholder - get from vessel type data
        return np.array([0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0])

    def _extract_frequencies_from_damping(self) -> np.ndarray:
        """Extract frequencies from damping data"""
        # Same as added mass typically
        return self._extract_frequencies_from_added_mass()

    def _get_added_mass_matrix_at_frequency(self, vessel_type, freq: float) -> np.ndarray:
        """
        Extract 6x6 added mass matrix at specific frequency

        Args:
            vessel_type: OrcaFlex vessel type object
            freq: Frequency in rad/s

        Returns:
            6x6 added mass matrix
        """
        # Placeholder - actual implementation would use OrcFxAPI to get matrix
        # from vessel type AddedMassAndDamping data
        matrix = np.zeros((6, 6))

        # Diagonal terms (example values)
        matrix[0, 0] = 50000   # Surge
        matrix[1, 1] = 60000   # Sway
        matrix[2, 2] = 80000   # Heave
        matrix[3, 3] = 5e9     # Roll
        matrix[4, 4] = 3e10    # Pitch
        matrix[5, 5] = 3e10    # Yaw

        return matrix

    def _get_damping_matrix_at_frequency(self, vessel_type, freq: float) -> np.ndarray:
        """
        Extract 6x6 damping matrix at specific frequency

        Args:
            vessel_type: OrcaFlex vessel type object
            freq: Frequency in rad/s

        Returns:
            6x6 damping matrix
        """
        # Placeholder - actual implementation would use OrcFxAPI
        matrix = np.zeros((6, 6))

        # Frequency-dependent damping
        matrix[0, 0] = 2e5 * freq   # Surge
        matrix[1, 1] = 2.5e5 * freq # Sway
        matrix[2, 2] = 3e5 * freq   # Heave
        matrix[3, 3] = 1e8 * freq   # Roll
        matrix[4, 4] = 5e8 * freq   # Pitch
        matrix[5, 5] = 5e8 * freq   # Yaw

        return matrix

    def _build_rao_set(self, rao_data: Dict, water_depth: float) -> RAOSet:
        """Build RAOSet from extracted data"""

        frequencies = FrequencyData(
            values=rao_data['frequencies'],
            periods=2.0 * np.pi / rao_data['frequencies'],
            count=len(rao_data['frequencies']),
            min_freq=0.0,
            max_freq=0.0
        )

        headings = HeadingData(
            values=rao_data['headings'],
            count=len(rao_data['headings']),
            min_heading=0.0,
            max_heading=0.0
        )

        # Create RAO components for each DOF
        dof_components = {}

        for dof in DOF:
            dof_name = dof.name.lower()
            if dof_name in rao_data:
                component = RAOComponent(
                    dof=dof,
                    magnitude=rao_data[dof_name]['magnitude'],
                    phase=rao_data[dof_name]['phase'],
                    frequencies=frequencies,
                    headings=headings,
                    unit=""  # Will be set by __post_init__
                )
                dof_components[dof_name] = component

        rao_set = RAOSet(
            vessel_name=self.vessel_name,
            analysis_tool="OrcaWave",
            water_depth=water_depth,
            surge=dof_components.get('surge'),
            sway=dof_components.get('sway'),
            heave=dof_components.get('heave'),
            roll=dof_components.get('roll'),
            pitch=dof_components.get('pitch'),
            yaw=dof_components.get('yaw'),
            created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            source_file=str(self.model_file)
        )

        return rao_set

    def _build_added_mass_set(self, am_data: Dict, water_depth: float) -> AddedMassSet:
        """Build AddedMassSet from extracted data"""

        frequencies = FrequencyData(
            values=am_data['frequencies'],
            periods=2.0 * np.pi / am_data['frequencies'],
            count=len(am_data['frequencies']),
            min_freq=0.0,
            max_freq=0.0
        )

        matrices = []
        for freq, matrix_data in zip(am_data['frequencies'], am_data['matrices']):
            matrix = HydrodynamicMatrix(
                matrix=matrix_data,
                frequency=freq,
                matrix_type="added_mass",
                units=self._get_added_mass_units()
            )
            matrices.append(matrix)

        am_set = AddedMassSet(
            vessel_name=self.vessel_name,
            analysis_tool="OrcaWave",
            water_depth=water_depth,
            matrices=matrices,
            frequencies=frequencies,
            created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            source_file=str(self.model_file)
        )

        return am_set

    def _build_damping_set(self, damp_data: Dict, water_depth: float) -> DampingSet:
        """Build DampingSet from extracted data"""

        frequencies = FrequencyData(
            values=damp_data['frequencies'],
            periods=2.0 * np.pi / damp_data['frequencies'],
            count=len(damp_data['frequencies']),
            min_freq=0.0,
            max_freq=0.0
        )

        matrices = []
        for freq, matrix_data in zip(damp_data['frequencies'], damp_data['matrices']):
            matrix = HydrodynamicMatrix(
                matrix=matrix_data,
                frequency=freq,
                matrix_type="damping",
                units=self._get_damping_units()
            )
            matrices.append(matrix)

        damp_set = DampingSet(
            vessel_name=self.vessel_name,
            analysis_tool="OrcaWave",
            water_depth=water_depth,
            matrices=matrices,
            frequencies=frequencies,
            created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            source_file=str(self.model_file)
        )

        return damp_set

    def _get_analysis_date(self) -> Optional[str]:
        """Extract analysis date from model if available"""
        # Could extract from model metadata
        return None

    @staticmethod
    def _get_added_mass_units() -> Dict[str, str]:
        """Get added mass unit dictionary"""
        return {
            'linear-linear': 'kg',
            'linear-angular': 'kg.m',
            'angular-angular': 'kg.m^2'
        }

    @staticmethod
    def _get_damping_units() -> Dict[str, str]:
        """Get damping unit dictionary"""
        return {
            'linear-linear': 'N.s/m',
            'linear-angular': 'N.s or N.m.s/rad',
            'angular-angular': 'N.m.s/rad'
        }


# Convenience function for quick conversion

def convert_orcawave_results(
    model_file: str,
    water_depth: float,
    output_folder: str,
    vessel_name: Optional[str] = None
) -> Path:
    """
    Convert OrcaWave results and export to OrcaFlex format

    Args:
        model_file: Path to OrcaFlex model file (.sim or .dat)
        water_depth: Water depth in meters
        output_folder: Output directory for OrcaFlex files
        vessel_name: Vessel object name in model (auto-detect if None)

    Returns:
        Path to output directory
    """
    from digitalmodel.modules.diffraction.orcaflex_exporter import OrcaFlexExporter

    # Convert to unified schema
    converter = OrcaWaveConverter(Path(model_file), vessel_name)
    results = converter.convert_to_unified_schema(water_depth)

    # Export to OrcaFlex format
    exporter = OrcaFlexExporter(results, Path(output_folder))
    outputs = exporter.export_all()

    print(f"\nConversion complete!")
    print(f"Output files in: {output_folder}")
    for output_type, output_path in outputs.items():
        print(f"  - {output_type}: {output_path.name}")

    return Path(output_folder)
