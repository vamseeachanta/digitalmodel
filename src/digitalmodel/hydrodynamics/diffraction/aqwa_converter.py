#!/usr/bin/env python3
"""
AQWA Results Converter

Reads AQWA analysis outputs (.LIS files, result files) and converts to
unified diffraction output schema.
"""

import numpy as np
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from datetime import datetime
from loguru import logger

from digitalmodel.diffraction.output_schemas import (
    DiffractionResults, RAOSet, AddedMassSet, DampingSet,
    RAOComponent, HydrodynamicMatrix,
    FrequencyData, HeadingData, DOF
)
from digitalmodel.diffraction.aqwa_lis_parser import parse_aqwa_lis_file


class AQWAConverter:
    """Convert AQWA results to unified schema"""

    def __init__(self, analysis_folder: Path, vessel_name: str):
        """
        Initialize AQWA converter

        Args:
            analysis_folder: Path to AQWA analysis folder
            vessel_name: Name of vessel/structure
        """
        self.analysis_folder = Path(analysis_folder)
        self.vessel_name = vessel_name

        # AQWA output file patterns - try multiple possible filenames
        possible_files = [
            self.analysis_folder / f"{vessel_name}.LIS",
            self.analysis_folder / f"{vessel_name.upper()}.LIS",
            self.analysis_folder / f"{vessel_name.lower()}.LIS",
        ]

        # Find first .LIS file that exists, or use first .LIS file in folder
        self.lis_file = None
        for file_path in possible_files:
            if file_path.exists():
                self.lis_file = file_path
                break

        # If no exact match, find any .LIS file in the folder
        if self.lis_file is None:
            lis_files = list(self.analysis_folder.glob("*.LIS"))
            if lis_files:
                self.lis_file = lis_files[0]
                logger.warning(f"Using first .LIS file found: {self.lis_file.name}")
            else:
                raise FileNotFoundError(f"No .LIS file found in {self.analysis_folder}")

        logger.info(f"Using AQWA .LIS file: {self.lis_file}")

    def convert_to_unified_schema(
        self,
        water_depth: float,
        rao_data: Optional[Dict] = None,
        added_mass_data: Optional[Dict] = None,
        damping_data: Optional[Dict] = None
    ) -> DiffractionResults:
        """
        Convert AQWA results to unified diffraction schema

        Args:
            water_depth: Water depth in meters
            rao_data: Pre-extracted RAO data (if available)
            added_mass_data: Pre-extracted added mass data
            damping_data: Pre-extracted damping data

        Returns:
            Unified diffraction results
        """

        # Extract data if not provided
        if rao_data is None:
            rao_data = self._extract_rao_data()

        if added_mass_data is None:
            added_mass_data = self._extract_added_mass_data()

        if damping_data is None:
            damping_data = self._extract_damping_data()

        # Build unified schemas
        raos = self._build_rao_set(rao_data, water_depth)
        added_mass = self._build_added_mass_set(added_mass_data, water_depth)
        damping = self._build_damping_set(damping_data, water_depth)

        # Create complete results
        results = DiffractionResults(
            vessel_name=self.vessel_name,
            analysis_tool="AQWA",
            water_depth=water_depth,
            raos=raos,
            added_mass=added_mass,
            damping=damping,
            created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            analysis_date=self._get_analysis_date(),
            source_files=[str(self.lis_file)],
            notes="Converted from AQWA analysis results"
        )

        return results

    def _extract_rao_data(self) -> Dict:
        """
        Extract RAO data from AQWA .LIS file

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
        logger.info(f"Extracting RAO data from: {self.lis_file}")

        # Parse the .LIS file
        parsed_data = parse_aqwa_lis_file(self.lis_file)

        frequencies = np.array(parsed_data['frequencies'])
        headings = np.array(parsed_data['headings'])
        raos = parsed_data['raos']

        # Organize RAO data into 2D arrays: [freq_idx, heading_idx]
        n_freqs = len(frequencies)
        n_headings = len(headings)

        # Initialize arrays for each DOF
        dof_data = {
            'surge': {'magnitude': np.zeros((n_freqs, n_headings)), 'phase': np.zeros((n_freqs, n_headings))},
            'sway': {'magnitude': np.zeros((n_freqs, n_headings)), 'phase': np.zeros((n_freqs, n_headings))},
            'heave': {'magnitude': np.zeros((n_freqs, n_headings)), 'phase': np.zeros((n_freqs, n_headings))},
            'roll': {'magnitude': np.zeros((n_freqs, n_headings)), 'phase': np.zeros((n_freqs, n_headings))},
            'pitch': {'magnitude': np.zeros((n_freqs, n_headings)), 'phase': np.zeros((n_freqs, n_headings))},
            'yaw': {'magnitude': np.zeros((n_freqs, n_headings)), 'phase': np.zeros((n_freqs, n_headings))},
        }

        # Fill in the data from parsed RAOs
        for (freq, heading), rao_values in raos.items():
            # Find indices
            freq_idx = np.where(np.isclose(frequencies, freq))[0]
            heading_idx = np.where(np.isclose(headings, heading))[0]

            if len(freq_idx) > 0 and len(heading_idx) > 0:
                fi = freq_idx[0]
                hi = heading_idx[0]

                # Fill in each DOF
                for dof_name in dof_data.keys():
                    amp, phase = rao_values[dof_name]
                    dof_data[dof_name]['magnitude'][fi, hi] = amp
                    dof_data[dof_name]['phase'][fi, hi] = phase

        rao_data = {
            'frequencies': frequencies,
            'headings': headings,
            **dof_data
        }

        logger.success(f"Extracted RAO data: {n_freqs} frequencies x {n_headings} headings")

        return rao_data

    def _extract_added_mass_data(self) -> Dict:
        """
        Extract added mass matrices from AQWA results

        Returns:
            Dictionary with structure:
            {
                'frequencies': ndarray,
                'matrices': List of 6x6 ndarrays
            }
        """
        logger.info(f"Extracting added mass data from AQWA .LIS file")

        # Parse the .LIS file
        parsed_data = parse_aqwa_lis_file(self.lis_file)

        frequencies = np.array(parsed_data['frequencies'])
        added_mass_matrices = parsed_data['added_mass']

        # Convert dictionary to sorted list of matrices
        matrices = []
        for freq in frequencies:
            if freq in added_mass_matrices:
                matrices.append(added_mass_matrices[freq])
            else:
                logger.warning(f"Added mass matrix missing for frequency {freq:.3f} rad/s")
                matrices.append(np.zeros((6, 6)))

        added_mass_data = {
            'frequencies': frequencies,
            'matrices': matrices
        }

        logger.success(f"Extracted {len(matrices)} added mass matrices")

        return added_mass_data

    def _extract_damping_data(self) -> Dict:
        """
        Extract damping matrices from AQWA results

        Returns:
            Dictionary with damping matrix data
        """
        logger.info(f"Extracting damping data from AQWA .LIS file")

        # Parse the .LIS file
        parsed_data = parse_aqwa_lis_file(self.lis_file)

        frequencies = np.array(parsed_data['frequencies'])
        damping_matrices = parsed_data['damping']

        # Convert dictionary to sorted list of matrices
        matrices = []
        for freq in frequencies:
            if freq in damping_matrices:
                matrices.append(damping_matrices[freq])
            else:
                logger.warning(f"Damping matrix missing for frequency {freq:.3f} rad/s")
                matrices.append(np.zeros((6, 6)))

        damping_data = {
            'frequencies': frequencies,
            'matrices': matrices
        }

        logger.success(f"Extracted {len(matrices)} damping matrices")

        return damping_data

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
            analysis_tool="AQWA",
            water_depth=water_depth,
            surge=dof_components.get('surge'),
            sway=dof_components.get('sway'),
            heave=dof_components.get('heave'),
            roll=dof_components.get('roll'),
            pitch=dof_components.get('pitch'),
            yaw=dof_components.get('yaw'),
            created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            source_file=str(self.lis_file)
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
            analysis_tool="AQWA",
            water_depth=water_depth,
            matrices=matrices,
            frequencies=frequencies,
            created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            source_file=str(self.lis_file)
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
            analysis_tool="AQWA",
            water_depth=water_depth,
            matrices=matrices,
            frequencies=frequencies,
            created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            source_file=str(self.lis_file)
        )

        return damp_set

    def _get_analysis_date(self) -> Optional[str]:
        """Extract analysis date from AQWA files if available"""
        # TODO: Parse from .LIS file header
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

def convert_aqwa_results(
    analysis_folder: str,
    vessel_name: str,
    water_depth: float,
    output_folder: str
) -> Path:
    """
    Convert AQWA results and export to OrcaFlex format

    Args:
        analysis_folder: Path to AQWA analysis folder
        vessel_name: Vessel name
        water_depth: Water depth in meters
        output_folder: Output directory for OrcaFlex files

    Returns:
        Path to output directory
    """
    from digitalmodel.diffraction.orcaflex_exporter import OrcaFlexExporter

    # Convert to unified schema
    converter = AQWAConverter(Path(analysis_folder), vessel_name)
    results = converter.convert_to_unified_schema(water_depth)

    # Export to OrcaFlex format
    exporter = OrcaFlexExporter(results, Path(output_folder))
    outputs = exporter.export_all()

    print(f"\nConversion complete!")
    print(f"Output files in: {output_folder}")
    for output_type, output_path in outputs.items():
        print(f"  - {output_type}: {output_path.name}")

    return Path(output_folder)
