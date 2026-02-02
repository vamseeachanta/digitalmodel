"""OrcaFlex YAML file parser for RAO data extraction.

This parser extracts displacement RAOs from OrcaFlex YAML files.
Structure: VesselTypes > Vessel Type1 > Draughts > Draught1 > DisplacementRAOs
"""

import yaml
from typing import Dict, List, Tuple, Optional, Set, Any
from pathlib import Path
import numpy as np

from ..models.rao_data import (
    UnifiedRAOData,
    DisplacementRAO,
    DOFData,
    RAOMetadata,
    SourceFormat,
    create_empty_rao_arrays
)


class OrcaFlexParseError(Exception):
    """Exception raised for OrcaFlex file parsing errors."""
    pass


class OrcaFlexYMLParser:
    """Parser for OrcaFlex YAML files to extract RAO data.

    Currently supports:
    - Displacement RAOs from VesselTypes structure

    Future support planned:
    - Velocity RAOs (if available in OrcaFlex format)
    - Acceleration RAOs (if available in OrcaFlex format)

    Example:
        >>> parser = OrcaFlexYMLParser()
        >>> rao_data = parser.parse_yml_file('vessel.yml')
        >>> print(rao_data.displacement.frequencies)
    """

    def __init__(self):
        """Initialize OrcaFlex YAML parser."""
        self.vessel_types_key = "VesselTypes"
        self.draughts_key = "Draughts"
        self.displacement_raos_key = "DisplacementRAOs"

        # DOF names in OrcaFlex
        self.dof_names = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']

    def parse_yml_file(self, file_path: str) -> UnifiedRAOData:
        """Parse OrcaFlex YAML file and extract RAO data.

        Args:
            file_path: Path to OrcaFlex YAML file

        Returns:
            UnifiedRAOData with displacement RAOs

        Raises:
            OrcaFlexParseError: If file cannot be parsed or structure is invalid
        """
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                # Handle multi-document YAML
                documents = list(yaml.safe_load_all(f))
                data = self._find_vessel_types_document(documents)
        except Exception as e:
            raise OrcaFlexParseError(f"Failed to read YAML file {file_path}: {str(e)}")

        if not data:
            raise OrcaFlexParseError("Empty or invalid YAML file")

        # Navigate structure
        vessel_data = self._navigate_to_vessel_type(data)
        vessel_name = vessel_data.get('Name', 'Unknown Vessel')

        draught_data = self._navigate_to_draught(vessel_data)
        rao_data_raw = self._extract_displacement_raos_raw(draught_data)

        # Parse RAO structure
        frequencies, headings, dof_arrays = self._parse_orcaflex_rao_structure(rao_data_raw)

        # Create metadata
        metadata = RAOMetadata(
            source_file=str(file_path),
            source_format=SourceFormat.ORCAFLEX_YML,
            vessel_name=vessel_name,
            license_required=False  # YAML reading doesn't require license
        )

        # Create DisplacementRAO
        displacement_rao = DisplacementRAO(
            frequencies=frequencies,
            headings=headings,
            surge=dof_arrays['surge'],
            sway=dof_arrays['sway'],
            heave=dof_arrays['heave'],
            roll=dof_arrays['roll'],
            pitch=dof_arrays['pitch'],
            yaw=dof_arrays['yaw'],
            metadata=metadata
        )

        # Return as UnifiedRAOData
        return UnifiedRAOData(
            displacement=displacement_rao,
            metadata=metadata
        )

    def _find_vessel_types_document(self, documents: List) -> Optional[Dict]:
        """Find the YAML document containing VesselTypes."""
        if len(documents) == 1:
            return documents[0]

        for doc in documents:
            if doc and self.vessel_types_key in doc:
                return doc

        # Fallback to first document
        return documents[0] if documents else None

    def _navigate_to_vessel_type(self, data: Dict) -> Dict:
        """Navigate to vessel type data in YAML structure."""
        if self.vessel_types_key not in data:
            raise OrcaFlexParseError(f"'{self.vessel_types_key}' not found in YAML")

        vessel_types = data[self.vessel_types_key]
        vessel_data = self._find_vessel_type(vessel_types)

        if not vessel_data:
            raise OrcaFlexParseError("No valid vessel type found")

        return vessel_data

    def _find_vessel_type(self, vessel_types: Any) -> Optional[Dict]:
        """Find a valid vessel type in the data structure."""
        if isinstance(vessel_types, dict):
            # Check if it's directly a vessel type
            if 'Name' in vessel_types and self.draughts_key in vessel_types:
                return vessel_types

            # Search in nested dictionaries
            for key, value in vessel_types.items():
                if isinstance(value, dict):
                    if 'Name' in value or self.draughts_key in value:
                        return value
                    result = self._find_vessel_type(value)
                    if result:
                        return result

        elif isinstance(vessel_types, list):
            for item in vessel_types:
                if isinstance(item, dict) and ('Name' in item or self.draughts_key in item):
                    return item

        return None

    def _navigate_to_draught(self, vessel_data: Dict) -> Dict:
        """Navigate to draught data containing RAOs."""
        if self.draughts_key not in vessel_data:
            raise OrcaFlexParseError(f"'{self.draughts_key}' not found in vessel data")

        draughts = vessel_data[self.draughts_key]
        draught_data = self._find_draught_with_raos(draughts)

        if not draught_data:
            raise OrcaFlexParseError("No draught with DisplacementRAOs found")

        return draught_data

    def _find_draught_with_raos(self, draughts: Any) -> Optional[Dict]:
        """Find draught containing DisplacementRAOs."""
        if isinstance(draughts, dict):
            if self.displacement_raos_key in draughts:
                return draughts

            for key, value in draughts.items():
                if isinstance(value, dict):
                    if self.displacement_raos_key in value:
                        return value
                    result = self._find_draught_with_raos(value)
                    if result:
                        return result

        elif isinstance(draughts, list):
            for item in draughts:
                if isinstance(item, dict) and self.displacement_raos_key in item:
                    return item

        return None

    def _extract_displacement_raos_raw(self, draught_data: Dict) -> Any:
        """Extract raw displacement RAO data from draught."""
        if self.displacement_raos_key not in draught_data:
            raise OrcaFlexParseError(
                f"'{self.displacement_raos_key}' not found in draught data"
            )

        return draught_data[self.displacement_raos_key]

    def _parse_orcaflex_rao_structure(self, rao_data_raw: Any) -> Tuple[np.ndarray, np.ndarray, Dict]:
        """Parse OrcaFlex RAO data structure.

        Returns:
            Tuple of (frequencies, headings, dof_arrays)
        """
        if not isinstance(rao_data_raw, dict):
            raise OrcaFlexParseError(f"Expected dict for RAO data, got {type(rao_data_raw)}")

        # Extract frequencies and headings
        headings = self._extract_headings(rao_data_raw)
        frequencies = self._extract_frequencies(rao_data_raw)

        # Create empty arrays
        n_freq = len(frequencies)
        n_head = len(headings)
        dof_arrays = create_empty_rao_arrays(n_freq, n_head)

        # Fill arrays from OrcaFlex structure
        self._fill_rao_values(rao_data_raw, frequencies, headings, dof_arrays)

        return np.array(frequencies), np.array(headings), dof_arrays

    def _extract_headings(self, rao_data: Dict) -> List[float]:
        """Extract heading values from RAO data."""
        headings = []

        if 'Headings' in rao_data:
            headings = rao_data['Headings']
        elif 'RAOs' in rao_data:
            headings_set = set()
            for rao_direction in rao_data['RAOs']:
                if 'RAODirection' in rao_direction:
                    headings_set.add(rao_direction['RAODirection'])
            headings = list(headings_set)
        else:
            # Try numeric keys
            for key in rao_data.keys():
                try:
                    heading = float(key)
                    headings.append(heading)
                except (ValueError, TypeError):
                    continue

        if not headings:
            # Default headings
            headings = list(range(0, 360, 15))

        return sorted(headings)

    def _extract_frequencies(self, rao_data: Dict) -> List[float]:
        """Extract frequency values from RAO data."""
        frequencies = []

        if 'Frequencies' in rao_data:
            frequencies = rao_data['Frequencies']
        elif 'Periods' in rao_data:
            periods = rao_data['Periods']
            frequencies = [2 * np.pi / p if p > 0 else 0 for p in periods]
        elif 'RAOs' in rao_data:
            periods_set = set()
            for rao_direction in rao_data['RAOs']:
                for key, value in rao_direction.items():
                    if 'RAOPeriodOrFreq' in key and isinstance(value, list):
                        for row in value:
                            if isinstance(row, list) and len(row) > 0:
                                periods_set.add(row[0])

            frequencies = [2 * np.pi / p if p > 0 else 0 for p in periods_set]

        if not frequencies:
            raise OrcaFlexParseError("No frequencies found in RAO data")

        return sorted(frequencies)

    def _fill_rao_values(self,
                        rao_data: Dict,
                        frequencies: List[float],
                        headings: List[float],
                        dof_arrays: Dict) -> None:
        """Fill DOF arrays from OrcaFlex RAO structure."""
        # Build lookup table
        data_lookup = {}  # {heading: {frequency: {dof: {'amplitude': amp, 'phase': phase}}}}

        if 'RAOs' not in rao_data:
            return

        for rao_direction in rao_data['RAOs']:
            heading = rao_direction.get('RAODirection')
            if heading is None:
                continue

            # Find data key
            data_key = None
            for key in rao_direction.keys():
                if 'RAOPeriodOrFreq' in key:
                    data_key = key
                    break

            if not data_key or data_key not in rao_direction:
                continue

            data_rows = rao_direction[data_key]

            for row in data_rows:
                if isinstance(row, list) and len(row) >= 13:
                    # Row: [period, surge_amp, surge_phase, sway_amp, sway_phase, ...]
                    period = row[0]
                    frequency = 2 * np.pi / period if period > 0 else 0

                    if heading not in data_lookup:
                        data_lookup[heading] = {}

                    data_lookup[heading][frequency] = {
                        'surge': {'amplitude': row[1], 'phase': row[2]},
                        'sway': {'amplitude': row[3], 'phase': row[4]},
                        'heave': {'amplitude': row[5], 'phase': row[6]},
                        'roll': {'amplitude': row[7], 'phase': row[8]},
                        'pitch': {'amplitude': row[9], 'phase': row[10]},
                        'yaw': {'amplitude': row[11], 'phase': row[12]}
                    }

        # Fill arrays
        for i, freq in enumerate(frequencies):
            for j, heading in enumerate(headings):
                if (heading in data_lookup and
                    freq in data_lookup[heading]):

                    for dof_name, dof_data in dof_arrays.items():
                        if dof_name in data_lookup[heading][freq]:
                            rao_value = data_lookup[heading][freq][dof_name]
                            dof_data.amplitude[i, j] = rao_value['amplitude']
                            dof_data.phase[i, j] = rao_value['phase']
