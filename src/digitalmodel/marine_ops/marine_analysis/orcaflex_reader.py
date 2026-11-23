"""OrcaFlex YAML file parser for RAO data extraction.

This module provides functionality to parse OrcaFlex YAML files and extract
displacement RAO data following the specified structure:
VesselTypes > Name: Vessel Type1 > Draughts > Name: Draught1 > DisplacementRAOs: RAOs
"""

import yaml
from typing import Dict, List, Tuple, Any, Optional
import numpy as np
from pathlib import Path


class OrcaFlexFileError(Exception):
    """Exception raised for OrcaFlex file parsing errors."""
    pass


class OrcaFlexReader:
    """Parser for OrcaFlex YAML files."""
    
    def __init__(self):
        """Initialize OrcaFlex reader."""
        self.vessel_types_key = "VesselTypes"
        self.draughts_key = "Draughts"
        self.displacement_raos_key = "DisplacementRAOs"
        
        # DOF mapping in OrcaFlex
        self.dof_names = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']
        self.dof_map = {name.lower(): idx for idx, name in enumerate(self.dof_names)}
    
    def parse_yml_file(self, file_path: str) -> Dict[str, Any]:
        """Parse OrcaFlex YAML file and extract RAO data.
        
        Returns:
            Dictionary with frequency, heading, and RAO arrays
        """
        try:
            with open(file_path, 'r') as f:
                # Handle multi-document YAML files
                documents = list(yaml.safe_load_all(f))
                if len(documents) == 1:
                    data = documents[0]
                else:
                    # Find the document with VesselTypes
                    data = None
                    for doc in documents:
                        if doc and self.vessel_types_key in doc:
                            data = doc
                            break
                    if data is None:
                        data = documents[0]  # Fall back to first document
        except Exception as e:
            raise OrcaFlexFileError(f"Failed to read YAML file {file_path}: {str(e)}")
        
        if not data:
            raise OrcaFlexFileError("Empty YAML file")
        
        # Navigate to VesselTypes
        if self.vessel_types_key not in data:
            raise OrcaFlexFileError(f"'{self.vessel_types_key}' not found in YAML file")
        
        vessel_types = data[self.vessel_types_key]
        
        # Find the vessel type (look for first vessel with proper structure)
        vessel_data = self._find_vessel_type(vessel_types)
        if not vessel_data:
            raise OrcaFlexFileError("No valid vessel type found in YAML file")
        
        vessel_name = vessel_data.get('Name', 'Unknown Vessel')
        
        # Navigate to Draughts
        if self.draughts_key not in vessel_data:
            raise OrcaFlexFileError(f"'{self.draughts_key}' not found in vessel data")
        
        draughts = vessel_data[self.draughts_key]
        
        # Find the draught (look for first draught with RAO data)
        draught_data = self._find_draught_with_raos(draughts)
        if not draught_data:
            raise OrcaFlexFileError("No draught with DisplacementRAOs found")
        
        # Extract RAO data
        if self.displacement_raos_key not in draught_data:
            raise OrcaFlexFileError(f"'{self.displacement_raos_key}' not found in draught data")
        
        rao_data_raw = draught_data[self.displacement_raos_key]
        
        # Parse the RAO data structure
        frequencies, headings, raos = self._parse_orcaflex_rao_data(rao_data_raw)
        
        return {
            'frequencies': frequencies,
            'headings': headings,
            'raos': raos,
            'vessel_name': vessel_name
        }
    
    def _find_vessel_type(self, vessel_types: Any) -> Optional[Dict]:
        """Find a valid vessel type in the vessel types data."""
        if isinstance(vessel_types, dict):
            # Check if it's directly a vessel type with the right structure
            if 'Name' in vessel_types and self.draughts_key in vessel_types:
                return vessel_types
            
            # Otherwise, iterate through the dictionary
            for key, value in vessel_types.items():
                if isinstance(value, dict):
                    # Check if this looks like a vessel type
                    if 'Name' in value or self.draughts_key in value:
                        return value
                    
                    # Recursively check
                    result = self._find_vessel_type(value)
                    if result:
                        return result
        
        elif isinstance(vessel_types, list):
            # If it's a list, check each item
            for item in vessel_types:
                if isinstance(item, dict) and ('Name' in item or self.draughts_key in item):
                    return item
        
        return None
    
    def _find_draught_with_raos(self, draughts: Any) -> Optional[Dict]:
        """Find a draught that contains DisplacementRAOs."""
        if isinstance(draughts, dict):
            # Check if it directly contains DisplacementRAOs
            if self.displacement_raos_key in draughts:
                return draughts
            
            # Otherwise, iterate through the dictionary
            for key, value in draughts.items():
                if isinstance(value, dict):
                    # Check if this draught has DisplacementRAOs
                    if self.displacement_raos_key in value:
                        return value
                    
                    # Recursively check
                    result = self._find_draught_with_raos(value)
                    if result:
                        return result
        
        elif isinstance(draughts, list):
            # If it's a list, check each item
            for item in draughts:
                if isinstance(item, dict) and self.displacement_raos_key in item:
                    return item
        
        return None
    
    def _parse_orcaflex_rao_data(self, rao_data_raw: Any) -> Tuple[np.ndarray, np.ndarray, Dict]:
        """Parse OrcaFlex RAO data structure.
        
        Returns:
            Tuple of (frequencies, headings, raos_dict)
            where raos_dict contains amplitude and phase for each DOF
        """
        # OrcaFlex RAO data can be in various formats
        # Common format is a table with headings as columns and frequencies as rows
        
        if isinstance(rao_data_raw, dict):
            # Extract headings from the data structure
            headings = self._extract_headings(rao_data_raw)
            frequencies = self._extract_frequencies(rao_data_raw)
            
            # Initialize RAO data structure
            raos = {
                dof.lower(): {
                    'amplitude': np.zeros((len(frequencies), len(headings))),
                    'phase': np.zeros((len(frequencies), len(headings)))
                }
                for dof in self.dof_names
            }
            
            # Parse RAO values
            self._fill_rao_values(rao_data_raw, frequencies, headings, raos)
            
        elif isinstance(rao_data_raw, list):
            # If it's a list, it might be a table format
            frequencies, headings, raos = self._parse_table_format(rao_data_raw)
        
        else:
            raise OrcaFlexFileError(f"Unexpected RAO data format: {type(rao_data_raw)}")
        
        return np.array(frequencies), np.array(headings), raos
    
    def _extract_headings(self, rao_data: Dict) -> List[float]:
        """Extract heading values from RAO data structure."""
        headings = []
        
        # Common patterns for headings in OrcaFlex
        # They might be keys in the dictionary or in a 'Headings' field
        if 'Headings' in rao_data:
            headings = rao_data['Headings']
        elif 'RAOs' in rao_data:
            # Extract from RAO direction values
            headings_set = set()
            for rao_direction in rao_data['RAOs']:
                if 'RAODirection' in rao_direction:
                    headings_set.add(rao_direction['RAODirection'])
            headings = list(headings_set)
        else:
            # Look for numeric keys that could be headings
            for key in rao_data.keys():
                try:
                    heading = float(key)
                    headings.append(heading)
                except (ValueError, TypeError):
                    continue
        
        if not headings:
            # Default headings if not found
            headings = list(range(0, 360, 15))  # 0 to 345 in 15-degree increments
        
        return sorted(headings)
    
    def _extract_frequencies(self, rao_data: Dict) -> List[float]:
        """Extract frequency values from RAO data structure."""
        frequencies = []
        
        # Common patterns for frequencies in OrcaFlex
        if 'Frequencies' in rao_data:
            frequencies = rao_data['Frequencies']
        elif 'Periods' in rao_data:
            # Convert periods to frequencies
            periods = rao_data['Periods']
            frequencies = [2 * np.pi / p if p > 0 else 0 for p in periods]
        elif 'RAOs' in rao_data:
            # Extract from the RAO data structure
            periods_set = set()
            for rao_direction in rao_data['RAOs']:
                # Look for the data key (it has a long name with comma-separated fields)
                for key, value in rao_direction.items():
                    if 'RAOPeriodOrFreq' in key and isinstance(value, list):
                        # Extract periods/frequencies from the first column of each row
                        for row in value:
                            if isinstance(row, list) and len(row) > 0:
                                periods_set.add(row[0])  # First column is period/frequency
            
            # Convert periods to frequencies (assuming periods in seconds)
            frequencies = [2 * np.pi / p if p > 0 else 0 for p in periods_set]
        
        if not frequencies:
            raise OrcaFlexFileError("No frequencies found in RAO data")
        
        return sorted(frequencies)
    
    def _fill_rao_values(self, rao_data: Dict, frequencies: List[float], 
                        headings: List[float], raos: Dict) -> None:
        """Fill RAO values from the raw data structure."""
        # Build a lookup table from the OrcaFlex RAO data
        data_lookup = {}  # {heading: {frequency: {dof: {'amplitude': amp, 'phase': phase}}}}
        
        for rao_direction in rao_data['RAOs']:
            heading = rao_direction['RAODirection']
            
            # Find the data key (long key with comma-separated field names)
            data_key = None
            for key in rao_direction.keys():
                if 'RAOPeriodOrFreq' in key:
                    data_key = key
                    break
            
            if data_key and data_key in rao_direction:
                data_rows = rao_direction[data_key]
                
                for row in data_rows:
                    if isinstance(row, list) and len(row) >= 13:
                        # Row format: [period, surge_amp, surge_phase, sway_amp, sway_phase, 
                        #              heave_amp, heave_phase, roll_amp, roll_phase, 
                        #              pitch_amp, pitch_phase, yaw_amp, yaw_phase]
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
        
        # Fill the RAO arrays
        for i, freq in enumerate(frequencies):
            for j, heading in enumerate(headings):
                for dof in self.dof_names:
                    dof_lower = dof.lower()
                    
                    if (heading in data_lookup and 
                        freq in data_lookup[heading] and 
                        dof_lower in data_lookup[heading][freq]):
                        
                        rao_value = data_lookup[heading][freq][dof_lower]
                        raos[dof_lower]['amplitude'][i, j] = rao_value['amplitude']
                        raos[dof_lower]['phase'][i, j] = rao_value['phase']
    
    
    def _parse_table_format(self, rao_table: List) -> Tuple[List[float], List[float], Dict]:
        """Parse RAO data in table format."""
        # This would handle cases where RAO data is provided as a list of rows
        # Implementation would depend on specific OrcaFlex table format
        
        # Placeholder implementation
        frequencies = []
        headings = []
        raos = {
            dof.lower(): {
                'amplitude': np.array([]),
                'phase': np.array([])
            }
            for dof in self.dof_names
        }
        
        # Actual parsing logic would go here
        
        return frequencies, headings, raos