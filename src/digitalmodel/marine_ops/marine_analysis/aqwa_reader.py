"""ANSYS AQWA .lis file parser for RAO data extraction.

This module provides functionality to parse ANSYS AQWA output files and extract
displacement RAO data. It searches for the last set of RAOs in the file as
specified in the requirements.
"""

import re
from typing import Dict, List, Tuple, Any, Optional
import numpy as np
from pathlib import Path
from .aqwa_reader_fixed import AQWAReaderFixed
from .aqwa_enhanced_parser import AQWAEnhancedParser


class AQWAFileError(Exception):
    """Exception raised for AQWA file parsing errors."""
    pass


class AQWAReader:
    """Parser for ANSYS AQWA .lis output files."""
    
    def __init__(self):
        """Initialize AQWA reader with parsing patterns."""
        # Common patterns for AQWA output parsing
        self.frequency_pattern = re.compile(r'FREQUENCY\s*=\s*([\d.E+-]+)\s*RAD/SEC')
        self.period_pattern = re.compile(r'PERIOD\s*=\s*([\d.E+-]+)\s*SECS')
        self.heading_pattern = re.compile(r'WAVE HEADING\s*=\s*([\d.]+)\s*DEGREES')
        # Updated pattern to search for displacement "R.A.O.S-VARIATION WITH WAVE DIRECTION" 
        # Exclude velocity (VEL) and acceleration (ACC) RAOs
        self.rao_section_pattern = re.compile(r'(?<!VEL\s)(?<!ACC\s)R\.A\.O\.S-VARIATION\s+WITH\s+WAVE\s+DIRECTION')
        
        # Updated patterns for AQWA tabular format
        # Header pattern to identify the data section
        self.header_pattern = re.compile(
            r'PERIOD\s+FREQ\s+DIRECTION\s+X\s+Y\s+Z\s+RX\s+RY\s+RZ'
        )
        
        # Flexible data patterns - handle lines that might be cut off
        # Full data line: PERIOD FREQ DIRECTION + 6 DOF (amp, phase each)
        self.data_line_pattern = re.compile(
            r'^\s*([\d.]+)\s+([\d.]+)\s+([-]?[\d.]+)\s+(.+)$',
            re.MULTILINE
        )
        
        # Continuation line: just direction + 6 DOF
        self.continuation_pattern = re.compile(
            r'^\s+([-]?[\d.]+)\s+(.+)$',
            re.MULTILINE
        )
        
        # Pattern to extract 6 DOF values from a data portion
        self.dof_values_pattern = re.compile(
            r'([\d.E+-]+)\s+([-]?[\d.E+-]+)(?:\s+|$)'
        )
    
    def parse_lis_file(self, file_path: str, use_enhanced_parser: bool = True) -> Dict[str, Any]:
        """Parse AQWA .lis file and extract RAO data.
        
        Args:
            file_path: Path to the AQWA .lis file
            use_enhanced_parser: If True, uses enhanced parser with data interpretation
        
        Returns:
            Dictionary with frequency, heading, and RAO arrays
        """
        if use_enhanced_parser:
            # Use the enhanced parser with data interpretation
            enhanced_parser = AQWAEnhancedParser()
            return enhanced_parser.parse_lis_file(file_path)
        else:
            # Use the fixed-width parser
            fixed_parser = AQWAReaderFixed()
            return fixed_parser.parse_lis_file(file_path)
        
        # Parse the RAO data from the last section
        rao_data = self._parse_rao_section(last_rao_section)
        
        # Organize data into structured format
        frequencies = sorted(list(rao_data.keys()))
        headings = sorted(list(rao_data[frequencies[0]].keys()))
        
        # Initialize arrays for each DOF
        n_freq = len(frequencies)
        n_head = len(headings)
        
        raos = {
            dof: {
                'amplitude': np.zeros((n_freq, n_head)),
                'phase': np.zeros((n_freq, n_head))
            }
            for dof in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        }
        
        # Fill arrays with parsed data
        for i, freq in enumerate(frequencies):
            for j, heading in enumerate(headings):
                if heading in rao_data[freq]:
                    for dof in raos:
                        if dof in rao_data[freq][heading]:
                            raos[dof]['amplitude'][i, j] = rao_data[freq][heading][dof]['amplitude']
                            raos[dof]['phase'][i, j] = rao_data[freq][heading][dof]['phase']
        
        return {
            'frequencies': np.array(frequencies),
            'headings': np.array(headings),
            'raos': raos
        }
    
    def _find_all_rao_sections(self, content: str) -> List[str]:
        """Find all RAO sections in the file content."""
        sections = []
        
        # Split content into potential sections
        lines = content.split('\n')
        
        # Find start indices of RAO sections
        rao_starts = []
        for i, line in enumerate(lines):
            if self.rao_section_pattern.search(line):
                rao_starts.append(i)
        
        # Extract each RAO section
        for i, start_idx in enumerate(rao_starts):
            # Determine end of section (next RAO section or end of file)
            if i < len(rao_starts) - 1:
                end_idx = rao_starts[i + 1]
            else:
                end_idx = len(lines)
            
            # Extract section
            section_lines = lines[start_idx:end_idx]
            sections.append('\n'.join(section_lines))
        
        return sections
    
    def _parse_rao_section(self, section: str) -> Dict[float, Dict[float, Dict[str, Dict[str, float]]]]:
        """Parse a single RAO section and extract data.
        
        Returns:
            Nested dictionary: {frequency: {heading: {dof: {'amplitude': val, 'phase': val}}}}
        """
        rao_data = {}
        lines = section.split('\n')
        
        # Find the header line to locate data start
        data_start_idx = -1
        for i, line in enumerate(lines):
            if self.header_pattern.search(line):
                data_start_idx = i
                break
        
        if data_start_idx == -1:
            raise AQWAFileError("Could not find data header in RAO section")
        
        # Skip header and separator lines
        data_lines = lines[data_start_idx + 3:]  # Skip header, separator, and units line
        
        current_freq = None
        current_period = None
        
        for line in data_lines:
            line = line.strip()
            if not line or line.startswith('-'):  # Skip empty lines and separators
                continue
            
            # Try to match full data line (with period, freq, direction)
            match = self.data_line_pattern.match(line)
            if match:
                period = float(match.group(1))
                freq = float(match.group(2))
                direction = float(match.group(3))
                data_portion = match.group(4)
                
                # Extract DOF values using the flexible pattern
                dof_values = self._extract_dof_values(data_portion)
                if dof_values:
                    # Store data
                    if freq not in rao_data:
                        rao_data[freq] = {}
                    
                    rao_data[freq][direction] = dof_values
                    current_freq = freq
                    current_period = period
                continue
            
            # Try to match continuation line (direction only)
            cont_match = self.continuation_pattern.match(line)
            if cont_match and current_freq is not None:
                direction = float(cont_match.group(1))
                data_portion = cont_match.group(2)
                
                # Extract DOF values
                dof_values = self._extract_dof_values(data_portion)
                if dof_values:
                    rao_data[current_freq][direction] = dof_values
        
        return rao_data
    
    def _extract_dof_values(self, data_portion: str) -> Optional[Dict[str, Dict[str, float]]]:
        """Extract 6 DOF amplitude and phase values from data portion.
        
        Returns:
            Dictionary with DOF values or None if extraction fails
        """
        # Find all amp/phase pairs in the data portion
        matches = self.dof_values_pattern.findall(data_portion)
        
        if len(matches) < 6:
            # If we don't have all 6 DOF, return what we have with defaults
            while len(matches) < 6:
                matches.append(('0.0', '0.0'))  # Default values
        
        # DOF order: X, Y, Z, RX, RY, RZ (surge, sway, heave, roll, pitch, yaw)
        dof_names = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        
        result = {}
        for i, dof in enumerate(dof_names):
            if i < len(matches):
                amp, phase = matches[i]
                result[dof] = {
                    'amplitude': float(amp),
                    'phase': float(phase)
                }
            else:
                result[dof] = {'amplitude': 0.0, 'phase': 0.0}
        
        return result
    
    def _extract_last_rao_section(self, content: str) -> str:
        """Extract the last RAO section from file content."""
        # Find all occurrences of "R.A.O.S-VARIATION WITH WAVE DIRECTION"
        sections = self.rao_section_pattern.split(content)
        
        if len(sections) < 2:
            raise AQWAFileError("No RAO section with 'R.A.O.S-VARIATION WITH WAVE DIRECTION' found in file")
        
        # The last section is what we want
        # But we need to find where the next major section starts
        last_section = sections[-1]
        
        # Common section headers that might follow RAO data
        end_patterns = [
            r'\n\s*\*{10,}',  # Line of asterisks
            r'\n\s*WAVE DRIFT',
            r'\n\s*HYDROSTATIC',
            r'\n\s*MOORING',
            r'\n\s*END OF',
            r'\n\s*R\.A\.O\.S',  # Next RAO section
            r'\n\s*H Y D R O D Y N A M I C',  # Hydrodynamic parameters section
            r'\n\d+\s*$'  # Line with just a number (page break indicator)
        ]
        
        # Find the earliest end pattern
        end_pos = len(last_section)
        for pattern in end_patterns:
            match = re.search(pattern, last_section)
            if match:
                end_pos = min(end_pos, match.start())
        
        return "R.A.O.S-VARIATION WITH WAVE DIRECTION" + last_section[:end_pos]
    
    def _parse_frequencies(self, rao_section: str) -> np.ndarray:
        """Parse frequency values from RAO section."""
        frequencies = set()
        
        for match in self.frequency_pattern.finditer(rao_section):
            freq = float(match.group(1))
            frequencies.add(freq)
        
        if not frequencies:
            # Try parsing periods and converting
            for match in self.period_pattern.finditer(rao_section):
                period = float(match.group(1))
                if period > 0:
                    freq = 2 * np.pi / period
                    frequencies.add(freq)
        
        if not frequencies:
            raise AQWAFileError("No frequencies found in RAO section")
        
        return np.array(sorted(list(frequencies)))
    
    def _parse_headings(self, rao_section: str) -> np.ndarray:
        """Parse heading values from RAO section."""
        headings = set()
        
        for match in self.heading_pattern.finditer(rao_section):
            heading = float(match.group(1))
            headings.add(heading)
        
        if not headings:
            raise AQWAFileError("No headings found in RAO section")
        
        return np.array(sorted(list(headings)))
    
    def _parse_dof_raos(self, rao_section: str, dof: int) -> Dict[str, np.ndarray]:
        """Parse RAO data for a specific degree of freedom.
        
        Args:
            rao_section: RAO section text
            dof: Degree of freedom (1-6)
                1: Surge, 2: Sway, 3: Heave, 4: Roll, 5: Pitch, 6: Yaw
        
        Returns:
            Dictionary with 'amplitude' and 'phase' arrays
        """
        dof_names = {1: 'surge', 2: 'sway', 3: 'heave', 4: 'roll', 5: 'pitch', 6: 'yaw'}
        dof_name = dof_names.get(dof, '')
        
        if not dof_name:
            raise ValueError(f"Invalid DOF: {dof}")
        
        # Parse the section to extract amplitude and phase values
        # This is a simplified implementation - actual parsing would be more complex
        # based on the specific AQWA output format
        
        frequencies = self._parse_frequencies(rao_section)
        headings = self._parse_headings(rao_section)
        
        n_freq = len(frequencies)
        n_head = len(headings)
        
        amplitude = np.zeros((n_freq, n_head))
        phase = np.zeros((n_freq, n_head))
        
        # Note: Actual implementation would parse the specific format
        # This is a placeholder that would need to be adapted to the actual AQWA format
        
        return {
            'amplitude': amplitude,
            'phase': phase
        }