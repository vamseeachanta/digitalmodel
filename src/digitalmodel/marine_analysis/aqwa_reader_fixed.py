"""ANSYS AQWA .lis file parser using Fortran fixed-width format.

This module provides functionality to parse ANSYS AQWA output files using
fixed character positions similar to Fortran formatting. It also handles
missing data by using values from the previous row.
"""

import re
from typing import Dict, List, Tuple, Any, Optional
import numpy as np
from pathlib import Path


class AQWAFileError(Exception):
    """Exception raised for AQWA file parsing errors."""
    pass


class AQWAReaderFixed:
    """Parser for ANSYS AQWA .lis output files using fixed-width format."""
    
    def __init__(self):
        """Initialize AQWA reader with parsing patterns."""
        # Pattern to find displacement RAO sections (not VEL or ACC)
        self.rao_section_pattern = re.compile(
            r'(?<!VEL\s)(?<!ACC\s)R\.A\.O\.S-VARIATION\s+WITH\s+WAVE\s+DIRECTION'
        )
        
        # Pattern to match the data header
        self.header_pattern = re.compile(
            r'PERIOD\s+FREQ\s+DIRECTION\s+X\s+Y\s+Z\s+RX\s+RY\s+RZ'
        )
        
        # Fixed column positions based on Fortran format
        # For full lines (with period, freq, direction)
        self.full_line_format = {
            'period': (0, 8),      # Columns 1-8
            'freq': (8, 16),       # Columns 9-16
            'direction': (16, 27), # Columns 17-27
            'data_start': 27       # Data starts at column 28
        }
        
        # For continuation lines (direction only)
        self.cont_line_format = {
            'direction': (19, 27), # Columns 20-27
            'data_start': 27       # Data starts at column 28
        }
        
        # Each value is approximately 9 characters wide
        self.value_width = 9
    
    def parse_lis_file(self, file_path: str) -> Dict[str, Any]:
        """Parse AQWA .lis file and extract RAO data."""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
        except Exception as e:
            raise AQWAFileError(f"Failed to read file {file_path}: {str(e)}")
        
        # Find all displacement RAO sections
        sections = self._find_displacement_rao_sections(content)
        if not sections:
            raise AQWAFileError("No displacement RAO sections found in the AQWA file")
        
        # Use the last section
        last_section = sections[-1]
        
        # Parse the RAO data using fixed-width format
        rao_data = self._parse_rao_section_fixed(last_section)
        
        # Convert to required format
        frequencies = sorted(list(rao_data.keys()))
        all_headings = set()
        for freq_data in rao_data.values():
            all_headings.update(freq_data.keys())
        headings = sorted(list(all_headings))
        
        # Create arrays
        n_freq = len(frequencies)
        n_head = len(headings)
        
        raos = {
            dof: {
                'amplitude': np.zeros((n_freq, n_head)),
                'phase': np.zeros((n_freq, n_head))
            }
            for dof in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        }
        
        # Fill arrays
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
    
    def _find_displacement_rao_sections(self, content: str) -> List[str]:
        """Find all displacement RAO sections in the content."""
        sections = []
        
        # Find all matches
        matches = list(self.rao_section_pattern.finditer(content))
        
        for i, match in enumerate(matches):
            start_pos = match.start()
            
            # Find the end of this section
            if i < len(matches) - 1:
                # End at the next RAO section
                end_pos = matches[i + 1].start()
            else:
                # This is the last section, find a good end point
                end_pos = len(content)
                
                # Look for common section endings
                section_content = content[start_pos:]
                end_patterns = [
                    r'\n\s*\*{10,}',  # Line of asterisks
                    r'\n\s*H Y D R O D Y N A M I C',  # Next major section
                    r'\n\s*WAVE DRIFT',
                    r'\n\s*END OF'
                ]
                
                for pattern in end_patterns:
                    match_end = re.search(pattern, section_content)
                    if match_end:
                        end_pos = start_pos + match_end.start()
                        break
            
            section = content[start_pos:end_pos]
            sections.append(section)
        
        return sections
    
    def _parse_rao_section_fixed(self, section: str) -> Dict[float, Dict[float, Dict[str, Dict[str, float]]]]:
        """Parse RAO section using fixed-width format."""
        lines = section.split('\n')
        
        # Find the header line
        header_idx = -1
        for i, line in enumerate(lines):
            if self.header_pattern.search(line):
                header_idx = i
                break
        
        if header_idx == -1:
            raise AQWAFileError("Could not find data header in RAO section")
        
        # Skip to the actual data (skip header, separator, units, separator)
        data_start = header_idx + 4
        
        rao_data = {}
        current_freq = None
        current_period = None
        previous_dof_values = None  # Store previous row's DOF values
        debug_mode = False  # Set to True to enable debug output
        
        for line_idx in range(data_start, len(lines)):
            line = lines[line_idx]
            
            # Skip empty lines and separators
            if not line or line.strip().startswith('-'):
                continue
            
            # Stop at major section boundary
            if '*' in line and len(line.replace('*', '').replace(' ', '')) < 5:
                break
            
            # Check if line is long enough
            if len(line) < 27:
                continue
            
            try:
                # Check if this is a full line or continuation
                period_str = line[self.full_line_format['period'][0]:self.full_line_format['period'][1]].strip()
                freq_str = line[self.full_line_format['freq'][0]:self.full_line_format['freq'][1]].strip()
                
                if period_str and freq_str:
                    # This is a full line
                    period = float(period_str)
                    freq = float(freq_str)
                    direction_str = line[self.full_line_format['direction'][0]:self.full_line_format['direction'][1]].strip()
                    direction = float(direction_str)
                    
                    current_freq = freq
                    current_period = period
                    
                    # Extract DOF values starting from data_start position
                    dof_values = self._extract_dof_values_fixed(line, self.full_line_format['data_start'])
                    
                    if dof_values:
                        if freq not in rao_data:
                            rao_data[freq] = {}
                        rao_data[freq][direction] = dof_values
                        previous_dof_values = dof_values  # Store for potential use
                    
                else:
                    # This is a continuation line
                    # Try to extract direction from the line more flexibly
                    # The direction should be the first numeric value in the line
                    line_stripped = line.strip()
                    if line_stripped and current_freq is not None:
                        parts = line_stripped.split()
                        if parts and parts[0]:
                            try:
                                direction = float(parts[0])
                                
                                # Sanity check: direction should be between -180 and 180
                                if -180 <= direction <= 180:
                                    # Find where this direction value starts in the original line
                                    dir_start = line.find(parts[0])
                                    
                                    # Extract DOF values starting after the direction
                                    # The data should still start around column 27
                                    dof_values = self._extract_dof_values_fixed(line, self.cont_line_format['data_start'])
                                    
                                    # If extraction failed but we have previous values, use them
                                    if not dof_values and previous_dof_values:
                                        dof_values = previous_dof_values
                                    
                                    if dof_values:
                                        rao_data[current_freq][direction] = dof_values
                                        previous_dof_values = dof_values  # Update stored values
                            except ValueError:
                                # Not a valid direction, skip
                                pass
                
            except (ValueError, IndexError) as e:
                # Skip lines that can't be parsed
                continue
        
        return rao_data
    
    def _extract_dof_values_fixed(self, line: str, start_pos: int) -> Optional[Dict[str, Dict[str, float]]]:
        """Extract DOF values using fixed-width format."""
        if len(line) < start_pos:
            return None
        
        data_part = line[start_pos:]
        dof_names = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        
        result = {}
        values = []
        
        # Extract values using fixed width
        pos = 0
        while pos < len(data_part) and len(values) < 12:  # 6 DOF * 2 (amp, phase)
            val_str = data_part[pos:pos + self.value_width].strip()
            if val_str:
                try:
                    val = float(val_str)
                    values.append(val)
                except ValueError:
                    # If we can't parse a value, use 0.0
                    values.append(0.0)
            pos += self.value_width
        
        # Ensure we have exactly 12 values (6 DOF * 2)
        while len(values) < 12:
            values.append(0.0)
        
        # Assign values to DOFs (amplitude, phase pairs)
        for i, dof in enumerate(dof_names):
            amp_idx = i * 2
            phase_idx = i * 2 + 1
            
            if amp_idx < len(values) and phase_idx < len(values):
                result[dof] = {
                    'amplitude': values[amp_idx],
                    'phase': values[phase_idx]
                }
            else:
                result[dof] = {'amplitude': 0.0, 'phase': 0.0}
        
        return result