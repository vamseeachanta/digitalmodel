"""Improved ANSYS AQWA .lis file parser for RAO data extraction."""

import re
from typing import Dict, List, Tuple, Any, Optional
import numpy as np
from pathlib import Path


class AQWAFileError(Exception):
    """Exception raised for AQWA file parsing errors."""
    pass


class AQWAReaderV2:
    """Improved parser for ANSYS AQWA .lis output files."""
    
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
        
        # Parse the RAO data
        rao_data = self._parse_rao_section_robust(last_section)
        
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
    
    def _parse_rao_section_robust(self, section: str) -> Dict[float, Dict[float, Dict[str, Dict[str, float]]]]:
        """Parse RAO section with robust table parsing."""
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
        current_freq = None  # Track the current frequency for continuation lines
        
        # Process each line of data
        for line_idx in range(data_start, len(lines)):
            line = lines[line_idx].strip()
            
            # Skip empty lines and separators
            if not line or line.startswith('-'):
                continue
            
            # Stop at major section boundary (full line of asterisks)
            if '*' in line and len(line.replace('*', '').replace(' ', '')) < 5:
                break
            
            # Parse the line
            parts = line.split()
            if len(parts) < 3:
                continue  # Skip incomplete lines
            
            try:
                # Check if this is a full line (has period and frequency)
                if len(parts) >= 15:  # Full line: period, freq, direction + 12 values (6 DOF × 2)
                    period = float(parts[0])
                    freq = float(parts[1])
                    direction = float(parts[2])
                    current_freq = freq  # Update current frequency
                    
                    # Extract the 6 DOF values (amp, phase pairs)
                    dof_values = self._extract_dof_from_parts(parts[3:])
                    
                    if freq not in rao_data:
                        rao_data[freq] = {}
                    
                    rao_data[freq][direction] = dof_values
                    
                elif len(parts) >= 13 and current_freq is not None:  # Continuation line: direction + 12 values
                    direction = float(parts[0])
                    dof_values = self._extract_dof_from_parts(parts[1:])
                    
                    # Use the current frequency (from the last full line)
                    rao_data[current_freq][direction] = dof_values
                
            except (ValueError, IndexError):
                # Skip lines that can't be parsed as numbers
                continue
        
        return rao_data
    
    def _extract_dof_from_parts(self, parts: List[str]) -> Dict[str, Dict[str, float]]:
        """Extract DOF values from list of string parts."""
        dof_names = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        
        result = {}
        
        # Each DOF has amplitude and phase
        for i, dof in enumerate(dof_names):
            amp_idx = i * 2
            phase_idx = i * 2 + 1
            
            if amp_idx < len(parts) and phase_idx < len(parts):
                try:
                    amplitude = float(parts[amp_idx])
                    phase = float(parts[phase_idx])
                    result[dof] = {'amplitude': amplitude, 'phase': phase}
                except (ValueError, IndexError):
                    result[dof] = {'amplitude': 0.0, 'phase': 0.0}
            else:
                result[dof] = {'amplitude': 0.0, 'phase': 0.0}
        
        return result