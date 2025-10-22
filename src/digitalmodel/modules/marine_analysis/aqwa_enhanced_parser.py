"""Enhanced ANSYS AQWA .lis file parser with data interpretation."""

import re
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

import numpy as np


FLOAT_PATTERN = re.compile(r"[-+]?\d*\.?\d+(?:[Ee][-+]?\d+)?")
DEFAULT_DOF_LABELS = ("surge", "sway", "heave", "roll", "pitch", "yaw")


class AQWAFileError(Exception):
    """Exception raised for AQWA file parsing errors."""
    pass


class AQWAEnhancedParser:
    """Enhanced parser for ANSYS AQWA .lis output files with data interpretation."""
    
    def __init__(self):
        """Initialize enhanced AQWA reader with parsing patterns."""
        self._content_cache: Dict[str, str] = {}
        # Pattern to find displacement RAO sections (not VEL or ACC)
        self.rao_section_pattern = re.compile(
            r'(?<!VEL\s)(?<!ACC\s)R\.A\.O\.S-VARIATION\s+WITH\s+WAVE\s+DIRECTION'
        )
        
        # Pattern to match the data header
        self.header_pattern = re.compile(
            r'PERIOD\s+FREQ\s+DIRECTION\s+X\s+Y\s+Z\s+RX\s+RY\s+RZ'
        )
        
        # Fixed column positions based on Fortran format
        self.full_line_format = {
            'period': (0, 8),      # Columns 1-8
            'freq': (8, 16),       # Columns 9-16
            'direction': (16, 27), # Columns 17-27
            'data_start': 27       # Data starts at column 28
        }
        
        # Each value is approximately 9 characters wide
        self.value_width = 9
    
    def parse_lis_file(self, file_path: str) -> Dict[str, Any]:
        """Parse AQWA .lis file and extract RAO data with data interpretation."""
        content = self._read_file_content(file_path)
        
        # Find all displacement RAO sections
        sections = self._find_displacement_rao_sections(content)
        if not sections:
            raise AQWAFileError("No displacement RAO sections found in the AQWA file")
        
        # Use the last section
        last_section = sections[-1]
        
        # Parse the RAO data with enhanced interpretation
        rao_data = self._parse_rao_section_enhanced(last_section)
        
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

    def _read_file_content(self, file_path: str) -> str:
        path = Path(file_path)
        cache_key = str(path.resolve())
        if cache_key not in self._content_cache:
            try:
                self._content_cache[cache_key] = path.read_text(
                    encoding='utf-8', errors='ignore'
                )
            except Exception as exc:  # pragma: no cover - direct IO
                raise AQWAFileError(f"Failed to read file {file_path}: {exc}")
        return self._content_cache[cache_key]
    
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
    
    def _parse_rao_section_enhanced(self, section: str) -> Dict[float, Dict[float, Dict[str, Dict[str, float]]]]:
        """Parse RAO section with enhanced data interpretation.
        
        This method handles the case where continuation lines need to be
        expanded with the period and frequency from the first row, as specified
        in the requirements.
        """
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
        
        for line_idx in range(data_start, len(lines)):
            line = lines[line_idx]
            
            # Skip empty lines and separators
            if not line or line.strip().startswith('-'):
                continue
            
            # Stop at major section boundary
            if '*' in line and len(line.replace('*', '').replace(' ', '').replace('\t', '')) < 5:
                break
            
            # Check if line is long enough
            if len(line) < 27:
                continue
            
            try:
                # Check if this is a full line or continuation
                period_str = line[self.full_line_format['period'][0]:self.full_line_format['period'][1]].strip()
                freq_str = line[self.full_line_format['freq'][0]:self.full_line_format['freq'][1]].strip()
                
                if period_str and freq_str:
                    # This is a full line with period and frequency
                    period = float(period_str)
                    freq = float(freq_str)
                    direction_str = line[self.full_line_format['direction'][0]:self.full_line_format['direction'][1]].strip()
                    direction = float(direction_str)
                    
                    current_freq = freq
                    current_period = period
                    
                    # Extract DOF values
                    dof_values = self._extract_dof_values_fixed(line, self.full_line_format['data_start'])
                    
                    if dof_values:
                        if freq not in rao_data:
                            rao_data[freq] = {}
                        rao_data[freq][direction] = dof_values
                    
                else:
                    # This is a continuation line - apply data interpretation
                    if current_freq is not None and current_period is not None:
                        # Parse the continuation line and expand it with current period/freq
                        expanded_data = self._interpret_continuation_line(
                            line, current_period, current_freq
                        )
                        
                        if expanded_data:
                            direction, dof_values = expanded_data
                            if current_freq not in rao_data:
                                rao_data[current_freq] = {}
                            rao_data[current_freq][direction] = dof_values
                
            except (ValueError, IndexError):
                # Skip lines that can't be parsed
                continue
        
        return rao_data
    
    def _interpret_continuation_line(self, line: str, period: float, freq: float) -> Optional[Tuple[float, Dict[str, Dict[str, float]]]]:
        """Interpret a continuation line by expanding it with period/freq data.
        
        This implements the requirement where abbreviated rows like:
                    -135.00    0.7173  -88.71  ...
        should be interpreted as:
           40.00   0.157   -135.00    0.7173  -88.71  ...
        
        Args:
            line: The continuation line to interpret
            period: Period value from the first row
            freq: Frequency value from the first row
            
        Returns:
            Tuple of (direction, dof_values) or None if parsing fails
        """
        line_stripped = line.strip()
        if not line_stripped:
            return None
        
        # Parse the direction from the continuation line
        # It should be the first numeric value, typically around column 20-27
        parts = line_stripped.split()
        if not parts or not parts[0]:
            return None
        
        try:
            direction = float(parts[0])
            
            # Sanity check: direction should be between -180 and 360
            if not (-180 <= direction <= 360):
                return None
            
            # Extract DOF values starting from the expected data position
            dof_values = self._extract_dof_values_fixed(line, self.full_line_format['data_start'])
            
            if not dof_values:
                return None
            
            return direction, dof_values
            
        except ValueError:
            return None
    
    def _extract_dof_values_fixed(self, line: str, start_pos: int) -> Optional[Dict[str, Dict[str, float]]]:
        """Extract DOF values using fixed-width format."""
        if len(line) < start_pos:
            return None
        
        data_part = line[start_pos:]
        dof_names = list(DEFAULT_DOF_LABELS)
        
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
            else:
                # Empty field, use 0.0
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

    def _extract_section_lines(
        self,
        content: str,
        header_pattern: str,
        stop_keywords: Optional[Sequence[str]] = None,
    ) -> List[str]:
        match = re.search(header_pattern, content, re.IGNORECASE)
        if not match:
            return []

        remaining = content[match.end():].splitlines()
        stop_keywords = stop_keywords or (
            "HYDRODYNAMIC",
            "WAVE",
            "END OF",
            "RESULTS",
            "OPTIONS",
        )

        section: List[str] = []
        blank_streak = 0
        for raw_line in remaining:
            line = raw_line.rstrip()
            stripped = line.strip()

            if not stripped:
                blank_streak += 1
                if blank_streak > 5 and section:
                    break
                section.append(line)
                continue

            blank_streak = 0
            upper = stripped.upper()
            if stripped.startswith('*') and section:
                break
            if any(keyword in upper for keyword in stop_keywords) and section:
                break

            section.append(line)

        return section

    def _parse_frequency_matrix_section(self, lines: List[str]) -> Dict[str, Any]:
        if not lines:
            empty = np.empty((0, 6, 6))
            return {"frequencies": np.array([]), "matrices": empty, "dofs": DEFAULT_DOF_LABELS}

        frequencies: List[float] = []
        matrices: List[List[float]] = []
        buffer: List[float] = []
        current_freq: Optional[float] = None

        for raw_line in lines:
            tokens = FLOAT_PATTERN.findall(raw_line)
            if not tokens:
                continue

            line_starts_with_number = raw_line.lstrip()[:1].isdigit()
            if line_starts_with_number and len(tokens) >= 2:
                if current_freq is not None and len(buffer) >= 36:
                    matrices.append(buffer[:36])
                    frequencies.append(current_freq)
                buffer = []
                try:
                    current_freq = float(tokens[1])
                except ValueError:
                    current_freq = None
                buffer.extend(float(val) for val in tokens[2:])
            else:
                buffer.extend(float(val) for val in tokens)

            if current_freq is not None and len(buffer) >= 36:
                matrices.append(buffer[:36])
                frequencies.append(current_freq)
                buffer = []
                current_freq = None

        if current_freq is not None and len(buffer) >= 36:
            matrices.append(buffer[:36])
            frequencies.append(current_freq)

        if not matrices:
            empty = np.empty((0, 6, 6))
            return {"frequencies": np.array([]), "matrices": empty, "dofs": DEFAULT_DOF_LABELS}

        matrices_array = np.asarray(matrices, dtype=float)
        if matrices_array.shape[1] != 36:
            matrices_array = matrices_array[:, :36]
        matrices_array = matrices_array.reshape(-1, 6, 6)

        return {
            "frequencies": np.asarray(frequencies, dtype=float),
            "matrices": matrices_array,
            "dofs": DEFAULT_DOF_LABELS,
        }

    def _parse_square_matrix(self, lines: List[str]) -> np.ndarray:
        rows: List[List[float]] = []
        for raw_line in lines:
            floats = FLOAT_PATTERN.findall(raw_line)
            if len(floats) < 1:
                continue
            try:
                row = [float(val) for val in floats[:6]]
            except ValueError:
                continue
            rows.append(row)

        if not rows:
            return np.empty((0, 0))

        matrix = np.asarray(rows, dtype=float)
        if matrix.shape[1] < 6:
            padding = 6 - matrix.shape[1]
            matrix = np.pad(matrix, ((0, 0), (0, padding)))
        if matrix.shape[0] < 6:
            padding = 6 - matrix.shape[0]
            matrix = np.pad(matrix, ((0, padding), (0, 0)))
        return matrix[:6, :6]

    def _parse_drag_table_lines(self, lines: List[str]) -> List[Dict[str, Any]]:
        entries: List[Dict[str, Any]] = []
        current_structure = None
        for raw_line in lines:
            stripped = raw_line.strip()
            if not stripped or stripped.startswith('-'):
                continue

            parts = stripped.split()
            if len(parts) < 3:
                continue

            element_id = self._safe_int(parts[0])
            structure_id = self._safe_int(parts[1])
            if structure_id is not None:
                current_structure = structure_id

            floats = [self._safe_float(value) for value in parts[2:]]
            floats = [val for val in floats if val is not None]

            area = floats[3] if len(floats) > 3 else 0.0
            cd_normal = floats[4] if len(floats) > 4 else 0.0
            cd_axial = floats[5] if len(floats) > 5 else 0.0
            cm = floats[6] if len(floats) > 6 else 0.0
            centroid = tuple(floats[0:3]) if len(floats) >= 3 else (0.0, 0.0, 0.0)

            entry = {
                "element_id": element_id,
                "structure_id": current_structure,
                "centroid": centroid,
                "area": area,
                "cd_normal": cd_normal,
                "cd_axial": cd_axial,
                "cm": cm,
                "raw": stripped,
            }
            entries.append(entry)

        return entries

    def _parse_external_damping_lines(self, lines: List[str]) -> Dict[str, Any]:
        contributions: Dict[str, List[float]] = {
            "B33": [],
            "B44": [],
            "B55": [],
        }

        for raw_line in lines:
            tokens = raw_line.split()
            if len(tokens) < 4:
                continue
            floats: List[float] = []
            for token in tokens:
                value = self._safe_float(token)
                if value is not None:
                    floats.append(value)
            if len(floats) < 3:
                continue
            if len(floats) > 0:
                contributions["B33"].append(floats[0])
            if len(floats) > 1:
                contributions["B44"].append(floats[1])
            if len(floats) > 2:
                contributions["B55"].append(floats[2])

        return {
            "B33": np.asarray(contributions["B33"], dtype=float) if contributions["B33"] else np.array([]),
            "B44": np.asarray(contributions["B44"], dtype=float) if contributions["B44"] else np.array([]),
            "B55": np.asarray(contributions["B55"], dtype=float) if contributions["B55"] else np.array([]),
            "raw": lines,
        }

    def _parse_percent_critical_section(self, lines: List[str]) -> Dict[str, Any]:
        frequencies: List[float] = []
        perc: Dict[str, List[float]] = {"B33": [], "B44": [], "B55": []}

        for raw_line in lines:
            tokens = FLOAT_PATTERN.findall(raw_line)
            if len(tokens) < 8:
                continue
            try:
                freq = float(tokens[1])
            except ValueError:
                continue
            frequencies.append(freq)
            try:
                perc["B33"].append(float(tokens[4]))
                perc["B44"].append(float(tokens[5]))
                perc["B55"].append(float(tokens[6]))
            except (ValueError, IndexError):
                continue

        return {
            "frequencies": np.asarray(frequencies, dtype=float),
            "B33": np.asarray(perc["B33"], dtype=float),
            "B44": np.asarray(perc["B44"], dtype=float),
            "B55": np.asarray(perc["B55"], dtype=float),
            "raw": lines,
        }

    def _safe_float(self, value: str) -> Optional[float]:
        try:
            return float(value)
        except ValueError:
            return None

    def _safe_int(self, value: str) -> Optional[int]:
        try:
            return int(value)
        except ValueError:
            return None

    # ------------------------------------------------------------------
    # Additional extraction helpers required for viscous damping workflow
    # ------------------------------------------------------------------

    def extract_damping_matrices(self, file_path: str) -> Dict[str, Any]:
        content = self._read_file_content(file_path)
        lines = self._extract_section_lines(
            content,
            r"DAMPING-VARIATION\s+WITH\s+WAVE\s+PERIOD/FREQUENCY",
        )
        return self._parse_frequency_matrix_section(lines)

    def extract_added_mass_matrices(self, file_path: str) -> Dict[str, Any]:
        content = self._read_file_content(file_path)
        lines = self._extract_section_lines(
            content,
            r"ADDED-MASS-VARIATION\s+WITH\s+WAVE\s+PERIOD/FREQUENCY",
        )
        return self._parse_frequency_matrix_section(lines)

    def extract_added_mass_at_infinite_frequency(self, file_path: str) -> Dict[str, Any]:
        content = self._read_file_content(file_path)
        lines = self._extract_section_lines(
            content,
            r"ADDED\s+MASS\s+AT\s+INFINITE\s+FREQUENCY",
        )
        matrix = self._parse_square_matrix(lines)
        diagonal = {}
        if matrix.size:
            for idx, dof in enumerate(DEFAULT_DOF_LABELS):
                diagonal[f"A{idx+1}{idx+1}"] = float(matrix[idx, idx])
        return {"matrix": matrix, "dofs": DEFAULT_DOF_LABELS, "diagonal": diagonal}

    def extract_drag_table(self, file_path: str) -> List[Dict[str, Any]]:
        content = self._read_file_content(file_path)
        lines = self._extract_section_lines(
            content,
            r"GEOMETRY\s+DRAG\s+ADDED\s+MASS",
            stop_keywords=("HYDRODYNAMIC", "END OF", "WAVE", "MASS"),
        )
        return self._parse_drag_table_lines(lines)

    def extract_external_damping(self, file_path: str) -> Dict[str, Any]:
        content = self._read_file_content(file_path)
        lines = self._extract_section_lines(
            content,
            r"H/I\s+DAMPING\s+FOR\s+FORCE\s+ON\s+STR#",
        )
        return self._parse_external_damping_lines(lines)

    def extract_percent_critical_damping(self, file_path: str) -> Dict[str, Any]:
        content = self._read_file_content(file_path)
        lines = self._extract_section_lines(
            content,
            r"APPROXIMATE\s+PERCENTAGE\s+CRITICAL\s+DAMPING",
        )
        return self._parse_percent_critical_section(lines)

    def extract_restoring_coefficients(self, file_path: str) -> Dict[str, Any]:
        content = self._read_file_content(file_path)
        lines = self._extract_section_lines(
            content,
            r"HYDROSTATIC\s+RESTORING\s+COEFFICIENTS",
        )
        matrix = self._parse_square_matrix(lines)
        diagonal: Dict[str, float] = {}
        if matrix.size:
            for idx, dof in enumerate(DEFAULT_DOF_LABELS):
                diagonal[f"C{idx+1}{idx+1}"] = float(matrix[idx, idx])
        return {"matrix": matrix, "dofs": DEFAULT_DOF_LABELS, "diagonal": diagonal}
    
    def expand_abbreviated_data(self, raw_data: str) -> str:
        """Expand abbreviated data format to full format.
        
        This is a utility method that demonstrates the data interpretation
        concept. It takes the abbreviated format and expands it to the full format.
        
        Args:
            raw_data: Raw data string in abbreviated format
            
        Returns:
            Expanded data string in full format
        """
        lines = raw_data.strip().split('\n')
        if not lines:
            return raw_data
        
        expanded_lines = []
        current_period = None
        current_freq = None
        
        for line in lines:
            if not line.strip():
                expanded_lines.append(line)
                continue
            
            # Check if this line starts with period/frequency (first non-whitespace chars are digits)
            stripped = line.lstrip()
            
            # Check if line has period and frequency at the start (full line)
            # A full line should have period in the first 8 columns
            period_str = line[0:8].strip() if len(line) >= 8 else ""
            freq_str = line[8:16].strip() if len(line) >= 16 else ""
            
            if period_str and freq_str:
                try:
                    # This is a full line with period and frequency
                    period = float(period_str)
                    freq = float(freq_str)
                    current_period = period
                    current_freq = freq
                    expanded_lines.append(line)
                except ValueError:
                    # Not a valid full line
                    expanded_lines.append(line)
            else:
                # This is likely a continuation line - try to expand it
                if current_period is not None and current_freq is not None:
                    # Find the first number (direction) in the line
                    parts = stripped.split()
                    if parts:
                        try:
                            direction = float(parts[0])
                            # Extract the data portion after the direction
                            # Find position of direction in original line
                            dir_pos = line.find(parts[0])
                            if dir_pos >= 0:
                                # Extract everything after the direction
                                data_portion = line[dir_pos + len(parts[0]):]
                                # Create expanded line with proper formatting
                                expanded_line = f"{current_period:8.2f}{current_freq:8.3f}{direction:11.2f}{data_portion}"
                                expanded_lines.append(expanded_line)
                            else:
                                expanded_lines.append(line)
                        except ValueError:
                            expanded_lines.append(line)
                else:
                    expanded_lines.append(line)
        
        return '\n'.join(expanded_lines)
    
    def export_to_csv(self, rao_data: Dict[str, Any], output_format: str = 'step2') -> str:
        """Export RAO data to CSV format.
        
        Args:
            rao_data: RAO data dictionary from parse_lis_file
            output_format: 'step1' (abbreviated - empty period/freq) or 'step2' (full - all period/freq)
            
        Returns:
            CSV formatted string
        """
        frequencies = rao_data['frequencies']
        headings = rao_data['headings'] 
        raos = rao_data['raos']
        
        # Create header
        header = "Period,Frequency,Direction,Surge Amplitude,Surge Phase,Sway Amplitude,Sway Phase,Heave Amplitude,Heave Phase,Roll Amplitude,Roll Phase,Pitch Amplitude,Pitch Phase,Yaw Amplitude,Yaw Phase"
        
        lines = [header]
        
        for i, freq in enumerate(frequencies):
            period = 2 * np.pi / freq if freq > 0 else 0
            
            for j, heading in enumerate(headings):
                # Get RAO values for this frequency/heading
                surge_amp = raos['surge']['amplitude'][i, j]
                surge_phase = raos['surge']['phase'][i, j]
                sway_amp = raos['sway']['amplitude'][i, j] 
                sway_phase = raos['sway']['phase'][i, j]
                heave_amp = raos['heave']['amplitude'][i, j]
                heave_phase = raos['heave']['phase'][i, j]
                roll_amp = raos['roll']['amplitude'][i, j]
                roll_phase = raos['roll']['phase'][i, j]
                pitch_amp = raos['pitch']['amplitude'][i, j]
                pitch_phase = raos['pitch']['phase'][i, j]
                yaw_amp = raos['yaw']['amplitude'][i, j]
                yaw_phase = raos['yaw']['phase'][i, j]
                
                if output_format == 'step1' and j > 0:
                    # Abbreviated format - empty period and frequency after first heading
                    line = f",,{heading},{surge_amp},{surge_phase},{sway_amp},{sway_phase},{heave_amp},{heave_phase},{roll_amp},{roll_phase},{pitch_amp},{pitch_phase},{yaw_amp},{yaw_phase}"
                else:
                    # Full format - include period and frequency on every line
                    line = f"{period},{freq},{heading},{surge_amp},{surge_phase},{sway_amp},{sway_phase},{heave_amp},{heave_phase},{roll_amp},{roll_phase},{pitch_amp},{pitch_phase},{yaw_amp},{yaw_phase}"
                
                lines.append(line)
        
        return '\n'.join(lines)