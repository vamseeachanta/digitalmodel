"""Comprehensive ANSYS AQWA .lis file parser.

This parser supports extraction of:
- Displacement RAOs
- Velocity RAOs
- Acceleration RAOs

All parsing is done via text file reading without requiring AQWA license.
"""

import re
from typing import Dict, List, Tuple, Optional, Set
from pathlib import Path
import numpy as np

from ..models.rao_data import (
    UnifiedRAOData,
    DisplacementRAO,
    VelocityRAO,
    AccelerationRAO,
    DOFData,
    RAOMetadata,
    SourceFormat,
    RAOType,
    create_empty_rao_arrays
)


class AQWAParseError(Exception):
    """Exception raised for AQWA file parsing errors."""
    pass


class AQWALISParser:
    """Comprehensive parser for ANSYS AQWA .lis output files.

    This parser extracts displacement, velocity, and acceleration RAOs from
    AQWA .lis files using pure text parsing (no license required).

    Features:
    - Displacement RAO extraction
    - Velocity RAO extraction (NEW)
    - Acceleration RAO extraction (NEW)
    - Fixed-width Fortran format support
    - Continuation line handling
    - Multiple RAO section detection (uses last occurrence)

    Example:
        >>> parser = AQWALISParser()
        >>> rao_data = parser.parse_lis_file('vessel_analysis.lis')
        >>> print(rao_data.get_available_types())
        [RAOType.DISPLACEMENT, RAOType.VELOCITY, RAOType.ACCELERATION]
    """

    def __init__(self):
        """Initialize AQWA LIS parser with patterns for all RAO types."""

        # Displacement RAO pattern (exclude VEL and ACC)
        self.displacement_pattern = re.compile(
            r'(?<!VEL\s)(?<!ACC\s)R\.A\.O\.S-VARIATION\s+WITH\s+WAVE\s+DIRECTION',
            re.IGNORECASE
        )

        # Velocity RAO pattern
        self.velocity_pattern = re.compile(
            r'VEL\s+R\.A\.O\.S-VARIATION\s+WITH\s+WAVE\s+DIRECTION',
            re.IGNORECASE
        )

        # Acceleration RAO pattern
        self.acceleration_pattern = re.compile(
            r'ACC\s+R\.A\.O\.S-VARIATION\s+WITH\s+WAVE\s+DIRECTION',
            re.IGNORECASE
        )

        self.any_rao_pattern = re.compile(
            r'(?:VEL\s+|ACC\s+)?R\.A\.O\.S-VARIATION\s+WITH\s+WAVE\s+DIRECTION',
            re.IGNORECASE
        )

        self.number_pattern = re.compile(
            r'[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[EeDd][+-]?\d+)?'
        )

        # Header pattern for data table
        self.header_pattern = re.compile(
            r'PERIOD\s+FREQ(?:UENCY)?\s+DIRECTION\s+X\s+Y\s+Z\s+RX\s+RY\s+RZ',
            re.IGNORECASE
        )

        # Fixed column positions (Fortran format)
        self.full_line_format = {
            'period': (0, 8),
            'freq': (8, 16),
            'direction': (16, 27),
            'data_start': 27
        }

        self.value_width = 9  # Each value ~9 characters wide

        # Common section ending patterns
        self.section_end_patterns = [
            r'\n\s*\*{10,}',
            r'\n\s*VEL\s+R\.A\.O\.S',
            r'\n\s*ACC\s+R\.A\.O\.S',
            r'\n\s*H Y D R O D Y N A M I C',
            r'\n\s*WAVE DRIFT',
            r'\n\s*END OF'
        ]

    def parse_lis_file(self,
                      file_path: str,
                      extract_displacement: bool = True,
                      extract_velocity: bool = True,
                      extract_acceleration: bool = True) -> UnifiedRAOData:
        """Parse AQWA .lis file and extract all requested RAO types.

        Args:
            file_path: Path to AQWA .lis file
            extract_displacement: Extract displacement RAOs
            extract_velocity: Extract velocity RAOs
            extract_acceleration: Extract acceleration RAOs

        Returns:
            UnifiedRAOData containing all available RAO types

        Raises:
            AQWAParseError: If file cannot be parsed or no RAO data found
        """
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
        except Exception as e:
            raise AQWAParseError(f"Failed to read file {file_path}: {str(e)}")

        # Create metadata
        metadata = RAOMetadata(
            source_file=str(file_path),
            source_format=SourceFormat.AQWA_LIS,
            vessel_name=self._extract_vessel_name(content),
            license_required=False
        )

        # Initialize unified data container
        unified_data = UnifiedRAOData(metadata=metadata)

        # Extract each RAO type if requested
        if extract_displacement:
            unified_data.displacement = self._extract_displacement_raos(content, metadata)

        if extract_velocity:
            unified_data.velocity = self._extract_velocity_raos(content, metadata)

        if extract_acceleration:
            unified_data.acceleration = self._extract_acceleration_raos(content, metadata)

        # Validate that at least one RAO type was extracted
        if not unified_data.get_available_types():
            raise AQWAParseError(
                f"No RAO data found in {file_path}. "
                f"File may not contain RAO analysis results."
            )

        return unified_data

    def _extract_vessel_name(self, content: str) -> str:
        """Extract vessel name from AQWA file content."""
        # Look for common vessel name patterns
        patterns = [
            r'VESSEL\s+NAME\s*:\s*(\w+)',
            r'STRUCTURE\s+NAME\s*:\s*(\w+)',
            r'MODEL\s+NAME\s*:\s*(\w+)'
        ]

        for pattern in patterns:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                return match.group(1)

        return "Unknown Vessel"

    def _extract_displacement_raos(self,
                                  content: str,
                                  metadata: RAOMetadata) -> Optional[DisplacementRAO]:
        """Extract displacement RAOs from content."""
        sections = self._find_rao_sections(content, self.displacement_pattern)

        if not sections:
            return None

        # Parse ALL sections and merge data (CRITICAL FIX)
        # AQWA output may span multiple pages/sections
        merged_data = {}
        for section in sections:
            section_data = self._parse_rao_section(section)
            if section_data:
                # Merge this section's data into the combined dataset
                for freq, freq_data in section_data.items():
                    if freq not in merged_data:
                        merged_data[freq] = {}
                    merged_data[freq].update(freq_data)

        if not merged_data:
            return None

        # Convert to DisplacementRAO
        return self._create_displacement_rao(merged_data, metadata)

    def _extract_velocity_raos(self,
                              content: str,
                              metadata: RAOMetadata) -> Optional[VelocityRAO]:
        """Extract velocity RAOs from content."""
        sections = self._find_rao_sections(content, self.velocity_pattern)

        if not sections:
            return None

        # Parse ALL sections and merge data
        merged_data = {}
        for section in sections:
            section_data = self._parse_rao_section(section)
            if section_data:
                for freq, freq_data in section_data.items():
                    if freq not in merged_data:
                        merged_data[freq] = {}
                    merged_data[freq].update(freq_data)

        if not merged_data:
            return None

        # Convert to VelocityRAO
        return self._create_velocity_rao(merged_data, metadata)

    def _extract_acceleration_raos(self,
                                  content: str,
                                  metadata: RAOMetadata) -> Optional[AccelerationRAO]:
        """Extract acceleration RAOs from content."""
        sections = self._find_rao_sections(content, self.acceleration_pattern)

        if not sections:
            return None

        # Parse ALL sections and merge data
        merged_data = {}
        for section in sections:
            section_data = self._parse_rao_section(section)
            if section_data:
                for freq, freq_data in section_data.items():
                    if freq not in merged_data:
                        merged_data[freq] = {}
                    merged_data[freq].update(freq_data)

        if not merged_data:
            return None

        # Convert to AccelerationRAO
        return self._create_acceleration_rao(merged_data, metadata)

    def _find_rao_sections(self, content: str, pattern: re.Pattern) -> List[str]:
        """Find all RAO sections matching the given pattern."""
        sections = []
        for match in pattern.finditer(content):
            start_pos = match.start()
            end_pos = self._find_section_end(content, match)
            sections.append(content[start_pos:end_pos])

        return sections

    def _find_section_end(self, content: str, match: re.Match) -> int:
        """Find the earliest boundary after a RAO section marker."""
        candidates = []
        next_rao = self.any_rao_pattern.search(content, match.end())
        if next_rao:
            candidates.append(next_rao.start())

        section_content = content[match.start():]
        for end_pattern in self.section_end_patterns:
            end_match = re.search(end_pattern, section_content, re.IGNORECASE)
            if end_match and end_match.start() > 0:
                candidates.append(match.start() + end_match.start())

        return min(candidates) if candidates else len(content)

    def _parse_rao_section(self, section: str) -> Optional[Dict]:
        """Parse a RAO section and extract structured data.

        Returns:
            Dictionary: {frequency: {heading: {dof: {'amplitude': val, 'phase': val}}}}
        """
        lines = section.split('\n')

        # Find header
        header_idx = -1
        for i, line in enumerate(lines):
            if self.header_pattern.search(line):
                header_idx = i
                break

        if header_idx == -1:
            return None

        rao_data = {}
        current_freq = None
        current_period = None
        current_heading = None  # Track current heading for inheritance

        for line in lines[header_idx + 1:]:
            parsed = self._parse_rao_data_line(
                line,
                current_period,
                current_freq,
                current_heading
            )
            if parsed is None:
                continue

            period, freq, direction, dof_values = parsed
            current_period = period
            current_freq = freq
            current_heading = direction
            rao_data.setdefault(freq, {})[direction] = dof_values

        return rao_data if rao_data else None

    def _parse_rao_data_line(self,
                             line: str,
                             current_period: Optional[float],
                             current_freq: Optional[float],
                             current_heading: Optional[float]) -> Optional[Tuple]:
        """Parse one RAO table row in full or continuation form."""
        if self._should_skip_table_line(line):
            return None

        values = self._numeric_values(line)
        if len(values) >= 15:
            period, freq, direction = values[:3]
            dof_numbers = values[3:15]
        elif len(values) >= 14 and current_heading is not None:
            period, freq = values[:2]
            direction = current_heading
            dof_numbers = values[2:14]
        elif len(values) >= 13 and current_freq is not None:
            period = current_period
            freq = current_freq
            direction = values[0]
            dof_numbers = values[1:13]
        else:
            return None

        if period is None or not self._is_valid_direction(direction):
            return None

        return period, freq, direction, self._extract_dof_values(dof_numbers)

    def _should_skip_table_line(self, line: str) -> bool:
        """Return True for header, unit, separator, and boundary lines."""
        stripped = line.strip()
        if not stripped:
            return True
        if '*' in line and len(line.replace('*', '').replace(' ', '')) < 5:
            return True
        if set(stripped) <= {'-'}:
            return True
        return stripped.upper().startswith(('PERIOD', '(SECS)', '(RAD/S)'))

    def _numeric_values(self, line: str) -> List[float]:
        """Extract numeric tokens, including scientific Fortran notation."""
        values = []
        for match in self.number_pattern.finditer(line):
            token = match.group(0).replace('D', 'E').replace('d', 'e')
            values.append(float(token))
        return values

    def _is_valid_direction(self, direction: float) -> bool:
        """Check whether a parsed wave direction is in the AQWA heading range."""
        return -180 <= direction <= 360

    def _extract_dof_values(self, values: List[float]) -> Dict:
        """Extract 6-DOF amplitude and phase values from line.

        Returns:
            Dictionary: {dof_name: {'amplitude': val, 'phase': val}}
        """
        dof_names = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']

        # Ensure we have 12 values
        while len(values) < 12:
            values.append(0.0)

        # Build result dictionary
        result = {}
        for i, dof in enumerate(dof_names):
            amp_idx = i * 2
            phase_idx = i * 2 + 1
            result[dof] = {
                'amplitude': values[amp_idx],
                'phase': values[phase_idx]
            }

        return result

    def _create_displacement_rao(self,
                                parsed_data: Dict,
                                metadata: RAOMetadata) -> DisplacementRAO:
        """Create DisplacementRAO from parsed data."""
        frequencies, headings, dof_arrays = self._convert_to_arrays(parsed_data)

        return DisplacementRAO(
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

    def _create_velocity_rao(self,
                            parsed_data: Dict,
                            metadata: RAOMetadata) -> VelocityRAO:
        """Create VelocityRAO from parsed data."""
        frequencies, headings, dof_arrays = self._convert_to_arrays(parsed_data)

        return VelocityRAO(
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

    def _create_acceleration_rao(self,
                                parsed_data: Dict,
                                metadata: RAOMetadata) -> AccelerationRAO:
        """Create AccelerationRAO from parsed data."""
        frequencies, headings, dof_arrays = self._convert_to_arrays(parsed_data)

        return AccelerationRAO(
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

    def _convert_to_arrays(self, parsed_data: Dict) -> Tuple[np.ndarray, np.ndarray, Dict]:
        """Convert parsed dictionary data to numpy arrays.

        Returns:
            Tuple of (frequencies, headings, dof_arrays)
            where dof_arrays is {dof_name: DOFData}
        """
        # Extract unique frequencies and headings
        frequencies = sorted(list(parsed_data.keys()))
        all_headings: Set[float] = set()
        for freq_data in parsed_data.values():
            all_headings.update(freq_data.keys())
        headings = sorted(list(all_headings))

        # Create empty arrays
        n_freq = len(frequencies)
        n_head = len(headings)
        dof_arrays = create_empty_rao_arrays(n_freq, n_head)

        # Fill arrays
        for i, freq in enumerate(frequencies):
            for j, heading in enumerate(headings):
                if heading in parsed_data[freq]:
                    for dof_name, dof_data in dof_arrays.items():
                        rao_value = parsed_data[freq][heading][dof_name]
                        dof_data.amplitude[i, j] = rao_value['amplitude']
                        dof_data.phase[i, j] = rao_value['phase']

        return (
            np.array(frequencies),
            np.array(headings),
            dof_arrays
        )
