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
        matches = list(pattern.finditer(content))

        for i, match in enumerate(matches):
            start_pos = match.start()

            # Find section end
            if i < len(matches) - 1:
                # Not last section - ends where next section of same type starts
                end_pos = matches[i + 1].start()
            else:
                # Last section - find end using end patterns OR next different RAO type
                end_pos = len(content)
                section_content = content[start_pos:]

                # First check if there's a velocity or acceleration section after this
                # (which would indicate end of displacement section)
                vel_match = self.velocity_pattern.search(section_content)
                acc_match = self.acceleration_pattern.search(section_content)

                earliest_end = None
                if vel_match:
                    earliest_end = vel_match.start()
                if acc_match and (earliest_end is None or acc_match.start() < earliest_end):
                    earliest_end = acc_match.start()

                if earliest_end is not None:
                    end_pos = start_pos + earliest_end
                else:
                    # Use generic end patterns
                    for end_pattern in self.section_end_patterns:
                        end_match = re.search(end_pattern, section_content)
                        if end_match:
                            end_pos = start_pos + end_match.start()
                            break

            section = content[start_pos:end_pos]
            sections.append(section)

        return sections

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

        # Skip header, separator, units, separator (4 lines)
        data_start = header_idx + 4

        rao_data = {}
        current_freq = None
        current_period = None
        current_heading = None  # Track current heading for inheritance

        for line_idx in range(data_start, len(lines)):
            line = lines[line_idx]

            # Skip empty lines
            if not line.strip():
                continue

            # Skip separator lines (lines with only dashes, spaces, and maybe other chars)
            # But NOT data lines that happen to start with negative numbers
            stripped = line.strip()
            if stripped.startswith('-') and not any(c.isdigit() for c in stripped[:15]):
                # It's a separator line like "----" or "-----  -----"
                continue

            # Stop at section boundary
            if '*' in line and len(line.replace('*', '').replace(' ', '')) < 5:
                break

            # Must be long enough for data
            if len(line) < 27:
                continue

            try:
                # Check for data line with period + frequency
                period_str = line[0:8].strip()
                freq_str = line[8:16].strip()
                direction_str = line[16:27].strip()

                if period_str and freq_str:
                    # Line has period and frequency - this is a new period/frequency block
                    period = float(period_str)
                    freq = float(freq_str)

                    # Check if direction is provided on this line
                    if direction_str:
                        # New heading block - direction explicitly provided
                        try:
                            direction = float(direction_str)
                            if -180 <= direction <= 360:
                                current_heading = direction  # Track this heading
                            else:
                                continue  # Invalid direction, skip this line
                        except ValueError:
                            continue  # Not a valid direction, skip
                    else:
                        # Direction column is blank - inherit from current_heading
                        if current_heading is None:
                            continue  # No heading to inherit, skip this line
                        direction = current_heading

                    current_freq = freq
                    current_period = period

                    dof_values = self._extract_dof_values(line, 27)

                    if dof_values:
                        if freq not in rao_data:
                            rao_data[freq] = {}
                        rao_data[freq][direction] = dof_values

                elif not period_str and not freq_str and direction_str:
                    # Continuation line: no period/freq, but has heading
                    # This is additional heading data for the current period/frequency
                    if current_freq is None:
                        continue  # No frequency context, skip

                    try:
                        direction = float(direction_str)
                        if not (-180 <= direction <= 360):
                            continue  # Invalid direction
                    except ValueError:
                        continue  # Not a valid direction

                    current_heading = direction  # Update current heading

                    dof_values = self._extract_dof_values(line, 27)

                    if dof_values:
                        if current_freq not in rao_data:
                            rao_data[current_freq] = {}
                        rao_data[current_freq][direction] = dof_values

            except (ValueError, IndexError):
                continue

        return rao_data if rao_data else None

    def _extract_dof_values(self, line: str, start_pos: int) -> Optional[Dict]:
        """Extract 6-DOF amplitude and phase values from line.

        Returns:
            Dictionary: {dof_name: {'amplitude': val, 'phase': val}}
        """
        if len(line) < start_pos:
            return None

        data_part = line[start_pos:]
        dof_names = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']

        values = []
        pos = 0

        # Extract 12 values (6 DOF * 2 for amp/phase)
        while pos < len(data_part) and len(values) < 12:
            val_str = data_part[pos:pos + self.value_width].strip()
            if val_str:
                try:
                    values.append(float(val_str))
                except ValueError:
                    values.append(0.0)
            else:
                values.append(0.0)
            pos += self.value_width

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
