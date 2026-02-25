#!/usr/bin/env python3
"""
AQWA .LIS File Parser

ABOUTME: Specialized parser for extracting hydrodynamic data from AQWA .LIS output files.

This module provides functions to parse AQWA .LIS files and extract:
- RAO (Response Amplitude Operator) data
- Added mass matrices
- Damping matrices
- Frequency and heading information

Version: 1.0.0
Status: Production
"""

from pathlib import Path
from typing import Dict, List, Tuple, Optional
import re
import numpy as np
from loguru import logger


class AQWALISParser:
    """
    Parser for AQWA .LIS files.

    Extracts hydrodynamic coefficients from AQWA diffraction analysis output.
    """

    def __init__(self, lis_file_path: Path):
        """
        Initialize parser with .LIS file path.

        Args:
            lis_file_path: Path to AQWA .LIS file
        """
        self.lis_file = Path(lis_file_path)
        if not self.lis_file.exists():
            raise FileNotFoundError(f"LIS file not found: {lis_file_path}")

        # Read entire file content
        with open(self.lis_file, 'r', encoding='utf-8', errors='ignore') as f:
            self.content = f.read()

        logger.info(f"Loaded AQWA .LIS file: {self.lis_file.name} ({len(self.content)} characters)")

    def find_section(self, section_marker: str) -> Optional[int]:
        """
        Find the starting position of a section in the file.

        Args:
            section_marker: String pattern to search for

        Returns:
            Position in content string, or None if not found
        """
        pos = self.content.find(section_marker)
        if pos == -1:
            logger.warning(f"Section not found: {section_marker}")
            return None
        return pos

    def extract_frequencies_and_periods(self) -> Tuple[List[float], List[float]]:
        """
        Extract wave frequencies and periods from the file.

        Returns:
            Tuple of (frequencies in rad/s, periods in seconds)
        """
        # Find the added mass table section
        section_marker = "ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY"
        pos = self.find_section(section_marker)

        if pos is None:
            raise ValueError("Could not find added mass frequency table")

        # Extract the table (next ~2000 characters should contain it)
        table_text = self.content[pos:pos + 3000]

        # Find all rows with period/frequency data followed by added mass values
        # AQWA format: " 22.00   0.286  2.33E+06  2.93E+07..." (period, freq, then 12 scientific values)
        # Pattern must match: period (decimal), freq (decimal), then at least 6 scientific notation values
        # This ensures we only match actual data rows, not headers or other numeric content
        pattern = r'^\s*([\d.]+)\s+([\d.]+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)'

        periods = []
        frequencies = []

        for match in re.finditer(pattern, table_text, re.MULTILINE):
            period = float(match.group(1))
            freq = float(match.group(2))

            # Sanity check: reasonable values for marine structures
            # Period typically 2-30 seconds, Frequency 0.2-3.5 rad/s
            if 0.1 < freq < 10 and 1.0 < period < 100:
                # Avoid duplicates
                if freq not in frequencies:
                    periods.append(period)
                    frequencies.append(freq)

        if not frequencies:
            raise ValueError("No valid frequency data found in added mass table")

        logger.info(f"Extracted {len(frequencies)} frequencies from {frequencies[0]:.3f} to {frequencies[-1]:.3f} rad/s")

        return frequencies, periods

    def extract_headings(self) -> List[float]:
        """
        Extract wave headings from RAO table.

        Returns:
            List of headings in degrees
        """
        # Find RAO section
        section_marker = "R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY"
        pos = self.find_section(section_marker)

        if pos is None:
            raise ValueError("Could not find RAO section")

        # Extract RAO table
        table_text = self.content[pos:pos + 10000]

        # Find all direction values
        # Format: "  62.83   0.100   -180.00    1.5204  -90.03..."
        pattern = r'\s+[\d.]+\s+[\d.]+\s+([-\d.]+)\s+[\d.]+\s+'

        headings = []

        for match in re.finditer(pattern, table_text):
            heading = float(match.group(1))

            # Only add unique headings
            if heading not in headings and -180 <= heading <= 180:
                headings.append(heading)

        # Sort headings
        headings.sort()

        if not headings:
            raise ValueError("No valid heading data found in RAO table")

        logger.info(f"Extracted {len(headings)} headings from {headings[0]:.1f}° to {headings[-1]:.1f}°")

        return headings

    def parse_added_mass_table(self) -> Dict[float, np.ndarray]:
        """
        Parse added mass table from .LIS file.

        Returns:
            Dictionary mapping frequency (rad/s) to 6x6 added mass matrix
        """
        # Find the added mass table
        section_marker = "ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY"
        pos = self.find_section(section_marker)

        if pos is None:
            raise ValueError("Could not find added mass table")

        table_text = self.content[pos:pos + 2000]

        # Pattern for data rows: period, freq, then 12 matrix values in scientific notation
        # Format: " 62.83   0.100  1.95E+06  2.60E+07  1.80E+08..."
        # Must contain 'E' to distinguish from divider lines
        pattern = r'^\s+([\d.]+)\s+([\d.]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)'

        added_mass_dict = {}

        for match in re.finditer(pattern, table_text, re.MULTILINE):
            # Check if line contains scientific notation (E format)
            line = match.group(0)
            if 'E' not in line.upper():
                continue

            freq = float(match.group(2))

            # Extract 12 coefficients: M11, M22, M33, M44, M55, M66, M13, M15, M24, M26, M35, M46
            coeffs = [float(match.group(i)) for i in range(3, 15)]

            # Build 6x6 symmetric matrix
            matrix = np.zeros((6, 6))

            # Diagonal and selected off-diagonal terms
            matrix[0, 0] = coeffs[0]   # M11
            matrix[1, 1] = coeffs[1]   # M22
            matrix[2, 2] = coeffs[2]   # M33
            matrix[3, 3] = coeffs[3]   # M44
            matrix[4, 4] = coeffs[4]   # M55
            matrix[5, 5] = coeffs[5]   # M66

            # Off-diagonal coupling terms
            matrix[0, 2] = matrix[2, 0] = coeffs[6]   # M13
            matrix[0, 4] = matrix[4, 0] = coeffs[7]   # M15
            matrix[1, 3] = matrix[3, 1] = coeffs[8]   # M24
            matrix[1, 5] = matrix[5, 1] = coeffs[9]   # M26
            matrix[2, 4] = matrix[4, 2] = coeffs[10]  # M35
            matrix[3, 5] = matrix[5, 3] = coeffs[11]  # M46

            added_mass_dict[freq] = matrix

        if not added_mass_dict:
            raise ValueError("No valid added mass data found")

        logger.info(f"Parsed added mass matrices for {len(added_mass_dict)} frequencies")

        return added_mass_dict

    def parse_damping_table(self) -> Dict[float, np.ndarray]:
        """
        Parse damping table from .LIS file.

        Returns:
            Dictionary mapping frequency (rad/s) to 6x6 damping matrix
        """
        # Find the damping table
        section_marker = "DAMPING-VARIATION WITH WAVE PERIOD/FREQUENCY"
        pos = self.find_section(section_marker)

        if pos is None:
            raise ValueError("Could not find damping table")

        table_text = self.content[pos:pos + 2000]

        # Same pattern as added mass - must contain scientific notation
        pattern = r'^\s+([\d.]+)\s+([\d.]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)\s+([\d.E+-]+)'

        damping_dict = {}

        for match in re.finditer(pattern, table_text, re.MULTILINE):
            # Check if line contains scientific notation (E format)
            line = match.group(0)
            if 'E' not in line.upper():
                continue

            freq = float(match.group(2))

            # Extract 12 coefficients: C11, C22, C33, C44, C55, C66, C13, C15, C24, C26, C35, C46
            coeffs = [float(match.group(i)) for i in range(3, 15)]

            # Build 6x6 symmetric matrix
            matrix = np.zeros((6, 6))

            # Diagonal and selected off-diagonal terms
            matrix[0, 0] = coeffs[0]   # C11
            matrix[1, 1] = coeffs[1]   # C22
            matrix[2, 2] = coeffs[2]   # C33
            matrix[3, 3] = coeffs[3]   # C44
            matrix[4, 4] = coeffs[4]   # C55
            matrix[5, 5] = coeffs[5]   # C66

            # Off-diagonal coupling terms
            matrix[0, 2] = matrix[2, 0] = coeffs[6]   # C13
            matrix[0, 4] = matrix[4, 0] = coeffs[7]   # C15
            matrix[1, 3] = matrix[3, 1] = coeffs[8]   # C24
            matrix[1, 5] = matrix[5, 1] = coeffs[9]   # C26
            matrix[2, 4] = matrix[4, 2] = coeffs[10]  # C35
            matrix[3, 5] = matrix[5, 3] = coeffs[11]  # C46

            damping_dict[freq] = matrix

        if not damping_dict:
            raise ValueError("No valid damping data found")

        logger.info(f"Parsed damping matrices for {len(damping_dict)} frequencies")

        return damping_dict

    def parse_rao_table(self) -> Dict[Tuple[float, float], Dict[str, Tuple[float, float]]]:
        """
        Parse RAO table from .LIS file.

        Returns:
            Dictionary mapping (frequency, heading) to DOF data
            DOF data is dict: {'surge': (amp, phase), 'sway': (amp, phase), ...}
        """
        # Find ALL DISPLACEMENT RAO sections (NOT velocity or acceleration!)
        # AQWA .LIS files contain 3 types:
        # - "R.A.O.S-VARIATION..." (displacement - what we want!)
        # - "VEL R.A.O.S-VARIATION..." (velocity)
        # - "ACC R.A.O.S-VARIATION..." (acceleration)

        # We must match EXACTLY "R.A.O.S-VARIATION", not "VEL R.A.O.S" or "ACC R.A.O.S"
        # Check the character BEFORE the marker to ensure it's whitespace/newline
        section_marker = "R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY"

        # Find all occurrences of DISPLACEMENT RAO sections only
        positions = []
        search_pos = 0
        while True:
            pos = self.content.find(section_marker, search_pos)
            if pos == -1:
                break

            # Check character before marker (should be whitespace, not "VEL " or "ACC ")
            if pos > 0:
                before_text = self.content[max(0, pos-20):pos]  # Check more context
                # Skip if preceded by "VEL" or "ACC"
                if "VEL" in before_text or "ACC" in before_text:
                    logger.debug(f"Skipping non-displacement RAO section (before_text: '{before_text[-10:]}')")
                    search_pos = pos + len(section_marker)
                    continue

            positions.append(pos)
            search_pos = pos + len(section_marker)

        if not positions:
            raise ValueError("Could not find any displacement RAO tables")

        logger.info(f"Found {len(positions)} displacement RAO table sections in .LIS file")

        # Collect all table text from all sections
        # For each displacement RAO section, read until the next major section marker
        # (which could be VEL, ACC, or another displacement section)
        all_table_text = []
        for i, pos in enumerate(positions):
            # Find the next section marker (any type: displacement, VEL, or ACC)
            next_marker = self.content.find("R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY", pos + len(section_marker))

            if next_marker != -1:
                # Read until next RAO section (any type)
                end_pos = next_marker
            else:
                # Last section - but still limit reading to avoid garbage
                # Look for major section break (multiple newlines)
                next_break = self.content.find("\n\n\n", pos + 1000)
                end_pos = next_break if next_break != -1 else pos + 50000  # Max 50k chars

            section_text = self.content[pos:end_pos]
            all_table_text.append(section_text)

        # Combine all sections
        table_text = "\n".join(all_table_text)

        # Pattern for RAO data rows
        # AQWA format: Direction is only listed once per heading group!
        # First row: "  22.00   0.286   -180.00    0.8926  -90.20..."
        # Next rows: "  19.00   0.331              0.8277  -90.29..." (no direction!)

        # Pattern for row WITH direction
        pattern_with_dir = r'\s+([\d.]+)\s+([\d.]+)\s+([-\d.]+)\s+' + \
                           r'([\d.]+)\s+([-\d.]+)\s+' + \
                           r'([\d.]+)\s+([-\d.]+)\s+' + \
                           r'([\d.]+)\s+([-\d.]+)\s+' + \
                           r'([\d.]+)\s+([-\d.]+)\s+' + \
                           r'([\d.]+)\s+([-\d.]+)\s+' + \
                           r'([\d.]+)\s+([-\d.]+)'

        # Pattern for row WITHOUT direction (just period, freq, then RAOs)
        pattern_no_dir = r'\s+([\d.]+)\s+([\d.]+)\s+' + \
                         r'([\d.]+)\s+([-\d.]+)\s+' + \
                         r'([\d.]+)\s+([-\d.]+)\s+' + \
                         r'([\d.]+)\s+([-\d.]+)\s+' + \
                         r'([\d.]+)\s+([-\d.]+)\s+' + \
                         r'([\d.]+)\s+([-\d.]+)\s+' + \
                         r'([\d.]+)\s+([-\d.]+)'

        rao_dict = {}
        current_heading = None

        # Split into lines for sequential processing
        lines = table_text.split('\n')
        logger.info(f"Processing {len(lines)} lines from displacement RAO sections")

        matched_with_dir = 0
        matched_no_dir = 0
        skipped_lines = 0
        unmatched_data_lines = []

        for line in lines:
            # Skip header/separator/page number lines
            stripped = line.strip()
            if not stripped or '---' in line or 'PERIOD' in line or 'SECS' in line:
                skipped_lines += 1
                continue

            # Skip page numbers (lines with just a single digit or small number)
            if stripped.isdigit() and len(stripped) <= 3:
                skipped_lines += 1
                continue

            # Try pattern WITH direction first
            match = re.match(pattern_with_dir, line)
            if match:
                period = float(match.group(1))
                freq = float(match.group(2))
                heading = float(match.group(3))

                # Sanity checks
                if not (0.01 < freq < 100 and -180 <= heading <= 180):
                    continue

                current_heading = heading  # Update current heading

                # Extract RAO data
                x_amp, x_phase = float(match.group(4)), float(match.group(5))
                y_amp, y_phase = float(match.group(6)), float(match.group(7))
                z_amp, z_phase = float(match.group(8)), float(match.group(9))
                rx_amp, rx_phase = float(match.group(10)), float(match.group(11))
                ry_amp, ry_phase = float(match.group(12)), float(match.group(13))
                rz_amp, rz_phase = float(match.group(14)), float(match.group(15))

                # Store in dictionary
                key = (freq, current_heading)
                rao_dict[key] = {
                    'surge': (x_amp, x_phase),
                    'sway': (y_amp, y_phase),
                    'heave': (z_amp, z_phase),
                    'roll': (rx_amp, rx_phase),
                    'pitch': (ry_amp, ry_phase),
                    'yaw': (rz_amp, rz_phase)
                }
                matched_with_dir += 1

            else:
                # Try pattern WITHOUT direction (use current_heading)
                match = re.match(pattern_no_dir, line)
                if match and current_heading is not None:
                    period = float(match.group(1))
                    freq = float(match.group(2))

                    # Sanity check
                    if not (0.01 < freq < 100):
                        continue

                    # Extract RAO data
                    x_amp, x_phase = float(match.group(3)), float(match.group(4))
                    y_amp, y_phase = float(match.group(5)), float(match.group(6))
                    z_amp, z_phase = float(match.group(7)), float(match.group(8))
                    rx_amp, rx_phase = float(match.group(9)), float(match.group(10))
                    ry_amp, ry_phase = float(match.group(11)), float(match.group(12))
                    rz_amp, rz_phase = float(match.group(13)), float(match.group(14))

                    # Store in dictionary with current heading
                    key = (freq, current_heading)
                    rao_dict[key] = {
                        'surge': (x_amp, x_phase),
                        'sway': (y_amp, y_phase),
                        'heave': (z_amp, z_phase),
                        'roll': (rx_amp, rx_phase),
                        'pitch': (ry_amp, ry_phase),
                        'yaw': (rz_amp, rz_phase)
                    }
                    matched_no_dir += 1
                else:
                    # Check if this looks like a data line that we failed to match
                    if stripped and stripped[0].isdigit():
                        unmatched_data_lines.append(line[:100])

        if not rao_dict:
            raise ValueError("No valid RAO data found")

        logger.info(f"Parsed {matched_with_dir} lines with direction, {matched_no_dir} without direction")
        logger.info(f"Skipped {skipped_lines} header/blank lines from {len(lines)} total lines")

        if unmatched_data_lines:
            logger.warning(f"Found {len(unmatched_data_lines)} data-like lines that didn't match patterns:")
            for i, unmatched in enumerate(unmatched_data_lines[:5]):  # Show first 5
                logger.warning(f"  Unmatched line {i+1}: {unmatched}")

        logger.info(f"Parsed RAO data for {len(rao_dict)} frequency-heading combinations")

        return rao_dict

    def parse_all(self) -> Dict:
        """
        Parse all hydrodynamic data from .LIS file.

        Returns:
            Dictionary containing all parsed data
        """
        logger.info(f"Parsing AQWA .LIS file: {self.lis_file.name}")

        try:
            frequencies, periods = self.extract_frequencies_and_periods()
            added_mass = self.parse_added_mass_table()
            damping = self.parse_damping_table()
            raos = self.parse_rao_table()

            # Derive headings from actual RAO data keys (more reliable than extract_headings)
            headings = sorted(set(h for f, h in raos.keys()))

            result = {
                'frequencies': frequencies,
                'periods': periods,
                'headings': headings,
                'added_mass': added_mass,
                'damping': damping,
                'raos': raos
            }

            logger.success(f"Successfully parsed AQWA .LIS file")
            logger.info(f"  - {len(frequencies)} frequencies")
            logger.info(f"  - {len(headings)} headings: {headings}")
            logger.info(f"  - {len(added_mass)} added mass matrices")
            logger.info(f"  - {len(damping)} damping matrices")
            logger.info(f"  - {len(raos)} RAO data points")

            return result

        except Exception as e:
            logger.error(f"Failed to parse AQWA .LIS file: {str(e)}")
            raise


# Standalone functions for convenience

def parse_aqwa_lis_file(lis_file_path: Path) -> Dict:
    """
    Parse AQWA .LIS file and extract all hydrodynamic data.

    Args:
        lis_file_path: Path to .LIS file

    Returns:
        Dictionary containing frequencies, headings, added_mass, damping, and RAOs
    """
    parser = AQWALISParser(lis_file_path)
    return parser.parse_all()


def extract_aqwa_frequencies(lis_file_path: Path) -> Tuple[List[float], List[float]]:
    """Extract frequencies and periods from AQWA .LIS file."""
    parser = AQWALISParser(lis_file_path)
    return parser.extract_frequencies_and_periods()


def extract_aqwa_headings(lis_file_path: Path) -> List[float]:
    """Extract wave headings from AQWA .LIS file."""
    parser = AQWALISParser(lis_file_path)
    return parser.extract_headings()


def extract_aqwa_added_mass(lis_file_path: Path) -> Dict[float, np.ndarray]:
    """Extract added mass matrices from AQWA .LIS file."""
    parser = AQWALISParser(lis_file_path)
    return parser.parse_added_mass_table()


def extract_aqwa_damping(lis_file_path: Path) -> Dict[float, np.ndarray]:
    """Extract damping matrices from AQWA .LIS file."""
    parser = AQWALISParser(lis_file_path)
    return parser.parse_damping_table()


def extract_aqwa_raos(lis_file_path: Path) -> Dict[Tuple[float, float], Dict[str, Tuple[float, float]]]:
    """Extract RAO data from AQWA .LIS file."""
    parser = AQWALISParser(lis_file_path)
    return parser.parse_rao_table()
