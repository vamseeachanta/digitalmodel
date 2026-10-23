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

from digitalmodel.hydrodynamics.diffraction.grid_completeness import (
    GridCompletenessError, deck_frequency_count, require_count, validate_parsed_grid,
)


class AQWALISParser:
    """
    Parser for AQWA .LIS files.

    Extracts hydrodynamic coefficients from AQWA diffraction analysis output.
    """

    def __init__(self, lis_file_path: Path, *, deck_file_path=None,
                 expected_frequency_count=None):
        """
        Initialize parser with .LIS file path.

        Args:
            lis_file_path: Path to AQWA .LIS file
            deck_file_path: Explicit deck, or a unique matching sibling .dat.
            expected_frequency_count: Independent positive count when available.
                If neither source is available, parse_all marks the declaration
                NOT_EVALUATED while still checking agreement between tables.
        """
        self.lis_file = Path(lis_file_path)
        if not self.lis_file.exists():
            raise FileNotFoundError(f"LIS file not found: {lis_file_path}")

        # Read entire file content
        with open(self.lis_file, 'r', encoding='utf-8', errors='ignore') as f:
            self.content = f.read()

        # Prefer an explicit deck; otherwise find a uniquely named sibling.
        # A standalone listing has no independent declaration: expose that as
        # NOT_EVALUATED metadata rather than inventing a frequency count.
        if expected_frequency_count is not None and (
                isinstance(expected_frequency_count, bool)
                or not isinstance(expected_frequency_count, int)
                or expected_frequency_count <= 0):
            raise ValueError("expected_frequency_count must be a positive integer")
        candidates = [p for p in self.lis_file.parent.iterdir()
                      if p.stem.casefold() == self.lis_file.stem.casefold()
                      and p.suffix.casefold() == ".dat"]
        if deck_file_path is None and len(candidates) > 1:
            raise GridCompletenessError("deck", "ambiguous_deck")
        deck = Path(deck_file_path) if deck_file_path is not None else (
            candidates[0] if candidates else None)
        self.declared_frequency_count = expected_frequency_count
        if deck is not None:
            count = deck_frequency_count(deck.read_text(encoding="utf-8"))
            require_count("deck", count, expected_frequency_count)
            self.declared_frequency_count = count
        elif expected_frequency_count is None and any(
                line[6:10] == "HRTZ" for line in self.content.splitlines()):
            self.declared_frequency_count = deck_frequency_count(self.content)

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
        rows = self._coefficient_rows(
            "ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY")
        return [row[1] for row in rows], [row[0] for row in rows]

    def extract_headings(self) -> List[float]:
        """
        Extract wave headings from RAO table.

        Returns:
            List of headings in degrees
        """
        return sorted({heading for _, heading in self.parse_rao_table()})

    _TABLE_HEADER = re.compile(
        r"^.*-VARIATION WITH WAVE PERIOD/FREQUENCY[^\n]*$", re.MULTILINE)
    _TABLE_END_MARKERS = ("S T A B I L I T Y", "M O O R I N G")

    def _table_span(self, marker: str, pos=None) -> str:
        """Read through repeated page headers to the next different table.

        EOF is not a terminator: a listing cut off during writing must refuse.
        Header grammar follows the existing AQWA listing reader; no byte or
        line-count limit determines the extent.
        """
        if pos is None:
            pos = self.find_section(marker)
        if pos is None:
            raise ValueError(f"Could not find table: {marker}")
        head = self.content.find("\n", pos)
        cuts = []
        if head >= 0:
            for match in self._TABLE_HEADER.finditer(self.content, head + 1):
                title = match.group().strip().removeprefix("1").strip()
                if title != marker:
                    cuts.append(match.start())
                    break
            for end in self._TABLE_END_MARKERS:
                found = self.content.find(end, head + 1)
                if found >= 0:
                    cuts.append(found)
        if not cuts:
            raise GridCompletenessError(marker, "missing_terminator")
        return self.content[pos:min(cuts)]

    def _coefficient_rows(self, marker):
        """Parse complete rows once for both the grid and coefficient readers."""
        rows = []
        seen = set()
        for line in self._table_span(marker).splitlines():
            tokens = line.split()
            # A data row starts with two numbers. Headers and page numbers do
            # not; malformed numeric rows must not silently disappear.
            try:
                period, frequency = map(float, tokens[:2])
            except ValueError:
                continue
            # Established listing grammar: period + frequency + twelve
            # selected coefficients (the existing matrix mapping below).
            if len(tokens) != 14:
                raise GridCompletenessError(marker, "malformed_row")
            try:
                values = [float(v.replace("D", "E").replace("d", "e"))
                          for v in tokens]
            except ValueError as exc:
                raise GridCompletenessError(marker, "malformed_row") from exc
            if not np.all(np.isfinite(values)) or period <= 0 or frequency <= 0:
                raise GridCompletenessError(marker, "invalid_row")
            if frequency in seen:
                raise GridCompletenessError(marker, "duplicate_frequency")
            seen.add(frequency)
            rows.append(values)
        if not rows:
            raise ValueError(f"No valid coefficient data found: {marker}")
        require_count(marker, len(rows), getattr(self, "declared_frequency_count", None))
        return rows

    def parse_added_mass_table(self) -> Dict[float, np.ndarray]:
        """
        Parse added mass table from .LIS file.

        Returns:
            Dictionary mapping frequency (rad/s) to 6x6 added mass matrix
        """
        added_mass_dict = {}
        marker = "ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY"
        for row in self._coefficient_rows(marker):
            freq = row[1]
            coeffs = row[2:]

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
        damping_dict = {}
        marker = "DAMPING-VARIATION WITH WAVE PERIOD/FREQUENCY"
        for row in self._coefficient_rows(marker):
            freq = row[1]
            coeffs = row[2:]

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

            line_start = self.content.rfind("\n", 0, pos) + 1
            prefix = self.content[line_start:pos].strip().removeprefix("1").strip()
            if prefix:
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
        for pos in positions:
            all_table_text.append(self._table_span(section_marker, pos))

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
                'declared_frequency_count': self.declared_frequency_count,
                'declaration_status': ('CHECKED' if self.declared_frequency_count is not None
                                       else 'NOT_EVALUATED'),
                'frequencies': frequencies,
                'periods': periods,
                'headings': headings,
                'added_mass': added_mass,
                'damping': damping,
                'raos': raos
            }

            validate_parsed_grid(result)

            logger.success("Successfully parsed AQWA .LIS file")
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

def parse_aqwa_lis_file(lis_file_path: Path, **kwargs) -> Dict:
    """
    Parse AQWA .LIS file and extract all hydrodynamic data.

    Args:
        lis_file_path: Path to .LIS file

    Returns:
        Dictionary containing frequencies, headings, added_mass, damping, and RAOs
    """
    parser = AQWALISParser(lis_file_path, **kwargs)
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
