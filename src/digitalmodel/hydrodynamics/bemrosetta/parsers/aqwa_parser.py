"""
AQWA output file parser.

Parser for AQWA diffraction analysis output files (.LIS format).
Extracts hydrodynamic coefficients including RAOs, added mass, and damping.
"""

import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import numpy as np
from loguru import logger

from .base import BaseParser
from ..core.exceptions import ParserError
from ..models import BEMSolverMetadata
from ..models.solver_metadata import BEMSolverType


@dataclass
class AQWAParseResult:
    """Container for AQWA parsing results.

    Attributes:
        frequencies: Wave frequencies in rad/s.
        periods: Wave periods in seconds.
        headings: Wave headings in degrees.
        added_mass: Dictionary mapping frequency to 6x6 added mass matrix.
        damping: Dictionary mapping frequency to 6x6 damping matrix.
        raos: Dictionary mapping (frequency, heading) to DOF data.
    """

    frequencies: List[float] = field(default_factory=list)
    periods: List[float] = field(default_factory=list)
    headings: List[float] = field(default_factory=list)
    added_mass: Dict[float, np.ndarray] = field(default_factory=dict)
    damping: Dict[float, np.ndarray] = field(default_factory=dict)
    raos: Dict[Tuple[float, float], Dict[str, Tuple[float, float]]] = field(
        default_factory=dict
    )


class AQWAParser(BaseParser):
    """Parser for AQWA diffraction analysis output files.

    Parses AQWA .LIS files and extracts:
    - Solver metadata (version, water depth, vessel name)
    - Wave frequencies and periods
    - Wave headings
    - RAO data (amplitude and phase for all 6 DOFs)
    - Added mass matrices (6x6 frequency-dependent)
    - Damping matrices (6x6 frequency-dependent)
    """

    @property
    def supported_extensions(self) -> List[str]:
        """Supported file extensions."""
        return [".lis", ".LIS"]

    @property
    def solver_name(self) -> str:
        """Solver name."""
        return "AQWA"

    def parse(self, file_path: Path | str) -> AQWAParseResult:
        """Parse AQWA .LIS file and return structured results.

        Args:
            file_path: Path to AQWA .LIS file.

        Returns:
            AQWAParseResult containing all extracted data.

        Raises:
            ParserError: If file not found or parsing fails.
        """
        file_path = Path(file_path)
        self._validate_file_exists(file_path)

        logger.info(f"Parsing AQWA file: {file_path}")

        try:
            with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
                content = f.read()
        except IOError as e:
            raise ParserError(
                f"Failed to read file: {e}",
                file_path=str(file_path),
            )

        if not content.strip():
            raise ParserError(
                "File is empty",
                file_path=str(file_path),
            )

        # Extract metadata
        self._metadata = self._parse_metadata(content, file_path)

        # Extract data
        try:
            frequencies, periods = self._extract_frequencies_and_periods(content)
            headings = self._extract_headings(content)
            added_mass = self._extract_added_mass(content)
            damping = self._extract_damping(content)
            raos = self._extract_raos(content)
        except ValueError as e:
            raise ParserError(
                f"Failed to parse AQWA data: {e}",
                file_path=str(file_path),
            )

        # Validate we got some data
        if not frequencies and not added_mass and not raos:
            raise ParserError(
                "No valid hydrodynamic data found in file",
                file_path=str(file_path),
            )

        # Derive headings from RAO data if not found elsewhere
        if not headings and raos:
            headings = sorted(set(h for f, h in raos.keys()))

        result = AQWAParseResult(
            frequencies=frequencies,
            periods=periods,
            headings=headings,
            added_mass=added_mass,
            damping=damping,
            raos=raos,
        )

        logger.success(f"Successfully parsed AQWA file: {file_path.name}")
        logger.info(f"  - {len(frequencies)} frequencies")
        logger.info(f"  - {len(headings)} headings")
        logger.info(f"  - {len(added_mass)} added mass matrices")
        logger.info(f"  - {len(damping)} damping matrices")
        logger.info(f"  - {len(raos)} RAO data points")

        return result

    def _parse_metadata(
        self, content: str, file_path: Path
    ) -> BEMSolverMetadata:
        """Extract metadata from AQWA output.

        Args:
            content: File content string.
            file_path: Path to source file.

        Returns:
            BEMSolverMetadata with extracted information.
        """
        # Look for AQWA version
        version_match = re.search(r"AQWA\s+(\d+\.?\d*)", content)
        version = version_match.group(1) if version_match else ""

        # Look for water depth
        depth_match = re.search(
            r"WATER\s+DEPTH\s*[=:]\s*([\d.]+|INFINITE)", content, re.IGNORECASE
        )
        if depth_match:
            depth_str = depth_match.group(1)
            water_depth = (
                float("inf") if depth_str.upper() == "INFINITE" else float(depth_str)
            )
        else:
            water_depth = float("inf")

        # Look for structure/vessel name
        name_match = re.search(
            r"STRUCTURE\s+NAME\s*[=:]\s*(\S+)", content, re.IGNORECASE
        )
        if not name_match:
            name_match = re.search(r"VESSEL\s+NAME\s*[=:]\s*(\S+)", content, re.IGNORECASE)

        project_name = name_match.group(1) if name_match else file_path.stem
        body_names = [project_name] if project_name else []

        return BEMSolverMetadata(
            solver_type=BEMSolverType.AQWA,
            solver_version=version,
            water_depth=water_depth,
            project_name=project_name,
            body_names=body_names,
        )

    def _extract_frequencies_and_periods(
        self, content: str
    ) -> Tuple[List[float], List[float]]:
        """Extract wave frequencies and periods from AQWA output.

        Args:
            content: File content string.

        Returns:
            Tuple of (frequencies in rad/s, periods in seconds).
        """
        # Find added mass table section
        section_marker = "ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY"
        pos = content.find(section_marker)

        if pos == -1:
            # Try alternate section markers
            pos = content.find("DAMPING-VARIATION WITH WAVE PERIOD/FREQUENCY")

        if pos == -1:
            return [], []

        # Extract table text
        table_text = content[pos : pos + 5000]

        # Pattern for data rows: period, freq, then scientific notation values
        pattern = r"^\s*([\d.]+)\s+([\d.]+)\s+([+-]?[\d.]+[Ee][+-]?\d+)"

        periods = []
        frequencies = []

        for match in re.finditer(pattern, table_text, re.MULTILINE):
            period = float(match.group(1))
            freq = float(match.group(2))

            # Sanity check: reasonable values
            if 0.01 < freq < 100 and 0.1 < period < 1000:
                if freq not in frequencies:
                    periods.append(period)
                    frequencies.append(freq)

        if frequencies:
            logger.info(
                f"Extracted {len(frequencies)} frequencies "
                f"from {frequencies[0]:.3f} to {frequencies[-1]:.3f} rad/s"
            )

        return frequencies, periods

    def _extract_headings(self, content: str) -> List[float]:
        """Extract wave headings from AQWA output.

        Args:
            content: File content string.

        Returns:
            List of headings in degrees.
        """
        # Find RAO section
        section_marker = "R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY"
        pos = content.find(section_marker)

        if pos == -1:
            return []

        table_text = content[pos : pos + 10000]

        # Find direction values in RAO rows
        pattern = r"\s+[\d.]+\s+[\d.]+\s+([-\d.]+)\s+[\d.]+"

        headings = set()
        for match in re.finditer(pattern, table_text):
            heading = float(match.group(1))
            if -180 <= heading <= 360:
                headings.add(heading)

        return sorted(headings)

    def _extract_added_mass(self, content: str) -> Dict[float, np.ndarray]:
        """Extract added mass matrices from AQWA output.

        Args:
            content: File content string.

        Returns:
            Dictionary mapping frequency (rad/s) to 6x6 added mass matrix.
        """
        section_marker = "ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY"
        pos = content.find(section_marker)

        if pos == -1:
            return {}

        table_text = content[pos : pos + 5000]

        # Pattern for full data row with 12 scientific notation values
        pattern = (
            r"^\s+([\d.]+)\s+([\d.]+)\s+"
            r"([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+"
            r"([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+"
            r"([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+"
            r"([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+"
            r"([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+"
            r"([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)"
        )

        added_mass_dict = {}

        for match in re.finditer(pattern, table_text, re.MULTILINE):
            freq = float(match.group(2))

            # Extract 12 coefficients
            coeffs = [float(match.group(i)) for i in range(3, 15)]

            # Build 6x6 symmetric matrix
            matrix = self._build_symmetric_matrix(coeffs)
            added_mass_dict[freq] = matrix

        if added_mass_dict:
            logger.info(
                f"Parsed added mass matrices for {len(added_mass_dict)} frequencies"
            )

        return added_mass_dict

    def _extract_damping(self, content: str) -> Dict[float, np.ndarray]:
        """Extract damping matrices from AQWA output.

        Args:
            content: File content string.

        Returns:
            Dictionary mapping frequency (rad/s) to 6x6 damping matrix.
        """
        section_marker = "DAMPING-VARIATION WITH WAVE PERIOD/FREQUENCY"
        pos = content.find(section_marker)

        if pos == -1:
            return {}

        table_text = content[pos : pos + 5000]

        # Same pattern as added mass
        pattern = (
            r"^\s+([\d.]+)\s+([\d.]+)\s+"
            r"([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+"
            r"([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+"
            r"([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+"
            r"([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+"
            r"([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)\s+"
            r"([+-]?[\d.]+[Ee][+-]?\d+)\s+([+-]?[\d.]+[Ee][+-]?\d+)"
        )

        damping_dict = {}

        for match in re.finditer(pattern, table_text, re.MULTILINE):
            freq = float(match.group(2))

            coeffs = [float(match.group(i)) for i in range(3, 15)]
            matrix = self._build_symmetric_matrix(coeffs)
            damping_dict[freq] = matrix

        if damping_dict:
            logger.info(f"Parsed damping matrices for {len(damping_dict)} frequencies")

        return damping_dict

    def _build_symmetric_matrix(self, coeffs: List[float]) -> np.ndarray:
        """Build 6x6 symmetric matrix from AQWA coefficient list.

        AQWA provides 12 coefficients:
        M11, M22, M33, M44, M55, M66, M13, M15, M24, M26, M35, M46

        Args:
            coeffs: List of 12 coefficients.

        Returns:
            6x6 numpy array with symmetric structure.
        """
        matrix = np.zeros((6, 6))

        # Diagonal terms
        matrix[0, 0] = coeffs[0]  # M11
        matrix[1, 1] = coeffs[1]  # M22
        matrix[2, 2] = coeffs[2]  # M33
        matrix[3, 3] = coeffs[3]  # M44
        matrix[4, 4] = coeffs[4]  # M55
        matrix[5, 5] = coeffs[5]  # M66

        # Off-diagonal coupling terms (symmetric)
        matrix[0, 2] = matrix[2, 0] = coeffs[6]  # M13
        matrix[0, 4] = matrix[4, 0] = coeffs[7]  # M15
        matrix[1, 3] = matrix[3, 1] = coeffs[8]  # M24
        matrix[1, 5] = matrix[5, 1] = coeffs[9]  # M26
        matrix[2, 4] = matrix[4, 2] = coeffs[10]  # M35
        matrix[3, 5] = matrix[5, 3] = coeffs[11]  # M46

        return matrix

    def _extract_raos(
        self, content: str
    ) -> Dict[Tuple[float, float], Dict[str, Tuple[float, float]]]:
        """Extract RAO data from AQWA output.

        Args:
            content: File content string.

        Returns:
            Dictionary mapping (frequency, heading) to DOF data.
            DOF data is dict: {'surge': (amp, phase), 'sway': (amp, phase), ...}
        """
        # Find displacement RAO sections only (not velocity or acceleration)
        section_marker = "R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY"

        positions = []
        search_pos = 0
        while True:
            pos = content.find(section_marker, search_pos)
            if pos == -1:
                break

            # Check preceding text for VEL or ACC
            if pos > 0:
                before_text = content[max(0, pos - 20) : pos]
                if "VEL" in before_text or "ACC" in before_text:
                    search_pos = pos + len(section_marker)
                    continue

            positions.append(pos)
            search_pos = pos + len(section_marker)

        if not positions:
            return {}

        # Collect table text from all sections
        all_table_text = []
        for i, pos in enumerate(positions):
            next_marker = content.find(section_marker, pos + len(section_marker))
            if next_marker != -1:
                end_pos = next_marker
            else:
                end_pos = pos + 50000

            section_text = content[pos:end_pos]
            all_table_text.append(section_text)

        table_text = "\n".join(all_table_text)

        # Pattern for row WITH direction
        pattern_with_dir = (
            r"\s+([\d.]+)\s+([\d.]+)\s+([-\d.]+)\s+"
            r"([\d.]+)\s+([-\d.]+)\s+"
            r"([\d.]+)\s+([-\d.]+)\s+"
            r"([\d.]+)\s+([-\d.]+)\s+"
            r"([\d.]+)\s+([-\d.]+)\s+"
            r"([\d.]+)\s+([-\d.]+)\s+"
            r"([\d.]+)\s+([-\d.]+)"
        )

        # Pattern for row WITHOUT direction (uses current heading)
        pattern_no_dir = (
            r"\s+([\d.]+)\s+([\d.]+)\s+"
            r"([\d.]+)\s+([-\d.]+)\s+"
            r"([\d.]+)\s+([-\d.]+)\s+"
            r"([\d.]+)\s+([-\d.]+)\s+"
            r"([\d.]+)\s+([-\d.]+)\s+"
            r"([\d.]+)\s+([-\d.]+)\s+"
            r"([\d.]+)\s+([-\d.]+)"
        )

        rao_dict = {}
        current_heading = None

        lines = table_text.split("\n")

        for line in lines:
            stripped = line.strip()
            if not stripped or "---" in line or "PERIOD" in line or "SECS" in line:
                continue

            if stripped.isdigit() and len(stripped) <= 3:
                continue

            # Try pattern WITH direction first
            match = re.match(pattern_with_dir, line)
            if match:
                freq = float(match.group(2))
                heading = float(match.group(3))

                if not (0.01 < freq < 100 and -180 <= heading <= 360):
                    continue

                current_heading = heading

                rao_dict[(freq, current_heading)] = {
                    "surge": (float(match.group(4)), float(match.group(5))),
                    "sway": (float(match.group(6)), float(match.group(7))),
                    "heave": (float(match.group(8)), float(match.group(9))),
                    "roll": (float(match.group(10)), float(match.group(11))),
                    "pitch": (float(match.group(12)), float(match.group(13))),
                    "yaw": (float(match.group(14)), float(match.group(15))),
                }
            else:
                # Try pattern WITHOUT direction
                match = re.match(pattern_no_dir, line)
                if match and current_heading is not None:
                    freq = float(match.group(2))

                    if not (0.01 < freq < 100):
                        continue

                    rao_dict[(freq, current_heading)] = {
                        "surge": (float(match.group(3)), float(match.group(4))),
                        "sway": (float(match.group(5)), float(match.group(6))),
                        "heave": (float(match.group(7)), float(match.group(8))),
                        "roll": (float(match.group(9)), float(match.group(10))),
                        "pitch": (float(match.group(11)), float(match.group(12))),
                        "yaw": (float(match.group(13)), float(match.group(14))),
                    }

        if rao_dict:
            logger.info(
                f"Parsed RAO data for {len(rao_dict)} frequency-heading combinations"
            )

        return rao_dict
