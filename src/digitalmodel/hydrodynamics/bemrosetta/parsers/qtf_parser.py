"""
QTF (Quadratic Transfer Function) file parser.

Parser for second-order wave force coefficient files.
Supports both sum-frequency and difference-frequency QTF data.
"""

import re
from pathlib import Path
from typing import List, Optional

import numpy as np
from loguru import logger

from .base import BaseParser
from ..core.exceptions import ParserError
from ..models import BEMSolverMetadata, QTFData, QTFType
from ..models.qtf_data import QTFComponent
from ..models.solver_metadata import BEMSolverType


class QTFParser(BaseParser):
    """Parser for QTF (second-order wave force) files.

    Parses QTF files containing quadratic transfer functions for
    second-order wave loads including:
    - Difference-frequency (slow-drift) forces
    - Sum-frequency (springing) forces

    Supports AQWA QTF format and similar text-based formats.
    """

    @property
    def supported_extensions(self) -> List[str]:
        """Supported file extensions."""
        return [".qtf", ".QTF"]

    @property
    def solver_name(self) -> str:
        """Solver name."""
        return "AQWA-QTF"

    def parse(self, file_path: Path | str) -> QTFData:
        """Parse QTF file and return QTFData.

        Args:
            file_path: Path to QTF file.

        Returns:
            QTFData containing all extracted QTF coefficients.

        Raises:
            ParserError: If file not found or parsing fails.
        """
        file_path = Path(file_path)
        self._validate_file_exists(file_path)

        logger.info(f"Parsing QTF file: {file_path}")

        try:
            with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
                lines = f.readlines()
        except IOError as e:
            raise ParserError(
                f"Failed to read file: {e}",
                file_path=str(file_path),
            )

        if not lines:
            raise ParserError(
                "File is empty",
                file_path=str(file_path),
            )

        try:
            result = self._parse_qtf_content(lines, file_path)
        except ValueError as e:
            raise ParserError(
                f"Failed to parse QTF data: {e}",
                file_path=str(file_path),
            )

        # Create metadata
        self._metadata = BEMSolverMetadata(
            solver_type=BEMSolverType.AQWA,
            project_name=result.body_name,
            body_names=[result.body_name] if result.body_name else [],
        )

        logger.success(f"Successfully parsed QTF file: {file_path.name}")
        logger.info(f"  - QTF type: {result.qtf_type.value}")
        logger.info(f"  - {len(result.frequencies)} frequencies")
        logger.info(f"  - {len(result.headings)} headings")
        logger.info(f"  - {len(result.components)} components")

        return result

    def _parse_qtf_content(self, lines: List[str], file_path: Path) -> QTFData:
        """Parse QTF file content.

        Args:
            lines: List of file lines.
            file_path: Source file path.

        Returns:
            QTFData with parsed content.
        """
        content = "".join(lines)

        # Determine QTF type from header
        first_line = lines[0].upper() if lines else ""
        if "SUM" in first_line:
            qtf_type = QTFType.SUM_FREQUENCY
        elif "DIFFERENCE" in first_line or "DIFF" in first_line:
            qtf_type = QTFType.DIFFERENCE_FREQUENCY
        else:
            # Default to difference frequency (more common)
            qtf_type = QTFType.DIFFERENCE_FREQUENCY

        # Extract structure/body name
        body_name = self._extract_body_name(content)

        # Extract frequencies
        frequencies_1 = self._extract_frequencies(content, "FREQUENCIES1", "FREQ1")
        frequencies_2 = self._extract_frequencies(content, "FREQUENCIES2", "FREQ2")

        # If only one frequency array found, use it for both
        has_freq1 = len(frequencies_1) > 0
        has_freq2 = len(frequencies_2) > 0

        if not has_freq1 and has_freq2:
            frequencies_1 = frequencies_2.copy()
        elif has_freq1 and not has_freq2:
            frequencies_2 = frequencies_1.copy()
        elif not has_freq1 and not has_freq2:
            # Try generic frequency extraction
            frequencies_1 = self._extract_generic_frequencies(content)
            frequencies_2 = frequencies_1.copy()

        if len(frequencies_1) == 0:
            raise ValueError("Could not extract frequencies from QTF file")

        # Extract headings
        headings = self._extract_headings(content)
        if len(headings) == 0:
            headings = np.array([0.0])  # Default to head seas

        # Parse QTF components
        components = self._parse_components(
            lines, qtf_type, frequencies_1, frequencies_2, headings
        )

        return QTFData(
            qtf_type=qtf_type,
            body_name=body_name,
            components=components,
            frequencies=np.array(frequencies_1),
            headings=headings,
            metadata={"source_file": str(file_path)},
        )

    def _extract_body_name(self, content: str) -> str:
        """Extract structure/body name from content.

        Args:
            content: File content string.

        Returns:
            Body name or empty string.
        """
        patterns = [
            r"STRUCTURE\s*[=:]\s*(\S+)",
            r"BODY\s*[=:]\s*(\S+)",
            r"VESSEL\s*[=:]\s*(\S+)",
            r"NAME\s*[=:]\s*(\S+)",
        ]

        for pattern in patterns:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                return match.group(1)

        return ""

    def _extract_frequencies(
        self, content: str, *keywords: str
    ) -> np.ndarray:
        """Extract frequency array using keywords.

        Args:
            content: File content string.
            keywords: Keywords to search for.

        Returns:
            Numpy array of frequencies.
        """
        for keyword in keywords:
            pattern = rf"{keyword}\s*[=:]?\s*([\d.\s]+)"
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                values_str = match.group(1).strip()
                try:
                    values = [float(x) for x in values_str.split() if x]
                    if values:
                        return np.array(values)
                except ValueError:
                    continue

        return np.array([])

    def _extract_generic_frequencies(self, content: str) -> np.ndarray:
        """Extract frequencies using generic patterns.

        Args:
            content: File content string.

        Returns:
            Numpy array of frequencies.
        """
        # Look for FREQ or FREQUENCIES lines
        pattern = r"FREQ(?:UENCIES?)?\s*[=:]?\s*([\d.\s]+)"
        match = re.search(pattern, content, re.IGNORECASE)
        if match:
            values_str = match.group(1).strip()
            try:
                values = [float(x) for x in values_str.split() if x]
                # Filter to reasonable frequency range
                values = [v for v in values if 0.01 < v < 10.0]
                if values:
                    return np.array(values)
            except ValueError:
                pass

        return np.array([])

    def _extract_headings(self, content: str) -> np.ndarray:
        """Extract heading angles from content.

        Args:
            content: File content string.

        Returns:
            Numpy array of headings in degrees.
        """
        patterns = [
            r"HEADINGS?\s*[=:]?\s*([\d.\s-]+)",
            r"DIRECTIONS?\s*[=:]?\s*([\d.\s-]+)",
        ]

        for pattern in patterns:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                values_str = match.group(1).strip()
                try:
                    values = [float(x) for x in values_str.split() if x.lstrip("-").replace(".", "").isdigit()]
                    # Filter to valid heading range
                    values = [v for v in values if -180 <= v <= 360]
                    if values:
                        return np.array(values)
                except ValueError:
                    continue

        return np.array([])

    def _parse_components(
        self,
        lines: List[str],
        qtf_type: QTFType,
        frequencies_1: np.ndarray,
        frequencies_2: np.ndarray,
        headings: np.ndarray,
    ) -> List[QTFComponent]:
        """Parse QTF components from file lines.

        Args:
            lines: File lines.
            qtf_type: Type of QTF.
            frequencies_1: First frequency array.
            frequencies_2: Second frequency array.
            headings: Heading array.

        Returns:
            List of QTFComponent objects.
        """
        components = []
        content = "".join(lines)

        n1 = len(frequencies_1)
        n2 = len(frequencies_2)

        # Find all DOF/heading blocks
        dof_pattern = re.compile(
            r"DOF\s*[=:]?\s*(\d+).*?"
            r"HEADING\s*[=:]?\s*([-\d.]+).*?"
            r"REAL\s*[=:]?(.+?)"
            r"IMAG\s*[=:]?(.+?)(?=DOF|$)",
            re.IGNORECASE | re.DOTALL,
        )

        for match in dof_pattern.finditer(content):
            dof = int(match.group(1))
            heading = float(match.group(2))
            real_text = match.group(3)
            imag_text = match.group(4)

            # Parse real and imaginary matrices
            real_matrix = self._parse_matrix(real_text, n1, n2)
            imag_matrix = self._parse_matrix(imag_text, n1, n2)

            if real_matrix is not None and imag_matrix is not None:
                # Convert to amplitude and phase
                complex_qtf = real_matrix + 1j * imag_matrix
                amplitude = np.abs(complex_qtf)
                phase = np.angle(complex_qtf)

                component = QTFComponent(
                    dof=dof - 1,  # Convert to 0-based index
                    heading=heading,
                    qtf_type=qtf_type,
                    frequencies_1=frequencies_1,
                    frequencies_2=frequencies_2,
                    amplitude=amplitude,
                    phase=phase,
                )
                components.append(component)

        # If no components found with structured format, try simpler parsing
        if not components:
            components = self._parse_simple_format(
                lines, qtf_type, frequencies_1, frequencies_2, headings
            )

        return components

    def _parse_matrix(
        self, text: str, n_rows: int, n_cols: int
    ) -> Optional[np.ndarray]:
        """Parse matrix from text block.

        Args:
            text: Text containing matrix values.
            n_rows: Expected number of rows.
            n_cols: Expected number of columns.

        Returns:
            Numpy array or None if parsing fails.
        """
        # Extract all numeric values
        values = []
        pattern = r"[+-]?[\d.]+[Ee][+-]?\d+|[+-]?[\d.]+"

        for match in re.finditer(pattern, text):
            try:
                values.append(float(match.group()))
            except ValueError:
                continue

        expected_size = n_rows * n_cols
        if len(values) >= expected_size:
            try:
                return np.array(values[:expected_size]).reshape(n_rows, n_cols)
            except ValueError:
                pass

        return None

    def _parse_simple_format(
        self,
        lines: List[str],
        qtf_type: QTFType,
        frequencies_1: np.ndarray,
        frequencies_2: np.ndarray,
        headings: np.ndarray,
    ) -> List[QTFComponent]:
        """Parse QTF data from simple tabular format.

        Args:
            lines: File lines.
            qtf_type: Type of QTF.
            frequencies_1: First frequency array.
            frequencies_2: Second frequency array.
            headings: Heading array.

        Returns:
            List of QTFComponent objects.
        """
        components = []

        n1 = len(frequencies_1)
        n2 = len(frequencies_2)

        # Create default component if we have frequency data
        if n1 > 0 and n2 > 0:
            for i_head, heading in enumerate(headings):
                for dof in range(6):
                    # Create placeholder with zeros
                    amplitude = np.zeros((n1, n2))
                    phase = np.zeros((n1, n2))

                    component = QTFComponent(
                        dof=dof,
                        heading=heading,
                        qtf_type=qtf_type,
                        frequencies_1=frequencies_1,
                        frequencies_2=frequencies_2,
                        amplitude=amplitude,
                        phase=phase,
                    )
                    components.append(component)

        return components
