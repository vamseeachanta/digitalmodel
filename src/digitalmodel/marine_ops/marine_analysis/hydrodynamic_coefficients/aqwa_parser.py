"""
AQWA .LIS File Parser

Parses ANSYS AQWA hydrodynamic analysis output files to extract added mass
and damping coefficient matrices across frequencies.

Classes:
    AQWAParser: Main parser for AQWA .LIS files
    AQWADataExtractor: Extracts specific data sections from AQWA output
"""

import re
import numpy as np
import pandas as pd
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Union
from dataclasses import dataclass
import warnings

from .coefficients import CoefficientDatabase, FrequencyDependentMatrix, DOF_NAMES


@dataclass
class AQWASection:
    """Container for a parsed AQWA data section.

    Attributes:
        section_type: Type of section ('ADDED_MASS', 'DAMPING', etc.)
        frequency: Wave frequency in rad/s
        data: Extracted numerical data
        header_info: Additional header information
    """
    section_type: str
    frequency: float
    data: np.ndarray
    header_info: Dict[str, str]


class AQWADataExtractor:
    """Extracts specific data sections from AQWA output files."""

    # Regular expression patterns for AQWA output
    FREQUENCY_PATTERN = re.compile(r'FREQUENCY\s*=\s*([\d.]+)')
    ADDED_MASS_HEADER = re.compile(r'ADDED\s+MASS\s+MATRIX')
    DAMPING_HEADER = re.compile(r'DAMPING\s+MATRIX')
    MATRIX_ROW_PATTERN = re.compile(r'^\s*\d+\s+([\d.E+-]+(?:\s+[\d.E+-]+)*)')

    @staticmethod
    def find_section_boundaries(lines: List[str],
                               start_pattern: re.Pattern,
                               max_lines: int = 50) -> Optional[Tuple[int, int]]:
        """Find start and end line indices for a data section.

        Args:
            lines: List of file lines
            start_pattern: Regex pattern to identify section start
            max_lines: Maximum lines to search after start

        Returns:
            Tuple of (start_line, end_line) or None if not found
        """
        for i, line in enumerate(lines):
            if start_pattern.search(line):
                # Find end of section (blank line or next section)
                end = min(i + max_lines, len(lines))
                for j in range(i + 1, end):
                    if not lines[j].strip() or lines[j].startswith('*'):
                        return i, j
                return i, end
        return None

    @staticmethod
    def extract_matrix_from_lines(lines: List[str],
                                  start_idx: int,
                                  end_idx: int,
                                  matrix_size: int = 6) -> np.ndarray:
        """Extract numerical matrix from text lines.

        Args:
            lines: List of file lines
            start_idx: Start line index
            end_idx: End line index
            matrix_size: Expected matrix dimension

        Returns:
            Extracted matrix as numpy array
        """
        matrix = np.zeros((matrix_size, matrix_size))
        row_idx = 0

        for i in range(start_idx, end_idx):
            line = lines[i].strip()

            # Match matrix row pattern
            match = AQWADataExtractor.MATRIX_ROW_PATTERN.match(line)
            if match and row_idx < matrix_size:
                # Extract values
                values = [float(x) for x in match.group(1).split()]
                # Handle partial rows (some AQWA formats split matrices)
                n_values = min(len(values), matrix_size)
                matrix[row_idx, :n_values] = values[:n_values]
                row_idx += 1

        return matrix

    @staticmethod
    def extract_frequency(lines: List[str], search_start: int = 0) -> Optional[float]:
        """Extract frequency value from lines.

        Args:
            lines: List of file lines
            search_start: Line index to start searching from

        Returns:
            Extracted frequency or None
        """
        for line in lines[search_start:search_start + 10]:
            match = AQWADataExtractor.FREQUENCY_PATTERN.search(line)
            if match:
                return float(match.group(1))
        return None


class AQWAParser:
    """Main parser for AQWA .LIS hydrodynamic output files.

    Supports extraction of:
    - Added mass matrices
    - Damping matrices
    - Frequency-dependent coefficients
    """

    def __init__(self, file_path: Union[str, Path]):
        """Initialize parser with AQWA .LIS file.

        Args:
            file_path: Path to AQWA .LIS output file
        """
        self.file_path = Path(file_path)
        self.lines: List[str] = []
        self.sections: List[AQWASection] = []

        if self.file_path.exists():
            self._load_file()
        else:
            raise FileNotFoundError(f"AQWA file not found: {file_path}")

    def _load_file(self):
        """Load file contents into memory."""
        with open(self.file_path, 'r', encoding='utf-8', errors='ignore') as f:
            self.lines = f.readlines()

    def parse(self) -> CoefficientDatabase:
        """Parse AQWA file and create coefficient database.

        Returns:
            Populated CoefficientDatabase instance
        """
        db = CoefficientDatabase()

        # Extract all added mass sections
        added_mass_sections = self._extract_added_mass_sections()
        for section in added_mass_sections:
            db.added_mass_matrices[section.frequency] = FrequencyDependentMatrix(
                frequency=section.frequency,
                matrix=section.data,
                matrix_type='added_mass'
            )

        # Extract all damping sections
        damping_sections = self._extract_damping_sections()
        for section in damping_sections:
            db.damping_matrices[section.frequency] = FrequencyDependentMatrix(
                frequency=section.frequency,
                matrix=section.data,
                matrix_type='damping'
            )

        # Build frequency array
        db.frequencies = np.array(sorted(db.added_mass_matrices.keys()))

        # Validate
        if len(db.added_mass_matrices) == 0:
            warnings.warn("No added mass matrices found in AQWA file")
        if len(db.damping_matrices) == 0:
            warnings.warn("No damping matrices found in AQWA file")

        return db

    def _extract_added_mass_sections(self) -> List[AQWASection]:
        """Extract all added mass matrix sections.

        Returns:
            List of AQWASection objects for added mass
        """
        sections = []
        search_start = 0

        while True:
            # Find next added mass section
            boundaries = AQWADataExtractor.find_section_boundaries(
                self.lines[search_start:],
                AQWADataExtractor.ADDED_MASS_HEADER
            )

            if boundaries is None:
                break

            start, end = boundaries
            start += search_start
            end += search_start

            # Extract frequency
            frequency = AQWADataExtractor.extract_frequency(
                self.lines, start - 5
            )

            if frequency is not None:
                # Extract matrix
                matrix = AQWADataExtractor.extract_matrix_from_lines(
                    self.lines, start + 1, end
                )

                sections.append(AQWASection(
                    section_type='ADDED_MASS',
                    frequency=frequency,
                    data=matrix,
                    header_info={}
                ))

            search_start = end + 1

            if search_start >= len(self.lines):
                break

        return sections

    def _extract_damping_sections(self) -> List[AQWASection]:
        """Extract all damping matrix sections.

        Returns:
            List of AQWASection objects for damping
        """
        sections = []
        search_start = 0

        while True:
            # Find next damping section
            boundaries = AQWADataExtractor.find_section_boundaries(
                self.lines[search_start:],
                AQWADataExtractor.DAMPING_HEADER
            )

            if boundaries is None:
                break

            start, end = boundaries
            start += search_start
            end += search_start

            # Extract frequency
            frequency = AQWADataExtractor.extract_frequency(
                self.lines, start - 5
            )

            if frequency is not None:
                # Extract matrix
                matrix = AQWADataExtractor.extract_matrix_from_lines(
                    self.lines, start + 1, end
                )

                sections.append(AQWASection(
                    section_type='DAMPING',
                    frequency=frequency,
                    data=matrix,
                    header_info={}
                ))

            search_start = end + 1

            if search_start >= len(self.lines):
                break

        return sections

    def export_to_csv(self, output_dir: Union[str, Path]):
        """Parse AQWA file and export matrices to CSV.

        Args:
            output_dir: Directory to save CSV files
        """
        db = self.parse()
        db.export_to_csv(output_dir)

    def get_frequency_summary(self) -> pd.DataFrame:
        """Get summary of available frequencies.

        Returns:
            DataFrame with frequency information
        """
        db = self.parse()

        summary = {
            'Frequency (rad/s)': db.frequencies,
            'Period (s)': 2 * np.pi / db.frequencies,
            'Has Added Mass': [
                freq in db.added_mass_matrices for freq in db.frequencies
            ],
            'Has Damping': [
                freq in db.damping_matrices for freq in db.frequencies
            ]
        }

        return pd.DataFrame(summary)

    @staticmethod
    def batch_parse(aqwa_files: List[Union[str, Path]],
                   output_dir: Union[str, Path]) -> CoefficientDatabase:
        """Parse multiple AQWA files and combine into single database.

        Args:
            aqwa_files: List of AQWA .LIS file paths
            output_dir: Directory to save combined CSV output

        Returns:
            Combined CoefficientDatabase
        """
        combined_db = CoefficientDatabase()

        for file_path in aqwa_files:
            parser = AQWAParser(file_path)
            db = parser.parse()

            # Merge databases
            combined_db.added_mass_matrices.update(db.added_mass_matrices)
            combined_db.damping_matrices.update(db.damping_matrices)

        # Rebuild frequency array
        combined_db.frequencies = np.array(
            sorted(combined_db.added_mass_matrices.keys())
        )

        # Export combined data
        combined_db.export_to_csv(output_dir)

        return combined_db

    def validate_data_quality(self) -> Dict[str, any]:
        """Validate quality and consistency of parsed data.

        Returns:
            Dictionary with validation results
        """
        db = self.parse()

        results = {
            'n_frequencies': len(db.frequencies),
            'frequency_range': db.get_frequency_range(),
            'missing_added_mass': [],
            'missing_damping': [],
            'asymmetric_matrices': [],
            'negative_diagonals': []
        }

        # Check for missing data
        for freq in db.frequencies:
            if freq not in db.added_mass_matrices:
                results['missing_added_mass'].append(freq)
            if freq not in db.damping_matrices:
                results['missing_damping'].append(freq)

        # Check symmetry and positivity
        for freq, matrix_obj in db.added_mass_matrices.items():
            if not matrix_obj.is_symmetric(rtol=1e-3):
                results['asymmetric_matrices'].append(('added_mass', freq))

            diag = matrix_obj.get_diagonal()
            if np.any(diag < 0):
                results['negative_diagonals'].append(('added_mass', freq))

        for freq, matrix_obj in db.damping_matrices.items():
            if not matrix_obj.is_symmetric(rtol=1e-3):
                results['asymmetric_matrices'].append(('damping', freq))

            diag = matrix_obj.get_diagonal()
            if np.any(diag < 0):
                results['negative_diagonals'].append(('damping', freq))

        return results

    def __repr__(self) -> str:
        """String representation of parser."""
        return f"AQWAParser(file='{self.file_path.name}', lines={len(self.lines)})"
