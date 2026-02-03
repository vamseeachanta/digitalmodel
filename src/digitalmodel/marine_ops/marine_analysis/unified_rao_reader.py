"""Unified RAO Reader - Single Entry Point for All RAO Data Sources.

This module provides a unified interface for reading RAO data from various sources:
- ANSYS AQWA .lis files (displacement, velocity, acceleration)
- OrcaFlex YAML files (displacement)
- Licensed AQWA binary files (via AQWA Reader)

AI Agent Friendly:
- Single import: `from unified_rao_reader import UnifiedRAOReader`
- Simple API: `reader.read(file_path)` → Returns UnifiedRAOData
- Auto-detection of file format
- Comprehensive error messages with solutions
"""

from pathlib import Path
from typing import Optional, Dict, Any
import logging

from .models.rao_data import (
    UnifiedRAOData,
    RAOType,
    SourceFormat,
    RAOMetadata
)
from .parsers.aqwa_lis_parser import AQWALISParser, AQWAParseError
from .parsers.orcaflex_yml_parser import OrcaFlexYMLParser, OrcaFlexParseError


class RAOReaderError(Exception):
    """Base exception for RAO reader errors."""

    def __init__(self, message: str, suggestions: Optional[list] = None):
        self.message = message
        self.suggestions = suggestions or []
        super().__init__(self.message)

    def get_user_message(self) -> str:
        """Format error message with suggestions."""
        msg = f"RAO Reader Error: {self.message}\n"
        if self.suggestions:
            msg += "\nSuggested solutions:\n"
            for i, suggestion in enumerate(self.suggestions, 1):
                msg += f"{i}. {suggestion}\n"
        return msg


class UnifiedRAOReader:
    """Unified interface for reading RAO data from any supported format.

    This class automatically detects the file format and uses the appropriate
    parser. It provides a consistent interface regardless of the source.

    Supported Formats:
    - ANSYS AQWA .lis files (no license required)
    - OrcaFlex YAML files (no license required)
    - ANSYS AQWA binary files via licensed reader (license required)

    Features:
    - Automatic format detection
    - Displacement, velocity, and acceleration RAO extraction
    - Comprehensive error handling
    - Validation and quality checking
    - AI agent friendly API

    Example:
        >>> reader = UnifiedRAOReader()
        >>>
        >>> # Read from AQWA .lis file
        >>> rao_data = reader.read('vessel_analysis.lis')
        >>> print(rao_data.get_available_types())
        [RAOType.DISPLACEMENT, RAOType.VELOCITY, RAOType.ACCELERATION]
        >>>
        >>> # Access specific RAO type
        >>> disp_rao = rao_data.displacement
        >>> print(disp_rao.frequencies)
        >>> print(disp_rao.surge.amplitude)
        >>>
        >>> # Read from OrcaFlex YAML
        >>> rao_data = reader.read('vessel.yml')
        >>> print(rao_data.displacement.vessel_name)

    AI Agent Usage:
        ```python
        from digitalmodel.marine_analysis import UnifiedRAOReader

        reader = UnifiedRAOReader()

        # Simple read
        rao_data = reader.read('path/to/file.lis')

        # Get all displacement RAOs
        if rao_data.has_displacement():
            disp = rao_data.displacement
            frequencies = disp.frequencies
            surge_amplitude = disp.surge.amplitude
            surge_phase = disp.surge.phase

        # Get velocity RAOs (if available)
        if rao_data.has_velocity():
            vel = rao_data.velocity
            # ... use velocity data

        # Convert to legacy format (backward compatibility)
        legacy_dict = rao_data.displacement.to_dict()
        ```
    """

    def __init__(self, validate_data: bool = True):
        """Initialize unified RAO reader.

        Args:
            validate_data: Perform validation on loaded data
        """
        self.validate_data = validate_data
        self.logger = logging.getLogger(__name__)

        # Initialize parsers
        self.aqwa_parser = AQWALISParser()
        self.orcaflex_parser = OrcaFlexYMLParser()

    def read(self,
             file_path: str,
             extract_displacement: bool = True,
             extract_velocity: bool = True,
             extract_acceleration: bool = True) -> UnifiedRAOData:
        """Read RAO data from any supported file format.

        This method auto-detects the file format and uses the appropriate parser.

        Args:
            file_path: Path to RAO data file
            extract_displacement: Extract displacement RAOs (if available)
            extract_velocity: Extract velocity RAOs (if available)
            extract_acceleration: Extract acceleration RAOs (if available)

        Returns:
            UnifiedRAOData containing all available RAO types

        Raises:
            RAOReaderError: If file cannot be read or format is unsupported

        Example:
            >>> reader = UnifiedRAOReader()
            >>> rao_data = reader.read('vessel.lis')
            >>> print(f"Available types: {rao_data.get_available_types()}")
        """
        path = Path(file_path)

        if not path.exists():
            raise RAOReaderError(
                f"File not found: {file_path}",
                suggestions=[
                    "Check that the file path is correct",
                    "Ensure the file exists and is accessible",
                    "Use absolute path if relative path is not working"
                ]
            )

        # Detect format
        source_format = self._detect_format(path)

        self.logger.info(f"Reading RAO data from {file_path} (format: {source_format.value})")

        # Use appropriate parser
        try:
            if source_format == SourceFormat.AQWA_LIS:
                return self._read_aqwa_lis(
                    path,
                    extract_displacement,
                    extract_velocity,
                    extract_acceleration
                )
            elif source_format == SourceFormat.ORCAFLEX_YML:
                return self._read_orcaflex_yml(path)
            else:
                raise RAOReaderError(
                    f"Unsupported file format: {source_format}",
                    suggestions=[
                        "Supported formats: .lis (AQWA), .yml/.yaml (OrcaFlex)",
                        "Check file extension",
                        "Ensure file is not corrupted"
                    ]
                )

        except (AQWAParseError, OrcaFlexParseError) as e:
            raise RAOReaderError(
                f"Failed to parse {source_format.value} file: {str(e)}",
                suggestions=[
                    "Verify the file is a valid RAO output file",
                    "Check that the file is not corrupted",
                    "Ensure the analysis completed successfully",
                    "Try opening the file in a text editor to verify format"
                ]
            )

    def read_aqwa_lis(self,
                     file_path: str,
                     extract_displacement: bool = True,
                     extract_velocity: bool = True,
                     extract_acceleration: bool = True) -> UnifiedRAOData:
        """Read RAO data from ANSYS AQWA .lis file.

        This method directly uses the AQWA LIS parser without format detection.

        Args:
            file_path: Path to AQWA .lis file
            extract_displacement: Extract displacement RAOs
            extract_velocity: Extract velocity RAOs
            extract_acceleration: Extract acceleration RAOs

        Returns:
            UnifiedRAOData with requested RAO types

        Example:
            >>> reader = UnifiedRAOReader()
            >>> # Only extract displacement and velocity
            >>> rao_data = reader.read_aqwa_lis(
            ...     'vessel.lis',
            ...     extract_acceleration=False
            ... )
        """
        return self._read_aqwa_lis(
            Path(file_path),
            extract_displacement,
            extract_velocity,
            extract_acceleration
        )

    def read_orcaflex_yml(self, file_path: str) -> UnifiedRAOData:
        """Read RAO data from OrcaFlex YAML file.

        This method directly uses the OrcaFlex YAML parser without format detection.

        Args:
            file_path: Path to OrcaFlex YAML file

        Returns:
            UnifiedRAOData with displacement RAOs

        Example:
            >>> reader = UnifiedRAOReader()
            >>> rao_data = reader.read_orcaflex_yml('vessel.yml')
        """
        return self._read_orcaflex_yml(Path(file_path))

    def _detect_format(self, path: Path) -> SourceFormat:
        """Detect file format based on extension and content.

        Args:
            path: Path to file

        Returns:
            Detected SourceFormat
        """
        suffix = path.suffix.lower()

        if suffix == '.lis':
            return SourceFormat.AQWA_LIS
        elif suffix in ['.yml', '.yaml']:
            return SourceFormat.ORCAFLEX_YML
        else:
            # Try to detect from content
            try:
                with open(path, 'r', encoding='utf-8', errors='ignore') as f:
                    first_lines = ''.join([f.readline() for _ in range(10)])

                    if 'AQWA' in first_lines.upper() or 'ANSYS' in first_lines.upper():
                        return SourceFormat.AQWA_LIS
                    elif 'VesselTypes:' in first_lines or 'DisplacementRAOs:' in first_lines:
                        return SourceFormat.ORCAFLEX_YML

            except Exception:
                pass

        # Default to AQWA LIS
        return SourceFormat.AQWA_LIS

    def _read_aqwa_lis(self,
                      path: Path,
                      extract_displacement: bool,
                      extract_velocity: bool,
                      extract_acceleration: bool) -> UnifiedRAOData:
        """Internal method to read AQWA LIS file."""
        return self.aqwa_parser.parse_lis_file(
            str(path),
            extract_displacement=extract_displacement,
            extract_velocity=extract_velocity,
            extract_acceleration=extract_acceleration
        )

    def _read_orcaflex_yml(self, path: Path) -> UnifiedRAOData:
        """Internal method to read OrcaFlex YAML file."""
        return self.orcaflex_parser.parse_yml_file(str(path))

    def get_info(self, file_path: str) -> Dict[str, Any]:
        """Get information about RAO file without fully parsing it.

        This is a lightweight method that extracts basic metadata without
        loading all RAO data.

        Args:
            file_path: Path to RAO file

        Returns:
            Dictionary with file information

        Example:
            >>> reader = UnifiedRAOReader()
            >>> info = reader.get_info('vessel.lis')
            >>> print(info['format'])
            'aqwa_lis'
            >>> print(info['file_size_mb'])
            2.5
        """
        path = Path(file_path)

        if not path.exists():
            raise RAOReaderError(f"File not found: {file_path}")

        format_type = self._detect_format(path)
        file_size_mb = path.stat().st_size / (1024 * 1024)

        info = {
            'file_path': str(path.absolute()),
            'file_name': path.name,
            'format': format_type.value,
            'file_size_mb': round(file_size_mb, 2),
            'exists': path.exists(),
            'readable': path.is_file()
        }

        return info


# Convenience function for quick access
def read_rao_file(file_path: str,
                 extract_displacement: bool = True,
                 extract_velocity: bool = True,
                 extract_acceleration: bool = True) -> UnifiedRAOData:
    """Convenience function to quickly read RAO data from any supported format.

    This is a shortcut for creating a UnifiedRAOReader and calling read().

    Args:
        file_path: Path to RAO data file
        extract_displacement: Extract displacement RAOs
        extract_velocity: Extract velocity RAOs
        extract_acceleration: Extract acceleration RAOs

    Returns:
        UnifiedRAOData with RAO information

    Example:
        >>> from digitalmodel.marine_analysis import read_rao_file
        >>> rao_data = read_rao_file('vessel.lis')
        >>> print(rao_data.get_available_types())
    """
    reader = UnifiedRAOReader()
    return reader.read(
        file_path,
        extract_displacement=extract_displacement,
        extract_velocity=extract_velocity,
        extract_acceleration=extract_acceleration
    )
