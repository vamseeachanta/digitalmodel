"""Base converter class for output format converters."""

from abc import abstractmethod
from pathlib import Path
from typing import Any, List, Optional

from loguru import logger

from digitalmodel.modules.diffraction import DiffractionResults
from ..core.interfaces import ConverterInterface


class BaseConverter(ConverterInterface):
    """Base class for output format converters.

    Provides common functionality for validating input data and managing
    output directories. Subclasses must implement the convert() method
    and output_format property.

    Attributes:
        output_dir: Directory for output files.
    """

    def __init__(self, output_dir: Optional[Path] = None):
        """Initialize the base converter.

        Args:
            output_dir: Directory for output files. Defaults to current
                working directory if not specified.
        """
        self.output_dir = Path(output_dir) if output_dir else Path.cwd()

    @property
    @abstractmethod
    def output_format(self) -> str:
        """Name of the output format this converter produces.

        Returns:
            Format name (e.g., 'orcaflex', 'hydrod').
        """

    @abstractmethod
    def convert(self, data: Any, output_dir: Optional[Path] = None) -> Path:
        """Convert data to target format and write to output directory.

        Args:
            data: Input data to convert.
            output_dir: Optional override for output directory.

        Returns:
            Path to the primary output file or directory.
        """

    def validate_input(self, results: DiffractionResults) -> List[str]:
        """Validate input DiffractionResults, return list of warnings.

        Checks for presence of required data components (RAOs, added mass,
        damping) and returns warnings for any missing data.

        Args:
            results: DiffractionResults to validate.

        Returns:
            List of warning messages for missing or incomplete data.
        """
        warnings = []

        if results.raos is None:
            warnings.append("No RAO data available")
        if results.added_mass is None:
            warnings.append("No added mass data available")
        if results.damping is None:
            warnings.append("No damping data available")

        return warnings

    def _ensure_output_dir(self) -> None:
        """Create output directory if it doesn't exist.

        Creates the full directory path including any necessary parent
        directories.
        """
        self.output_dir.mkdir(parents=True, exist_ok=True)
