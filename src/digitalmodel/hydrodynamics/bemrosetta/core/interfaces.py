"""
BEMRosetta Interface Definitions

Abstract base classes defining contracts for module components.
All implementations must adhere to these interfaces for consistency.
"""

from abc import ABC, abstractmethod
from pathlib import Path
from typing import Any, TypeVar

# Generic type for parser results
T = TypeVar("T")


class ParserInterface(ABC):
    """Abstract interface for BEM solver output parsers.

    Parsers read solver output files and extract hydrodynamic data
    into standardized data models.
    """

    @abstractmethod
    def parse(self, file_path: Path | str) -> Any:
        """Parse a file and return structured data.

        Args:
            file_path: Path to the input file.

        Returns:
            Parsed data in standardized format.

        Raises:
            ParserError: If parsing fails.
            FileNotFoundError: If file does not exist.
        """

    @abstractmethod
    def can_parse(self, file_path: Path | str) -> bool:
        """Check if this parser can handle the given file.

        Args:
            file_path: Path to the file to check.

        Returns:
            True if this parser supports the file format.
        """

    @property
    @abstractmethod
    def supported_extensions(self) -> list[str]:
        """List of file extensions this parser supports.

        Returns:
            List of extensions (e.g., ['.lis', '.LIS']).
        """


class ConverterInterface(ABC):
    """Abstract interface for format converters.

    Converters transform parsed hydrodynamic data into output formats
    suitable for target applications (e.g., OrcaFlex).
    """

    @abstractmethod
    def convert(self, data: Any, output_dir: Path | str) -> Path:
        """Convert data to target format and write to output directory.

        Args:
            data: Parsed hydrodynamic data.
            output_dir: Directory for output files.

        Returns:
            Path to the primary output file.

        Raises:
            ConverterError: If conversion fails.
        """

    @abstractmethod
    def validate_input(self, data: Any) -> list[str]:
        """Validate input data before conversion.

        Args:
            data: Data to validate.

        Returns:
            List of validation warnings (empty if valid).

        Raises:
            ValidationError: If data is invalid for conversion.
        """

    @property
    @abstractmethod
    def output_format(self) -> str:
        """Name of the output format this converter produces.

        Returns:
            Format name (e.g., 'OrcaFlex YAML').
        """


class MeshHandlerInterface(ABC):
    """Abstract interface for mesh format handlers.

    Mesh handlers read and write panel mesh files in various formats
    used by BEM solvers.
    """

    @abstractmethod
    def read(self, file_path: Path | str) -> Any:
        """Read mesh data from file.

        Args:
            file_path: Path to the mesh file.

        Returns:
            PanelMesh data model.

        Raises:
            MeshError: If reading fails.
            FileNotFoundError: If file does not exist.
        """

    @abstractmethod
    def write(self, mesh: Any, file_path: Path | str) -> None:
        """Write mesh data to file.

        Args:
            mesh: PanelMesh data model.
            file_path: Path for output file.

        Raises:
            MeshError: If writing fails.
        """

    @property
    @abstractmethod
    def format_name(self) -> str:
        """Name of the mesh format this handler supports.

        Returns:
            Format name (e.g., 'GDF', 'DAT', 'STL').
        """

    @property
    @abstractmethod
    def file_extension(self) -> str:
        """Primary file extension for this format.

        Returns:
            Extension including dot (e.g., '.gdf').
        """


class ValidatorInterface(ABC):
    """Abstract interface for data validators.

    Validators check hydrodynamic data for quality, consistency,
    and physical validity.
    """

    @abstractmethod
    def validate(self, data: Any) -> "ValidationReport":
        """Validate data and return detailed report.

        Args:
            data: Data to validate.

        Returns:
            ValidationReport with results.
        """

    @abstractmethod
    def is_valid(self, data: Any) -> bool:
        """Quick check if data passes validation.

        Args:
            data: Data to validate.

        Returns:
            True if data passes all validation checks.
        """


class ValidationReport:
    """Container for validation results.

    Attributes:
        is_valid: Overall validation status.
        errors: Critical issues that must be fixed.
        warnings: Non-critical issues to review.
        info: Informational messages.
        metrics: Quantitative validation metrics.
    """

    def __init__(self):
        self.is_valid: bool = True
        self.errors: list[str] = []
        self.warnings: list[str] = []
        self.info: list[str] = []
        self.metrics: dict[str, Any] = {}

    def add_error(self, message: str) -> None:
        """Add error and mark report as invalid."""
        self.errors.append(message)
        self.is_valid = False

    def add_warning(self, message: str) -> None:
        """Add warning message."""
        self.warnings.append(message)

    def add_info(self, message: str) -> None:
        """Add informational message."""
        self.info.append(message)

    def set_metric(self, name: str, value: Any) -> None:
        """Set a validation metric."""
        self.metrics[name] = value

    def __repr__(self) -> str:
        status = "VALID" if self.is_valid else "INVALID"
        return (
            f"ValidationReport(status={status}, "
            f"errors={len(self.errors)}, warnings={len(self.warnings)})"
        )
