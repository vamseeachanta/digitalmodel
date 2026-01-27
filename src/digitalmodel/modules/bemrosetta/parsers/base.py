"""
Base parser class for BEM solver outputs.

Provides common functionality for all BEM solver output parsers including:
- File existence validation
- Extension checking
- Metadata storage
"""

from abc import abstractmethod
from pathlib import Path
from typing import Any, Optional

from loguru import logger

from ..core.interfaces import ParserInterface
from ..core.exceptions import ParserError
from ..models import BEMSolverMetadata


class BaseParser(ParserInterface):
    """Base class for BEM solver output parsers.

    Provides common functionality for parsing BEM solver output files.
    Subclasses must implement the abstract methods and properties.

    Attributes:
        _metadata: Solver metadata extracted during parsing.
    """

    def __init__(self):
        """Initialize base parser."""
        self._metadata: Optional[BEMSolverMetadata] = None

    @property
    def metadata(self) -> Optional[BEMSolverMetadata]:
        """Get metadata extracted from parsed file.

        Returns:
            BEMSolverMetadata if a file has been parsed, None otherwise.
        """
        return self._metadata

    @property
    @abstractmethod
    def supported_extensions(self) -> list[str]:
        """List of file extensions this parser supports.

        Returns:
            List of extensions (e.g., ['.lis', '.LIS']).
        """

    @property
    @abstractmethod
    def solver_name(self) -> str:
        """Name of the BEM solver this parser handles.

        Returns:
            Solver name string (e.g., 'AQWA', 'WAMIT').
        """

    def can_parse(self, file_path: Path | str) -> bool:
        """Check if this parser can handle the given file.

        Verifies that:
        1. The file exists
        2. The file extension matches supported extensions

        Args:
            file_path: Path to the file to check.

        Returns:
            True if this parser supports the file format.
        """
        path = Path(file_path)

        if not path.exists():
            return False

        suffix = path.suffix.lower()
        supported_lower = [ext.lower() for ext in self.supported_extensions]

        return suffix in supported_lower

    def _validate_file_exists(self, file_path: Path) -> None:
        """Validate that a file exists.

        Args:
            file_path: Path to validate.

        Raises:
            ParserError: If file does not exist.
        """
        if not file_path.exists():
            raise ParserError(
                f"File not found: {file_path}",
                file_path=str(file_path),
            )

    @abstractmethod
    def parse(self, file_path: Path | str) -> Any:
        """Parse a file and return structured data.

        Args:
            file_path: Path to the input file.

        Returns:
            Parsed data in standardized format.

        Raises:
            ParserError: If parsing fails.
        """
