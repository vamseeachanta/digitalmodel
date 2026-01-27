"""
BEMRosetta Exception Classes

Custom exceptions for error handling throughout the BEMRosetta module.
All exceptions inherit from BEMRosettaError for unified error handling.

Exception Hierarchy:
    BEMRosettaError (base)
    |- ParserError      - File parsing failures
    |- ConverterError   - Format conversion failures
    |- ValidationError  - Data validation failures
    |- MeshError        - Mesh processing failures
    |- ExecutableNotFoundError - BEMRosetta_cl.exe not found

Each exception includes:
    - error_code: Programmatic identifier for the error type
    - context: Dictionary with additional debugging information
    - suggestions: List of potential resolutions
    - to_dict(): Serialization for logging/reporting
"""

from pathlib import Path
from typing import Any


class BEMRosettaError(Exception):
    """Base exception for all BEMRosetta module errors.

    Provides common functionality including error codes, context information,
    and suggestions for resolution.

    Attributes:
        message: Human-readable error description.
        error_code: Programmatic identifier for error type.
        context: Additional debugging information as key-value pairs.
        suggestions: List of potential fixes or next steps.
    """

    def __init__(
        self,
        message: str,
        error_code: str | None = None,
        context: dict[str, Any] | None = None,
        suggestions: list[str] | None = None,
    ):
        """Initialize BEMRosetta exception.

        Args:
            message: Human-readable error description.
            error_code: Optional error code for programmatic handling.
            context: Optional context information for debugging.
            suggestions: Optional list of suggestions for resolution.
        """
        super().__init__(message)
        self.message = message
        self.error_code = error_code or "BEMROSETTA_ERROR"
        self.context = context or {}
        self.suggestions = suggestions or []

    def __str__(self) -> str:
        """String representation including error code and context."""
        result = f"[{self.error_code}] {self.message}"

        if self.context:
            context_str = ", ".join(f"{k}={v}" for k, v in self.context.items())
            result += f"\nContext: {context_str}"

        if self.suggestions:
            result += "\nSuggestions:"
            for suggestion in self.suggestions:
                result += f"\n  - {suggestion}"

        return result

    def to_dict(self) -> dict[str, Any]:
        """Convert exception to dictionary for serialization.

        Returns:
            Dictionary with error details suitable for JSON serialization.
        """
        return {
            "type": self.__class__.__name__,
            "error_code": self.error_code,
            "message": self.message,
            "context": self.context,
            "suggestions": self.suggestions,
        }


class ParserError(BEMRosettaError):
    """Error during file parsing operations.

    Raised when:
    - File format is invalid or unsupported
    - Required data sections are missing
    - Data values cannot be interpreted
    """

    def __init__(
        self,
        message: str,
        file_path: Path | str | None = None,
        line_number: int | None = None,
        **kwargs,
    ):
        """Initialize parser error.

        Args:
            message: Error description.
            file_path: Path to the file being parsed.
            line_number: Line number where error occurred.
            **kwargs: Additional arguments for base class.
        """
        context = kwargs.setdefault("context", {})
        if file_path is not None:
            context["file_path"] = file_path if isinstance(file_path, Path) else Path(file_path)
        if line_number is not None:
            context["line_number"] = line_number

        kwargs.setdefault("error_code", "PARSER_ERROR")
        kwargs.setdefault("suggestions", []).extend([
            "Check file format matches expected BEM solver output",
            "Verify file is not corrupted or truncated",
            "Ensure file encoding is correct (typically ASCII or UTF-8)",
            "Review file extension matches content type",
        ])

        super().__init__(message, **kwargs)


class ConverterError(BEMRosettaError):
    """Error during format conversion operations.

    Raised when:
    - Input data is incomplete or invalid
    - Output format requirements cannot be met
    - Conversion rules produce invalid results
    """

    def __init__(
        self,
        message: str,
        source_format: str | None = None,
        target_format: str | None = None,
        **kwargs,
    ):
        """Initialize converter error.

        Args:
            message: Error description.
            source_format: Source format being converted from.
            target_format: Target format being converted to.
            **kwargs: Additional arguments for base class.
        """
        context = kwargs.setdefault("context", {})
        if source_format is not None:
            context["source_format"] = source_format
        if target_format is not None:
            context["target_format"] = target_format

        kwargs.setdefault("error_code", "CONVERTER_ERROR")
        kwargs.setdefault("suggestions", []).extend([
            "Verify input data completeness before conversion",
            "Check source format is supported",
            "Review target format requirements",
            "Ensure all required fields are present",
        ])

        super().__init__(message, **kwargs)


class ValidationError(BEMRosettaError):
    """Error during data validation.

    Raised when:
    - Coefficient values exceed physical limits
    - Symmetry requirements are violated
    - Causality checks fail
    - Required data is missing
    """

    def __init__(
        self,
        message: str,
        validation_errors: list[str] | None = None,
        field: str | None = None,
        **kwargs,
    ):
        """Initialize validation error.

        Args:
            message: Error description.
            validation_errors: List of specific validation failures.
            field: Name of the field that failed validation.
            **kwargs: Additional arguments for base class.
        """
        context = kwargs.setdefault("context", {})
        if validation_errors is not None:
            context["validation_errors"] = validation_errors
        if field is not None:
            context["field"] = field

        kwargs.setdefault("error_code", "VALIDATION_ERROR")
        kwargs.setdefault("suggestions", []).extend([
            "Review input data format and types",
            "Check coefficient values are within physical limits",
            "Verify matrix symmetry requirements",
            "Ensure all required fields are present",
        ])

        super().__init__(message, **kwargs)


class MeshError(BEMRosettaError):
    """Error during mesh operations.

    Raised when:
    - Mesh file format is invalid
    - Mesh topology is inconsistent
    - Panel normals are inverted
    - Mesh quality checks fail
    """

    def __init__(
        self,
        message: str,
        mesh_file: Path | str | None = None,
        element_count: int | None = None,
        **kwargs,
    ):
        """Initialize mesh error.

        Args:
            message: Error description.
            mesh_file: Path to the mesh file.
            element_count: Number of mesh elements.
            **kwargs: Additional arguments for base class.
        """
        context = kwargs.setdefault("context", {})
        if mesh_file is not None:
            context["mesh_file"] = mesh_file if isinstance(mesh_file, Path) else Path(mesh_file)
        if element_count is not None:
            context["element_count"] = element_count

        kwargs.setdefault("error_code", "MESH_ERROR")
        kwargs.setdefault("suggestions", []).extend([
            "Check mesh file format is supported (GDF, DAT, STL)",
            "Verify mesh topology is consistent",
            "Ensure panel normals point outward",
            "Review mesh quality metrics",
        ])

        super().__init__(message, **kwargs)


class ExecutableNotFoundError(BEMRosettaError):
    """BEMRosetta executable not found or not accessible.

    Raised when:
    - BEMRosetta_cl.exe is not in PATH or specified location
    - Executable permissions are insufficient
    - Required DLLs are missing
    """

    def __init__(
        self,
        message: str = "BEMRosetta executable not found",
        executable_path: Path | str | None = None,
        searched_paths: list[str] | None = None,
        **kwargs,
    ):
        """Initialize executable not found error.

        Args:
            message: Error description.
            executable_path: Expected path to executable.
            searched_paths: List of paths that were searched.
            **kwargs: Additional arguments for base class.
        """
        context = kwargs.setdefault("context", {})
        if executable_path is not None:
            context["executable_path"] = executable_path if isinstance(executable_path, Path) else Path(executable_path)
        if searched_paths is not None:
            context["searched_paths"] = searched_paths

        kwargs.setdefault("error_code", "EXECUTABLE_NOT_FOUND")
        kwargs.setdefault("suggestions", []).extend([
            "Install BEMRosetta from https://github.com/BEMRosetta/BEMRosetta",
            "Add BEMRosetta_cl.exe to system PATH",
            "Set BEMROSETTA_PATH environment variable",
            "Specify executable path explicitly in configuration",
        ])

        super().__init__(message, **kwargs)
