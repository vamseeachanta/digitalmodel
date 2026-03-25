"""
Conversion Result Model

Data model for conversion operation results.
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any


class ConversionStatus(Enum):
    """Status of a conversion operation."""

    SUCCESS = "success"
    PARTIAL = "partial"  # Conversion completed with warnings
    FAILED = "failed"


@dataclass
class ConversionResult:
    """Result of a format conversion operation.

    Attributes:
        status: Overall conversion status.
        source_format: Source data format.
        target_format: Target output format.
        output_files: List of generated output file paths.
        primary_output: Path to the main output file.
        timestamp: Time of conversion.
        duration_seconds: Time taken for conversion.
        source_info: Information about the source data.
        warnings: List of non-critical warnings.
        errors: List of errors (if status is FAILED).
        metadata: Additional conversion metadata.
    """

    status: ConversionStatus = ConversionStatus.SUCCESS
    source_format: str = ""
    target_format: str = ""
    output_files: list[Path] = field(default_factory=list)
    primary_output: Path | None = None
    timestamp: datetime = field(default_factory=datetime.now)
    duration_seconds: float = 0.0
    source_info: dict[str, Any] = field(default_factory=dict)
    warnings: list[str] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

    @property
    def is_success(self) -> bool:
        """Check if conversion was fully successful."""
        return self.status == ConversionStatus.SUCCESS

    @property
    def has_warnings(self) -> bool:
        """Check if conversion produced warnings."""
        return len(self.warnings) > 0

    @property
    def has_errors(self) -> bool:
        """Check if conversion produced errors."""
        return len(self.errors) > 0

    def add_warning(self, message: str) -> None:
        """Add a warning message.

        Args:
            message: Warning message.
        """
        self.warnings.append(message)
        if self.status == ConversionStatus.SUCCESS:
            self.status = ConversionStatus.PARTIAL

    def add_error(self, message: str) -> None:
        """Add an error message.

        Args:
            message: Error message.
        """
        self.errors.append(message)
        self.status = ConversionStatus.FAILED

    def add_output_file(self, path: Path | str, is_primary: bool = False) -> None:
        """Add an output file to the result.

        Args:
            path: Path to the output file.
            is_primary: Whether this is the primary output file.
        """
        path = Path(path)
        self.output_files.append(path)
        if is_primary:
            self.primary_output = path

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for serialization.

        Returns:
            Dictionary representation.
        """
        return {
            "status": self.status.value,
            "source_format": self.source_format,
            "target_format": self.target_format,
            "output_files": [str(p) for p in self.output_files],
            "primary_output": str(self.primary_output) if self.primary_output else None,
            "timestamp": self.timestamp.isoformat(),
            "duration_seconds": self.duration_seconds,
            "source_info": self.source_info,
            "warnings": self.warnings,
            "errors": self.errors,
            "metadata": self.metadata,
        }

    def summary(self) -> str:
        """Get human-readable summary of the conversion.

        Returns:
            Summary string.
        """
        lines = [
            f"Conversion: {self.source_format} -> {self.target_format}",
            f"Status: {self.status.value.upper()}",
            f"Duration: {self.duration_seconds:.2f}s",
            f"Output files: {len(self.output_files)}",
        ]

        if self.primary_output:
            lines.append(f"Primary output: {self.primary_output}")

        if self.warnings:
            lines.append(f"Warnings: {len(self.warnings)}")
            for w in self.warnings[:3]:  # Show first 3 warnings
                lines.append(f"  - {w}")
            if len(self.warnings) > 3:
                lines.append(f"  ... and {len(self.warnings) - 3} more")

        if self.errors:
            lines.append(f"Errors: {len(self.errors)}")
            for e in self.errors[:3]:  # Show first 3 errors
                lines.append(f"  - {e}")
            if len(self.errors) > 3:
                lines.append(f"  ... and {len(self.errors) - 3} more")

        return "\n".join(lines)

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "ConversionResult":
        """Create from dictionary representation.

        Args:
            data: Dictionary with conversion result data.

        Returns:
            ConversionResult instance.
        """
        return cls(
            status=ConversionStatus(data.get("status", "success")),
            source_format=data.get("source_format", ""),
            target_format=data.get("target_format", ""),
            output_files=[Path(p) for p in data.get("output_files", [])],
            primary_output=Path(data["primary_output"]) if data.get("primary_output") else None,
            timestamp=datetime.fromisoformat(data["timestamp"]) if data.get("timestamp") else datetime.now(),
            duration_seconds=data.get("duration_seconds", 0.0),
            source_info=data.get("source_info", {}),
            warnings=data.get("warnings", []),
            errors=data.get("errors", []),
            metadata=data.get("metadata", {}),
        )
