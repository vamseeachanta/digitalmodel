"""Conversion protocols and report dataclass.

The FormatConverter protocol is module-agnostic: other modules
(mooring_analysis, hydrodynamics, etc.) can implement it when ready.
Until a second module needs it, it stays in the OrcaFlex package.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Protocol, runtime_checkable


@dataclass
class ConversionReport:
    """Report from a format conversion operation."""

    success: bool
    source_format: str
    target_format: str
    source_path: Path
    target_path: Path
    unmapped_sections: list[str] = field(default_factory=list)
    confidence: float = 1.0
    warnings: list[str] = field(default_factory=list)

    def summary(self) -> str:
        """One-line summary for CLI output."""
        status = "OK" if self.success else "FAILED"
        parts = [f"[{status}] {self.source_format} -> {self.target_format}"]
        if self.confidence < 1.0:
            parts.append(f"confidence={self.confidence:.2f}")
        if self.unmapped_sections:
            parts.append(f"unmapped={len(self.unmapped_sections)}")
        if self.warnings:
            parts.append(f"warnings={len(self.warnings)}")
        return " | ".join(parts)


@runtime_checkable
class FormatConverter(Protocol):
    """Generic interface for format converters.

    Other modules can implement this protocol when they need
    their own format conversion.
    """

    def convert(self, source: Path, target: Path) -> ConversionReport:
        """Convert source to target format."""
        ...

    def supported_formats(self) -> tuple[str, str]:
        """Return (source_format, target_format) pair."""
        ...
