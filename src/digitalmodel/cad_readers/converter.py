# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Explicit DWG conversion call form; no converter is bundled or discovered."""

import math
import subprocess
from dataclasses import dataclass
from pathlib import Path

from .models import CadReadError


@dataclass(frozen=True)
class Converter:
    """An executable argument vector with input/output placeholders and a budget."""

    argv: tuple[str, ...]
    timeout_seconds: float

    def __post_init__(self) -> None:
        """Validate the call form without executing anything."""
        if (
            not self.argv
            or not all(isinstance(a, str) and a for a in self.argv)
            or "{input}" not in self.argv
            or "{output}" not in self.argv
        ):
            raise ValueError(
                "Converter needs standalone {input} and {output} arguments"
            )
        if not math.isfinite(self.timeout_seconds) or self.timeout_seconds <= 0:
            raise ValueError("Converter timeout must be finite and positive")

    def convert(self, source: Path, output: Path) -> None:
        """Retain converted output for review; refuse any existing destination."""
        if output.exists() or output.is_symlink():
            raise CadReadError("Conversion output already exists")
        if output.suffix.lower() != ".dxf":
            raise CadReadError("Conversion output must be DXF")
        argv = [
            str(source) if a == "{input}" else str(output) if a == "{output}" else a
            for a in self.argv
        ]
        try:
            # Converter diagnostics can include sensitive drawing content.
            # Do not echo them or include raw exception text in errors.
            subprocess.run(
                argv,
                shell=False,
                check=True,
                timeout=self.timeout_seconds,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
        except (OSError, subprocess.SubprocessError) as exc:
            raise CadReadError("DWG conversion failed or timed out") from exc
        if not output.is_file():
            raise CadReadError("DWG converter produced no DXF file")
