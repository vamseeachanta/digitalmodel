# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""An explicit, shell-free DWG-to-DXF converter interface; no bundled tool."""

import math
import subprocess
import tempfile
from dataclasses import dataclass, replace
from pathlib import Path
from string import Formatter

from .dxf import read_dxf
from .models import CadGeometry, CadReadError, LengthUnit


@dataclass(frozen=True)
class DwgConverter:
    """Caller-configured argument vector and retained conversion directory.

    ``command`` contains both ``{input}`` and ``{output}`` placeholders. Other
    fields, format specifications and conversions are refused. No shell is
    used. Supply an absolute executable path or an executable available on PATH.
    The caller supplies a writable scratch directory and a positive timeout.
    The tool must preserve coordinate values and units in its DXF output.
    """

    command: tuple[str, ...]
    timeout_seconds: float
    work_dir: Path

    def __post_init__(self) -> None:
        if (
            not isinstance(self.command, (tuple, list))
            or not self.command
            or any(
                not isinstance(arg, str) or not arg or "\0" in arg
                for arg in self.command
            )
        ):
            raise CadReadError("Converter command must be a nonempty argument vector")
        fields = set()
        try:
            for arg in self.command:
                for _, field, spec, conversion in Formatter().parse(arg):
                    if field is None:
                        continue
                    if field not in {"input", "output"} or spec or conversion:
                        raise ValueError
                    fields.add(field)
        except ValueError:
            raise CadReadError("Converter command has invalid placeholders") from None
        if fields != {"input", "output"}:
            raise CadReadError(
                "Converter command requires input and output placeholders"
            )
        if (
            not isinstance(self.timeout_seconds, (int, float))
            or not math.isfinite(self.timeout_seconds)
            or self.timeout_seconds <= 0
        ):
            raise CadReadError("Converter timeout must be finite and positive")
        object.__setattr__(self, "command", tuple(self.command))
        object.__setattr__(self, "work_dir", Path(self.work_dir))


def read_dwg(
    path: Path, units: LengthUnit | None, converter: DwgConverter
) -> CadGeometry:
    """Convert to a fresh retained DXF then apply the ordinary DXF unit gate.

    Converter diagnostics remain in the supplied scratch directory. Diagnostics
    and command arguments are never included in an exception message. A
    successful converter exit is not evidence of geometric fidelity.
    """
    if not converter.work_dir.is_dir():
        raise CadReadError("Converter scratch directory does not exist")
    try:
        directory = Path(
            tempfile.mkdtemp(prefix="cad-converter-", dir=converter.work_dir)
        )
        output = directory / "converted.dxf"
        command = [
            arg.format(input=str(path.resolve()), output=str(output.resolve()))
            for arg in converter.command
        ]
        with (directory / "converter.log").open("wb") as log:
            result = subprocess.run(
                command,
                cwd=directory,
                stdin=subprocess.DEVNULL,
                stdout=log,
                stderr=subprocess.STDOUT,
                timeout=converter.timeout_seconds,
                check=False,
            )
    except subprocess.TimeoutExpired:
        raise CadReadError(
            "Converter timed out; inspect retained diagnostics"
        ) from None
    except OSError:
        raise CadReadError("Converter could not start or write its output") from None
    if result.returncode != 0:
        raise CadReadError("Converter failed; inspect retained diagnostics")
    if output.is_symlink() or not output.is_file():
        raise CadReadError("Converter did not create a regular DXF output")
    return replace(read_dxf(output, units), format="dwg")
