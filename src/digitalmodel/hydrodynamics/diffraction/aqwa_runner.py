"""AQWA diffraction analysis runner (WRK-025).

Bridges DiffractionSpec -> file generation -> AQWA execution -> result capture.

Data flow::

    DiffractionSpec (spec.yml)
      -> AQWABackend.generate_single()          [existing, WRK-058]
      -> AQWARunner.prepare()                    [generates .dat file, copies mesh, validates]
      -> AQWARunner.execute()                    [invokes aqwa.exe /nowind or dry-run]
      -> AQWARunResult                           [status, paths, logs, duration]

Executable detection order
--------------------------
1. ``AQWARunConfig.executable_path`` (explicit)
2. ``AQWA_PATH`` environment variable
3. Standard ANSYS install paths (Windows only)
4. ``shutil.which("aqwa")``
5. ``None`` -> triggers dry-run mode

Dry-run mode
------------
When the executable is not found or ``AQWARunConfig.dry_run`` is True, the runner
performs all file preparation steps (input generation, mesh copying, validation)
but skips the actual solver invocation.  This is useful for CI, pre-flight
checks, and environments without an AQWA license.
"""

from __future__ import annotations

import os
import platform
import shutil
import subprocess
import time
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any

from pydantic import BaseModel, Field, field_validator

from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------


class AQWARunStatus(str, Enum):
    """Status of an AQWA runner execution."""

    PENDING = "pending"
    PREPARING = "preparing"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    DRY_RUN = "dry_run"
    CANCELLED = "cancelled"


# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------


class AQWARunConfig(BaseModel):
    """Configuration for an AQWA runner instance.

    All paths accept ``str`` or ``Path``; they are coerced to ``Path``.
    """

    executable_path: Path | None = Field(
        default=None,
        description="Explicit path to aqwa executable. Auto-detected if None.",
    )
    output_dir: Path | None = Field(
        default=None,
        description="Directory for generated files and results.",
    )
    dry_run: bool = Field(
        default=False,
        description="If True, generate files but skip solver invocation.",
    )
    timeout_seconds: int = Field(
        default=7200,
        gt=0,
        description="Maximum solver execution time in seconds.",
    )
    copy_mesh_files: bool = Field(
        default=True,
        description=(
            "Copy mesh files referenced by the spec into the output directory."
        ),
    )
    extra_args: list[str] = Field(
        default_factory=list,
        description="Additional command-line arguments passed to the solver.",
    )
    nowind: bool = Field(
        default=True,
        description="Pass /nowind flag for headless (no GUI window) mode.",
    )

    model_config = {"arbitrary_types_allowed": True}

    @field_validator("executable_path", mode="before")
    @classmethod
    def coerce_executable_path(cls, v: Any) -> Path | None:
        if v is None:
            return None
        return Path(v)

    @field_validator("output_dir", mode="before")
    @classmethod
    def coerce_output_dir(cls, v: Any) -> Path | None:
        if v is None:
            return None
        return Path(v)


# ---------------------------------------------------------------------------
# Result
# ---------------------------------------------------------------------------


@dataclass
class AQWARunResult:
    """Result of an AQWA runner execution."""

    status: AQWARunStatus
    input_file: Path | None = None
    output_dir: Path | None = None
    mesh_files: list[Path] = field(default_factory=list)
    log_file: Path | None = None
    lis_file: Path | None = None
    stdout: str = ""
    stderr: str = ""
    return_code: int | None = None
    duration_seconds: float | None = None
    error_message: str | None = None
    spec_name: str | None = None


# ---------------------------------------------------------------------------
# Standard ANSYS install paths (Windows only)
# ---------------------------------------------------------------------------

_STANDARD_PATHS: list[str] = []
if platform.system() == "Windows":
    _STANDARD_PATHS = [
        r"C:\Program Files\ANSYS Inc\v252\aqwa\bin\winx64\Aqwa.exe",
        r"C:\Program Files\ANSYS Inc\v251\aqwa\bin\winx64\Aqwa.exe",
        r"C:\Program Files\ANSYS Inc\v241\aqwa\bin\winx64\Aqwa.exe",
        r"C:\Program Files\ANSYS Inc\v232\aqwa\bin\winx64\Aqwa.exe",
        r"C:\Program Files\ANSYS Inc\v231\aqwa\bin\winx64\Aqwa.exe",
        r"C:\Program Files\ANSYS Inc\v222\aqwa\bin\winx64\Aqwa.exe",
    ]


# ---------------------------------------------------------------------------
# Runner
# ---------------------------------------------------------------------------


class AQWARunner:
    """Orchestrates AQWA input preparation and solver execution.

    Example::

        from digitalmodel.hydrodynamics.diffraction.input_schemas import (
            DiffractionSpec,
        )
        from digitalmodel.hydrodynamics.diffraction.aqwa_runner import (
            AQWARunner, AQWARunConfig,
        )

        spec = DiffractionSpec.from_yaml("analysis.yml")
        runner = AQWARunner(AQWARunConfig(output_dir="output", dry_run=True))
        result = runner.run(spec)
        print(result.status, result.input_file)
    """

    def __init__(self, config: AQWARunConfig | None = None) -> None:
        self._config = config or AQWARunConfig()
        self._backend = AQWABackend()
        self._result: AQWARunResult | None = None

    @property
    def result(self) -> AQWARunResult | None:
        """Most recent run result, or None if no run has been performed."""
        return self._result

    # ----- public API -----

    def run(
        self, spec: DiffractionSpec, spec_path: Path | str | None = None
    ) -> AQWARunResult:
        """Execute the full pipeline: prepare + execute.

        If the executable cannot be detected and ``dry_run`` is False,
        the runner falls back to dry-run mode automatically.

        Args:
            spec: Canonical diffraction specification.
            spec_path: Path to the source YAML file (for resolving relative
                       mesh paths). Optional.
        """
        self.prepare(spec, spec_path=spec_path)

        if not self._config.dry_run:
            exe = self._detect_executable()
            if exe is None:
                self._result.status = AQWARunStatus.DRY_RUN
                self._result.duration_seconds = 0.0
                return self._result

        result = self.execute()
        return result

    def prepare(
        self,
        spec: DiffractionSpec,
        spec_path: Path | str | None = None,
    ) -> AQWARunResult:
        """Generate .dat input file, copy mesh files, and validate.

        Args:
            spec: Canonical diffraction specification.
            spec_path: Path to the source YAML file (for resolving relative
                       mesh paths). Optional.

        Returns a AQWARunResult with status PREPARING (ready for execute).
        """
        output_dir = self._config.output_dir or Path("output")
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        spec_name = self._derive_spec_name(spec)
        spec_dir = Path(spec_path).parent if spec_path else None

        self._result = AQWARunResult(
            status=AQWARunStatus.PREPARING,
            output_dir=output_dir,
            spec_name=spec_name,
        )

        # Generate single .dat input file via AQWABackend
        input_file = self._generate_input_file(spec, output_dir, spec_dir=spec_dir)
        self._result.input_file = input_file

        # Copy mesh files
        if self._config.copy_mesh_files:
            mesh_files = self._copy_mesh_files(
                spec, output_dir, spec_dir=spec_dir
            )
            self._result.mesh_files = mesh_files

        # Validate mesh references
        warnings = self._validate_mesh_references(spec, output_dir)
        if warnings:
            self._result.error_message = "; ".join(warnings)

        return self._result

    def execute(self) -> AQWARunResult:
        """Invoke the AQWA solver or perform dry-run.

        Must be called after prepare().
        """
        if self._result is None:
            msg = "prepare() must be called before execute()"
            raise RuntimeError(msg)

        start = time.monotonic()

        if self._config.dry_run:
            self._result.status = AQWARunStatus.DRY_RUN
            self._result.duration_seconds = time.monotonic() - start
            return self._result

        exe = self._detect_executable()
        if exe is None:
            self._result.status = AQWARunStatus.DRY_RUN
            self._result.duration_seconds = time.monotonic() - start
            return self._result

        self._result.status = AQWARunStatus.RUNNING
        return_code, stdout, stderr = self._invoke_solver(
            executable=exe,
            input_file=self._result.input_file,
            output_dir=self._result.output_dir,
            timeout=self._config.timeout_seconds,
            extra_args=self._config.extra_args,
            nowind=self._config.nowind,
        )

        elapsed = time.monotonic() - start
        self._result.stdout = stdout
        self._result.stderr = stderr
        self._result.return_code = return_code
        self._result.duration_seconds = elapsed

        # Capture output files first (needed for error detection)
        self._result.lis_file = self._capture_lis_file(self._result.output_dir)
        self._result.log_file = self._capture_log(self._result.output_dir)

        # AQWA writes errors to stdout/LIS and may return 0 even on failure.
        # Check for error patterns in stdout, stderr, and LIS file.
        lis_content = ""
        if self._result.lis_file and self._result.lis_file.exists():
            lis_content = self._result.lis_file.read_text(errors="replace")

        aqwa_error = self._detect_aqwa_error(
            stdout, stderr, return_code, lis_content
        )

        if aqwa_error is None:
            self._result.status = AQWARunStatus.COMPLETED
        else:
            self._result.status = AQWARunStatus.FAILED
            self._result.error_message = aqwa_error

        return self._result

    # ----- internal methods -----

    def _detect_executable(self) -> Path | None:
        """Auto-detect AQWA executable.

        Search order:
        1. AQWARunConfig.executable_path (explicit)
        2. AQWA_PATH environment variable
        3. Standard ANSYS install paths
        4. shutil.which("aqwa")
        """
        if self._config.executable_path is not None:
            if self._config.executable_path.exists():
                return self._config.executable_path
            return None

        env_path = os.environ.get("AQWA_PATH")
        if env_path:
            p = Path(env_path)
            if p.exists():
                return p

        for std_path in _STANDARD_PATHS:
            p = Path(std_path)
            if p.exists():
                return p

        which_result = shutil.which("aqwa")
        if which_result:
            return Path(which_result)

        return None

    def _generate_input_file(
        self,
        spec: DiffractionSpec,
        output_dir: Path,
        spec_dir: Optional[Path] = None,
    ) -> Path:
        """Generate AQWA .dat input file using AQWABackend.generate_single().

        Args:
            spec: Canonical diffraction specification.
            output_dir: Directory for generated .dat file.
            spec_dir: Directory containing the spec YAML file, for resolving
                      relative mesh paths.

        Returns:
            Path to the generated .dat file.
        """
        return self._backend.generate_single(spec, output_dir, spec_dir=spec_dir)

    def _copy_mesh_files(
        self,
        spec: DiffractionSpec,
        output_dir: Path,
        spec_dir: Path | None = None,
    ) -> list[Path]:
        """Copy mesh files referenced in the spec to the output directory.

        Resolves mesh paths relative to *spec_dir* (the directory containing
        the spec YAML file) when the mesh path is relative.

        Returns list of destination paths.
        """
        copied: list[Path] = []
        bodies = spec.get_bodies()

        for body in bodies:
            mesh_file_str = body.vessel.geometry.mesh_file
            mesh_source = Path(mesh_file_str)

            if not mesh_source.is_absolute() and spec_dir is not None:
                mesh_source = (spec_dir / mesh_file_str).resolve()

            if mesh_source.exists():
                dest = output_dir / mesh_source.name
                if not dest.exists() or not dest.samefile(mesh_source):
                    shutil.copy2(mesh_source, dest)
                copied.append(dest)

        return copied

    def _validate_mesh_references(
        self, spec: DiffractionSpec, output_dir: Path
    ) -> list[str]:
        """Check that mesh files referenced in the spec exist in output_dir.

        Returns a list of warning messages (empty if all OK).
        """
        warnings: list[str] = []
        bodies = spec.get_bodies()

        for body in bodies:
            mesh_name = Path(body.vessel.geometry.mesh_file).name
            expected = output_dir / mesh_name

            if self._config.copy_mesh_files and not expected.exists():
                warnings.append(
                    f"Mesh file '{mesh_name}' for body "
                    f"'{body.vessel.name}' not found in {output_dir}"
                )

        return warnings

    def _invoke_solver(
        self,
        executable: Path,
        input_file: Path,
        output_dir: Path,
        timeout: int,
        extra_args: list[str],
        nowind: bool,
    ) -> tuple[int, str, str]:
        """Run the AQWA solver as a subprocess.

        Command format (running from output_dir as cwd):
            [aqwa.exe, /nowind, input_filename.dat, *extra_args]

        When ``nowind`` is False:
            [aqwa.exe, input_filename.dat, *extra_args]

        Note: Only the filename is passed (not the full path) because
        AQWA is run with cwd set to output_dir where the .dat file resides.

        Returns:
            Tuple of (return_code, stdout, stderr).
        """
        cmd = [str(executable)]
        if nowind:
            cmd.append("/nowind")
        # Pass only the filename since we run from output_dir
        cmd.append(input_file.name)
        cmd.extend(extra_args)

        try:
            proc = subprocess.run(
                cmd,
                cwd=str(output_dir),
                capture_output=True,
                text=True,
                timeout=timeout,
            )
            return proc.returncode, proc.stdout, proc.stderr

        except subprocess.TimeoutExpired:
            return -1, "", f"Timeout after {timeout} seconds"

        except FileNotFoundError as e:
            return -1, "", f"Executable not found: {e}"

        except OSError as e:
            return -1, "", f"OS error: {e}"

    @staticmethod
    def _detect_aqwa_error(
        stdout: str, stderr: str, return_code: int, lis_content: str = ""
    ) -> str | None:
        """Detect AQWA solver errors from output streams and LIS file.

        AQWA writes errors to stdout or the .LIS file and often returns
        exit code 0 even when it fails. This method checks for known
        error patterns in all outputs.

        Returns:
            Error message if an error is detected, None otherwise.
        """
        # Non-zero return code is always an error
        if return_code != 0:
            return stderr or f"Exit code {return_code}"

        # Error patterns to check in stdout and LIS file
        error_patterns = [
            "**** ERROR ****",
            "NON-EXISTENT FILE",
            "FATAL ERROR",
            "ERROR IN",
            "MESH FILE NOT FOUND",
            "LICENSE ERROR",
            "CANNOT OPEN",
            "INPUT DATA ERROR",
            "TERMINATED WITH ERRORS",
        ]

        # Check stdout for AQWA error patterns
        stdout_upper = stdout.upper()
        for pattern in error_patterns:
            if pattern in stdout_upper:
                # Extract the error line for better reporting
                for line in stdout.strip().split("\n"):
                    if pattern in line.upper():
                        return line.strip()
                return f"AQWA error detected: {pattern}"

        # Check LIS file for error patterns
        if lis_content:
            lis_upper = lis_content.upper()
            for pattern in error_patterns:
                if pattern in lis_upper:
                    # Extract the error line from LIS
                    for line in lis_content.strip().split("\n"):
                        if pattern in line.upper():
                            return f"AQWA LIS: {line.strip()}"
                    return f"AQWA LIS error: {pattern}"

        # Check if stderr has content (unusual but check anyway)
        if stderr and stderr.strip():
            return stderr.strip()

        return None

    def _capture_lis_file(self, output_dir: Path) -> Path | None:
        """Find and return the path to the AQWA .LIS output file, if present.

        AQWA writes solver results to a ``.LIS`` file in the working directory.
        Searches for both upper- and lower-case extensions.
        """
        lis_patterns = ["*.LIS", "*.lis"]
        for pattern in lis_patterns:
            files = sorted(output_dir.glob(pattern))
            if files:
                return files[-1]
        return None

    def _capture_log(self, output_dir: Path) -> Path | None:
        """Find and return the path to the AQWA log file, if present."""
        log_patterns = ["*.log", "*.txt"]
        for pattern in log_patterns:
            logs = sorted(output_dir.glob(pattern))
            if logs:
                return logs[-1]
        return None

    @staticmethod
    def _derive_spec_name(spec: DiffractionSpec) -> str:
        """Derive a human-readable name from the spec."""
        bodies = spec.get_bodies()
        if len(bodies) == 1:
            return bodies[0].vessel.name
        if spec.metadata and spec.metadata.project:
            return spec.metadata.project
        return "aqwa_analysis"


# ---------------------------------------------------------------------------
# Convenience function
# ---------------------------------------------------------------------------


def run_aqwa(
    spec: DiffractionSpec,
    output_dir: Path | str = "output",
    dry_run: bool = False,
    timeout_seconds: int = 7200,
    spec_path: Path | str | None = None,
) -> AQWARunResult:
    """Run an AQWA diffraction analysis from a DiffractionSpec.

    This is a module-level convenience wrapper around AQWARunner.

    Args:
        spec: Canonical diffraction specification.
        output_dir: Directory for generated files and results.
        dry_run: If True, generate files but skip solver invocation.
        timeout_seconds: Maximum solver execution time.
        spec_path: Path to the source YAML file (for resolving relative
                   mesh paths). Optional.

    Returns:
        AQWARunResult with status, paths, and logs.
    """
    config = AQWARunConfig(
        output_dir=Path(output_dir),
        dry_run=dry_run,
        timeout_seconds=timeout_seconds,
    )
    runner = AQWARunner(config)
    return runner.run(spec, spec_path=spec_path)
