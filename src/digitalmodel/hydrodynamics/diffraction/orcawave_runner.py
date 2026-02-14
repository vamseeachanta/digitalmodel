"""OrcaWave diffraction analysis runner (WRK-029).

Bridges DiffractionSpec -> file generation -> OrcaWave execution -> result capture.

Data flow::

    DiffractionSpec (spec.yml)
      -> OrcaWaveBackend.generate_single()  [existing, WRK-059]
      -> OrcaWaveRunner.prepare()           [generates files, copies mesh, validates]
      -> OrcaWaveRunner.execute()           [invokes orcawave.exe or dry-run]
      -> RunResult                          [status, paths, logs, duration]

Modular file decision
---------------------
OrcaWave batch mode requires a single ``.yml`` input file.  The runner always
generates this single file for solver consumption via
``OrcaWaveBackend.generate_single()``.  When ``RunConfig.generate_modular`` is
True (the default), modular section files are *also* generated alongside via
``OrcaWaveBackend.generate_modular()`` for review, reuse, and debugging.  The
solver is invoked with the single file only.

Executable detection order
--------------------------
1. ``RunConfig.executable_path`` (explicit)
2. ``ORCAWAVE_PATH`` environment variable
3. Standard Orcina install paths (platform-dependent)
4. ``shutil.which("orcawave")``
5. ``None`` -> triggers dry-run mode

Dry-run mode
------------
When the executable is not found or ``RunConfig.dry_run`` is True, the runner
performs all file preparation steps (input generation, mesh copying, validation)
but skips the actual solver invocation.  This is useful for CI, pre-flight
checks, and environments without an OrcaWave license.
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

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------


class RunStatus(str, Enum):
    """Status of an OrcaWave runner execution."""

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


class RunConfig(BaseModel):
    """Configuration for an OrcaWave runner instance.

    All paths accept ``str`` or ``Path``; they are coerced to ``Path``.
    """

    executable_path: Path | None = Field(
        default=None,
        description="Explicit path to orcawave executable. Auto-detected if None.",
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
    generate_modular: bool = Field(
        default=True,
        description="Generate modular section files alongside the single input file.",
    )
    copy_mesh_files: bool = Field(
        default=True,
        description="Copy mesh files referenced by the spec into the output directory.",
    )
    extra_args: list[str] = Field(
        default_factory=list,
        description="Additional command-line arguments passed to the solver.",
    )
    use_api: bool = Field(
        default=True,
        description=(
            "Use OrcFxAPI Python binding instead of subprocess. "
            "Falls back to subprocess if OrcFxAPI is not available."
        ),
    )
    thread_count: int = Field(
        default=4,
        gt=0,
        description="Number of threads for OrcFxAPI diffraction calculation.",
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
class RunResult:
    """Result of an OrcaWave runner execution."""

    status: RunStatus
    input_file: Path | None = None
    modular_files: list[Path] = field(default_factory=list)
    output_dir: Path | None = None
    mesh_files: list[Path] = field(default_factory=list)
    log_file: Path | None = None
    stdout: str = ""
    stderr: str = ""
    return_code: int | None = None
    duration_seconds: float | None = None
    error_message: str | None = None
    spec_name: str | None = None


# ---------------------------------------------------------------------------
# Standard Orcina install paths
# ---------------------------------------------------------------------------

_STANDARD_PATHS: list[str] = []
if platform.system() == "Windows":
    _STANDARD_PATHS = [
        r"C:\Program Files\Orcina\OrcaWave\OrcaWave.exe",
        r"C:\Program Files (x86)\Orcina\OrcaWave\OrcaWave.exe",
    ]
elif platform.system() == "Linux":
    _STANDARD_PATHS = [
        "/opt/orcina/orcawave/orcawave",
        "/usr/local/bin/orcawave",
    ]


# ---------------------------------------------------------------------------
# Runner
# ---------------------------------------------------------------------------


class OrcaWaveRunner:
    """Orchestrates OrcaWave input preparation and solver execution.

    Example::

        from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
        from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
            OrcaWaveRunner, RunConfig,
        )

        spec = DiffractionSpec.from_yaml("analysis.yml")
        runner = OrcaWaveRunner(RunConfig(output_dir="output", dry_run=True))
        result = runner.run(spec)
        print(result.status, result.input_file)
    """

    def __init__(self, config: RunConfig | None = None) -> None:
        self._config = config or RunConfig()
        self._backend = OrcaWaveBackend()
        self._result: RunResult | None = None

    @property
    def result(self) -> RunResult | None:
        """Most recent run result, or None if no run has been performed."""
        return self._result

    # ----- public API -----

    def run(
        self, spec: DiffractionSpec, spec_path: Path | str | None = None
    ) -> RunResult:
        """Execute the full pipeline: prepare + execute.

        Execution priority:
        1. OrcFxAPI Python binding (if ``use_api`` is True and available)
        2. Subprocess with detected executable
        3. Dry-run mode (fallback)

        Args:
            spec: Canonical diffraction specification.
            spec_path: Path to the source YAML file (for resolving relative
                       mesh paths). Optional.
        """
        self.prepare(spec, spec_path=spec_path)

        if not self._config.dry_run:
            # Check API or executable availability before executing
            has_api = self._config.use_api and self._check_api_available()
            has_exe = self._detect_executable() is not None

            if not has_api and not has_exe:
                self._result.status = RunStatus.DRY_RUN
                self._result.duration_seconds = 0.0
                return self._result

        result = self.execute()
        return result

    def prepare(
        self,
        spec: DiffractionSpec,
        spec_path: Path | str | None = None,
    ) -> RunResult:
        """Generate input files, copy mesh files, and validate.

        Args:
            spec: Canonical diffraction specification.
            spec_path: Path to the source YAML file (for resolving relative
                       mesh paths). Optional.

        Returns a RunResult with status PREPARING (ready for execute).
        """
        output_dir = self._config.output_dir or Path("output")
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        spec_name = self._derive_spec_name(spec)
        spec_dir = Path(spec_path).parent if spec_path else None

        self._result = RunResult(
            status=RunStatus.PREPARING,
            output_dir=output_dir,
            spec_name=spec_name,
        )

        # Generate single input file (required for solver)
        input_file, modular_files = self._generate_input_files(spec, output_dir)
        self._result.input_file = input_file
        self._result.modular_files = modular_files

        # Copy mesh files
        if self._config.copy_mesh_files:
            mesh_files = self._copy_mesh_files(spec, output_dir, spec_dir=spec_dir)
            self._result.mesh_files = mesh_files

        # Validate mesh references
        warnings = self._validate_mesh_references(spec, output_dir)
        if warnings:
            self._result.error_message = "; ".join(warnings)

        return self._result

    def execute(self) -> RunResult:
        """Invoke the OrcaWave solver or perform dry-run.

        Must be called after prepare().

        Execution priority:
        1. If ``use_api`` is True and OrcFxAPI is available, use Python API
        2. Otherwise try subprocess with detected executable
        3. Fall back to DRY_RUN if neither is available
        """
        if self._result is None:
            msg = "prepare() must be called before execute()"
            raise RuntimeError(msg)

        start = time.monotonic()

        if self._config.dry_run:
            self._result.status = RunStatus.DRY_RUN
            self._result.duration_seconds = time.monotonic() - start
            return self._result

        # Try OrcFxAPI Python binding first
        if self._config.use_api and self._check_api_available():
            return self._execute_via_api(start)

        # Fall back to subprocess
        exe = self._detect_executable()
        if exe is None:
            self._result.status = RunStatus.DRY_RUN
            self._result.duration_seconds = time.monotonic() - start
            return self._result

        self._result.status = RunStatus.RUNNING
        return_code, stdout, stderr = self._invoke_solver(
            executable=exe,
            input_file=self._result.input_file,
            output_dir=self._result.output_dir,
            timeout=self._config.timeout_seconds,
            extra_args=self._config.extra_args,
        )

        elapsed = time.monotonic() - start
        self._result.stdout = stdout
        self._result.stderr = stderr
        self._result.return_code = return_code
        self._result.duration_seconds = elapsed

        if return_code == 0:
            self._result.status = RunStatus.COMPLETED
        else:
            self._result.status = RunStatus.FAILED
            self._result.error_message = stderr or f"Exit code {return_code}"

        # Capture log
        self._result.log_file = self._capture_log(self._result.output_dir)

        return self._result

    @staticmethod
    def _check_api_available() -> bool:
        """Check if OrcFxAPI Python binding is importable."""
        try:
            import OrcFxAPI  # noqa: F401

            return True
        except ImportError:
            return False

    def _execute_via_api(self, start: float) -> RunResult:
        """Execute diffraction analysis using OrcFxAPI Python binding.

        Loads the generated .yml input file into OrcFxAPI.Diffraction,
        runs Calculate(), and saves results.
        """
        import OrcFxAPI

        self._result.status = RunStatus.RUNNING
        input_file = self._result.input_file
        output_dir = self._result.output_dir

        try:
            # Load the generated .yml into OrcaWave Diffraction
            diffraction = OrcFxAPI.Diffraction(
                str(input_file.resolve()),
                threadCount=self._config.thread_count,
            )

            # Run the diffraction calculation
            diffraction.Calculate()

            elapsed = time.monotonic() - start

            # Save results
            results_file = output_dir / f"{input_file.stem}.owr"
            diffraction.SaveResults(str(results_file.resolve()))

            # Save data file for reproducibility
            data_file = output_dir / f"{input_file.stem}_data.dat"
            diffraction.SaveData(str(data_file.resolve()))

            self._result.status = RunStatus.COMPLETED
            self._result.return_code = 0
            self._result.duration_seconds = elapsed
            self._result.stdout = (
                f"OrcFxAPI: Calculated {len(diffraction.frequencies)} "
                f"frequencies x {len(diffraction.headings)} headings"
            )
            self._result.log_file = results_file

        except Exception as e:
            elapsed = time.monotonic() - start
            self._result.status = RunStatus.FAILED
            self._result.return_code = -1
            self._result.duration_seconds = elapsed
            self._result.error_message = str(e)
            self._result.stderr = str(e)

        return self._result

    # ----- internal methods -----

    def _detect_executable(self) -> Path | None:
        """Auto-detect OrcaWave executable.

        Search order:
        1. RunConfig.executable_path (explicit)
        2. ORCAWAVE_PATH environment variable
        3. Standard Orcina install paths
        4. shutil.which("orcawave")
        """
        if self._config.executable_path is not None:
            if self._config.executable_path.exists():
                return self._config.executable_path
            return None

        env_path = os.environ.get("ORCAWAVE_PATH")
        if env_path:
            p = Path(env_path)
            if p.exists():
                return p

        for std_path in _STANDARD_PATHS:
            p = Path(std_path)
            if p.exists():
                return p

        which_result = shutil.which("orcawave")
        if which_result:
            return Path(which_result)

        return None

    def _generate_input_files(
        self, spec: DiffractionSpec, output_dir: Path
    ) -> tuple[Path, list[Path]]:
        """Generate OrcaWave input files using the backend.

        Returns:
            Tuple of (single_file_path, list_of_modular_file_paths).
        """
        single_file = self._backend.generate_single(spec, output_dir)

        modular_files: list[Path] = []
        if self._config.generate_modular:
            modular_dir = output_dir / "modular"
            self._backend.generate_modular(spec, modular_dir)
            modular_files = sorted(modular_dir.glob("*.yml"))

        return single_file, modular_files

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

        # Copy damping lid mesh if present
        if spec.damping_lid is not None:
            lid_file_str = spec.damping_lid.mesh_file
            lid_source = Path(lid_file_str)
            if not lid_source.is_absolute() and spec_dir is not None:
                lid_source = (spec_dir / lid_file_str).resolve()
            if lid_source.exists():
                dest = output_dir / lid_source.name
                if not dest.exists() or not dest.samefile(lid_source):
                    shutil.copy2(lid_source, dest)
                copied.append(dest)

        # Copy free surface zone mesh if present
        fsz = getattr(spec, "free_surface_zone", None)
        if fsz is not None and fsz.mesh_file:
            fsz_file_str = fsz.mesh_file
            fsz_source = Path(fsz_file_str)
            if not fsz_source.is_absolute() and spec_dir is not None:
                fsz_source = (spec_dir / fsz_file_str).resolve()
            if fsz_source.exists():
                dest = output_dir / fsz_source.name
                if not dest.exists() or not dest.samefile(fsz_source):
                    shutil.copy2(fsz_source, dest)
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
    ) -> tuple[int, str, str]:
        """Run the OrcaWave solver as a subprocess.

        Returns:
            Tuple of (return_code, stdout, stderr).
        """
        cmd = [str(executable), str(input_file), *extra_args]

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

    def _capture_log(self, output_dir: Path) -> Path | None:
        """Find and return the path to the OrcaWave log file, if present."""
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
        return "orcawave_analysis"


# ---------------------------------------------------------------------------
# Convenience function
# ---------------------------------------------------------------------------


def run_orcawave(
    spec: DiffractionSpec,
    output_dir: Path | str = "output",
    dry_run: bool = False,
    timeout_seconds: int = 7200,
    spec_path: Path | str | None = None,
) -> RunResult:
    """Run an OrcaWave diffraction analysis from a DiffractionSpec.

    This is a module-level convenience wrapper around OrcaWaveRunner.

    Args:
        spec: Canonical diffraction specification.
        output_dir: Directory for generated files and results.
        dry_run: If True, generate files but skip solver invocation.
        timeout_seconds: Maximum solver execution time.
        spec_path: Path to the source YAML file (for resolving relative
                   mesh paths). Optional.

    Returns:
        RunResult with status, paths, and logs.
    """
    config = RunConfig(
        output_dir=Path(output_dir),
        dry_run=dry_run,
        timeout_seconds=timeout_seconds,
    )
    runner = OrcaWaveRunner(config)
    return runner.run(spec, spec_path=spec_path)
