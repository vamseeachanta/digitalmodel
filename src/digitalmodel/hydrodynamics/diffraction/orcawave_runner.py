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
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import TYPE_CHECKING, Any

import numpy as np
from pydantic import BaseModel, Field, field_validator

from digitalmodel.hydrodynamics.diffraction.diffraction_units import (
    hz_to_rad_per_s,
    radians_to_degrees,
    rad_per_s_to_period_s,
)
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.mesh_packaging import copy_spec_meshes
from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    AddedMassSet,
    DampingSet,
    DiffractionResults,
    DOF,
    FrequencyData,
    HeadingData,
    HydrodynamicMatrix,
    HydrostaticResults,
    RAOComponent,
    RAOSet,
)
from digitalmodel.hydrodynamics.diffraction.validation_runner import run_validation

if TYPE_CHECKING:
    from digitalmodel.hydrodynamics.diffraction.assumption_ledger import (
        AssumptionLedger,
    )


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
    validate_outputs: bool = Field(
        default=True,
        description="Run output validation on produced DiffractionResults.",
    )
    validation_strict: bool = Field(
        default=False,
        description=(
            "Treat a FAIL/ERROR validation verdict as a run failure "
            "(status FAILED, return code -2) even if the solver succeeded."
        ),
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
    """Result of an OrcaWave runner execution.

    Output contract (#611): explicit result paths replace the previous
    ``log_file = .owr`` overload. ``owr_path`` / ``data_file`` populate for real
    when OrcaWave produces them; ``xlsx_path`` / ``report_path`` are reserved for
    a later exporter/report phase and remain ``None`` here (locked decision D2).

    Validation contract (#625): ``validation_verdict`` defaults to ``"SKIPPED"``
    and uses canonical strings ``PASS``/``WARNING``/``FAIL``/``ERROR``/``SKIPPED``
    (never aliased to ``WARN`` in stored values, per locked decision D3).
    """

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
    # --- #611 output contract ---
    owr_path: Path | None = None
    data_file: Path | None = None
    xlsx_path: Path | None = None
    report_path: Path | None = None
    # --- #625 validation contract ---
    validation_report_path: Path | None = None
    validation_verdict: str = "SKIPPED"
    validation_issues: list[str] = field(default_factory=list)
    validation_report: dict[str, Any] | None = None
    diffraction_results: DiffractionResults | None = None
    # --- solver metadata ---
    orcf_api_version: str | None = None
    thread_count: int | None = None
    assumption_ledger: "AssumptionLedger | None" = None


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
        # Retained from the spec during prepare() so the post-Calculate()
        # adapter can stamp DiffractionResults sub-schemas with water_depth.
        self._water_depth: float = 0.0

    @property
    def result(self) -> RunResult | None:
        """Most recent run result, or None if no run has been performed."""
        return self._result

    # ----- public API -----

    def run(
        self,
        spec: DiffractionSpec,
        spec_path: Path | str | None = None,
        assumption_ledger: "AssumptionLedger | None" = None,
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
            assumption_ledger: Optional provenance ledger (e.g. from
                ``resolver.resolve``) to carry onto the result so it reaches
                report generation. Attached to every return path.
        """
        self.prepare(spec, spec_path=spec_path)
        # Attach after prepare() (which builds self._result) so the ledger
        # rides both the dry-run early return and the execute() result.
        self._result.assumption_ledger = assumption_ledger

        if not self._config.dry_run:
            # Check API or executable availability before executing
            has_api = self._config.use_api and self._check_api_available()
            has_exe = self._detect_executable() is not None

            if not has_api and not has_exe:
                self._result.status = RunStatus.DRY_RUN
                self._result.duration_seconds = 0.0
                self._mark_validation_skipped(
                    "No OrcaWave executable or OrcFxAPI available; "
                    "no diffraction results produced"
                )
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

        # Retain water depth from the spec for the post-Calculate() adapter.
        self._water_depth = self._spec_water_depth(spec)

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
            self._mark_validation_skipped(
                "No diffraction results produced in dry-run mode"
            )
            return self._result

        # Try OrcFxAPI Python binding first
        if self._config.use_api and self._check_api_available():
            return self._execute_via_api(start)

        # Fall back to subprocess
        exe = self._detect_executable()
        if exe is None:
            self._result.status = RunStatus.DRY_RUN
            self._result.duration_seconds = time.monotonic() - start
            self._mark_validation_skipped(
                "No OrcaWave executable found; no diffraction results produced"
            )
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
            self._mark_validation_skipped(
                "Solver failed; no diffraction results to validate"
            )

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
            # #611: expose real result paths instead of overloading log_file.
            # log_file stays None here (no subprocess log on the API path).
            self._result.owr_path = results_file
            self._result.data_file = data_file
            self._result.thread_count = self._config.thread_count
            self._result.orcf_api_version = self._detect_api_version(OrcFxAPI)

            # #625: build DiffractionResults from the live object (with a
            # mandatory .owr LoadResults() fallback) and validate.
            results = self._diffraction_to_results(
                diffraction,
                owr_path=results_file,
                data_file=data_file,
            )
            self._result.diffraction_results = results
            self._run_validation(results, output_dir)

        except Exception as e:
            elapsed = time.monotonic() - start
            self._result.status = RunStatus.FAILED
            self._result.return_code = -1
            self._result.duration_seconds = elapsed
            self._result.error_message = str(e)
            self._result.stderr = str(e)
            self._mark_validation_skipped(
                "OrcFxAPI calculation failed; no diffraction results to validate"
            )

        return self._result

    # ----- #625 validation wiring -----

    def _run_validation(
        self, results: DiffractionResults | None, output_dir: Path
    ) -> None:
        """Validate *results* via the shared helper and attach the outcome.

        Honors ``RunConfig.validate_outputs`` (disabled -> SKIPPED) and applies
        strict-mode enforcement when ``RunConfig.validation_strict`` is set.
        """
        if self._result is None:
            return

        stem = (self._result.input_file.stem if self._result.input_file else "orcawave")
        outcome = run_validation(
            results,
            output_dir,
            stem,
            enabled=self._config.validate_outputs,
            skip_reason=(
                None
                if self._config.validate_outputs
                else "Validation disabled via RunConfig.validate_outputs"
            ),
        )
        self._result.validation_verdict = outcome.verdict
        self._result.validation_report = outcome.report
        self._result.validation_report_path = outcome.report_path
        if outcome.issues:
            self._result.validation_issues = outcome.issues
        elif outcome.reason:
            self._result.validation_issues = [outcome.reason]

        self._apply_strict_mode(outcome.verdict)

    def _apply_strict_mode(self, verdict: str) -> None:
        """In strict mode, escalate a FAIL/ERROR verdict to a run failure.

        Only fires when the solver otherwise succeeded (status COMPLETED).
        WARNING stays non-fatal. Return code becomes ``-2`` to distinguish a
        validation-driven failure from a solver failure (``-1``).
        """
        if self._result is None or not self._config.validation_strict:
            return
        if self._result.status != RunStatus.COMPLETED:
            return
        if verdict in ("FAIL", "ERROR"):
            self._result.status = RunStatus.FAILED
            self._result.return_code = -2
            self._result.error_message = (
                f"Strict validation failed: verdict {verdict}"
            )

    @staticmethod
    def _detect_api_version(orcfxapi_module: Any) -> str | None:
        """Best-effort read of the OrcFxAPI version string, if exposed."""
        for attr in ("DLLVersion", "ModuleVersion", "__version__"):
            value = getattr(orcfxapi_module, attr, None)
            if value is None:
                continue
            try:
                return str(value() if callable(value) else value)
            except Exception:  # noqa: BLE001 - version probe must never raise
                continue
        return None

    def _diffraction_to_results(
        self,
        diffraction: Any,
        owr_path: Path,
        data_file: Path,
    ) -> DiffractionResults:
        """Build a :class:`DiffractionResults` from a live OrcFxAPI object.

        Load-bearing assumption (unverified on this license-free host; tracked
        on ``licensed-win-1`` / #610): result attributes are populated after
        ``Calculate()`` without ``LoadResults()``. If the live attributes are
        empty or raise, this method falls back to ``d.LoadResults(.owr)`` and
        re-reads — acceptable because OrcFxAPI is already present on the success
        path.

        Attribute conventions mirror ``solver/report_extractors.py``:
        ``frequencies`` are Hz (descending), ``addedMass`` / ``damping`` are
        ``(nfreq, 6, 6)``, ``displacementRAOs`` are ``(nheading, nfreq, 6)``
        complex (rad/m for rotations).
        """
        try:
            self._read_live_attrs(diffraction)
        except (AttributeError, IndexError, TypeError, ValueError):
            # Live attributes missing/empty: reload from the just-saved .owr.
            diffraction.LoadResults(str(Path(owr_path).resolve()))

        return self._build_results_from_object(
            diffraction, source_files=[str(owr_path), str(data_file)]
        )

    @staticmethod
    def _read_live_attrs(diffraction: Any) -> None:
        """Probe the load-bearing result attributes, raising if unavailable.

        Raises one of (AttributeError/IndexError/TypeError/ValueError) when an
        attribute is missing or empty, which triggers the .owr fallback.
        """
        freqs = np.asarray(diffraction.frequencies, dtype=float)
        np.asarray(diffraction.headings, dtype=float)
        am = np.asarray(diffraction.addedMass, dtype=float)
        np.asarray(diffraction.damping, dtype=float)
        np.asarray(diffraction.displacementRAOs)
        if freqs.size == 0 or am.size == 0:
            raise ValueError("Live OrcFxAPI result arrays are empty")

    def _build_results_from_object(
        self, diffraction: Any, source_files: list[str]
    ) -> DiffractionResults:
        """Translate populated OrcFxAPI result arrays into DiffractionResults."""
        vessel_name = self._result.spec_name or "OrcaWave_Vessel"
        water_depth = float(self._water_depth)
        created = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        # Frequencies (Hz, descending) -> rad/s ascending.
        freq_hz = np.asarray(diffraction.frequencies, dtype=float)
        freq_rad_s = np.asarray(hz_to_rad_per_s(freq_hz), dtype=float)
        sort_idx = np.argsort(freq_rad_s)
        freq_rad_s = freq_rad_s[sort_idx]
        headings = np.asarray(diffraction.headings, dtype=float)

        n_freq = freq_rad_s.size
        n_head = headings.size

        freq_data = FrequencyData(
            values=freq_rad_s.copy(),
            periods=np.asarray(rad_per_s_to_period_s(freq_rad_s), dtype=float),
            count=n_freq,
            min_freq=float(np.min(freq_rad_s)),
            max_freq=float(np.max(freq_rad_s)),
        )
        head_data = HeadingData(
            values=headings.copy(),
            count=n_head,
            min_heading=float(np.min(headings)) if n_head else 0.0,
            max_heading=float(np.max(headings)) if n_head else 0.0,
        )

        # Displacement RAOs: (nheading, nfreq, 6) complex -> (nfreq, nheading, 6)
        raw_raos = np.asarray(diffraction.displacementRAOs)
        raw_raos = np.transpose(raw_raos, (1, 0, 2))[sort_idx, :, :]
        rotation_dofs = {DOF.ROLL, DOF.PITCH, DOF.YAW}

        rao_components: dict[str, RAOComponent] = {}
        for i, dof in enumerate(DOF):
            mag = np.abs(raw_raos[:, :, i])
            phase = np.angle(raw_raos[:, :, i], deg=True)
            if dof in rotation_dofs:
                # OrcFxAPI rotation RAOs are rad/m; schema expects deg/m.
                mag = np.asarray(radians_to_degrees(mag), dtype=float)
            rao_components[dof.name.lower()] = RAOComponent(
                dof=dof,
                magnitude=np.ascontiguousarray(mag, dtype=float),
                phase=np.ascontiguousarray(phase, dtype=float),
                frequencies=freq_data,
                headings=head_data,
                unit="",
            )

        rao_set = RAOSet(
            vessel_name=vessel_name,
            analysis_tool="OrcaWave",
            water_depth=water_depth,
            created_date=created,
            source_file=source_files[0] if source_files else None,
            **rao_components,
        )

        # Added mass / damping: (nfreq, 6, 6).
        added_mass_raw = np.asarray(diffraction.addedMass, dtype=float)[sort_idx]
        damping_raw = np.asarray(diffraction.damping, dtype=float)[sort_idx]
        am_units = {"linear": "kg", "angular": "kg.m^2"}
        dp_units = {"linear": "N.s/m", "angular": "N.m.s/rad"}

        added_mass_set = AddedMassSet(
            vessel_name=vessel_name,
            analysis_tool="OrcaWave",
            water_depth=water_depth,
            matrices=[
                HydrodynamicMatrix(
                    matrix=np.ascontiguousarray(added_mass_raw[i], dtype=float),
                    frequency=float(freq_rad_s[i]),
                    matrix_type="added_mass",
                    units=am_units,
                )
                for i in range(n_freq)
            ],
            frequencies=freq_data,
            created_date=created,
            source_file=source_files[0] if source_files else None,
        )
        damping_set = DampingSet(
            vessel_name=vessel_name,
            analysis_tool="OrcaWave",
            water_depth=water_depth,
            matrices=[
                HydrodynamicMatrix(
                    matrix=np.ascontiguousarray(damping_raw[i], dtype=float),
                    frequency=float(freq_rad_s[i]),
                    matrix_type="damping",
                    units=dp_units,
                )
                for i in range(n_freq)
            ],
            frequencies=freq_data,
            created_date=created,
            source_file=source_files[0] if source_files else None,
        )

        hydrostatics = self._extract_hydrostatics(diffraction, vessel_name)

        return DiffractionResults(
            vessel_name=vessel_name,
            analysis_tool="OrcaWave",
            water_depth=water_depth,
            raos=rao_set,
            added_mass=added_mass_set,
            damping=damping_set,
            hydrostatics=hydrostatics,
            created_date=created,
            source_files=source_files,
            notes="Built from OrcFxAPI.Diffraction object (#625)",
            phase_convention="orcina_lag",
            unit_system="SI",
        )

    @staticmethod
    def _extract_hydrostatics(
        diffraction: Any, vessel_name: str
    ) -> HydrostaticResults | None:
        """Best-effort hydrostatics extraction; None if unavailable."""
        try:
            hs = diffraction.hydrostaticResults[0]
            restoring = np.asarray(hs["restoringMatrix"], dtype=float)
            return HydrostaticResults(
                vessel_name=vessel_name,
                displacement_volume=float(hs["volume"]),
                mass=float(hs["mass"]),
                centre_of_gravity=list(np.asarray(hs["centreOfMass"], dtype=float)),
                centre_of_buoyancy=list(
                    np.asarray(hs["centreOfBuoyancy"], dtype=float)
                ),
                waterplane_area=float(hs["Awp"]),
                stiffness_matrix=restoring,
            )
        except Exception:  # noqa: BLE001 - hydrostatics are optional
            return None

    @staticmethod
    def _spec_water_depth(spec: DiffractionSpec) -> float:
        """Resolve a numeric water depth from the spec (0.0 for 'infinite')."""
        try:
            wd = spec.environment.water_depth
        except AttributeError:
            return 0.0
        if isinstance(wd, (int, float)):
            return float(wd)
        return 0.0  # "infinite" -> sentinel 0.0

    def _mark_validation_skipped(self, reason: str) -> None:
        """Record a SKIPPED validation verdict with an explanatory reason.

        Keeps the canonical ``SKIPPED`` verdict (default) and stores the reason
        on the result so downstream consumers know why no validation ran. The
        runtime extraction + validation wiring lands in the #625 increment.
        """
        if self._result is None:
            return
        self._result.validation_verdict = "SKIPPED"
        self._result.validation_issues = [reason]

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

        Delegates to :func:`mesh_packaging.copy_spec_meshes`, the shared
        implementation also used by ``SpecConverter`` (#605). Relative mesh
        paths resolve against *spec_dir* (the directory containing the spec
        YAML file).

        Returns list of destination paths.
        """
        return copy_spec_meshes(spec, output_dir, spec_dir=spec_dir)

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
    assumption_ledger: "AssumptionLedger | None" = None,
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
        assumption_ledger: Optional provenance ledger (e.g. from
            ``resolver.resolve``) carried onto the result for reporting.

    Returns:
        RunResult with status, paths, and logs.
    """
    config = RunConfig(
        output_dir=Path(output_dir),
        dry_run=dry_run,
        timeout_seconds=timeout_seconds,
    )
    runner = OrcaWaveRunner(config)
    return runner.run(
        spec, spec_path=spec_path, assumption_ledger=assumption_ledger
    )
