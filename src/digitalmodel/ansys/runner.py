"""Fail-closed ANSYS Mechanical APDL solver runner (subprocess `mapdl`).

The ANSYS module otherwise only *generates* APDL scripts; this runs them. It
mirrors the OrcaWave/AQWA runner contract so the licensed-run lane's fixed
``uv run python -m digitalmodel <input>`` command can drive an ANSYS solve on a
licensed host (digitalmodel #940):

  - executes a prepared APDL ``.inp``/``.ans`` script via ``mapdl -b -i .. -o ..``;
  - **fail-closed**: if no executable/license is found and a real solve was
    requested, the run falls back to DRY_RUN (the workflow then raises, so the
    lane can never record a false finish);
  - metadata-only result (status, paths, return code) — heavy ``.rst``/``.out``
    files stay on the host.

Invocation is subprocess-based (no PyANSYS hard dependency).
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
from typing import Optional

# APDL solver error markers (MAPDL may return rc=0 yet write errors to the log).
_ERROR_MARKERS = ("*** ERROR ***", "SOLUTION NOT CONVERGED", "*** FATAL ***")
_RESULT_SUFFIXES = (".rst", ".rth", ".out", ".mntr", ".db")


class ANSYSRunStatus(str, Enum):
    PENDING = "pending"
    PREPARING = "preparing"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    DRY_RUN = "dry_run"


@dataclass
class ANSYSRunConfig:
    output_dir: Path = Path("output")
    executable_path: Optional[Path] = None
    timeout_seconds: int = 7200
    dry_run: bool = False
    extra_args: list[str] = field(default_factory=list)


@dataclass
class ANSYSRunResult:
    status: ANSYSRunStatus
    output_dir: Path
    input_file: Optional[Path] = None
    log_file: Optional[Path] = None
    result_files: list[Path] = field(default_factory=list)
    return_code: Optional[int] = None
    stdout: str = ""
    stderr: str = ""
    error_message: Optional[str] = None
    duration_seconds: float = 0.0


# Standard Windows MAPDL install locations (probed only on Windows).
def _standard_paths() -> list[Path]:
    if platform.system() != "Windows":
        return []
    roots = [Path(r"C:\Program Files\ANSYS Inc")]
    exes: list[Path] = []
    for root in roots:
        if not root.is_dir():
            continue
        for ver_dir in sorted(root.glob("v*"), reverse=True):
            exe = ver_dir / "ansys" / "bin" / "winx64" / "MAPDL.exe"
            if exe.is_file():
                exes.append(exe)
    return exes


class ANSYSRunner:
    """Run a prepared APDL script through MAPDL, fail-closed."""

    def __init__(self, config: Optional[ANSYSRunConfig] = None) -> None:
        self._config = config or ANSYSRunConfig()
        self._result: Optional[ANSYSRunResult] = None

    def run(self, script_path: Path | str) -> ANSYSRunResult:
        """Prepare + execute. Falls back to DRY_RUN if no executable is found."""
        self.prepare(script_path)
        assert self._result is not None
        if self._result.status == ANSYSRunStatus.FAILED:
            return self._result  # e.g. missing script — fail-closed, don't run
        if not self._config.dry_run and self._detect_executable() is None:
            self._result.status = ANSYSRunStatus.DRY_RUN
            self._result.error_message = (
                "No ANSYS/MAPDL executable found; no results produced"
            )
            return self._result
        return self.execute()

    def prepare(self, script_path: Path | str) -> ANSYSRunResult:
        script = Path(script_path)
        output_dir = Path(self._config.output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        self._result = ANSYSRunResult(
            status=ANSYSRunStatus.PREPARING,
            output_dir=output_dir,
            input_file=script,
        )
        if not script.is_file():
            self._result.status = ANSYSRunStatus.FAILED
            self._result.error_message = f"APDL script not found: {script}"
        return self._result

    def execute(self) -> ANSYSRunResult:
        if self._result is None:
            raise RuntimeError("prepare() must be called before execute()")
        if self._result.status == ANSYSRunStatus.FAILED:
            return self._result  # missing script; nothing to run

        start = time.monotonic()
        if self._config.dry_run:
            self._result.status = ANSYSRunStatus.DRY_RUN
            return self._result

        exe = self._detect_executable()
        if exe is None:
            self._result.status = ANSYSRunStatus.DRY_RUN
            self._result.error_message = (
                "No ANSYS/MAPDL executable found; no results produced"
            )
            return self._result

        self._result.status = ANSYSRunStatus.RUNNING
        out_file = self._result.output_dir / f"{self._result.input_file.stem}.out"
        argv = [
            str(exe),
            "-b",
            "-i",
            str(self._result.input_file),
            "-o",
            str(out_file),
            *self._config.extra_args,
        ]
        try:
            proc = subprocess.run(  # noqa: S603 - argv is fixed; script is fs-resolved.
                argv,
                cwd=str(self._result.output_dir),
                capture_output=True,
                text=True,
                timeout=self._config.timeout_seconds,
                check=False,
            )
        except (OSError, subprocess.TimeoutExpired) as exc:
            self._result.status = ANSYSRunStatus.FAILED
            self._result.error_message = f"MAPDL invocation failed: {exc}"
            self._result.duration_seconds = time.monotonic() - start
            return self._result

        self._result.return_code = proc.returncode
        self._result.stdout = proc.stdout or ""
        self._result.stderr = proc.stderr or ""
        self._result.duration_seconds = time.monotonic() - start
        self._result.log_file = out_file if out_file.is_file() else None
        self._result.result_files = self._capture_result_files(self._result.output_dir)

        error = self._detect_error(proc.returncode, out_file)
        if error is None:
            self._result.status = ANSYSRunStatus.COMPLETED
        else:
            self._result.status = ANSYSRunStatus.FAILED
            self._result.error_message = error
        return self._result

    def _detect_executable(self) -> Optional[Path]:
        configured = self._config.executable_path
        if configured and Path(configured).is_file():
            return Path(configured)
        for env_var in ("ANSYS_MAPDL_PATH", "MAPDL_PATH", "ANSYS_EXECUTABLE"):
            val = os.environ.get(env_var)
            if val and Path(val).is_file():
                return Path(val)
        for name in ("mapdl", "ansys", "MAPDL.exe", "ansys.exe"):
            found = shutil.which(name)
            if found:
                return Path(found)
        for std in _standard_paths():
            return std
        return None

    def _detect_error(self, return_code: int, out_file: Path) -> Optional[str]:
        if return_code != 0:
            return f"MAPDL returned non-zero exit code {return_code}"
        # MAPDL can return 0 yet write errors to the .out log.
        if out_file.is_file():
            text = out_file.read_text(errors="replace")
            for marker in _ERROR_MARKERS:
                if marker in text:
                    return f"MAPDL log reported: {marker}"
        return None

    @staticmethod
    def _capture_result_files(output_dir: Path) -> list[Path]:
        return [
            p
            for p in sorted(output_dir.iterdir())
            if p.is_file() and p.suffix.lower() in _RESULT_SUFFIXES
        ]


def run_ansys(
    script_path: Path | str,
    output_dir: Path | str = "output",
    dry_run: bool = False,
    timeout_seconds: int = 7200,
    executable_path: Path | str | None = None,
    extra_args: Optional[list[str]] = None,
) -> ANSYSRunResult:
    """Run a prepared APDL script through MAPDL (module-level convenience)."""
    config = ANSYSRunConfig(
        output_dir=Path(output_dir),
        executable_path=Path(executable_path) if executable_path else None,
        timeout_seconds=timeout_seconds,
        dry_run=dry_run,
        extra_args=list(extra_args or []),
    )
    return ANSYSRunner(config).run(script_path)
