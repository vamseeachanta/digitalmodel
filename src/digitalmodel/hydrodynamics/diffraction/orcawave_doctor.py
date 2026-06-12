"""OrcaWave environment and license readiness diagnostics (#613).

Surfaces the runner's existing detection logic (OrcFxAPI import, executable
search, ``ORCAWAVE_PATH``) as a PASS/WARN/FAIL report so users know whether a
host can actually solve before preparing a large run, instead of discovering
a silent dry-run fallback afterwards.

Exit-code policy (enforced by the CLI command):
- ``0`` — host usable, including dry-run-only hosts (a supported mode);
- nonzero — any FAIL check (e.g. unwritable output directory), or a
  dry-run-only host when ``--require-solver`` was passed.
"""

from __future__ import annotations

import os
import platform
import tempfile
from dataclasses import dataclass
from pathlib import Path


@dataclass
class DoctorCheck:
    """One diagnostic line: PASS / WARN / FAIL plus human detail."""

    name: str
    status: str
    detail: str


def _orcfxapi_info() -> tuple[bool, str]:
    """(available, detail) for the OrcFxAPI Python binding."""
    try:
        import OrcFxAPI  # noqa: F401
    except ImportError:
        return False, "OrcFxAPI not importable - API solve unavailable"
    detail = "OrcFxAPI importable"
    try:
        detail += f" (DLL version {OrcFxAPI.DLLVersion()})"
    except Exception:
        try:
            detail += f" (version {OrcFxAPI.Version()})"
        except Exception:
            pass
    return True, detail


def _detect_executable(executable: Path | None) -> Path | None:
    """Reuse the runner's executable search order."""
    from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
        OrcaWaveRunner,
        RunConfig,
    )

    runner = OrcaWaveRunner(RunConfig(executable_path=executable))
    return runner._detect_executable()


def _memory_gb() -> float | None:
    try:
        for line in Path("/proc/meminfo").read_text().splitlines():
            if line.startswith("MemTotal:"):
                return round(int(line.split()[1]) / 1024 / 1024, 1)
    except OSError:
        pass
    return None


def run_doctor(
    output_dir: Path | None = None,
    executable: Path | None = None,
) -> tuple[list[DoctorCheck], str]:
    """Run all readiness checks.

    Returns ``(checks, capability)`` where capability is one of
    ``"api-solve"``, ``"subprocess-solve"``, or ``"dry-run-only"``.
    """
    checks: list[DoctorCheck] = []

    api_available, api_detail = _orcfxapi_info()
    checks.append(
        DoctorCheck(
            "OrcFxAPI binding",
            "PASS" if api_available else "WARN",
            api_detail,
        )
    )
    if api_available:
        checks.append(
            DoctorCheck(
                "OrcaWave license",
                "PASS",
                "API present; the license itself is claimed at solve time "
                "(Calculate) - a licensed dongle/server must be reachable then",
            )
        )

    env_path = os.environ.get("ORCAWAVE_PATH")
    if env_path:
        exists = Path(env_path).exists()
        checks.append(
            DoctorCheck(
                "ORCAWAVE_PATH",
                "PASS" if exists else "WARN",
                f"set to '{env_path}'"
                + ("" if exists else " (path does not exist)"),
            )
        )
    else:
        checks.append(DoctorCheck("ORCAWAVE_PATH", "WARN", "not set"))

    detected = _detect_executable(executable)
    if executable is not None:
        checks.append(
            DoctorCheck(
                "Explicit executable",
                "PASS" if detected else "FAIL",
                f"'{executable}'"
                + ("" if detected else " does not exist"),
            )
        )
    else:
        checks.append(
            DoctorCheck(
                "OrcaWave executable",
                "PASS" if detected else "WARN",
                f"detected at '{detected}'"
                if detected
                else "not found (explicit path, ORCAWAVE_PATH, standard "
                "install dirs, PATH all empty)",
            )
        )

    target = Path(output_dir) if output_dir is not None else Path.cwd()
    try:
        target.mkdir(parents=True, exist_ok=True)
        with tempfile.NamedTemporaryFile(dir=target, prefix=".doctor_"):
            pass
        checks.append(
            DoctorCheck("Output directory", "PASS", f"'{target}' is writable")
        )
    except OSError as error:
        checks.append(
            DoctorCheck(
                "Output directory", "FAIL", f"'{target}' not writable: {error}"
            )
        )

    cpu = os.cpu_count() or 1
    mem = _memory_gb()
    guidance = f"{cpu} CPUs"
    if mem is not None:
        guidance += f", {mem} GB RAM"
    guidance += (
        ". Moderate thread counts are safer: very high thread counts have "
        "produced out-of-memory failures on large cases (see #714)."
    )
    checks.append(DoctorCheck("Threads/memory", "PASS", guidance))

    if api_available:
        capability = "api-solve"
    elif detected is not None:
        capability = "subprocess-solve"
    else:
        capability = "dry-run-only"
    checks.append(
        DoctorCheck(
            "Host capability",
            "PASS" if capability != "dry-run-only" else "WARN",
            {
                "api-solve": "solver-capable via OrcFxAPI (preferred)",
                "subprocess-solve": "solver-capable via OrcaWave executable",
                "dry-run-only": (
                    f"dry-run only on this {platform.system()} host - runs "
                    "will generate input packages but not solve"
                ),
            }[capability],
        )
    )
    return checks, capability
