"""OpenFOAM CFD environment readiness diagnostics (#1161 Phase 1).

Surfaces the runner's detection logic (are the OpenFOAM utilities on PATH?
is the Python post-processing stack importable? is the output root writable?)
as a PASS/WARN/FAIL report, so a user knows whether a host can actually solve
*before* preparing a large case — instead of discovering the runner's
fail-closed dry-run fallback afterwards.

Mirrors ``orcawave_doctor`` (#613). Exit-code policy is enforced by the CLI:
- ``0`` — host usable, including dry-run-only hosts (a supported mode);
- nonzero — any FAIL check (e.g. unwritable output root), or a dry-run-only
  host when ``--require-solver`` was passed.
"""

from __future__ import annotations

import importlib.util
import os
import platform
import shutil
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

# Utilities the runner invokes (mesh -> solve -> convert). blockMesh is the
# availability sentinel used by OpenFOAMRunner._openfoam_available().
_CORE_UTILITIES = ("blockMesh", "foamToVTK")
# Solvers the marine/aero setups select; at least one should be present.
_SOLVERS = ("interFoam", "simpleFoam", "pimpleFoam")
# Default shared output root (env spec §6); overridable.
DEFAULT_OUTPUT_ROOT = Path("/mnt/ace/cfd-output")


@dataclass
class DoctorCheck:
    """One diagnostic line: PASS / WARN / FAIL plus human detail."""

    name: str
    status: str
    detail: str


def _which(name: str) -> Optional[str]:
    return shutil.which(name)


def _openfoam_version() -> Optional[str]:
    """Best-effort OpenFOAM version from the environment (set by etc/bashrc)."""
    for var in ("WM_PROJECT_VERSION", "FOAM_API"):
        val = os.environ.get(var)
        if val:
            return val
    return None


def _module_available(module: str) -> bool:
    try:
        return importlib.util.find_spec(module) is not None
    except (ImportError, ValueError):
        return False


def _memory_gb() -> Optional[float]:
    try:
        for line in Path("/proc/meminfo").read_text().splitlines():
            if line.startswith("MemTotal:"):
                return round(int(line.split()[1]) / 1024 / 1024, 1)
    except OSError:
        pass
    return None


def run_doctor(
    output_dir: Path | None = None,
) -> tuple[list[DoctorCheck], str]:
    """Run all readiness checks.

    Returns ``(checks, capability)`` where capability is ``"solver-capable"``
    (the OpenFOAM toolchain can run a case) or ``"dry-run-only"`` (case
    generation works but no solve — the runner will fail-close to DRY_RUN).
    """
    checks: list[DoctorCheck] = []

    # --- OpenFOAM core utilities -----------------------------------------
    missing_core = [u for u in _CORE_UTILITIES if _which(u) is None]
    core_present = not missing_core
    checks.append(
        DoctorCheck(
            "OpenFOAM utilities",
            "PASS" if core_present else "WARN",
            "blockMesh + foamToVTK on PATH"
            if core_present
            else (
                f"missing {missing_core} — is the OpenFOAM environment sourced? "
                "(source .../etc/bashrc)"
            ),
        )
    )

    # --- at least one solver ---------------------------------------------
    solvers_found = [s for s in _SOLVERS if _which(s) is not None]
    checks.append(
        DoctorCheck(
            "OpenFOAM solvers",
            "PASS" if solvers_found else "WARN",
            f"available: {solvers_found}"
            if solvers_found
            else f"none of {list(_SOLVERS)} found on PATH",
        )
    )

    # --- version (informational) -----------------------------------------
    version = _openfoam_version()
    checks.append(
        DoctorCheck(
            "OpenFOAM version",
            "PASS" if version else "WARN",
            f"WM_PROJECT_VERSION={version}"
            if version
            else "version env not set (etc/bashrc not sourced?)",
        )
    )

    # --- Python post-processing / meshing stack --------------------------
    for module, role in (("pyvista", "post-processing"), ("meshio", "mesh I/O")):
        present = _module_available(module)
        checks.append(
            DoctorCheck(
                f"Python: {module}",
                "PASS" if present else "FAIL",
                f"importable ({role})"
                if present
                else f"not importable — required for {role}",
            )
        )
    gmsh_present = _module_available("gmsh")
    checks.append(
        DoctorCheck(
            "Python: gmsh",
            "PASS" if gmsh_present else "WARN",
            "importable (meshing)"
            if gmsh_present
            else "not importable — install with 'uv sync --extra solvers'",
        )
    )

    # --- output root writability (env spec §6) ---------------------------
    target = Path(output_dir) if output_dir is not None else DEFAULT_OUTPUT_ROOT
    checks.append(_check_writable(target))

    # --- threads / memory guidance ---------------------------------------
    cpu = os.cpu_count() or 1
    mem = _memory_gb()
    guidance = f"{cpu} CPUs"
    if mem is not None:
        guidance += f", {mem} GB RAM"
    guidance += ". Use decomposePar/MPI for 3D cases; match subdomain count to cores."
    checks.append(DoctorCheck("Threads/memory", "PASS", guidance))

    # --- overall capability ----------------------------------------------
    capability = "solver-capable" if core_present and solvers_found else "dry-run-only"
    checks.append(
        DoctorCheck(
            "Host capability",
            "PASS" if capability == "solver-capable" else "WARN",
            "solver-capable — cases can be meshed and solved here"
            if capability == "solver-capable"
            else (
                f"dry-run only on this {platform.system()} host — cases will be "
                "generated but not solved (runner fail-closes to DRY_RUN)"
            ),
        )
    )
    return checks, capability


def _check_writable(target: Path) -> DoctorCheck:
    """Probe the output root without creating a deep tree if it's absent."""
    if not target.exists():
        return DoctorCheck(
            "Output root",
            "WARN",
            f"'{target}' does not exist — create it or pass --output-dir",
        )
    try:
        with tempfile.NamedTemporaryFile(dir=target, prefix=".cfd_doctor_"):
            pass
    except OSError as error:
        return DoctorCheck("Output root", "FAIL", f"'{target}' not writable: {error}")
    return DoctorCheck("Output root", "PASS", f"'{target}' is writable")


def has_failure(checks: list[DoctorCheck]) -> bool:
    return any(c.status == "FAIL" for c in checks)
