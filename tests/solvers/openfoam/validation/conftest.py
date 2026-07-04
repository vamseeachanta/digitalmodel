"""Shared helpers for foundational CFD validation tests (#1161 Phase 2)."""

from __future__ import annotations

from pathlib import Path

from digitalmodel.solvers.openfoam.doctor import run_doctor


def solver_capable(output_dir: Path) -> bool:
    """Return True iff the doctor reports a solver-capable host.

    The solve assertions are gated on this so the suite stays green on
    dry-run-only hosts (case generation works; no OpenFOAM toolchain).
    """
    _checks, capability = run_doctor(output_dir=output_dir)
    return capability == "solver-capable"


def assert_valid_case_dir(case_dir: Path) -> None:
    """Assert the directory is a structurally valid OpenFOAM case."""
    assert case_dir.is_dir(), f"case dir not created: {case_dir}"
    for sub in ("system", "constant", "0"):
        assert (case_dir / sub).is_dir(), f"missing {sub}/ in {case_dir}"
    assert (case_dir / "system" / "controlDict").is_file(), "missing controlDict"
    assert (case_dir / "system" / "blockMeshDict").is_file(), "missing blockMeshDict"
    assert (case_dir / "constant" / "transportProperties").is_file()
    assert (case_dir / "constant" / "turbulenceProperties").is_file()
