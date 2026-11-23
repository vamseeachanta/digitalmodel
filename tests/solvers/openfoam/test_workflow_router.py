"""OpenFOAM workflow router — engine dispatch + build/solve + fail-closed.

Proves the CFD capability is reachable through the main digitalmodel engine
(``basename: openfoam`` / ``cfd``) and that the thin handler delegates to the
existing OpenFOAMCaseBuilder / OpenFOAMRunner (digitalmodel #1161).
"""

from __future__ import annotations

from pathlib import Path
from unittest.mock import patch

import pytest

from digitalmodel.engine import engine
from digitalmodel.solvers.openfoam.workflow import OpenFOAMWorkflow


def _cfg(tmp_path: Path, **openfoam_overrides) -> dict:
    settings = {
        "operation": "run_openfoam",
        "case_type": "current_loading",
        "name": "router_case",
        "output_directory": "out",
    }
    settings.update(openfoam_overrides)
    return {
        "basename": "openfoam",
        "Analysis": {"result_folder": str(tmp_path)},
        "openfoam": settings,
    }


# --------------------------------------------------------------------------- #
#  router: build (license-free, real OpenFOAMCaseBuilder)                      #
# --------------------------------------------------------------------------- #
def test_build_case_writes_case_tree(tmp_path: Path):
    cfg = _cfg(tmp_path, operation="build_case")
    out = OpenFOAMWorkflow().router(cfg)
    settings = out["openfoam"]
    assert settings["run_status"] == "built"
    case_dir = Path(settings["case_dir"])
    # The real builder authored the standard OpenFOAM tree.
    assert (case_dir / "system" / "controlDict").is_file()
    assert (case_dir / "constant").is_dir()
    assert (case_dir / "0").is_dir()


def test_solver_override_is_honored(tmp_path: Path):
    cfg = _cfg(tmp_path, operation="build_case", solver="pimpleFoam")
    out = OpenFOAMWorkflow().router(cfg)
    control = Path(out["openfoam"]["case_dir"]) / "system" / "controlDict"
    assert "pimpleFoam" in control.read_text()


# --------------------------------------------------------------------------- #
#  router: run (delegates to OpenFOAMRunner)                                   #
# --------------------------------------------------------------------------- #
def test_explicit_dry_run_records_status_without_raising(tmp_path: Path):
    cfg = _cfg(tmp_path, dry_run=True)
    out = OpenFOAMWorkflow().router(cfg)
    assert out["openfoam"]["run_status"] == "dry_run"
    assert "case_dir" in out["openfoam"]["outputs"]


def test_real_solve_without_openfoam_is_fail_closed(tmp_path: Path):
    # No OpenFOAM on PATH -> runner falls back to DRY_RUN -> router must raise,
    # so the licensed-run lane can never record a false finish.
    cfg = _cfg(tmp_path, dry_run=False)
    with patch("digitalmodel.solvers.openfoam.runner.shutil.which", return_value=None):
        with pytest.raises(RuntimeError, match="dry-run|unavailable"):
            OpenFOAMWorkflow().router(cfg)


# --------------------------------------------------------------------------- #
#  router: input validation                                                   #
# --------------------------------------------------------------------------- #
def test_unsupported_operation_raises(tmp_path: Path):
    cfg = _cfg(tmp_path, operation="bogus")
    with pytest.raises(ValueError, match="Unsupported openfoam operation"):
        OpenFOAMWorkflow().router(cfg)


def test_missing_case_type_raises(tmp_path: Path):
    cfg = _cfg(tmp_path)
    cfg["openfoam"].pop("case_type")
    with pytest.raises(ValueError, match="openfoam.case_type is required"):
        OpenFOAMWorkflow().router(cfg)


def test_unknown_case_type_raises(tmp_path: Path):
    cfg = _cfg(tmp_path, case_type="not_a_case")
    with pytest.raises(ValueError, match="Unknown openfoam.case_type"):
        OpenFOAMWorkflow().router(cfg)


# --------------------------------------------------------------------------- #
#  engine dispatch: basename openfoam / cfd resolve to the handler            #
# --------------------------------------------------------------------------- #
@pytest.mark.parametrize("basename", ["openfoam", "cfd"])
def test_engine_resolves_keyword_to_openfoam_handler(basename: str):
    cfg = {"basename": basename, "openfoam": {"operation": "build_case"}}
    with patch("digitalmodel.engine.app_manager") as mock_app_manager:
        mock_app_manager.save_cfg.return_value = None
        with patch(
            "digitalmodel.solvers.openfoam.workflow.OpenFOAMWorkflow.router",
            return_value=cfg,
        ) as mock_router:
            result = engine(cfg=cfg, config_flag=False)
    mock_router.assert_called_once()
    assert result is not None
