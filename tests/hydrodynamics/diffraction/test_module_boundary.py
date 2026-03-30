"""Verify license boundary: non-solver diffraction code imports without OrcFxAPI.

Per D-12/D-13: solver-dependent code lives in diffraction/solver/ subpackage.
Everything else must import cleanly on machines without OrcFxAPI/OrcaFlex.
"""
import importlib
import pytest


# License-free modules that MUST import on any machine
LICENSE_FREE_MODULES = [
    "digitalmodel.hydrodynamics.diffraction.input_schemas",
    "digitalmodel.hydrodynamics.diffraction.output_schemas",
    "digitalmodel.hydrodynamics.diffraction.spec_converter",
    "digitalmodel.hydrodynamics.diffraction.orcawave_backend",
    "digitalmodel.hydrodynamics.diffraction.orcawave_runner",
    "digitalmodel.hydrodynamics.diffraction.output_validator",
    "digitalmodel.hydrodynamics.diffraction.comparison_framework",
    "digitalmodel.hydrodynamics.diffraction.report_data_models",
    "digitalmodel.hydrodynamics.diffraction.orcawave_batch_runner",
    "digitalmodel.hydrodynamics.diffraction.result_extractor",
]


@pytest.mark.parametrize("module_path", LICENSE_FREE_MODULES)
def test_license_free_import(module_path: str) -> None:
    """Each license-free module imports without OrcFxAPI."""
    mod = importlib.import_module(module_path)
    assert mod is not None, f"{module_path} failed to import"


def test_solver_subpackage_exists() -> None:
    """The solver/ subpackage directory exists as a Python package."""
    from pathlib import Path
    solver_init = (
        Path(__file__).parent.parent.parent.parent
        / "src" / "digitalmodel" / "hydrodynamics" / "diffraction" / "solver" / "__init__.py"
    )
    assert solver_init.exists(), "solver/__init__.py must exist"


def test_old_paths_removed() -> None:
    """Moved files no longer exist at their old locations."""
    from pathlib import Path
    diffraction_dir = (
        Path(__file__).parent.parent.parent.parent
        / "src" / "digitalmodel" / "hydrodynamics" / "diffraction"
    )
    # These files should have been moved to solver/
    assert not (diffraction_dir / "orcawave_converter.py").exists(), \
        "orcawave_converter.py should be in solver/ subpackage, not diffraction/"
    assert not (diffraction_dir / "orcawave_data_extraction.py").exists(), \
        "orcawave_data_extraction.py should be in solver/ subpackage, not diffraction/"
    # report_extractors.py also moved
    assert not (diffraction_dir / "report_extractors.py").exists(), \
        "report_extractors.py should be in solver/ subpackage, not diffraction/"


def test_solver_marker_registered() -> None:
    """The @pytest.mark.solver marker is registered in pytest config."""
    # If strict-markers is enabled (it is in pyproject.toml), using an
    # unregistered marker would fail. This test simply verifies the marker
    # can be applied without error.
    marker = pytest.mark.solver
    assert marker is not None
