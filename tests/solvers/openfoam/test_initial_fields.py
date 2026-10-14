"""Pressure field names and dimensions for the supported solver families."""

import re
from pathlib import Path

from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder
from digitalmodel.solvers.openfoam.initial_fields import write_pressure_field
from digitalmodel.solvers.openfoam.models import CaseType, OpenFOAMCase


def _dimensions(path: Path) -> str:
    match = re.search(
        r"^dimensions\s+(\[[^\]]+\]);$", path.read_text(), re.MULTILINE
    )
    assert match is not None, f"Missing dimensions entry in {path.name}"
    return match.group(1)


def test_single_phase_pressure_is_kinematic(tmp_path: Path) -> None:
    """The incompressible pressure file is p with pressure-over-density units."""
    write_pressure_field(tmp_path, is_multiphase=False)

    assert (tmp_path / "p").is_file()
    assert not (tmp_path / "p_rgh").exists()
    assert _dimensions(tmp_path / "p") == "[0 2 -2 0 0 0 0]"


def test_multiphase_pressure_dimensions_regression_pin(tmp_path: Path) -> None:
    """Regression pin: VOF p_rgh retains pascal dimensions from BASE."""
    write_pressure_field(tmp_path, is_multiphase=True)

    assert (tmp_path / "p_rgh").is_file()
    assert not (tmp_path / "p").exists()
    assert _dimensions(tmp_path / "p_rgh") == "[1 -1 -2 0 0 0 0]"


def test_pressure_name_never_has_the_other_family_dimensions(
    tmp_path: Path,
) -> None:
    """Neither phase flag produces a pressure name with the other units."""
    for is_multiphase in (False, True):
        zero_dir = tmp_path / ("multiphase" if is_multiphase else "single_phase")
        zero_dir.mkdir()
        write_pressure_field(zero_dir, is_multiphase=is_multiphase)

        for field in zero_dir.iterdir():
            pairing = (field.name, _dimensions(field))
            assert pairing != ("p", "[1 -1 -2 0 0 0 0]")
            assert pairing != ("p_rgh", "[0 2 -2 0 0 0 0]")


def test_case_builder_preserves_pressure_name_dimension_pairing(
    tmp_path: Path,
) -> None:
    """Single-phase and VOF contracts emit their respective pressure units."""
    cases = (
        (CaseType.CURRENT_LOADING, "simpleFoam", False, "p", "[0 2 -2 0 0 0 0]"),
        (CaseType.WAVE_LOADING, "interFoam", True, "p_rgh", "[1 -1 -2 0 0 0 0]"),
    )
    for case_type, solver, is_multiphase, field_name, dimensions in cases:
        case = OpenFOAMCase.for_case_type(case_type, name=solver)
        assert case.solver_config.solver_name == solver
        assert case.solver_config.is_multiphase == is_multiphase
        zero_dir = OpenFOAMCaseBuilder(case).build(tmp_path) / "0"

        assert (zero_dir / field_name).is_file()
        assert _dimensions(zero_dir / field_name) == dimensions
        other_field = "p" if is_multiphase else "p_rgh"
        assert not (zero_dir / other_field).exists()
