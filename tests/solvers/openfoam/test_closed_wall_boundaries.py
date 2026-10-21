"""Closed-wall dictionary contracts, independent of a solver installation.

Oracles: OpenFOAM v2312 (2023), noSlip and nutkWallFunction Usage require
mesh type wall; kqRWallFunction, omegaWallFunction and epsilonWallFunction
specify the field types below. fixedFluxPressure Description requires pressure
flux to match velocity while compensating for gravity. See the linked sources
in docs/domains/openfoam/closed_wall_boundaries.md.

Hand calculation: noSlip gives U_wall = (0, 0, 0), hence volumetric face flux
U_wall dot S_face = 0 for any face area vector. This is a boundary contract,
not evidence of a solved tank-volume balance or wall-shear accuracy.
"""

import re

import pytest

from digitalmodel.solvers.openfoam.block_mesh import render_block_mesh_dict_body
from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder
from digitalmodel.solvers.openfoam.case_coupling import (
    map_sweep_case_to_openfoam_case,
    synthetic_coupling_spec,
    synthetic_sweep_case,
)
from digitalmodel.solvers.openfoam.initial_fields import (
    write_pressure_field,
    write_turbulence_fields,
)
from digitalmodel.solvers.openfoam.models import (
    BoundaryCondition,
    BoundaryType,
    CaseType,
    OpenFOAMCase,
    TurbulenceModel,
    TurbulenceType,
)


def _patch(text, name):
    """Read the flat entries of one named patch in these generated dictionaries."""
    match = re.search(rf"\b{re.escape(name)}\s*\{{([^{{}}]*)\}}", text)
    assert match is not None, f"Missing patch {name}"
    return dict(re.findall(r"(\w+)\s+([^;]+);", match.group(1)))


def _closed_case():
    return map_sweep_case_to_openfoam_case(
        synthetic_sweep_case(), synthetic_coupling_spec()
    )


@pytest.mark.parametrize(
    "turbulence,wall_fields",
    [
        (
            TurbulenceType.K_OMEGA_SST,
            {
                "k": "kqRWallFunction",
                "omega": "omegaWallFunction",
                "nut": "nutkWallFunction",
            },
        ),
        (
            TurbulenceType.K_EPSILON,
            {
                "k": "kqRWallFunction",
                "epsilon": "epsilonWallFunction",
                "nut": "nutkWallFunction",
            },
        ),
        (
            TurbulenceType.LES_SMAGORINSKY,
            {"k": "kqRWallFunction", "nut": "nutkWallFunction"},
        ),
        (TurbulenceType.LAMINAR, {}),
    ],
)
def test_coupled_closed_outlet_is_a_wall_in_mesh_and_every_live_field(
    tmp_path, turbulence, wall_fields
):
    """The outlet and bottom require the sourced wall BCs, including pressure."""
    case = _closed_case()
    case.turbulence_model = TurbulenceModel(turbulence_type=turbulence)
    root = OpenFOAMCaseBuilder(case).build(tmp_path)
    mesh = (root / "system/blockMeshDict").read_text()
    for patch in ("outlet", "bottom"):
        assert _patch(mesh, patch)["type"] == "wall"
        assert _patch((root / "0/U").read_text(), patch)["type"] == "noSlip"
        assert (
            _patch((root / "0/p_rgh").read_text(), patch)["type"] == "fixedFluxPressure"
        )
        assert (
            _patch((root / "0/alpha.water").read_text(), patch)["type"]
            == "zeroGradient"
        )
        for field, expected in wall_fields.items():
            actual = _patch((root / "0" / field).read_text(), patch)
            assert actual["type"] == expected
            assert "value" in actual
    if turbulence is TurbulenceType.LAMINAR:
        assert not any(
            (root / "0" / f).exists() for f in ("k", "omega", "epsilon", "nut")
        )
    assert _patch(mesh, "inlet")["type"] == "patch"
    assert _patch(mesh, "top")["type"] == "patch"
    assert _patch(mesh, "sides")["type"] == "symmetry"
    assert (
        _patch((root / "0/U").read_text(), "top")["type"]
        == "pressureInletOutletVelocity"
    )
    assert _patch((root / "0/p_rgh").read_text(), "top")["type"] == "totalPressure"


@pytest.mark.parametrize("case_type", list(CaseType))
def test_default_open_outlets_retain_free_patch_conditions(tmp_path, case_type):
    """A case type alone must not turn an open outlet into a wall."""
    case = OpenFOAMCase.for_case_type(case_type, name=case_type.value)
    root = OpenFOAMCaseBuilder(case).build(tmp_path)
    assert (
        _patch((root / "system/blockMeshDict").read_text(), "outlet")["type"] == "patch"
    )
    assert _patch((root / "0/U").read_text(), "outlet")["type"] == "zeroGradient"
    assert _patch((root / "0/k").read_text(), "outlet")["type"] == "zeroGradient"
    assert _patch((root / "0/nut").read_text(), "outlet")["type"] == "calculated"


@pytest.mark.parametrize("case_type", list(CaseType))
@pytest.mark.parametrize("patch", ["inlet", "outlet", "top", "sides"])
def test_no_slip_overrides_drive_mesh_and_turbulence_together(
    tmp_path, patch, case_type
):
    """No-slip on any generated face must satisfy the same wall contract."""
    case = OpenFOAMCase.for_case_type(case_type, name="closed_face")
    case.boundary_conditions = [BoundaryCondition(patch, BoundaryType.NO_SLIP, "U")]
    root = OpenFOAMCaseBuilder(case).build(tmp_path)
    assert _patch((root / "system/blockMeshDict").read_text(), patch)["type"] == "wall"
    assert _patch((root / "0/omega").read_text(), patch)["type"] == "omegaWallFunction"
    for field_path in (root / "0").iterdir():
        field_type = _patch(field_path.read_text(), patch)["type"]
        assert field_type not in {"symmetry", "empty"}, field_path.name
    pressure = "p_rgh" if case.solver_config.is_multiphase else "p"
    expected = "fixedFluxPressure" if case.solver_config.is_multiphase else "zeroGradient"
    assert _patch((root / "0" / pressure).read_text(), patch)["type"] == expected
    if case.solver_config.is_multiphase:
        assert (
            _patch((root / "0/alpha.water").read_text(), patch)["type"]
            == "zeroGradient"
        )


def test_last_velocity_override_controls_topology(tmp_path):
    """Match the field writer's last-definition-wins semantics when reopening."""
    case = _closed_case()
    case.boundary_conditions.append(
        BoundaryCondition("outlet", BoundaryType.ZERO_GRADIENT, "U")
    )
    root = OpenFOAMCaseBuilder(case).build(tmp_path)
    assert (
        _patch((root / "system/blockMeshDict").read_text(), "outlet")["type"] == "patch"
    )
    assert _patch((root / "0/k").read_text(), "outlet")["type"] == "zeroGradient"


def test_turbulence_writer_honours_explicit_field_overrides(tmp_path):
    """An explicitly supplied field BC remains authoritative over defaults."""
    case = _closed_case()
    override = BoundaryCondition("inlet", BoundaryType.ZERO_GRADIENT, "k")
    case.boundary_conditions.append(override)
    write_turbulence_fields(
        tmp_path, case.turbulence_model, boundary_conditions=case.boundary_conditions
    )
    assert _patch((tmp_path / "k").read_text(), "inlet") == {"type": "zeroGradient"}
    assert _patch((tmp_path / "k").read_text(), "outlet")["type"] == "kqRWallFunction"


def test_wall_pressure_default_preserves_explicit_overrides(tmp_path):
    """The automatic fixed-flux pressure is a default, not a forced override."""
    case = _closed_case()
    override = BoundaryCondition("bottom", BoundaryType.ZERO_GRADIENT, "p_rgh")
    case.boundary_conditions.append(override)
    write_pressure_field(tmp_path, True, case.boundary_conditions)
    assert _patch((tmp_path / "p_rgh").read_text(), "bottom")["type"] == "zeroGradient"


def test_velocity_closure_alone_selects_fixed_flux_pressure(tmp_path):
    """A closure without explicit pressure BCs gets the sourced wall default."""
    bcs = [BoundaryCondition("outlet", BoundaryType.NO_SLIP, "U")]
    write_pressure_field(tmp_path, True, bcs)
    for patch in ("bottom", "outlet"):
        assert (
            _patch((tmp_path / "p_rgh").read_text(), patch)["type"]
            == "fixedFluxPressure"
        )


def test_serialized_velocity_type_controls_all_wall_defaults(tmp_path):
    """The mesh uses the effective serialized type, including extra entries."""
    case = _closed_case()
    case.boundary_conditions.append(
        BoundaryCondition(
            "inlet", BoundaryType.ZERO_GRADIENT, "U", extra={"type": "noSlip"}
        )
    )
    root = OpenFOAMCaseBuilder(case).build(tmp_path)
    assert (
        _patch((root / "system/blockMeshDict").read_text(), "inlet")["type"] == "wall"
    )
    assert _patch((root / "0/U").read_text(), "inlet")["type"] == "noSlip"
    assert _patch((root / "0/nut").read_text(), "inlet")["type"] == "nutkWallFunction"


def test_single_phase_wall_pressure_is_unchanged(tmp_path):
    """Kinematic p keeps its existing zero-gradient wall default."""
    write_pressure_field(tmp_path, False)
    assert _patch((tmp_path / "p").read_text(), "bottom")["type"] == "zeroGradient"


def test_mesh_geometry_and_patch_names_are_preserved():
    """Closure changes only the outlet patch type, not faces or cell counts."""
    case = _closed_case()
    open_mesh = render_block_mesh_dict_body(case.domain)
    closed_mesh = render_block_mesh_dict_body(case.domain, case.boundary_conditions)
    assert closed_mesh == open_mesh.replace(
        "outlet\n    {\n        type patch;", "outlet\n    {\n        type wall;"
    )
