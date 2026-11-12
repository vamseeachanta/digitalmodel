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
from pathlib import Path

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


# OpenFOAM v2312 (2023), Usage sections for kqRWallFunction,
# epsilonWallFunction and nutkWallFunction: mesh type wall. The omegaWallFunction
# API identifies wall treatment; the preflight applies the same wall contract.
# Sources are linked in docs/domains/openfoam/closed_wall_boundaries.md.
_WALL_OVERRIDES = [
    (TurbulenceType.K_OMEGA_SST, "k", "kqRWallFunction"),
    (TurbulenceType.K_OMEGA_SST, "omega", "omegaWallFunction"),
    (TurbulenceType.K_OMEGA_SST, "nut", "nutkWallFunction"),
    (TurbulenceType.K_EPSILON, "epsilon", "epsilonWallFunction"),
    (TurbulenceType.LES_SMAGORINSKY, "nut", "nutkWallFunction"),
]


def _wall_override(patch, field, wall_function):
    return BoundaryCondition(
        patch, BoundaryType.ZERO_GRADIENT, field,
        value="$internalField", extra={"type": wall_function},
    )


@pytest.mark.parametrize("turbulence,field,wall_function", _WALL_OVERRIDES)
@pytest.mark.parametrize("patch", ["inlet", "outlet", "top", "sides"])
def test_wall_function_on_open_patch_is_rejected_before_build(
    tmp_path, turbulence, field, wall_function, patch
):
    """The published mesh-type requirement is the oracle; no solver is used."""
    case = OpenFOAMCase.for_case_type(CaseType.SLOSHING, name="invalid_wall")
    case.turbulence_model = TurbulenceModel(turbulence_type=turbulence)
    case.boundary_conditions = [_wall_override(patch, field, wall_function)]
    with pytest.raises(ValueError, match=rf"{patch}.*{field}.*{wall_function}.*wall"):
        OpenFOAMCaseBuilder(case).build(tmp_path)
    assert not (tmp_path / case.name).exists()


@pytest.mark.parametrize("turbulence,field,wall_function", _WALL_OVERRIDES)
def test_direct_turbulence_writer_rejects_before_any_field_is_written(
    tmp_path, turbulence, field, wall_function
):
    """Preflight covers all emitted fields before creating even the k file."""
    with pytest.raises(ValueError, match="outlet"):
        write_turbulence_fields(
            tmp_path, TurbulenceModel(turbulence_type=turbulence),
            [_wall_override("outlet", field, wall_function)],
        )
    assert not list(tmp_path.iterdir())


@pytest.mark.parametrize("turbulence,field,wall_function", _WALL_OVERRIDES)
@pytest.mark.parametrize("patch", ["bottom", "outlet"])
def test_compatible_wall_function_override_is_preserved(
    tmp_path, turbulence, field, wall_function, patch
):
    """Native bottom and promoted outlet walls accept explicit wall functions."""
    case = _closed_case()
    case.turbulence_model = TurbulenceModel(turbulence_type=turbulence)
    case.boundary_conditions.append(_wall_override(patch, field, wall_function))
    root = OpenFOAMCaseBuilder(case).build(tmp_path)
    assert _patch((root / "0" / field).read_text(), patch) == {
        "type": wall_function, "value": "$internalField",
    }


@pytest.mark.parametrize("last_is_wall_function", [False, True])
def test_preflight_checks_only_the_effective_field_override(
    tmp_path, last_is_wall_function
):
    """Last-definition-wins applies to validation as well as serialization."""
    case = OpenFOAMCase.for_case_type(CaseType.SLOSHING, name="override_order")
    wall = _wall_override("outlet", "nut", "nutkWallFunction")
    free = BoundaryCondition("outlet", BoundaryType.ZERO_GRADIENT, "nut")
    case.boundary_conditions = [free, wall] if last_is_wall_function else [wall, free]
    if last_is_wall_function:
        with pytest.raises(ValueError, match="outlet"):
            OpenFOAMCaseBuilder(case).build(tmp_path)
    else:
        root = OpenFOAMCaseBuilder(case).build(tmp_path)
        assert _patch((root / "0/nut").read_text(), "outlet") == {
            "type": "zeroGradient"
        }


def test_reopening_a_wall_rejects_leftover_wall_function_override(tmp_path):
    """A later U definition can invalidate a previously compatible override."""
    case = _closed_case()
    case.boundary_conditions.extend([
        _wall_override("outlet", "nut", "nutkWallFunction"),
        BoundaryCondition("outlet", BoundaryType.ZERO_GRADIENT, "U"),
    ])
    with pytest.raises(ValueError, match="outlet"):
        OpenFOAMCaseBuilder(case).build(tmp_path)


@pytest.mark.parametrize("turbulence,field,wall_function", [
    (TurbulenceType.LAMINAR, "nut", "nutkWallFunction"),
    (TurbulenceType.K_EPSILON, "omega", "omegaWallFunction"),
    (TurbulenceType.K_OMEGA_SST, "epsilon", "epsilonWallFunction"),
    (TurbulenceType.LES_SMAGORINSKY, "omega", "omegaWallFunction"),
])
def test_unwritten_turbulence_fields_do_not_trigger_preflight(
    tmp_path, turbulence, field, wall_function
):
    """An unused field cannot cause a runtime patch-type failure."""
    case = OpenFOAMCase.for_case_type(CaseType.SLOSHING, name="inactive_field")
    case.turbulence_model = TurbulenceModel(turbulence_type=turbulence)
    case.boundary_conditions = [_wall_override("outlet", field, wall_function)]
    root = OpenFOAMCaseBuilder(case).build(tmp_path)
    assert not (root / "0" / field).exists()


def test_failed_preflight_preserves_existing_case_files(tmp_path):
    """Reject an invalid rebuild without partially overwriting a valid case."""
    case = _closed_case()
    root = OpenFOAMCaseBuilder(case).build(tmp_path)
    before = {p.relative_to(root): p.read_bytes() for p in root.rglob("*") if p.is_file()}
    case.boundary_conditions.append(_wall_override("top", "nut", "nutkWallFunction"))
    with pytest.raises(ValueError, match="top"):
        OpenFOAMCaseBuilder(case).build(tmp_path)
    after = {p.relative_to(root): p.read_bytes() for p in root.rglob("*") if p.is_file()}
    assert after == before


@pytest.mark.parametrize("serialized_velocity", ["noSlip", "zeroGradient"])
def test_preflight_uses_serialized_velocity_type(tmp_path, serialized_velocity):
    """Topology follows extra.type, even when it replaces a noSlip enum."""
    case = _closed_case()
    case.boundary_conditions.extend([
        BoundaryCondition(
            "outlet", BoundaryType.NO_SLIP, "U", extra={"type": serialized_velocity}
        ),
        _wall_override("outlet", "nut", "nutkWallFunction"),
    ])
    if serialized_velocity == "noSlip":
        root = OpenFOAMCaseBuilder(case).build(tmp_path)
        assert _patch((root / "0/nut").read_text(), "outlet")["type"] == "nutkWallFunction"
    else:
        with pytest.raises(ValueError, match="outlet"):
            OpenFOAMCaseBuilder(case).build(tmp_path)


def test_documented_worked_examples(tmp_path):
    """Execute the documented inputs; the published wall contract is the oracle.

    The closed example has U_wall = 0, so Q_face = U_wall dot S_face = 0
    (OpenFOAM v2312 noSlip Description, linked in the document). Its mesh
    is wall and accepts nutkWallFunction. Reopening restores patch topology
    and violates the nutkWallFunction Usage mesh requirement, so must fail.
    """
    document = (
        Path(__file__).resolve().parents[3]
        / "docs/domains/openfoam/closed_wall_boundaries.md"
    ).read_text()
    snippets = re.findall(r"```python\n(.*?)```", document, re.DOTALL)
    namespace = {}
    for snippet in snippets[:-1]:
        exec(compile(snippet, "<worked-example>", "exec"), namespace)
    root = namespace["build_example"](tmp_path)
    assert _patch((root / "system/blockMeshDict").read_text(), "outlet")["type"] == "wall"
    assert _patch((root / "0/nut").read_text(), "outlet") == {
        "type": "nutkWallFunction", "value": "$internalField",
    }
    exec(compile(snippets[-1], "<worked-example>", "exec"), namespace)
    with pytest.raises(ValueError, match="outlet"):
        namespace["build_example"](tmp_path)
