"""fowt model type: routing safeguards.

Radiation-matrix symmetry, RNA inertia routing, repeated generation and
finite inputs. No licensed solver is used. Split from test_fowt_model_type.py.
"""
from __future__ import annotations

import copy
import math

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.floating_wind.iea15_orcaflex_spec import (
    build_spec,
    load_sources,
)
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.routers.fowt_router import (
    FowtRouter,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.modular_generator.schema.fowt import FowtSpec

from ._fowt_support import _flexible


@pytest.fixture(scope="module")
def sources():
    return load_sources()


@pytest.fixture(scope="module")
def spec_dict(sources):
    basis, data = sources
    return build_spec(basis, data)


# ------------------------------------------------------- routing safeguards
MASS_KEY = ("AddedMassMatrixX, AddedMassMatrixY, AddedMassMatrixZ, "
            "AddedMassMatrixRx, AddedMassMatrixRy, AddedMassMatrixRz")
DAMPING_KEY = "DampingX, DampingY, DampingZ, DampingRx, DampingRy, DampingRz"


def _radiation(fowt):
    spec = fowt if isinstance(fowt, FowtSpec) else FowtSpec(**fowt)
    routed = FowtRouter().route(spec)
    return routed["vessel_types"][0]["properties"]["Draughts"][0][
        "FrequencyDependentAddedMassAndDamping"]


def test_emitted_radiation_is_exactly_symmetric_without_mutating_source(spec_dict):
    fowt = FowtSpec(**spec_dict["fowt"])
    original = fowt.model_dump()
    entries = _radiation(fowt)
    for source, emitted in zip(fowt.hull.radiation, entries, strict=True):
        for matrix, key in ((source.added_mass, MASS_KEY), (source.damping, DAMPING_KEY)):
            for i in range(6):
                assert emitted[key][i][i] == matrix[i][i]
                for j in range(i + 1, 6):
                    # Orcina WAMIT importer policy: upper triangle mirrored.
                    assert emitted[key][i][j] == matrix[i][j]
                    assert emitted[key][j][i] == matrix[i][j]
    assert fowt.model_dump() == original


@pytest.mark.parametrize("matrix_name,key", [("added_mass", MASS_KEY), ("damping", DAMPING_KEY)])
def test_radiation_symmetry_tolerance_accepts_boundary_and_rejects_excess(
        spec_dict, matrix_name, key):
    fowt = copy.deepcopy(spec_dict["fowt"])
    matrix = [[0.0] * 6 for _ in range(6)]
    for i in range(6):
        matrix[i][i] = 100.0
    matrix[0][1], matrix[1][0] = 2.0, 3.0
    fowt["hull"]["radiation"][0][matrix_name] = matrix
    emitted = _radiation(fowt)[0][key]
    assert emitted[0][1] == emitted[1][0] == 2.0
    matrix[1][0] = 3.01  # Exceeds 1 % of sqrt(diagonal_i * diagonal_j).
    with pytest.raises(ValueError, match="symmetry tolerance"):
        _radiation(fowt)


@pytest.mark.parametrize("matrix_name", ["added_mass", "damping"])
def test_radiation_zero_diagonal_does_not_hide_asymmetry(spec_dict, matrix_name):
    fowt = copy.deepcopy(spec_dict["fowt"])
    matrix = [[0.0] * 6 for _ in range(6)]
    matrix[0][1] = 1.0
    fowt["hull"]["radiation"][0][matrix_name] = matrix
    with pytest.raises(ValueError, match="symmetry tolerance"):
        _radiation(fowt)


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
@pytest.mark.parametrize("matrix_name", ["added_mass", "damping"])
def test_radiation_nonfinite_entries_rejected(spec_dict, matrix_name, value):
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["hull"]["radiation"][0][matrix_name][0][1] = value
    with pytest.raises(ValueError, match="finite"):
        _radiation(fowt)


def test_first_order_wave_load_rejected_without_sourced_raos(spec_dict):
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["hull"]["include_wave_load_first_order"] = True
    with pytest.raises(ValidationError, match="sourced load RAOs"):
        FowtSpec(**fowt)


@pytest.mark.parametrize("i,j", [(0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1)])
def test_rna_asymmetric_inertia_rejected(spec_dict, i, j):
    fowt = _flexible(spec_dict)
    fowt["rna"]["inertia_about_centre_of_mass"][i][j] = -1.2e4
    with pytest.raises(ValidationError, match="symmetric"):
        FowtSpec(**fowt)


def test_rna_non_positive_definite_inertia_rejected(spec_dict):
    fowt = _flexible(spec_dict)
    inertia = fowt["rna"]["inertia_about_centre_of_mass"]
    inertia[0][2] = inertia[2][0] = 2.0 * max(inertia[0][0], inertia[2][2])
    with pytest.raises(ValidationError, match="positive definite"):
        FowtSpec(**fowt)


def _rotation_from_buoy_attitude(angles):
    a, b, c = (math.radians(x) for x in angles)
    rx = [[1, 0, 0], [0, math.cos(a), -math.sin(a)], [0, math.sin(a), math.cos(a)]]
    ry = [[math.cos(b), 0, math.sin(b)], [0, 1, 0], [-math.sin(b), 0, math.cos(b)]]
    rz = [[math.cos(c), -math.sin(c), 0], [math.sin(c), math.cos(c), 0], [0, 0, 1]]

    def mul(p, q):
        return [[sum(p[i][k] * q[k][j] for k in range(3)) for j in range(3)] for i in range(3)]
    return mul(mul(rx, ry), rz)


def _buoys(fowt):
    return {b["name"]: b["properties"]
            for b in FowtRouter().route(FowtSpec(**fowt))["buoys_6d"]}


@pytest.mark.parametrize("products", [
    {(0, 2): -1.2e4},
    {(0, 1): 3.0e3, (0, 2): -4.5e4, (1, 2): -7.0e2},
])
@pytest.mark.parametrize("tilted", [False, True])
def test_rna_products_of_inertia_carried_by_principal_axes_child(spec_dict, products, tilted):
    """Products are not dropped: the host carries mass and centre, a rigidly
    attached child carries the principal moments at the principal attitude, and
    the pair reproduces the full tensor about the centre of mass."""
    fowt = _flexible(spec_dict)
    rna = fowt["rna"]
    for (i, j), value in products.items():
        rna["inertia_about_centre_of_mass"][i][j] = value
        rna["inertia_about_centre_of_mass"][j][i] = value
    if tilted:
        fowt["hull"]["initial_orientation"] = [1.0, 3.5, 20.0]
    upright = copy.deepcopy(fowt)
    upright["rna"]["inertia_about_centre_of_mass"] = [
        [rna["inertia_about_centre_of_mass"][i][i] if i == j else 0.0 for j in range(3)]
        for i in range(3)]
    buoys, reference = _buoys(fowt), _buoys(upright)
    name = rna.get("name", "RNA")
    host = buoys[name]
    child = buoys[f"{name} inertia"]
    # Host: unchanged position, attitude, mass and centre; moments moved to the child.
    for key in ("InitialPosition", "InitialAttitude", "Mass", "CentreOfMass", "Connection"):
        assert host[key] == reference[name][key]
    assert host["MomentsOfInertia"] == [0, 0, 0]
    assert child["Connection"] == name
    assert child["InitialPosition"] == rna["centre_of_mass_from_tower_top"]
    assert child["Mass"] == 0 and child["CentreOfMass"] == [0, 0, 0]
    r = _rotation_from_buoy_attitude(child["InitialAttitude"])
    moments = child["MomentsOfInertia"]
    rebuilt = [[sum(r[i][k] * moments[k] * r[j][k] for k in range(3)) for j in range(3)]
               for i in range(3)]
    full = rna["inertia_about_centre_of_mass"]
    scale = max(full[i][i] for i in range(3))
    for i in range(3):
        for j in range(3):
            assert rebuilt[i][j] == pytest.approx(full[i][j], abs=1e-9 * scale)
    # Child axes stay close to the host axes (no axis permutation).
    assert all(abs(angle) < 45.0 for angle in child["InitialAttitude"])
    # Hub load point and tower connection are unaffected.
    for other in set(reference) - {name}:
        assert buoys[other] == reference[other]


def _assert_rebuilds(full, attitude, moments):
    r = _rotation_from_buoy_attitude(attitude)
    rebuilt = [[sum(r[i][k] * moments[k] * r[j][k] for k in range(3)) for j in range(3)]
               for i in range(3)]
    scale = max(full[i][i] for i in range(3))
    for i in range(3):
        for j in range(3):
            assert rebuilt[i][j] == pytest.approx(full[i][j], abs=1e-9 * scale)


def test_rna_tied_axis_inertia_routes(spec_dict):
    """Principal axes at 45 deg tie two host axes; the assignment is one-to-one.

    [[2, 1, 0], [1, 2, 0], [0, 0, 2]] passes schema validation; its x and y
    principal axes lie at 45 deg in the xy plane, so a per-axis argmax picks
    the same host axis for both and routing failed.
    """
    fowt = _flexible(spec_dict)
    rna = fowt["rna"]
    unit = rna["inertia_about_centre_of_mass"][2][2] / 2.0
    tensor = [[2 * unit, unit, 0.0], [unit, 2 * unit, 0.0], [0.0, 0.0, 2 * unit]]
    rna["inertia_about_centre_of_mass"] = tensor
    FowtSpec(**fowt)  # valid input
    child = _buoys(fowt)[f"{rna.get('name', 'RNA')} inertia"]
    assert sorted(child["MomentsOfInertia"]) == pytest.approx(
        [unit, 2 * unit, 3 * unit], rel=1e-12)
    _assert_rebuilds(tensor, child["InitialAttitude"], child["MomentsOfInertia"])
    assert all(abs(angle) <= 45.0 + 1e-9 for angle in child["InitialAttitude"])


def test_principal_axes_one_to_one_for_valid_tensors():
    """Every symmetric positive-definite tensor yields a proper rotation.

    Seeded random tensors, the 45 deg tie, a fully tied (isotropic-pair)
    tensor and principal axes from a 180 deg turn about (1, 1, 1), whose
    columns all have negative diagonals before sign correction.
    """
    import numpy as np

    from digitalmodel.solvers.orcaflex.modular_generator.routers.fowt_router import (
        buoy_attitude,
        principal_axes,
    )

    rng = np.random.default_rng(4014)
    n = np.ones(3) / math.sqrt(3.0)
    turn = 2.0 * np.outer(n, n) - np.eye(3)
    tensors = [
        [[2.0, 1.0, 0.0], [1.0, 2.0, 0.0], [0.0, 0.0, 2.0]],
        [[2.0, 0.0, 1.0], [0.0, 2.0, 0.0], [1.0, 0.0, 2.0]],
        [[3.0, 1.0, 1.0], [1.0, 3.0, 1.0], [1.0, 1.0, 3.0]],
        (turn @ np.diag([1.0, 2.0, 3.0]) @ turn.T).tolist(),
    ]
    for _ in range(200):
        q, _r = np.linalg.qr(rng.normal(size=(3, 3)))
        tensors.append((q @ np.diag(rng.uniform(1.0, 5.0, 3)) @ q.T).tolist())
    for tensor in tensors:
        moments, axes = principal_axes(tensor)
        assert np.linalg.det(np.array(axes)) == pytest.approx(1.0, abs=1e-9)
        _assert_rebuilds(tensor, buoy_attitude(axes), moments)


def test_diagonal_rna_inertia_emits_no_child(spec_dict):
    buoys = _buoys(_flexible(spec_dict))
    assert not any(name.endswith(" inertia") for name in buoys)


def test_generated_source_note_preserves_inertia_qualification(spec_dict, sources):
    basis, _ = sources
    note = spec_dict["fowt"]["source_note"]
    assert f"Basis readiness {basis['qualification']['readiness']}" in note
    assert f"use for analysis {basis['qualification']['use_for_analysis']}" in note
    assert "System inertia assumes the RNA tensor is about tower top with CG tensor F-P" in note
    assert "Retaining F24:K24 as trusted endpoint is an additional assumption" in note
    assert "L24:Q24 remains the independent published comparator" in note
    assert "tensor-reference discrepancy remains unresolved" in note


@pytest.mark.parametrize("representation", ["rigid", "flexible"])
@pytest.mark.parametrize("method", ["generate", "generate_with_overrides"])
@pytest.mark.parametrize("from_file", [False, True])
def test_repeated_generation_preserves_bundle_and_domain_spec(
        spec_dict, tmp_path, representation, method, from_file):
    data = copy.deepcopy(spec_dict)
    if representation == "flexible":
        data["fowt"] = _flexible(spec_dict)
    source = ProjectInputSpec(**data)
    original = source.model_dump()
    if from_file:
        path = tmp_path / "spec.yml"
        path.write_text(yaml.safe_dump(data), encoding="utf-8")
        generator = ModularModelGenerator(path)
    else:
        generator = ModularModelGenerator.from_spec(source)
    outputs = []
    for name in ("first", "second"):
        out = tmp_path / name
        getattr(generator, method)(out)
        outputs.append({p.relative_to(out): p.read_bytes() for p in out.rglob("*") if p.is_file()})
    assert outputs[0] == outputs[1]
    assert source.model_dump() == original
    assert generator.spec.fowt == source.fowt
    assert len(generator.spec.generic.vessels) == 1
    assert len(generator.spec.generic.lines) == (4 if representation == "flexible" else 3)


def test_repeat_routing_reflects_domain_edits(spec_dict):
    generator = ModularModelGenerator.from_spec(ProjectInputSpec(**spec_dict))
    generator._run_routers()
    generator.spec.fowt.hull.initial_position[0] = 5.0
    generator._run_routers()
    assert len(generator.spec.generic.vessels) == 1
    assert generator.spec.generic.vessels[0].properties["InitialPosition"][0] == 5.0


# ------------------------------------------------------- finite inputs
def _set_path(data: dict, path: tuple, value: float) -> None:
    target = data
    for key in path[:-1]:
        target = target[key]
    target[path[-1]] = value


NONFINITE_PATHS = [
    ("hull", "mass"),
    ("hull", "centre_of_mass", 2),
    ("hull", "inertia_about_centre_of_mass", 0, 0),
    ("hull", "displaced_volume"),
    ("hull", "centre_of_buoyancy", 0),
    ("hull", "hydrostatic_stiffness_excluding_gravity", 1, 1),
    ("hull", "radiation", -1, "period"),
    ("hull", "radiation", 0, "added_mass", 2, 2),
    ("hull", "other_quadratic_damping", 0),
    ("mooring_line_types", 0, "mass_per_length"),
    ("mooring_lines", 0, "fairlead", 0),
    ("mooring_lines", 0, "anchor_xy", 1),
    ("mooring_lines", 0, "anchor_height_above_seabed"),
    ("mooring_lines", 0, "length"),
]
FLEXIBLE_NONFINITE_PATHS = [
    ("tower", "base_elevation"),
    ("tower", "sections", 0, "bending_stiffness"),
    ("tower", "sections", 0, "poisson_ratio"),
    ("rna", "mass"),
    ("rna", "centre_of_mass_from_tower_top", 0),
    ("rna", "inertia_about_centre_of_mass", 1, 1),
]


@pytest.mark.parametrize("value", [float("nan"), float("inf")])
@pytest.mark.parametrize("path", NONFINITE_PATHS)
def test_nonfinite_rigid_inputs_rejected(spec_dict, path, value):
    """NaN or infinity must never reach the generated model."""
    data = copy.deepcopy(spec_dict)
    _set_path(data["fowt"], path, value)
    with pytest.raises(ValidationError):
        FowtSpec(**data["fowt"])
    with pytest.raises(ValidationError):
        ProjectInputSpec(**data)


@pytest.mark.parametrize("value", [float("nan"), -float("inf")])
@pytest.mark.parametrize("path", FLEXIBLE_NONFINITE_PATHS)
def test_nonfinite_flexible_inputs_rejected(spec_dict, path, value):
    fowt = _flexible(spec_dict)
    _set_path(fowt, path, value)
    with pytest.raises(ValidationError):
        FowtSpec(**fowt)


@pytest.mark.parametrize("value", [float("nan"), float("inf")])
def test_nonfinite_thrust_inputs_rejected(spec_dict, value):
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["rotor_thrust"] = {"thrust": 1000.0, "shaft_tilt": 6.0,
                            "hub_position": [-10.0, 0.0, 150.0], "wind_speed": 10.0}
    FowtSpec(**fowt)  # finite reference accepted
    for key, index in (("thrust", None), ("shaft_tilt", None), ("wind_speed", None),
                       ("hub_position", 2)):
        probe = copy.deepcopy(fowt)
        if index is None:
            probe["rotor_thrust"][key] = value
        else:
            probe["rotor_thrust"][key][index] = value
        with pytest.raises(ValidationError):
            FowtSpec(**probe)
