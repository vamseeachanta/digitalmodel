"""Contract tests for the public synthetic L-tank Gmsh fixture."""

from __future__ import annotations

import copy
import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.gmsh_meshing.tank_fixture import (
    EXPECTED_FRAME,
    TankFixtureSpec,
    build_tank_fixture,
    inspect_msh2,
    load_tank_fixture_spec,
)


def valid_spec_data() -> dict:
    return {
        "schema_version": 1,
        "length_unit": "m",
        "frame": dict(EXPECTED_FRAME),
        "tank": {
            "breadth": 1.2,
            "height": 0.8,
            "length": 1.6,
            "longitudinal_leg_width": 0.6,
            "transverse_leg_length": 0.6,
        },
        "members": {
            "height": 0.5,
            "thickness": 0.06,
            "edge_clearance": 0.08,
        },
        "openings": {
            "longitudinal": {
                "vertical_radius": 0.18,
                "span_radius": 0.32,
            },
            "transverse": {
                "transverse_radius": 0.18,
                "vertical_radius": 0.18,
            },
        },
        "mesh": {"element_size": 0.16},
    }


def test_spec_uses_frozen_si_y_up_roll_frame_and_analytic_volume() -> None:
    spec = TankFixtureSpec.from_mapping(valid_spec_data())

    gross = 0.8 * (0.6 * 1.6 + 1.2 * 0.6 - 0.6 * 0.6)
    longitudinal_span = 1.6 - 0.6 - 2.0 * 0.08
    transverse_span = 1.2 - 0.6 - 2.0 * 0.08
    longitudinal_solid = 0.06 * (
        0.5 * longitudinal_span - math.pi * 0.18 * 0.32
    )
    transverse_solid = 0.06 * (
        0.5 * transverse_span - math.pi * 0.18 * 0.18
    )

    assert spec.length_unit == "m"
    assert spec.frame == EXPECTED_FRAME
    assert spec.gross_volume == pytest.approx(gross)
    assert spec.expected_fluid_volume == pytest.approx(
        gross - longitudinal_solid - transverse_solid
    )


@pytest.mark.parametrize(
    ("mutate", "message"),
    [
        (lambda data: data.update(length_unit="mm"), "length_unit"),
        (
            lambda data: data["frame"].update(y="vertical_down"),
            "frame",
        ),
        (
            lambda data: data["tank"].update(height=0.0),
            "positive",
        ),
        (
            lambda data: data["tank"].update(longitudinal_leg_width=1.2),
            "longitudinal_leg_width",
        ),
        (
            lambda data: data["openings"]["longitudinal"].update(
                vertical_radius=0.26
            ),
            "longitudinal opening",
        ),
    ],
)
def test_spec_rejects_unit_frame_dimension_and_opening_mismatch(
    mutate, message: str
) -> None:
    data = copy.deepcopy(valid_spec_data())
    mutate(data)

    with pytest.raises(ValueError, match=message):
        TankFixtureSpec.from_mapping(data)


@pytest.fixture(scope="module")
def serialized_meshes(tmp_path_factory):
    root = tmp_path_factory.mktemp("synthetic_l_tank")
    spec = TankFixtureSpec.from_mapping(valid_spec_data())
    first = root / "first.msh"
    second = root / "second.msh"
    first_summary = build_tank_fixture(spec, first)
    second_summary = build_tank_fixture(spec, second)
    return spec, first, second, first_summary, second_summary


def test_occ_fixture_has_expected_volume_members_openings_and_surface_coverage(
    serialized_meshes,
) -> None:
    spec, _, _, summary, _ = serialized_meshes

    assert summary.volume_count == 1
    assert summary.member_count == 2
    assert summary.opening_count == 2
    assert summary.fluid_volume == pytest.approx(
        spec.expected_fluid_volume, rel=1e-8
    )
    assert summary.wall_surface_tags
    assert summary.atmosphere_surface_tags
    assert summary.unassigned_surface_tags == ()
    assert summary.overlapping_surface_tags == ()
    assert set(summary.wall_surface_tags).isdisjoint(
        summary.atmosphere_surface_tags
    )


def test_serialized_mesh_is_tagged_first_order_ascii_msh2_with_positive_tets(
    serialized_meshes,
) -> None:
    _, first, _, summary, _ = serialized_meshes
    contract = inspect_msh2(first)

    assert contract.version == "2.2"
    assert contract.binary is False
    assert contract.element_types == frozenset({2, 4})
    assert contract.triangle_count > 0
    assert contract.tetrahedron_count > 0
    assert contract.untagged_element_count == 0
    assert contract.element_ids_are_unique
    assert contract.physical_names == {
        "fluid": (3, 1),
        "walls": (2, 2),
        "atmosphere": (2, 3),
    }
    assert contract.physical_ids == frozenset({1, 2, 3})
    assert contract.min_signed_determinant > 0.0
    assert summary.shape_min_jacobian > 0.0
    assert summary.reopened_serialized_mesh is True


def test_mesh_options_and_repeat_build_are_deterministic(serialized_meshes) -> None:
    _, first, second, first_summary, second_summary = serialized_meshes

    assert first.read_bytes() == second.read_bytes()
    assert first_summary.mesh_options == second_summary.mesh_options == {
        "Mesh.MshFileVersion": 2.2,
        "Mesh.Binary": 0.0,
        "Mesh.SaveAll": 0.0,
        "Mesh.ElementOrder": 1.0,
        "Mesh.Reproducible": 1.0,
        "Mesh.RandomSeed": 0.0,
        "General.NumThreads": 1.0,
        "Mesh.MaxNumThreads1D": 1.0,
        "Mesh.MaxNumThreads2D": 1.0,
        "Mesh.MaxNumThreads3D": 1.0,
    }


def test_scaled_fixture_keeps_groups_and_cubic_volume(tmp_path: Path) -> None:
    original = TankFixtureSpec.from_mapping(valid_spec_data())
    scaled = original.scaled(0.5)
    output = tmp_path / "scaled.msh"

    summary = build_tank_fixture(scaled, output)
    contract = inspect_msh2(output)

    assert scaled.expected_fluid_volume == pytest.approx(
        original.expected_fluid_volume * 0.5**3
    )
    assert contract.physical_names == {
        "fluid": (3, 1),
        "walls": (2, 2),
        "atmosphere": (2, 3),
    }
    assert summary.unassigned_surface_tags == ()


def test_source_mesh_promotion_uses_temporary_sibling(
    tmp_path: Path, monkeypatch
) -> None:
    spec = TankFixtureSpec.from_mapping(valid_spec_data())
    output = tmp_path / "source.msh"
    promotions: list[tuple[Path, Path]] = []
    from digitalmodel.solvers.gmsh_meshing import _tank_fixture_gmsh

    real_replace = _tank_fixture_gmsh.os.replace

    def record_replace(source, destination) -> None:
        promotions.append((Path(source), Path(destination)))
        real_replace(source, destination)

    monkeypatch.setattr(_tank_fixture_gmsh.os, "replace", record_replace)

    build_tank_fixture(spec, output)

    assert len(promotions) == 1
    temporary, destination = promotions[0]
    assert temporary.parent == destination.parent == output.parent
    assert temporary != destination
    assert destination == output


def test_public_fixture_is_synthetic_si_methodology_input() -> None:
    repo_root = next(
        parent
        for parent in Path(__file__).resolve().parents
        if (parent / "pyproject.toml").is_file()
    )
    fixture = repo_root / "examples" / "cfd" / "synthetic_l_tank" / "input.yml"

    raw = yaml.safe_load(fixture.read_text(encoding="utf-8"))
    spec = load_tank_fixture_spec(fixture)

    assert raw["metadata"] == {
        "name": "synthetic_l_tank",
        "purpose": "methodology_bridge_validation_only",
        "engineering_claim": "none",
    }
    assert spec == TankFixtureSpec.from_mapping(valid_spec_data())
