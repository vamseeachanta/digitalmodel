"""ABOUTME: Independent comparator/rejection tests and optional real CAD pipeline."""

from dataclasses import replace
from importlib import import_module
from math import nan, pi
from types import SimpleNamespace
from unittest.mock import Mock
import sys

import pytest

from digitalmodel.solvers.gmsh_meshing.spool_pilot import (
    ELEMENT_COUNT_BAND,
    MeshMeasurements,
    SolidMeasurements,
    SpoolParameters,
    hand_calculation,
    judge_mesh,
    judge_solid,
    mesh_step,
)


@pytest.fixture
def specification():
    return SpoolParameters()


@pytest.fixture
def reference_observations():
    # Independently specified synthetic observations, NOT CAD evidence.
    dims = {
        "pipe_length": 300,
        "pipe_outer_diameter": 90,
        "bore_diameter": 78,
        "total_length": 396,
    }
    for end in ("left", "right"):
        dims.update(
            {
                f"{end}.flange_thickness": 18,
                f"{end}.flange_outer_diameter": 160,
                f"{end}.neck_length": 30,
                f"{end}.neck_base_diameter": 120,
                f"{end}.hole_count": 4,
            }
        )
        for i in range(4):
            dims[f"{end}.hole_{i}.diameter"] = 12
            dims[f"{end}.hole_{i}.bolt_circle"] = 140
    return SolidMeasurements(396_900 * pi, dims, True, 1)


def test_hand_calculation_matches_independent_components(specification):
    result = hand_calculation(specification)
    assert result.pipe_mm3 == pytest.approx(151_200 * pi)
    assert result.necks_mm3 == pytest.approx(75_240 * pi)
    assert result.flanges_mm3 == pytest.approx(175_644 * pi)
    assert result.bolt_holes_mm3 == pytest.approx(5_184 * pi)
    assert result.volume_mm3 == pytest.approx(396_900 * pi)
    assert result.mass_kg == pytest.approx(396_900 * pi * 7.85e-6)


def test_density_and_unit_conversion(specification):
    steel = hand_calculation(specification)
    other = hand_calculation(replace(specification, density_kg_m3=1000))
    assert other.volume_mm3 == steel.volume_mm3
    assert other.mass_kg == pytest.approx(steel.volume_mm3 * 1e-6)


def test_uniform_geometric_scaling(specification):
    fields = specification.__dataclass_fields__
    scaled = replace(
        specification,
        **{
            name: getattr(specification, name) * 2
            for name in fields
            if name not in ("density_kg_m3", "bolt_hole_count")
        },
    )
    assert hand_calculation(scaled).volume_mm3 == pytest.approx(
        8 * hand_calculation(specification).volume_mm3
    )


@pytest.mark.parametrize(
    "change",
    [
        {"pipe_length": 0},
        {"neck_length": -1},
        {"density_kg_m3": nan},
        {"pipe_outer_diameter": 78},
        {"neck_base_diameter": 160},
        {"bolt_circle_diameter": 130},
        {"bolt_circle_diameter": 150},
        {"bolt_hole_count": 4.5},
        {"bolt_hole_count": True},
        {"bolt_hole_count": 100},
    ],
)
def test_impossible_parameters_rejected(specification, change):
    with pytest.raises(ValueError):
        replace(specification, **change)


def test_solid_judge_accepts_reference(specification, reference_observations):
    judge_solid(specification, reference_observations).assert_passed()


@pytest.mark.parametrize(
    "factor,passes", [(1.0049, True), (1.0051, False), (0.9951, True), (0.9949, False)]
)
def test_volume_tolerance_is_half_percent(
    specification, reference_observations, factor, passes
):
    observations = replace(
        reference_observations, volume_mm3=reference_observations.volume_mm3 * factor
    )
    verdict = judge_solid(specification, observations)
    assert verdict.passed is passes
    if not passes:
        assert any(f.startswith("volume:") for f in verdict.failures)
        assert any(f.startswith("mass:") for f in verdict.failures)


@pytest.mark.parametrize("dimension", sorted(SpoolParameters().dimensions()))
def test_every_dimension_is_checked_even_at_correct_volume(
    specification,
    reference_observations,
    dimension,
):
    dims = dict(reference_observations.dimensions)
    dims[dimension] += 1 if dimension.endswith("hole_count") else 0.011
    verdict = judge_solid(
        specification, replace(reference_observations, dimensions=dims)
    )
    assert not verdict.passed
    assert any(f.startswith(f"dimension {dimension}:") for f in verdict.failures)
    assert not any(f.startswith("volume:") for f in verdict.failures)


def test_dimension_inside_tolerance(specification, reference_observations):
    dims = dict(reference_observations.dimensions, pipe_length=300.0099)
    assert judge_solid(
        specification, replace(reference_observations, dimensions=dims)
    ).passed


@pytest.mark.parametrize(
    "change",
    [
        {"volume_mm3": nan},
        {"volume_mm3": 0},
        {"valid": False},
        {"solid_count": 0},
        {"solid_count": 2},
        {"dimensions": {}},
        {"dimensions": {**SpoolParameters().dimensions(), "extra": 42}},
        {"dimensions": {**SpoolParameters().dimensions(), "bore_diameter": nan}},
    ],
)
def test_missing_invalid_or_nonfinite_geometry_rejected(
    specification,
    reference_observations,
    change,
):
    verdict = judge_solid(specification, replace(reference_observations, **change))
    assert not verdict.passed
    with pytest.raises(AssertionError):
        verdict.assert_passed()


def test_planted_thickness_error_has_closed_form_signature(specification):
    defective = replace(specification, left_flange_thickness=19.8)
    nominal = hand_calculation(specification)
    actual = hand_calculation(defective)
    assert actual.volume_mm3 - nominal.volume_mm3 == pytest.approx(8_523 * pi)
    assert (actual.volume_mm3 / nominal.volume_mm3 - 1) > 0.005
    verdict = judge_solid(
        specification,
        SolidMeasurements(actual.volume_mm3, defective.dimensions(), True, 1),
    )
    assert not verdict.passed
    assert any(f.startswith("volume:") for f in verdict.failures)
    assert any(
        f.startswith("dimension left.flange_thickness:") for f in verdict.failures
    )


def test_mesh_quality_and_count_boundaries():
    for count in ELEMENT_COUNT_BAND:
        assert judge_mesh(MeshMeasurements((1.0,) * count, (0.2,) * count)).passed


@pytest.mark.parametrize("count", [0, 999, 80_001])
def test_out_of_band_mesh_rejected(count):
    assert not judge_mesh(MeshMeasurements((1.0,) * count, (0.9,) * count)).passed


@pytest.mark.parametrize(
    "determinant,quality",
    [
        (-1.0, 0.9),
        (0.0, 0.9),
        (nan, 0.9),
        (1.0, 0.19999),
        (1.0, nan),
    ],
)
def test_one_bad_element_rejects_entire_mesh(determinant, quality):
    determinants = (1.0,) * 999 + (determinant,)
    qualities = (0.9,) * 999 + (quality,)
    assert not judge_mesh(MeshMeasurements(determinants, qualities)).passed


def test_missing_quality_values_rejected():
    assert not judge_mesh(MeshMeasurements((1.0,) * 1000, ())).passed


@pytest.fixture
def mesh_adapter(monkeypatch):
    # Fake only the native boundary: verify metric selection and resource cleanup.
    from digitalmodel.solvers.gmsh_meshing import mesh_generator

    tags = list(range(1, 1001))
    mesh = Mock()
    mesh.getElements.return_value = ([4], [tags], [[]])
    mesh.getElementQualities.side_effect = lambda tags, metric: (
        [1.0] * len(tags) if metric == "minDetJac" else [0.8] * len(tags)
    )
    native = SimpleNamespace(
        isInitialized=lambda: False, model=SimpleNamespace(mesh=mesh)
    )
    monkeypatch.setitem(sys.modules, "gmsh", native)
    generator = Mock()
    monkeypatch.setattr(mesh_generator, "GMSHMeshGenerator", lambda: generator)
    return native, mesh, generator, tags


def test_mesh_adapter_requests_signed_and_scaled_metrics(mesh_adapter, tmp_path):
    _, mesh, generator, tags = mesh_adapter
    result = mesh_step(tmp_path / "spool.step")
    assert judge_mesh(result).passed
    assert result.scaled_jacobians == (0.8,) * 1000
    mesh.getElements.assert_called_once_with(3)
    assert mesh.getElementQualities.call_args_list == [
        ((tags, "minDetJac"),),
        ((tags, "minSJ"),),
    ]
    generator.generate_mesh_from_step.assert_called_once_with(
        str(tmp_path / "spool.step"), element_size=5.0
    )
    generator.finalize.assert_called_once_with()


@pytest.mark.parametrize("bad_type,incomplete", [(11, False), (4, True)])
def test_mesh_adapter_rejects_unsupported_or_incomplete_results(
    mesh_adapter,
    tmp_path,
    bad_type,
    incomplete,
):
    _, mesh, generator, tags = mesh_adapter
    mesh.getElements.return_value = ([bad_type], [tags], [[]])
    if incomplete:
        mesh.getElementQualities.side_effect = lambda tags, metric: [1.0] * 999
    with pytest.raises(ValueError):
        mesh_step(tmp_path / "spool.step")
    generator.finalize.assert_called_once_with()


def test_mesh_generation_failure_propagates_and_finalizes(mesh_adapter, tmp_path):
    _, _, generator, _ = mesh_adapter
    generator.generate_mesh_from_step.side_effect = RuntimeError("meshing failed")
    with pytest.raises(RuntimeError, match="meshing failed"):
        mesh_step(tmp_path / "spool.step")
    generator.finalize.assert_called_once_with()


@pytest.fixture
def native_dependencies():
    missing = []
    for name in ("FreeCAD", "Part", "gmsh"):
        try:
            import_module(name)
        except (ImportError, OSError) as error:
            missing.append(f"{name}: {error}")
    if missing:
        pytest.skip(
            "real CAD/mesh pilot requires importable FreeCAD, Part and gmsh; "
            + "; ".join(missing)
        )


@pytest.mark.integration
def test_real_spool_passes_all_three_checks(
    native_dependencies, specification, tmp_path
):
    from digitalmodel.visualization.design_tools.flanged_spool import (
        build_spool,
        measure_spool,
    )

    shape = build_spool(specification)
    observed = measure_spool(shape)
    judge_solid(specification, observed).assert_passed()
    step = tmp_path / "nominal.step"
    shape.exportStep(str(step))
    mesh = mesh_step(step)
    judge_mesh(mesh).assert_passed()
    print(
        f"CAD volume={observed.volume_mm3:.9f} mm^3; "
        f"CAD mass={observed.volume_mm3 * 7.85e-6:.9f} kg; "
        f"tetrahedra={mesh.element_count}; "
        f"minSJ={min(mesh.scaled_jacobians):.9f}; "
        f"minDetJac={min(mesh.determinants):.9f}"
    )


@pytest.mark.integration
def test_real_planted_defect_fails_original_specification(
    native_dependencies,
    specification,
):
    from digitalmodel.visualization.design_tools.flanged_spool import (
        build_spool,
        measure_spool,
    )

    defective = replace(
        specification, left_flange_thickness=specification.left_flange_thickness * 1.10
    )
    shape = build_spool(defective)
    observed = measure_spool(shape)
    # Prove the defect is a valid solid, then compare it to the ORIGINAL parameters.
    assert observed.valid and observed.solid_count == 1
    verdict = judge_solid(specification, observed)
    assert not verdict.passed
    assert any(f.startswith("volume:") for f in verdict.failures)
    assert any(
        f.startswith("dimension left.flange_thickness:") for f in verdict.failures
    )
    with pytest.raises(AssertionError):
        verdict.assert_passed()


@pytest.mark.integration
def test_real_solid_regenerates_with_changed_parameters(
    native_dependencies, specification
):
    from digitalmodel.visualization.design_tools.flanged_spool import (
        build_spool,
        measure_spool,
    )

    changed = replace(
        specification,
        pipe_length=240,
        pipe_outer_diameter=92,
        bore_diameter=76,
        neck_length=25,
        neck_base_diameter=118,
        flange_outer_diameter=164,
        left_flange_thickness=20,
        right_flange_thickness=22,
        bolt_circle_diameter=142,
        bolt_hole_diameter=10,
        bolt_hole_count=6,
    )
    judge_solid(changed, measure_spool(build_spool(changed))).assert_passed()
