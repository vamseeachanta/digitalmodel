"""ABOUTME: Independent comparator/rejection tests and optional real CAD pipeline."""

from dataclasses import replace
from importlib import import_module
from math import cos, nan, pi, sin, sqrt
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
            dims[f"{end}.hole_{i}.next_hole_spacing"] = 70 * sqrt(2)
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
        mass_failure = next(f for f in verdict.failures if f.startswith("mass:"))
        assert "derived from volume and specified density" in mass_failure


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


def test_mesh_adapter_requests_signed_and_shape_metrics(mesh_adapter, tmp_path):
    _, mesh, generator, tags = mesh_adapter
    result = mesh_step(tmp_path / "spool.step")
    assert judge_mesh(result).passed
    assert result.min_sicn == (0.8,) * 1000
    assert mesh.optimize.call_args_list == [((),), (("Netgen",),)]
    mesh.getElements.assert_called_once_with(3)
    assert mesh.getElementQualities.call_args_list == [
        ((tags, "minDetJac"),),
        ((tags, "minSICN"),),
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
def native_freecad():
    missing = []
    for name in ("FreeCAD", "Part"):
        try:
            import_module(name)
        except (ImportError, OSError) as error:
            missing.append(f"{name}: {error}")
    if missing:
        pytest.skip(
            "B-rep metrology requires importable FreeCAD and Part; "
            + "; ".join(missing)
        )


@pytest.mark.integration
def test_real_spool_passes_all_three_checks(
    native_freecad, native_gmsh, specification, tmp_path
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
        f"minSICN={min(mesh.min_sicn):.9f}; "
        f"minDetJac={min(mesh.determinants):.9f}"
    )


@pytest.mark.integration
def test_real_planted_defect_fails_original_specification(
    native_freecad,
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
def test_real_solid_regenerates_with_changed_parameters(native_freecad, specification):
    from digitalmodel.visualization.design_tools.flanged_spool import (
        build_spool,
        measure_spool,
    )

    changed = changed_parameters(specification)
    judge_solid(changed, measure_spool(build_spool(changed))).assert_passed()


def changed_parameters(specification):
    return replace(
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


@pytest.fixture
def native_gmsh():
    try:
        return import_module("gmsh")
    except (ImportError, OSError) as error:
        pytest.skip(f"native mesh checks require gmsh: {error}")


@pytest.mark.integration
@pytest.mark.parametrize("plant_sliver", [False, True])
def test_native_shape_gate_detects_a_straight_sided_sliver(native_gmsh, plant_sliver):
    from digitalmodel.solvers.gmsh_meshing.spool_pilot import measure_mesh

    gmsh = native_gmsh
    gmsh.initialize()
    try:
        gmsh.option.setNumber("General.Terminal", 0)
        volume = gmsh.model.addDiscreteEntity(3)
        coordinates = []
        for i in range(1000):
            height = 0.001 if plant_sliver and i == 999 else sqrt(2 / 3)
            coordinates.extend(
                [
                    3 * i, 0, 0,
                    3 * i + 1, 0, 0,
                    3 * i + 0.5, sqrt(3) / 2, 0,
                    3 * i + 0.5, sqrt(3) / 6, height,
                ]
            )
        gmsh.model.mesh.addNodes(3, volume, list(range(1, 4001)), coordinates)
        tags = list(range(1, 1001))
        gmsh.model.mesh.addElementsByType(volume, 4, tags, list(range(1, 4001)))
        # minSJ cannot distinguish these slivers from regular linear tetrahedra.
        assert gmsh.model.mesh.getElementQualities(tags, "minSJ") == pytest.approx(
            [1.0] * 1000
        )
        measured = measure_mesh()
        assert measured.element_count == 1000
        assert min(measured.determinants) > 0
        verdict = judge_mesh(measured)
        if plant_sliver:
            assert min(measured.min_sicn) < 0.2
            assert verdict.failures == (
                "mesh: minimum signed inverse condition number below 0.2 or nonfinite",
            )
        else:
            assert min(measured.min_sicn) == pytest.approx(1)
            verdict.assert_passed()
    finally:
        gmsh.finalize()

def test_metrology_dependencies_do_not_require_mesher(monkeypatch, request):
    def cad_only(name):
        if name not in ("FreeCAD", "Part"):
            raise ImportError("mesher deliberately unavailable")
        return SimpleNamespace()

    monkeypatch.setattr(sys.modules[__name__], "import_module", cad_only)
    request.getfixturevalue("native_freecad")


@pytest.fixture
def occ_step(native_gmsh, tmp_path):
    """Make independent STEP evidence using only gmsh's OCC primitives."""

    def build(p):
        gmsh = native_gmsh
        gmsh.initialize()
        try:
            gmsh.option.setNumber("General.Terminal", 0)
            gmsh.model.add("spool")
            occ = gmsh.model.occ
            z1 = p.left_flange_thickness
            z2 = z1 + p.neck_length
            z3 = z2 + p.pipe_length
            z4 = z3 + p.neck_length
            fl = occ.addCylinder(0, 0, 0, 0, 0, z1, p.flange_outer_diameter / 2)
            nl = occ.addCone(
                0, 0, z1, 0, 0, p.neck_length,
                p.neck_base_diameter / 2, p.pipe_outer_diameter / 2,
            )
            pipe = occ.addCylinder(
                0, 0, z2, 0, 0, p.pipe_length, p.pipe_outer_diameter / 2,
            )
            nr = occ.addCone(
                0, 0, z3, 0, 0, p.neck_length,
                p.pipe_outer_diameter / 2, p.neck_base_diameter / 2,
            )
            fr = occ.addCylinder(
                0, 0, z4, 0, 0, p.right_flange_thickness, p.flange_outer_diameter / 2,
            )
            body, _ = occ.fuse([(3, fl)], [(3, nl), (3, pipe), (3, nr), (3, fr)])
            bore = occ.addCylinder(
                0, 0, -1, 0, 0, p.total_length + 2, p.bore_diameter / 2,
            )
            tools = [(3, bore)]
            for z, thickness in ((0, z1), (z4, p.right_flange_thickness)):
                for i in range(p.bolt_hole_count):
                    angle = 2 * pi * i / p.bolt_hole_count
                    hole = occ.addCylinder(
                        p.bolt_circle_diameter / 2 * cos(angle),
                        p.bolt_circle_diameter / 2 * sin(angle),
                        z - 1, 0, 0, thickness + 2, p.bolt_hole_diameter / 2,
                    )
                    tools.append((3, hole))
            solids, _ = occ.cut(body, tools)
            assert len(solids) == 1 and solids[0][0] == 3
            volume = occ.getMass(*solids[0])
            assert volume == pytest.approx(hand_calculation(p).volume_mm3, rel=1e-9)
            occ.synchronize()
            step = tmp_path / "occ-spool.step"
            gmsh.write(str(step))
            return step, volume
        finally:
            gmsh.finalize()

    return build


@pytest.mark.integration
@pytest.mark.parametrize("changed", [False, True], ids=["nominal", "changed"])
def test_native_mesh_without_freecad(occ_step, specification, changed):
    p = changed_parameters(specification) if changed else specification
    step, volume = occ_step(p)
    measured = mesh_step(step)
    judge_mesh(measured).assert_passed()
    print(
        f"OCC volume={volume:.1f} mm^3; "
        f"tetrahedra={measured.element_count}; "
        f"minSICN={min(measured.min_sicn):.3f}; "
        f"minDetJac={min(measured.determinants):.3f}"
    )


@pytest.fixture
def synthetic_brep(monkeypatch):
    """Mock analytic surfaces for extraction tests; no native CAD evidence."""

    class Cylinder(SimpleNamespace):
        pass

    class Cone(SimpleNamespace):
        pass

    class Circle(SimpleNamespace):
        pass

    monkeypatch.setitem(
        sys.modules, "Part", SimpleNamespace(Cylinder=Cylinder, Cone=Cone, Circle=Circle)
    )

    def cylinder(radius, low, high, x=0, y=0):
        return SimpleNamespace(
            Surface=Cylinder(
                Radius=radius, Axis=SimpleNamespace(z=1),
                Center=SimpleNamespace(x=x, y=y),
            ),
            BoundBox=SimpleNamespace(ZMin=low, ZMax=high),
        )

    def build(shift=0):
        faces = [
            cylinder(39, 0, 396), cylinder(45, 48, 348),
            cylinder(80, 0, 18), cylinder(80, 378, 396),
        ]
        for low, high in ((18, 48), (348, 378)):
            faces.append(
                SimpleNamespace(
                    Surface=Cone(Axis=SimpleNamespace(z=1)),
                    BoundBox=SimpleNamespace(ZMin=low, ZMax=high),
                    Edges=[SimpleNamespace(Curve=Circle(Radius=r)) for r in (45, 60)],
                )
            )
        for low, high in ((0, 18), (378, 396)):
            for i in range(4):
                angle = i * pi / 2 + (shift if i == 0 and low == 0 else 0)
                faces.append(cylinder(6, low, high, 70 * cos(angle), 70 * sin(angle)))
        return SimpleNamespace(
            Faces=faces,
            Volume=396_900 * pi,
            BoundBox=SimpleNamespace(ZLength=396),
            Solids=[object()],
            isValid=lambda: True,
            isClosed=lambda: True,
        )

    return build


def test_expected_dimensions_include_each_hole_spacing(specification):
    for end in ("left", "right"):
        for i in range(4):
            assert specification.dimensions()[
                f"{end}.hole_{i}.next_hole_spacing"
            ] == pytest.approx(70 * sqrt(2))


@pytest.mark.parametrize("shift", [0, 0.5 * pi / 180])
def test_metrology_checks_angular_hole_displacement(synthetic_brep, specification, shift):
    from digitalmodel.visualization.design_tools.flanged_spool import measure_spool

    observed = measure_spool(synthetic_brep(shift))
    verdict = judge_solid(specification, observed)
    if shift:
        assert not verdict.passed
        assert any("next_hole_spacing" in failure for failure in verdict.failures)
        assert not any("bolt_circle:" in failure for failure in verdict.failures)
        assert not any(failure.startswith("volume:") for failure in verdict.failures)
    else:
        judge_solid(specification, observed).assert_passed()
        for end in ("left", "right"):
            for i in range(4):
                assert observed.dimensions[f"{end}.hole_{i}.next_hole_spacing"] == (
                    pytest.approx(70 * sqrt(2))
                )


def test_bolt_cutters_extend_past_both_flange_faces(monkeypatch, specification):
    from digitalmodel.visualization.design_tools.flanged_spool import build_spool

    shape = Mock()
    shape.cut.return_value = shape
    shape.removeSplitter.return_value = shape
    face = Mock()
    face.revolve.return_value = shape
    part = SimpleNamespace(
        makePolygon=Mock(), Face=Mock(return_value=face), makeCylinder=Mock()
    )
    monkeypatch.setitem(sys.modules, "Part", part)
    monkeypatch.setitem(
        sys.modules, "FreeCAD",
        SimpleNamespace(Vector=lambda x=0, y=0, z=0: SimpleNamespace(x=x, y=y, z=z)),
    )
    build_spool(specification)
    calls = part.makeCylinder.call_args_list
    assert len(calls) == 8
    for call, start, thickness in zip(calls, [0] * 4 + [378] * 4, [18] * 8):
        radius, height, center = call.args
        assert radius == 6
        assert center.z < start
        assert center.z + height > start + thickness


@pytest.mark.integration
def test_real_hole_displacement_fails_at_unchanged_volume(native_freecad, specification):
    import FreeCAD as App
    import Part

    from digitalmodel.visualization.design_tools.flanged_spool import (
        build_spool,
        measure_spool,
    )

    shape = build_spool(specification)
    # Fill one hole and re-drill at the same radius with a small angular shift.
    fill = Part.makeCylinder(6, 18, App.Vector(70, 0, 0))
    angle = 0.5 * pi / 180
    shifted = Part.makeCylinder(
        6, 20, App.Vector(70 * cos(angle), 70 * sin(angle), -1),
    )
    shape = shape.fuse(fill).cut(shifted).removeSplitter()
    observed = measure_spool(shape)
    assert observed.valid and observed.solid_count == 1
    assert observed.volume_mm3 == pytest.approx(hand_calculation(specification).volume_mm3)
    verdict = judge_solid(specification, observed)
    assert verdict.failures
    assert all("next_hole_spacing" in failure for failure in verdict.failures)
