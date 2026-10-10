"""Regression criteria for review findings; geometry uses metres."""
import numpy as np
import pytest
from digitalmodel.hydrodynamics.hull_library import column_pontoon_form as form


def aspect(mesh):
    points = mesh.vertices[mesh.panels]
    edges = np.linalg.norm(points - np.roll(points, -1, axis=1), axis=2)
    return np.max(edges.max(axis=1) / edges.min(axis=1))


@pytest.mark.parametrize('target', [3, 1.5])
def test_spar_structured_panels(target):
    mesh, report = form.generate_column_pontoon(form.ColumnPontoonParameters(
        diameter=10, draft=20, panel_target_size=target, lid=True))
    assert mesh.n_panels < 600 * (3 / target)**2
    assert aspect(mesh) < 10
    assert report.max_aspect_ratio == pytest.approx(aspect(mesh))


def test_primitive_checks_are_mesh_comparisons():
    _, report = form.generate_column_pontoon(form.ColumnPontoonParameters(
        diameter=10, draft=20, panel_target_size=3))
    for check in report.primitive_checks:
        for metric in ['volume', 'waterplane_area', 'kb', 'bm']:
            result = check[metric]
            assert result['mesh'] == pytest.approx(check['analytic_' + metric], rel=.005)
            assert np.max(result['relative_difference']) <= result['tolerance']
            assert result['passed']


@pytest.mark.parametrize('count,square', [(6, False), (3, True), (6, True)])
def test_additional_layouts(count, square):
    mesh, report = form.generate_column_pontoon(form.ColumnPontoonParameters(
        count=count, spacing=20, draft=10, panel_target_size=3, lid=True,
        **({'square_side': 4} if square else {'diameter': 4}),
        pontoon_layout='ring', pontoon_width=1, pontoon_height=2))
    assert report.watertight
    assert report.winding.outward


def test_open_status_and_oriented_lid():
    for lid in [False, True]:
        mesh, report = form.generate_column_pontoon(form.ColumnPontoonParameters(
            diameter=10, draft=20, panel_target_size=3, lid=lid))
        assert report.closure_status == ('closed-with-lid' if lid else 'open-at-waterline')
        assert report.winding.outward
        if lid:
            top = np.max(np.abs(mesh.vertices[mesh.panels, 2]), axis=1) < 1e-8
            assert np.all(mesh.normals[top, 2] > 0)


def test_target_controls_circumference_and_panel_count():
    coarse, _ = form.generate_column_pontoon(form.ColumnPontoonParameters(
        diameter=10, draft=20, panel_target_size=3))
    fine, _ = form.generate_column_pontoon(form.ColumnPontoonParameters(
        diameter=10, draft=20, panel_target_size=1.5))
    assert fine.n_panels > 2 * coarse.n_panels
    assert len(np.unique(fine.vertices[:, :2], axis=0)) > len(np.unique(coarse.vertices[:, :2], axis=0))


def test_primitive_check_detects_mesh_error(monkeypatch):
    params = form.ColumnPontoonParameters(diameter=10, draft=20)
    solid = form._primitives(params)[0][0]
    original = form._quad_mesh
    def altered(faces, lid):
        mesh = original(faces, lid)
        vertices = mesh.vertices.copy()
        vertices[:, :2] *= 1.1
        return form.PanelMesh(vertices, mesh.panels)
    monkeypatch.setattr(form, '_quad_mesh', altered)
    check = form._primitive_check(solid, params.draft)
    assert not check['passed']
    assert not check['volume']['passed']


def test_oc4_style_target_controls_count_and_quality(record_property):
    params = form.ColumnPontoonParameters(
        count=3, spacing=50, diameter=12, draft=20, center_diameter=6.5,
        heave_plate_diameter=24, heave_plate_thickness=6,
        pontoon_layout='ring', pontoon_width=1.6, pontoon_height=1.6,
        pontoon_corner_radius=.8, pontoon_center_z=-17, lid=True,
        panel_target_size=5)
    coarse, coarse_report = form.generate_column_pontoon(params)
    fine, fine_report = form.generate_column_pontoon(params.model_copy(
        update={'panel_target_size': 2.5}))
    record_property('oc4_coarse_count', coarse.n_panels)
    record_property('oc4_fine_count', fine.n_panels)
    record_property('oc4_coarse_max_edge_ratio', coarse_report.max_aspect_ratio)
    record_property('oc4_fine_max_edge_ratio', fine_report.max_aspect_ratio)
    assert fine.n_panels > coarse.n_panels
    assert max(coarse_report.max_aspect_ratio, fine_report.max_aspect_ratio) < 20
    for mesh in [coarse, fine]:
        points = mesh.vertices[mesh.panels]
        edges = np.roll(points, -1, axis=1)-points
        turns = np.cross(edges, np.roll(edges, -1, axis=1))
        assert np.all(np.sum(turns*mesh.normals[:, None, :], axis=2) > 0)


@pytest.mark.parametrize('lid', [False, True])
def test_union_paneling_runs_once_with_or_without_lid(monkeypatch, lid):
    original = form._quad_mesh
    calls = []
    def counted(faces, include_lid):
        calls.append(include_lid)
        return original(faces, include_lid)
    monkeypatch.setattr(form, '_quad_mesh', counted)
    form.generate_column_pontoon(form.ColumnPontoonParameters(
        square_side=4, draft=10, panel_target_size=4, lid=lid))
    # One union plus its one independently integrated primitive.
    assert calls == [True, True]
