"""Mesh-derived hydrostatics; tolerance criteria from issue 2318, SI units."""

from collections import Counter
from math import pi

import numpy as np
import pytest
from pydantic import ValidationError
from scipy.integrate import quad

from digitalmodel.hydrodynamics.hull_library import column_pontoon_form as form
from digitalmodel.hydrodynamics.diffraction.mesh_orientation import orientation_report


def assert_closed(mesh):
    edges = Counter()
    directed = Counter()
    for face in mesh.panels:
        assert len(set(face)) == 4
        for a, b in zip(face, np.roll(face, -1)):
            edges[tuple(sorted((a, b)))] += 1
            directed[(a, b)] += 1
    assert set(edges.values()) == {2}
    assert all(directed[(b, a)] == n for (a, b), n in directed.items())
    assert np.min(mesh.panel_areas) > 1e-10
    assert len({tuple(sorted(face)) for face in mesh.panels}) == mesh.n_panels


@pytest.fixture(scope="module")
def spar():
    return form.generate_column_pontoon(form.ColumnPontoonParameters(
        diameter=10, draft=20, panel_target_size=3,
    ))


def test_spar_hand_hydrostatics(spar):
    mesh, report = spar
    expected = pi * 5**2 * 20
    assert abs(report.displacement / expected - 1) <= 0.005
    assert abs(report.waterplane_area / (pi * 5**2) - 1) <= 0.005
    assert report.kb == pytest.approx(10, rel=0.005)
    assert report.bm == pytest.approx((5**2 / 80, 5**2 / 80), rel=0.005)
    check = orientation_report(mesh)
    assert check.volume == pytest.approx(report.displacement)
    assert report.panel_count == mesh.n_panels
    primitive = report.primitive_checks[0]
    assert primitive["analytic_kb"] == pytest.approx(10)
    assert primitive["analytic_bm"] == pytest.approx((5**2 / 80,) * 2)
    assert primitive["analytic_waterplane_area"] == pytest.approx(pi * 5**2)
    assert report.winding.outward and report.winding.submerged_boundary_edges == 0


def test_spar_column_normals(spar):
    mesh, _ = spar
    sides = np.abs(mesh.normals[:, 2]) < 0.1
    radial = mesh.panel_centers[sides, :2]
    assert np.all(np.sum(radial * mesh.normals[sides, :2], axis=1) > 0)
    assert np.max(mesh.vertices[:, 2]) == 0


@pytest.mark.parametrize("lid", [False, True])
def test_spar_closure(lid):
    mesh, report = form.generate_column_pontoon(form.ColumnPontoonParameters(
        diameter=10, draft=20, lid=lid, panel_target_size=4,
    ))
    assert report.wetted_closed
    assert report.watertight is lid
    if lid:
        assert_closed(mesh)
    else:
        assert report.winding.boundary_edges > 0
        assert report.winding.submerged_boundary_edges == 0


@pytest.fixture(scope="module")
def ring():
    # Four 4x4 columns at (+/-10,+/-10). Four 2x3 pontoons run between
    # column centres at the keel. Exposed pontoon length = 20 - 4 = 16 m.
    return form.generate_column_pontoon(form.ColumnPontoonParameters(
        count=4, spacing=20, square_side=4, draft=10,
        pontoon_layout="ring", pontoon_width=2, pontoon_height=3,
        panel_target_size=4, lid=True,
    ))


def test_ring_hand_hydrostatics(ring):
    mesh, report = ring
    volume = 4 * 4**2 * 10 + 4 * 16 * 2 * 3
    assert abs(report.displacement / volume - 1) <= 0.005
    assert abs(report.waterplane_area / 64 - 1) <= 0.005
    assert report.kb == pytest.approx((640 * 5 + 384 * 1.5) / volume)
    inertia = 4 * (4**4 / 12 + 16 * 10**2)
    assert report.bm == pytest.approx((inertia / volume,) * 2)
    assert report.column_spacing == pytest.approx(20)
    assert report.pontoon_section_area == pytest.approx(6)
    assert report.winding.outward
    assert report.winding.volume == pytest.approx(report.displacement, rel=1e-6)
    assert_closed(mesh)


def test_twin_layout_and_bracing_caveat():
    params = form.ColumnPontoonParameters(
        count=4, spacing=20, square_side=4, draft=10,
        pontoon_layout="twin", pontoon_width=2, pontoon_height=3,
        panel_target_size=5, lid=True,
    )
    mesh, report = form.generate_column_pontoon(params)
    braced, flagged = form.generate_column_pontoon(params.model_copy(update={"bracing": True}))
    assert np.array_equal(mesh.vertices, braced.vertices)
    assert np.array_equal(mesh.panels, braced.panels)
    assert any("Morison" in note and "bracing" in note for note in flagged.notes)
    assert report.displacement == pytest.approx(640 + 2 * 16 * 6)
    assert_closed(mesh)


@pytest.mark.parametrize("corner_radius", [0, 1])
def test_rounded_columns_and_pontoons(corner_radius):
    mesh, report = form.generate_column_pontoon(form.ColumnPontoonParameters(
        count=4, spacing=20, square_side=4, corner_radius=corner_radius,
        draft=10, pontoon_layout="ring", pontoon_width=3, pontoon_height=3,
        pontoon_corner_radius=corner_radius, panel_target_size=4, lid=True,
    ))
    area = 4 * (16 - (4 - pi) * corner_radius**2)
    assert abs(report.waterplane_area / area - 1) <= 0.005
    assert report.pontoon_section_area == pytest.approx(9 - (4 - pi) * corner_radius**2)
    assert_closed(mesh)


def test_hard_tank_and_heave_plate():
    mesh, report = form.generate_column_pontoon(form.ColumnPontoonParameters(
        diameter=6, draft=20, heave_plate_diameter=12, heave_plate_thickness=2,
        panel_target_size=3, lid=True,
    ))
    expected = pi * (3**2 * 18 + 6**2 * 2)
    assert abs(report.displacement / expected - 1) <= 0.005
    assert_closed(mesh)
    _, tank = form.generate_column_pontoon(form.ColumnPontoonParameters(
        diameter=6, draft=20, hard_tank_diameter=12, hard_tank_height=2,
        panel_target_size=3,
    ))
    assert tank.displacement == pytest.approx(report.displacement, rel=0.005)
    assert tank.waterplane_area == pytest.approx(pi * 6**2, rel=0.005)
    assert report.waterplane_area == pytest.approx(pi * 3**2, rel=0.005)
    assert tank.kb > report.kb


def test_oc4_published_geometry(record_property):
    # Robertson et al. (2014), NREL/TP-5000-60601, Tables 3-1, 3-2, 4-6.
    # https://www.nlr.gov/docs/fy14osti/60601.pdf
    # Published volume 13917 m3 includes cross braces. Only the six wetted
    # horizontal lower pontoons are included here; upper members are dry.
    # B=H=1.6, r=0.8 is the circular limit of a rounded-box sweep.
    mesh, report = form.generate_column_pontoon(form.ColumnPontoonParameters(
        count=3, spacing=50, diameter=12, draft=20, center_diameter=6.5,
        heave_plate_diameter=24, heave_plate_thickness=6,
        pontoon_layout="ring", pontoon_width=1.6, pontoon_height=1.6,
        pontoon_corner_radius=0.8, pontoon_center_z=-17,
        panel_target_size=5, bracing=True, lid=True,
        comparator_class="published-geometry",
    ))
    assert abs(report.displacement / 13917 - 1) <= 0.01
    r = 0.8
    def overlap(radius):
        return quad(lambda y: 2 * np.sqrt(r*r-y*y) * np.sqrt(radius*radius-y*y),
                    -r, r, epsabs=1e-8)[0]
    columns = pi * (3 * (6**2 * 14 + 12**2 * 6) + 3.25**2 * 20)
    ring = 3 * (pi * r*r * 50 - 2 * overlap(12))
    radial = 3 * (pi * r*r * 50 / np.sqrt(3) - overlap(12) - overlap(3.25))
    modeled = columns + ring + radial
    # An independent circular-section integral catches union errors separately
    # from the published comparator's allowance for omitted cross braces.
    assert abs(report.displacement / modeled - 1) <= 0.0005
    record_property("oc4_mesh_volume_m3", report.displacement)
    record_property("oc4_max_aspect_ratio", report.max_aspect_ratio)
    record_property("oc4_panel_count", mesh.n_panels)
    assert report.max_aspect_ratio < 20
    record_property("oc4_unbraced_analytic_volume_m3", modeled)
    record_property("oc4_published_volume_m3", 13917)
    assert report.winding.volume == pytest.approx(report.displacement, rel=1e-6)
    assert abs(report.waterplane_area / (pi * (3 * 6**2 + 3.25**2)) - 1) <= 0.005
    assert all(check["passed"] for check in report.primitive_checks)
    assert report.comparator_class == "published-geometry"
    assert_closed(mesh)


def test_screening_whole_and_primitives(monkeypatch):
    calls = []
    monkeypatch.setattr(form, "hullprod_available", lambda: True)
    def screen(mesh, **kwargs):
        calls.append((mesh, kwargs))
        return "signature"
    monkeypatch.setattr(form, "screen_panel_mesh", screen)
    _, report = form.generate_column_pontoon(form.ColumnPontoonParameters(
        count=4, spacing=20, square_side=4, draft=10,
        pontoon_layout="twin", pontoon_width=2, pontoon_height=3,
        panel_target_size=5,
    ), screen=True)
    assert len(calls) == 7  # Whole mesh, four columns, two pontoons.
    assert all(call[1]["hull_type"] == form.HullType.SEMI_PONTOON for call in calls)
    assert all(call[1]["lref"] > 0 for call in calls)
    assert len(report.primitive_checks) == 6
    assert any("crease" in note for note in report.notes)


def test_unavailable_screening_is_explicit(monkeypatch):
    monkeypatch.setattr(form, "hullprod_available", lambda: False)
    _, report = form.generate_column_pontoon(form.ColumnPontoonParameters(
        diameter=6, draft=10, panel_target_size=4,
    ), screen=True)
    assert report.screening is None
    assert any("unavailable" in note for note in report.notes)


@pytest.mark.parametrize("changes", [
    {"draft": float("nan")}, {"panel_target_size": 0}, {"diameter": -1},
    {"diameter": None}, {"square_side": 4}, {"count": 0},
    {"count": 4}, {"corner_radius": 10},
    {"heave_plate_diameter": 12}, {"pontoon_layout": "ring"},
    {"count": 4, "spacing": 1}, {"spacing": 20, "layout_radius": 10},
    {"count": 4, "spacing": 20, "pontoon_layout": "ring", "pontoon_width": 2,
     "pontoon_height": 3, "pontoon_center_z": 0},
])
def test_invalid_geometry(changes):
    with pytest.raises(ValidationError):
        form.ColumnPontoonParameters(**({"draft": 10, "diameter": 6} | changes))


def test_refinement_increases_density():
    params = form.ColumnPontoonParameters(diameter=6, draft=20, panel_target_size=5)
    coarse, _ = form.generate_column_pontoon(params)
    fine, _ = form.generate_column_pontoon(params.model_copy(update={"panel_target_size": 2}))
    assert fine.n_panels > coarse.n_panels
    sides = np.abs(fine.normals[:, 2]) < 0.1
    side_vertices = fine.vertices[fine.panels[sides]]
    assert np.max(np.ptp(side_vertices[:, :, 2], axis=1)) <= 2 + 1e-8


def test_disconnected_columns_have_independent_outward_winding():
    mesh, report = form.generate_column_pontoon(form.ColumnPontoonParameters(
        count=4, spacing=20, square_side=4, draft=10,
        panel_target_size=1.5, lid=True,
    ))
    assert_closed(mesh)
    assert report.winding.components == 4
    assert report.winding.outward
    assert report.displacement == pytest.approx(640)
    assert report.comparator_class == "analytic-control"


def test_component_orientation_cannot_hide_an_inward_body():
    mesh, _ = form.generate_column_pontoon(form.ColumnPontoonParameters(
        count=4, spacing=20, square_side=4, draft=10, panel_target_size=5,
    ))
    panels = mesh.panels.copy()
    inverted = mesh.panel_centers[:, 0] > 0
    panels[inverted] = panels[inverted, ::-1]
    broken = form.PanelMesh(mesh.vertices, panels)
    fixed = form._orient_wetted(broken)
    assert orientation_report(fixed).outward


def test_closure_status_checks_returned_lid(monkeypatch):
    original = form._quad_mesh
    def missing_lid_panel(faces, lid):
        mesh = original(faces, lid)
        if not lid:
            return mesh
        top = np.max(np.abs(mesh.vertices[mesh.panels, 2]), axis=1) < 1e-8
        removed = np.flatnonzero(top)[0]
        return form.PanelMesh(mesh.vertices, np.delete(mesh.panels, removed, axis=0))
    monkeypatch.setattr(form, "_quad_mesh", missing_lid_panel)
    _, report = form.generate_column_pontoon(form.ColumnPontoonParameters(
        square_side=4, draft=10, lid=True, panel_target_size=5,
    ))
    assert report.wetted_closed
    assert not report.watertight
