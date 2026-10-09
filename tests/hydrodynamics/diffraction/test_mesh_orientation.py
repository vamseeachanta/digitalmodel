"""Panel orientation checks for surface-piercing diffraction meshes.

A panel mesh that closes at the free surface satisfies a strong invariant: the
displaced volume obtained by applying the divergence theorem along x, along y
and along z must give the same answer. The waterplane lid contributes nothing
to any of the three, because it lies in z = 0 and its normal has no horizontal
component. Any disagreement between the three therefore proves the panel
normals are not consistently outward, independently of what the body looks
like.

That invariant is what these tests are built on. It catches both faults found
in the committed fixtures, which the existing mean-normal check in
geometry_quality.check_normals cannot: comparing each normal against the mean
normal is vacuous for a closed body, where the mean is zero.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.bemrosetta.mesh import GDFHandler
from digitalmodel.hydrodynamics.diffraction.mesh_orientation import (
    OrientationReport,
    UnreliableOrientation,
    orient_outward,
    orientation_report,
    repair_gdf_text,
)

REPO = Path(__file__).resolve().parents[3]
COMMITTED_UNIT_BOX = (
    REPO / "examples" / "hydrodynamics" / "diffraction" / "unit_box_rao" / "unit_box.gdf"
)
CLEAN_UNIT_BOX = (
    REPO / "docs" / "domains" / "orcawave" / "L01_aqwa_benchmark"
    / "benchmark_results" / "aqwa" / "unit_box_clean.gdf"
)
VENDOR_CYLINDER = (
    REPO / "docs" / "domains" / "orcawave" / "L00_validation_wamit" / "2.6"
    / "Wamit v7.3 files" / "test05c.gdf"
)


# --------------------------------------------------------------- fixtures

def _box_panels(length=1.0, beam=1.0, draft=1.0):
    """Wetted surface of a box centred on x=y=0, every panel outward."""
    hx, hy, z = length / 2.0, beam / 2.0, -draft
    bottom = [(-hx, -hy, z), (-hx, hy, z), (hx, hy, z), (hx, -hy, z)]
    y_neg = [(-hx, -hy, z), (hx, -hy, z), (hx, -hy, 0.0), (-hx, -hy, 0.0)]
    x_pos = [(hx, -hy, z), (hx, hy, z), (hx, hy, 0.0), (hx, -hy, 0.0)]
    y_pos = [(hx, hy, z), (-hx, hy, z), (-hx, hy, 0.0), (hx, hy, 0.0)]
    x_neg = [(-hx, hy, z), (-hx, -hy, z), (-hx, -hy, 0.0), (-hx, hy, 0.0)]
    return np.asarray([bottom, y_neg, x_pos, y_pos, x_neg], dtype=float)


def _mesh_from_quads(quads):
    """Build a PanelMesh from explicit quads, preserving vertex order."""
    flat = quads.reshape(-1, 3)
    verts, inverse = np.unique(flat, axis=0, return_inverse=True)
    panels = inverse.reshape(-1, 4)

    from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
        MeshFormat,
        PanelMesh,
    )

    n = len(panels)
    return PanelMesh(
        vertices=verts,
        panels=panels,
        normals=np.zeros((n, 3)),
        panel_areas=np.zeros(n),
        panel_centers=np.zeros((n, 3)),
        name="synthetic",
        format_origin=MeshFormat.GDF,
    )


def _flip(quads, indices):
    out = quads.copy()
    for i in indices:
        out[i] = out[i][::-1]
    return out


# ------------------------------------------------------- invariant itself

def test_outward_box_has_three_agreeing_axis_volumes():
    mesh = _mesh_from_quads(_box_panels())
    r = orientation_report(mesh)
    assert isinstance(r, OrientationReport)
    assert r.consistent
    assert r.outward
    assert r.inverted_panels == ()
    assert r.volume == pytest.approx(1.0, rel=1e-12)
    for v in r.axis_volumes:
        assert v == pytest.approx(1.0, rel=1e-12)


def test_vendor_mesh_is_accepted():
    """Guards against a check that rejects everything."""
    mesh = GDFHandler().read(VENDOR_CYLINDER)
    r = orientation_report(mesh)
    assert r.consistent
    assert r.outward
    assert r.inverted_panels == ()


# ------------------------------------------------------- fault detection

def test_inverted_sides_are_located():
    """The fault in the committed example: bottom correct, four sides flipped."""
    mesh = _mesh_from_quads(_flip(_box_panels(), [1, 2, 3, 4]))
    r = orientation_report(mesh)
    assert not r.outward
    assert r.inverted_panels == (1, 2, 3, 4)


def test_inverted_bottom_is_located():
    """The fault in the L01 fixture: sides correct, bottom flipped."""
    mesh = _mesh_from_quads(_flip(_box_panels(), [0]))
    r = orientation_report(mesh)
    assert not r.outward
    assert r.inverted_panels == (0,)


def test_wholly_inverted_mesh_is_rejected():
    """Every panel flipped is self-consistent but points into the body."""
    mesh = _mesh_from_quads(_flip(_box_panels(), [0, 1, 2, 3, 4]))
    r = orientation_report(mesh)
    assert r.consistent, "a uniformly flipped mesh is still mutually consistent"
    assert not r.outward
    assert r.volume == pytest.approx(-1.0, rel=1e-12)
    assert r.inverted_panels == (0, 1, 2, 3, 4)


def test_report_records_the_axis_disagreement():
    mesh = _mesh_from_quads(_flip(_box_panels(), [0]))
    r = orientation_report(mesh)
    assert r.max_axis_discrepancy > 0.5
    assert not r.consistent


# ------------------------------------------------------------- the repair

def test_orient_outward_fixes_inverted_sides():
    quads = _flip(_box_panels(), [1, 2, 3, 4])
    fixed, flipped = orient_outward(_mesh_from_quads(quads))
    assert flipped == (1, 2, 3, 4)
    r = orientation_report(fixed)
    assert r.outward and r.inverted_panels == ()
    assert r.volume == pytest.approx(1.0, rel=1e-12)


def test_orient_outward_is_idempotent():
    mesh = _mesh_from_quads(_flip(_box_panels(), [0]))
    once, first = orient_outward(mesh)
    twice, second = orient_outward(once)
    assert first == (0,)
    assert second == ()
    assert orientation_report(twice).outward


def test_orient_outward_preserves_geometry():
    """Repair may reorder vertices within a panel but must not move them."""
    quads = _flip(_box_panels(), [1, 3])
    mesh = _mesh_from_quads(quads)
    fixed, _ = orient_outward(mesh)
    before = np.sort(mesh.vertices, axis=0)
    after = np.sort(fixed.vertices, axis=0)
    np.testing.assert_allclose(before, after, atol=1e-12)
    r = orientation_report(fixed)
    assert r.volume == pytest.approx(1.0, rel=1e-12)


# ------------------------------------- regression on the committed fixtures

@pytest.mark.parametrize(
    "path",
    [
        pytest.param(COMMITTED_UNIT_BOX, id="examples-unit_box"),
        pytest.param(CLEAN_UNIT_BOX, id="L01-unit_box_clean"),
    ],
)
def test_committed_fixtures_are_outward(path):
    """Both fixtures shipped with inverted panels; this pins the repair.

    Before the repair, examples/unit_box.gdf reports axis volumes
    (-1, -1, +1) and OrcaWave refuses it with error 63; unit_box_clean.gdf
    reports (+1, +1, -1) and OrcaWave silently returns a negative heave
    stiffness. See issue #898.
    """
    mesh = GDFHandler().read(path)
    r = orientation_report(mesh)
    assert r.consistent, (
        f"{path.name}: axis volumes disagree {r.axis_volumes}, "
        f"inverted panels {r.inverted_panels}"
    )
    assert r.outward, f"{path.name}: inward-facing normals, volume {r.volume}"
    assert r.volume > 0.0


# --------------------------------------- warped panels and disjoint parts

def test_warped_panel_volume_matches_its_own_triangulation():
    """A quadrilateral that is not planar must integrate as its triangles do.

    Collapsing such a panel to one centroid and one summed vector area is
    exact only when the two triangle normals are parallel. Hull meshes are
    routinely warped, so this is the case that matters in practice.
    """
    quads = _box_panels()
    # Lift one bottom corner so two panels become non-planar.
    warped = quads.copy()
    for p in range(len(warped)):
        for v in range(4):
            if np.allclose(warped[p, v], (-0.5, -0.5, -1.0)):
                warped[p, v] = (-0.5, -0.5, -0.7)
    mesh = _mesh_from_quads(warped)
    r = orientation_report(mesh)

    # Same surface, but every quad split into two triangles explicitly. The
    # divergence theorem is exact on triangles, so this is the reference.
    tris = []
    for q in warped:
        tris.append(np.asarray([q[0], q[1], q[2], q[0]]))
        tris.append(np.asarray([q[0], q[2], q[3], q[0]]))
    tri_mesh = _mesh_from_quads(np.asarray(tris))
    rt = orientation_report(tri_mesh)

    assert r.volume == pytest.approx(rt.volume, rel=1e-12)
    for a, b in zip(r.axis_volumes, rt.axis_volumes):
        assert a == pytest.approx(b, rel=1e-12)


def test_disjoint_components_are_not_reported_as_ok():
    """Two separate bodies: adjacency cannot fix their relative orientation."""
    first = _box_panels(1.0, 1.0, 1.0)
    second = _box_panels(1.0, 1.0, 1.0) + np.array([10.0, 0.0, 0.0])
    mesh = _mesh_from_quads(np.concatenate([first, second]))
    r = orientation_report(mesh)
    assert r.components == 2
    assert not r.ok, "a multi-component mesh must not pass unchecked"


def test_single_component_box_reports_one_component():
    r = orientation_report(_mesh_from_quads(_box_panels()))
    assert r.components == 1
    assert r.non_manifold_edges == 0
    assert r.above_waterline_vertices == 0
    assert r.ok


def test_geometry_above_the_waterline_is_not_reported_as_ok():
    """The axis-volume identity assumes the body closes in the plane z = 0."""
    quads = _box_panels()
    lifted = quads.copy()
    lifted[:, :, 2] += 0.25  # push the top edge above the free surface
    r = orientation_report(_mesh_from_quads(lifted))
    assert r.above_waterline_vertices > 0
    assert not r.ok


# -------------------------------------------- lids and symmetry-reduced meshes

def _lid_panel(L=1.0, B=1.0):
    hx, hy = L / 2, B / 2
    return np.asarray([[(-hx, -hy, 0.0), (hx, -hy, 0.0),
                        (hx, hy, 0.0), (-hx, hy, 0.0)]])


def test_free_surface_lid_is_detected_not_silently_absorbed():
    """A lid cancels the hull's waterline edges, zeroing the waterplane area.

    Every other indicator still looks correct: the axis volumes agree, the
    volume is right and no panel is inverted. Only an explicit check catches
    it, so the report must carry one.
    """
    with_lid = np.concatenate([_box_panels(), _lid_panel()])
    r = orientation_report(_mesh_from_quads(with_lid))
    assert r.waterline_lid_panels == 1
    assert r.volume == pytest.approx(1.0, rel=1e-12)
    assert r.waterplane_area == pytest.approx(0.0, abs=1e-12)
    assert not r.ok, "a lid must not pass as a plain wetted-hull mesh"
    assert "free surface" in r.describe()


def test_repair_refuses_a_mesh_carrying_a_lid():
    with_lid = np.concatenate([_box_panels(), _lid_panel()])
    with pytest.raises(UnreliableOrientation, match="free surface"):
        orient_outward(_mesh_from_quads(with_lid))


def test_symmetry_reduced_mesh_reports_its_scope():
    """The vendor cylinder stores a quarter body and declares both planes."""
    mesh = GDFHandler().read(VENDOR_CYLINDER)
    r = orientation_report(mesh)
    assert r.symmetry_plane is not None
    assert r.sector_fraction == 4
    assert "stored sector" in r.describe()
    # The orientation verdict remains valid: the symmetry cuts lie at x = 0
    # and y = 0, so they contribute nothing to the axis integrals.
    assert r.outward


def test_plain_mesh_reports_no_symmetry_scope():
    r = orientation_report(_mesh_from_quads(_box_panels()))
    assert r.symmetry_plane is None
    assert r.sector_fraction == 1
    assert "stored sector" not in r.describe()


# ------------------------------------- repair refuses what it cannot judge

def test_orient_outward_refuses_disjoint_components():
    first = _box_panels(1.0, 1.0, 1.0)
    second = _box_panels(1.0, 1.0, 1.0) + np.array([10.0, 0.0, 0.0])
    mesh = _mesh_from_quads(np.concatenate([first, second]))
    with pytest.raises(UnreliableOrientation, match="disconnected"):
        orient_outward(mesh)


def test_orient_outward_refuses_geometry_above_the_waterline():
    lifted = _box_panels()
    lifted[:, :, 2] += 0.25
    with pytest.raises(UnreliableOrientation, match="above z = 0"):
        orient_outward(_mesh_from_quads(lifted))


def test_orient_outward_can_be_forced():
    """The caller may override, but must say so."""
    first = _box_panels(1.0, 1.0, 1.0)
    second = _box_panels(1.0, 1.0, 1.0) + np.array([10.0, 0.0, 0.0])
    mesh = _mesh_from_quads(np.concatenate([first, second]))
    fixed, flipped = orient_outward(mesh, strict=False)
    assert fixed is not None


def test_repair_gdf_text_refuses_disjoint_components():
    first = _box_panels(1.0, 1.0, 1.0)
    second = _box_panels(1.0, 1.0, 1.0) + np.array([10.0, 0.0, 0.0])
    text = _gdf_text(np.concatenate([first, second]))
    with pytest.raises(UnreliableOrientation, match="disconnected"):
        repair_gdf_text(text)


def test_repair_gdf_text_refuses_geometry_above_the_waterline():
    lifted = _box_panels()
    lifted[:, :, 2] += 0.25
    with pytest.raises(UnreliableOrientation, match="above z = 0"):
        repair_gdf_text(_gdf_text(lifted))


# ------------------------------------------- format-preserving text repair

def _gdf_text(quads, eol="\r\n"):
    """Serialise quads as GDF text, so tests do not depend on file state."""
    lines = [
        "synthetic unit box",
        "1.0  9.80665",
        "0  0",
        str(len(quads)),
    ]
    for q in quads:
        for v in q:
            lines.append(f"{v[0]:.6f}  {v[1]:.6f}  {v[2]:.6f}")
    return eol.join(lines) + eol


@pytest.mark.parametrize(
    "bad_panels, expected",
    [
        pytest.param([1, 2, 3, 4], (1, 2, 3, 4), id="sides-inverted"),
        pytest.param([0], (0,), id="bottom-inverted"),
    ],
)
def test_repair_touches_only_vertex_ordering(bad_panels, expected):
    """The repair rewrites line order, never the numbers or the line endings."""
    original = _gdf_text(_flip(_box_panels(), bad_panels))
    repaired, flipped = repair_gdf_text(original)
    assert flipped == expected

    assert original.count("\r\n") == repaired.count("\r\n")
    assert len(original) == len(repaired)
    assert sorted(original.splitlines()) == sorted(repaired.splitlines())
    # Header, ULEN/GRAV, symmetry flags and panel count are untouched.
    assert original.splitlines()[:4] == repaired.splitlines()[:4]


def test_repair_is_idempotent():
    original = _gdf_text(_flip(_box_panels(), [1, 2, 3, 4]))
    once, first = repair_gdf_text(original)
    twice, second = repair_gdf_text(once)
    assert first == (1, 2, 3, 4)
    assert second == ()
    assert once == twice


def test_repair_preserves_lf_line_endings():
    original = _gdf_text(_flip(_box_panels(), [0]), eol="\n")
    repaired, flipped = repair_gdf_text(original)
    assert flipped == (0,)
    assert "\r" not in repaired


def test_repaired_text_parses_as_outward(tmp_path):
    original = _gdf_text(_flip(_box_panels(), [0]))
    repaired, _ = repair_gdf_text(original)
    out = tmp_path / "repaired.gdf"
    out.write_bytes(repaired.encode("ascii"))
    r = orientation_report(GDFHandler().read(out))
    assert r.ok
    assert r.volume == pytest.approx(1.0, rel=1e-9)
    assert r.waterplane_area == pytest.approx(1.0, rel=1e-9)


def test_committed_unit_box_hydrostatics_are_physical():
    """Heave stiffness must be positive, which is what #898 reported failing."""
    mesh = GDFHandler().read(COMMITTED_UNIT_BOX)
    r = orientation_report(mesh)
    rho, g = 1025.0, 9.80665
    assert r.waterplane_area > 0.0
    k33 = rho * g * r.waterplane_area
    assert k33 == pytest.approx(10051.81625, rel=1e-9)
