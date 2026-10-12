"""W0 (#2239) tests: triangle-mesh geometry contract, waterline clipping, hydrostatics.

Rows follow the plan's TDD table (docs/plans/2026-09-27-issue-2239-analytical-resistance-
methods.md). Geometry is analytic only: boxes, a tapered hexahedron and the Wigley hull
y = (B/2)(1-(2x/L)^2)(1-(z/T)^2), L/B = 10, L/T = 16.
"""
from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.naval_architecture.mesh_hydrostatics import (
    MeshContractError,
    TriMesh,
    box_mesh,
    clip_at_waterline,
    compute_hydrostatics,
    volume_divergence,
    volume_tetra,
    wigley_mesh,
    wigley_wetted_area_reference,
)

L_W, B_W, T_W = 100.0, 10.0, 6.25  # Wigley L/B = 10, L/T = 16
V_W = 4.0 * B_W * L_W * T_W / 9.0

# Box
L_B, B_B, D_B, T_B = 20.0, 4.0, 3.0, 1.5


def _box(units="m", scale=1.0):
    v, f = box_mesh(L_B, B_B, D_B)
    return TriMesh(v * scale, f, units=units, axes=("forward", "port", "up"))


def _hexa(corners):
    """Closed, outward-oriented hexahedron from 8 corners in box_mesh vertex order."""
    _, f = box_mesh(1.0, 1.0, 1.0)
    return np.asarray(corners, dtype=float), f


def _tapered_hull():
    """Asymmetric (fore/aft) hexahedron: 20 m long, 3 m beam aft, 5 m beam forward, 3 m deep."""
    v, _ = box_mesh(L_B, B_B, D_B)
    v = v.copy()
    fwd = v[:, 0] > 0
    v[fwd, 1] *= 5.0 / 4.0
    v[~fwd, 1] *= 3.0 / 4.0
    return _hexa(v)


# ------------------------------------------------------------ box, exact


def test_box_hydrostatics_exact():
    r = compute_hydrostatics(_box(), draft=T_B)
    assert r["V"].value == pytest.approx(L_B * B_B * T_B, rel=1e-9)
    s_exact = L_B * B_B + 2 * L_B * T_B + 2 * B_B * T_B  # waterplane cap excluded
    assert r["S"].value == pytest.approx(s_exact, rel=1e-9)
    assert r["L_wl"].value == pytest.approx(L_B, rel=1e-9)
    assert r["B_wl"].value == pytest.approx(B_B, rel=1e-9)
    for name in ("C_B", "C_P", "C_M", "C_WP"):
        assert r[name].value == pytest.approx(1.0, rel=1e-9)
    assert r["LCB"].value == pytest.approx(0.0, abs=1e-9)
    assert r["S"].provenance == "computed"
    assert r["draft_mean"].provenance == "declared"


def test_cap_faces_excluded_from_wetted_area():
    for beam in (2.0, 4.0, 8.0):  # cap (waterplane) area L*beam changes with the beam
        v, f = box_mesh(L_B, beam, D_B)
        mesh = TriMesh(v, f, units="m", axes=("forward", "port", "up"))
        clipped = clip_at_waterline(mesh, draft=T_B)
        cap_area = clipped.face_areas()[clipped.artificial].sum()
        phys_area = clipped.face_areas()[~clipped.artificial].sum()
        assert cap_area == pytest.approx(L_B * beam, rel=1e-12)
        s = compute_hydrostatics(mesh, draft=T_B)["S"].value
        assert s == pytest.approx(phys_area, rel=1e-12)
        assert s == pytest.approx(L_B * beam + 2 * L_B * T_B + 2 * beam * T_B, rel=1e-9)
        assert abs(s - (phys_area + cap_area)) > 0.5 * cap_area


# ------------------------------------------------------------ verification


def test_integration_two_ways_agree():
    mesh = TriMesh(*wigley_mesh(L_W, B_W, T_W, nx=60, nz=16), units="m",
                   axes=("forward", "port", "up"))
    clipped = clip_at_waterline(mesh, draft=0.8 * T_W, trim=-0.4)
    for verts, faces in ((mesh.vertices, mesh.faces), (clipped.vertices, clipped.faces)):
        v1 = volume_divergence(verts, faces)
        v2 = volume_tetra(verts, faces)
        assert v1 > 0
        assert v1 == pytest.approx(v2, rel=1e-9)
        # tetra sum is origin independent for a closed surface
        assert volume_tetra(verts, faces, origin=np.array([37.0, -5.0, 11.0])) == pytest.approx(v1, rel=1e-9)


# ------------------------------------------------------------ Wigley accuracy


def test_wigley_generator_meshes_are_independent():
    a = wigley_mesh(L_W, B_W, T_W, nx=40, nz=12, spacing="uniform", diagonal="ac")
    b = wigley_mesh(L_W, B_W, T_W, nx=61, nz=17, spacing="cosine", diagonal="bd")
    c = wigley_mesh(L_W, B_W, T_W, nx=97, nz=29, spacing="uniform", diagonal="alternate")
    xs = [set(np.round(m[0][:, 0], 9)) for m in (a, b, c)]
    # no mesh's station set is a superset of another's (not a subdivision of one mesh)
    for i in range(3):
        for j in range(3):
            if i != j:
                assert not xs[i] <= xs[j]


def test_wigley_independent_tessellations():
    s_ref = wigley_wetted_area_reference(L_W, B_W, T_W)
    specs = [
        dict(nx=40, nz=12, spacing="uniform", diagonal="ac"),
        dict(nx=61, nz=17, spacing="cosine", diagonal="bd"),
        dict(nx=97, nz=29, spacing="uniform", diagonal="alternate"),
    ]
    results = []
    for spec in specs:
        mesh = TriMesh(*wigley_mesh(L_W, B_W, T_W, **spec), units="m",
                       axes=("forward", "port", "up"))
        results.append(compute_hydrostatics(mesh, draft=T_W))
    finest = results[-1]
    assert finest["V"].value == pytest.approx(V_W, rel=2e-3)
    assert finest["S"].value == pytest.approx(s_ref, rel=2e-3)
    assert finest["C_B"].value == pytest.approx(4.0 / 9.0, rel=2e-3)
    assert finest["L_wl"].value == pytest.approx(L_W, abs=1e-3 * L_W)
    assert finest["B_wl"].value == pytest.approx(B_W, abs=1e-3 * L_W)
    lcb_m = finest["LCB"].value / 100.0 * finest["L_wl"].value
    assert lcb_m == pytest.approx(0.0, abs=1e-3 * L_W)
    # every independent tessellation is already within a loose band of the references
    for r in results:
        assert r["V"].value == pytest.approx(V_W, rel=2e-2)
        assert r["S"].value == pytest.approx(s_ref, rel=2e-2)


def test_wigley_wetted_area_reference_is_converged():
    s1 = wigley_wetted_area_reference(L_W, B_W, T_W)
    # 1-D Gauss-Legendre cross-check of the same analytic integral
    x, wx = np.polynomial.legendre.leggauss(400)
    z, wz = np.polynomial.legendre.leggauss(200)
    X = x[:, None] * L_W / 2
    Z = (z[None, :] + 1) * T_W / 2  # baseline-referenced
    xi = 2 * X / L_W
    zeta = (Z - T_W) / T_W
    dydx = (B_W / 2) * (-8 * X / L_W**2) * (1 - zeta**2)
    dydz = (B_W / 2) * (1 - xi**2) * (-2 * zeta / T_W)
    integrand = np.sqrt(1 + dydx**2 + dydz**2)
    s2 = 2 * (wx[:, None] * wz[None, :] * integrand).sum() * (L_W / 2) * (T_W / 2)
    assert s1 == pytest.approx(s2, rel=1e-9)


# ------------------------------------------------------------ conventions


def test_trim_sign_moves_lcb():
    mesh = _box()
    level = compute_hydrostatics(mesh, draft=T_B)
    stern = compute_hydrostatics(mesh, draft=T_B, trim=-1.0)  # T_fwd - T_aft < 0
    bow = compute_hydrostatics(mesh, draft=T_B, trim=+1.0)
    assert stern["LCB"].value < 0.0 < bow["LCB"].value
    # box: submerged trapezoidal prism; hull-frame centroid xc = t L/(12 T),
    # zc = T/2 + t^2/(24 T); LCB is measured along the inclined waterplane from the
    # midship waterline point: LCB = cos(th) xc + sin(th) (zc - T), tan(th) = t / L.
    t = 1.0
    th = np.arctan(t / L_B)
    xc, zc = t * L_B / (12 * T_B), T_B / 2 + t**2 / (24 * T_B)
    lcb_exact = np.cos(th) * xc + np.sin(th) * (zc - T_B)
    assert bow["L_wl"].value == pytest.approx(L_B / np.cos(th), rel=1e-12)
    assert bow["LCB"].value / 100 * bow["L_wl"].value == pytest.approx(lcb_exact, rel=1e-9)
    # mean draft preserved: rotation about the midship waterline point keeps V for a box
    for r in (stern, bow):
        assert r["V"].value == pytest.approx(level["V"].value, rel=1e-9)
        assert r["draft_mean"].value == T_B
    assert bow["draft_fwd"].value == pytest.approx(T_B + 0.5)
    assert bow["draft_aft"].value == pytest.approx(T_B - 0.5)
    clipped = clip_at_waterline(mesh, draft=T_B, trim=-1.0)
    wl = clipped.waterline_points
    fwd_z = wl[np.isclose(wl[:, 0], L_B / 2), 2]
    aft_z = wl[np.isclose(wl[:, 0], -L_B / 2), 2]
    assert fwd_z.mean() == pytest.approx(T_B - 0.5, abs=1e-12)
    assert aft_z.mean() == pytest.approx(T_B + 0.5, abs=1e-12)
    assert 0.5 * (fwd_z.mean() + aft_z.mean()) == pytest.approx(T_B, abs=1e-12)


def _numeric_outputs(r):
    return {k: q.value for k, q in r.items() if isinstance(q.value, float)}


def test_units_mm_equals_m():
    v, f = wigley_mesh(L_W, B_W, T_W, nx=40, nz=12)
    r_m = compute_hydrostatics(TriMesh(v, f, units="m", axes=("forward", "port", "up")),
                               draft=0.9 * T_W, trim=-0.3)
    r_mm = compute_hydrostatics(TriMesh(v * 1000.0, f, units="mm", axes=("forward", "port", "up")),
                                draft=0.9 * T_W, trim=-0.3)
    a, b = _numeric_outputs(r_m), _numeric_outputs(r_mm)
    assert a.keys() == b.keys() and len(a) >= 10
    for k in a:
        assert b[k] == pytest.approx(a[k], rel=1e-12, abs=1e-12), k
    with pytest.raises(MeshContractError, match="units"):
        TriMesh(v, f, units=None, axes=("forward", "port", "up"))
    with pytest.raises(MeshContractError, match="units"):
        TriMesh(v, f, units="ft", axes=("forward", "port", "up"))


@pytest.mark.parametrize(
    "axes",
    [("aft", "starboard", "up"), ("port", "aft", "up"), ("forward", "starboard", "down"),
     ("up", "forward", "port")],
)
def test_axis_transform_invariance(axes):
    v, f = _tapered_hull()
    ref = compute_hydrostatics(TriMesh(v, f, units="m", axes=("forward", "port", "up")),
                               draft=T_B, trim=-0.6)
    unit = {"forward": (1, 0, 0), "aft": (-1, 0, 0), "port": (0, 1, 0),
            "starboard": (0, -1, 0), "up": (0, 0, 1), "down": (0, 0, -1)}
    r_mat = np.column_stack([unit[a] for a in axes]).astype(float)  # source -> canonical
    v_src = v @ r_mat  # canonical = v_src @ r_mat.T  <=>  v_src = v @ r_mat (orthonormal)
    other = compute_hydrostatics(TriMesh(v_src, f, units="m", axes=axes), draft=T_B, trim=-0.6)
    a, b = _numeric_outputs(ref), _numeric_outputs(other)
    assert a.keys() == b.keys()
    for k in a:
        assert b[k] == pytest.approx(a[k], rel=1e-12, abs=1e-12), k
    assert ref["LCB"].value != pytest.approx(0.0, abs=1e-3)  # asymmetric hull, sign matters


def test_axis_convention_must_be_declared_and_right_handed():
    v, f = box_mesh(L_B, B_B, D_B)
    with pytest.raises(MeshContractError, match="axes"):
        TriMesh(v, f, units="m", axes=None)
    with pytest.raises(MeshContractError, match="right-handed"):
        TriMesh(v, f, units="m", axes=("forward", "starboard", "up"))
    with pytest.raises(MeshContractError, match="axes"):
        TriMesh(v, f, units="m", axes=("forward", "forward", "up"))


def test_baseline_must_be_at_z_zero():
    v, f = box_mesh(L_B, B_B, D_B)
    with pytest.raises(MeshContractError, match="baseline"):
        TriMesh(v + np.array([0.0, 0.0, -0.2]), f, units="m", axes=("forward", "port", "up"))


# ------------------------------------------------------------ mesh checks


def test_reversed_normals_refused():
    v, f = box_mesh(L_B, B_B, D_B)
    with pytest.raises(MeshContractError, match="inward|normals"):
        TriMesh(v, f[:, ::-1], units="m", axes=("forward", "port", "up"))
    flipped = TriMesh(v, f[:, ::-1], units="m", axes=("forward", "port", "up"), flip_normals=True)
    assert compute_hydrostatics(flipped, draft=T_B)["V"].value == pytest.approx(L_B * B_B * T_B)


def test_inconsistent_orientation_refused():
    v, f = box_mesh(L_B, B_B, D_B)
    f = f.copy()
    f[0] = f[0][::-1]
    with pytest.raises(MeshContractError, match="orientation"):
        TriMesh(v, f, units="m", axes=("forward", "port", "up"))


def test_open_or_nonmanifold_refused():
    v, f = box_mesh(L_B, B_B, D_B)
    with pytest.raises(MeshContractError, match="open|watertight"):
        TriMesh(v, f[1:], units="m", axes=("forward", "port", "up"))
    # two boxes sharing one edge: that edge is used by four faces
    v2 = v.copy()
    v2[:, 0] += L_B
    v2[:, 1] += B_B
    idx = np.arange(len(v)) + len(v)
    shared_src = np.where(np.isclose(v2[:, 0], L_B / 2) & np.isclose(v2[:, 1], B_B / 2))[0]
    for s in shared_src:  # merge vertex s of box 2 onto the coincident vertex of box 1
        target = np.where(np.all(np.isclose(v, v2[s]), axis=1))[0]
        if len(target):
            idx[s] = target[0]
    faces2 = idx[f]
    both = np.vstack([f, faces2])
    with pytest.raises(MeshContractError, match="non-manifold"):
        TriMesh(np.vstack([v, v2]), both, units="m", axes=("forward", "port", "up"))


def test_degenerate_and_nonfinite_refused():
    v, f = box_mesh(L_B, B_B, D_B)
    bad = np.vstack([f, [[0, 0, 1]]])
    with pytest.raises(MeshContractError, match="degenerate"):
        TriMesh(v, bad, units="m", axes=("forward", "port", "up"))
    # collinear, zero-area face
    v3 = np.vstack([v, [[0.0, 0.0, 0.0]], [[1.0, 0.0, 0.0]], [[2.0, 0.0, 0.0]]])
    n = len(v)
    bad2 = np.vstack([f, [[n, n + 1, n + 2]]])
    with pytest.raises(MeshContractError, match="degenerate"):
        TriMesh(v3, bad2, units="m", axes=("forward", "port", "up"))
    vn = v.copy()
    vn[3, 1] = np.nan
    with pytest.raises(MeshContractError, match="non-finite"):
        TriMesh(vn, f, units="m", axes=("forward", "port", "up"))
    with pytest.raises(MeshContractError, match="index"):
        TriMesh(v, np.vstack([f, [[0, 1, 99]]]), units="m", axes=("forward", "port", "up"))


def test_dry_hull_refused():
    mesh = _box()
    for draft in (0.0, -0.5):
        with pytest.raises(MeshContractError, match="dry|keel"):
            compute_hydrostatics(mesh, draft=draft)
    with pytest.raises(MeshContractError, match="submerged"):
        compute_hydrostatics(mesh, draft=D_B + 1.0)


# ------------------------------------------------------------ features and schema


def test_bulb_transom_need_declared_stations():
    mesh = _box()
    r = compute_hydrostatics(mesh, draft=T_B)
    for name in ("A_BT", "A_T"):
        assert r[name].value is None
        assert r[name].provenance == "not_computed"
        assert "station" in r[name].reason
    for name in ("V", "S", "L_wl", "B_wl", "C_B", "C_P", "C_M", "C_WP", "LCB"):
        assert r[name].value is not None
    r2 = compute_hydrostatics(mesh, draft=T_B, bulb_station=L_B / 2 - 0.25,
                              transom_station=-L_B / 2 + 0.25)
    assert r2["A_BT"].value == pytest.approx(B_B * T_B, abs=1e-3 * B_B * T_B)
    assert r2["A_T"].value == pytest.approx(B_B * T_B, abs=1e-3 * B_B * T_B)
    with pytest.raises(MeshContractError, match="station"):
        compute_hydrostatics(mesh, draft=T_B, bulb_station=L_B)


def test_half_breadth_grid_on_declared_stations():
    mesh = TriMesh(*wigley_mesh(L_W, B_W, T_W, nx=97, nz=29), units="m",
                   axes=("forward", "port", "up"))
    r = compute_hydrostatics(mesh, draft=T_W)
    assert r["half_breadth"].value is None and "station" in r["half_breadth"].reason
    xs = np.array([-40.0, -10.0, 0.0, 25.0])
    zs = np.array([1.0, 3.0, T_W])
    r = compute_hydrostatics(mesh, draft=T_W, grid_stations=xs, grid_waterlines=zs)
    grid = np.asarray(r["half_breadth"].value)
    assert grid.shape == (4, 3)
    exact = (B_W / 2) * (1 - (2 * xs[:, None] / L_W) ** 2) * (1 - ((zs[None, :] - T_W) / T_W) ** 2)
    assert np.allclose(grid, exact, atol=1e-3 * B_W)


def test_outputs_tagged_with_input_hash():
    mesh = _box()
    r1 = compute_hydrostatics(mesh, draft=T_B)
    r2 = compute_hydrostatics(mesh, draft=T_B + 0.1)
    hashes = {q.input_hash for q in r1.values()}
    assert len(hashes) == 1 and len(next(iter(hashes))) == 64
    assert r1["V"].input_hash != r2["V"].input_hash
    assert all(q.provenance in ("computed", "declared", "not_computed") for q in r1.values())
    d = r1.to_dict()
    assert d["schema"].startswith("mesh_hydrostatics/")
    assert d["quantities"]["V"]["provenance"] == "computed"


# ============================================================ review r1 regressions
# Codex code-stage review of PR #2254 (r1): findings 1-4, 6, 7, 9.

AXES = ("forward", "port", "up")


def _voxel_hull(cells, depth, dx=1.0, dy=1.0):
    """Closed single-layer prism over unit (dx x dy) waterplane cells (i, j): x in
    [i dx, (i+1) dx], y in [j dy, (j+1) dy], z in [0, depth]. Shared grid vertices make it
    watertight; each quad is oriented outward by construction."""
    cells = set(cells)
    index, pts, tris = {}, [], []

    def vid(i, j, k):
        if (i, j, k) not in index:
            index[(i, j, k)] = len(pts)
            pts.append((i * dx, j * dy, k * depth))
        return index[(i, j, k)]

    def quad(corners, outward):
        ids = [vid(*c) for c in corners]
        p = np.array([pts[i] for i in ids])
        n = np.cross(p[1] - p[0], p[2] - p[0])
        if n @ np.asarray(outward, float) < 0:
            ids = ids[::-1]
        tris.append((ids[0], ids[1], ids[2]))
        tris.append((ids[0], ids[2], ids[3]))

    for (i, j) in cells:
        quad([(i, j, 0), (i + 1, j, 0), (i + 1, j + 1, 0), (i, j + 1, 0)], (0, 0, -1))
        quad([(i, j, 1), (i + 1, j, 1), (i + 1, j + 1, 1), (i, j + 1, 1)], (0, 0, 1))
        if (i - 1, j) not in cells:
            quad([(i, j, 0), (i, j + 1, 0), (i, j + 1, 1), (i, j, 1)], (-1, 0, 0))
        if (i + 1, j) not in cells:
            quad([(i + 1, j, 0), (i + 1, j + 1, 0), (i + 1, j + 1, 1), (i + 1, j, 1)], (1, 0, 0))
        if (i, j - 1) not in cells:
            quad([(i, j, 0), (i + 1, j, 0), (i + 1, j, 1), (i, j, 1)], (0, -1, 0))
        if (i, j + 1) not in cells:
            quad([(i, j + 1, 0), (i + 1, j + 1, 0), (i + 1, j + 1, 1), (i, j + 1, 1)], (0, 1, 0))
    return np.asarray(pts, float), np.asarray(tris, np.int64)


def _cells(xs, ys):
    return {(i, j) for i in xs for j in ys}


def _two_boxes(offset, *, reverse_second=False, merge_vertex=None):
    v, f = box_mesh(L_B, B_B, D_B)
    v2 = v + np.asarray(offset, float)
    f2 = f + len(v)
    if reverse_second:
        f2 = f2[:, ::-1]
    faces = np.vstack([f, f2])
    if merge_vertex is not None:  # (index in box 1, index in box 2) that coincide
        a, b = merge_vertex
        faces = np.where(faces == b + len(v), a, faces)
    return np.vstack([v, v2]), faces


# ---- finding 1: endpoint stations


def test_endpoint_sections_box_exact():
    mesh = _box()
    r = compute_hydrostatics(mesh, draft=T_B, transom_station=-L_B / 2, bulb_station=L_B / 2)
    assert r["A_T"].value == pytest.approx(B_B * T_B, rel=1e-12)
    assert r["A_BT"].value == pytest.approx(B_B * T_B, rel=1e-12)
    trimmed = compute_hydrostatics(mesh, draft=T_B, trim=-1.0,
                                   transom_station=-L_B / 2, bulb_station=L_B / 2)
    assert trimmed["A_T"].value == pytest.approx(B_B * (T_B + 0.5), rel=1e-12)
    assert trimmed["A_BT"].value == pytest.approx(B_B * (T_B - 0.5), rel=1e-12)


def test_endpoint_sections_tapered_hull():
    v, f = _tapered_hull()  # 3 m beam at the transom, 5 m at the bow, linear taper
    mesh = TriMesh(v, f, units="m", axes=AXES)
    r = compute_hydrostatics(mesh, draft=T_B, transom_station=-L_B / 2, bulb_station=L_B / 2)
    assert r["A_T"].value == pytest.approx(3.0 * T_B, rel=1e-12)
    assert r["A_BT"].value == pytest.approx(5.0 * T_B, rel=1e-12)
    mid = compute_hydrostatics(mesh, draft=T_B, transom_station=-L_B / 4)
    assert mid["A_T"].value == pytest.approx(3.5 * T_B, rel=1e-12)


# ---- finding 2: topology


def test_vertex_only_contact_refused():
    # box 2 sits on box 1's forward-port-deck corner (index 7) with its own aft-stbd-keel
    # corner (index 0): every edge is used twice, but vertex 7 joins two separate fans.
    v, f = _two_boxes((L_B, B_B, D_B), merge_vertex=(7, 0))
    with pytest.raises(MeshContractError, match="non-manifold vertex"):
        TriMesh(v, f, units="m", axes=AXES)


def test_mixed_orientation_components_refused():
    v, f = box_mesh(L_B, B_B, D_B)
    small, fs = box_mesh(2.0, 1.0, 1.0)
    verts = np.vstack([v, small + np.array([50.0, 0.0, 0.0])])
    faces = np.vstack([f, (fs + len(v))[:, ::-1]])  # disjoint second shell, inward
    assert volume_divergence(verts, faces) > 0  # aggregate volume alone would accept it
    with pytest.raises(MeshContractError, match="inward"):
        TriMesh(verts, faces, units="m", axes=AXES)


def test_nested_inward_shell_refused():
    v, f = box_mesh(L_B, B_B, D_B)
    small, fs = box_mesh(2.0, 1.0, 1.0)
    verts = np.vstack([v, small + np.array([0.0, 0.0, 1.0])])
    faces = np.vstack([f, (fs + len(v))[:, ::-1]])  # internal cavity
    with pytest.raises(MeshContractError, match="inward"):
        TriMesh(verts, faces, units="m", axes=AXES)


def test_overlapping_shells_refused():
    v, f = _two_boxes((5.0, 1.0, 0.0))
    with pytest.raises(MeshContractError, match="overlap"):
        TriMesh(v, f, units="m", axes=AXES)
    v, f = box_mesh(L_B, B_B, D_B)
    small, fs = box_mesh(2.0, 1.0, 1.0)  # nested, both outward: double-counted volume
    with pytest.raises(MeshContractError, match="overlap"):
        TriMesh(np.vstack([v, small + np.array([0.0, 0.0, 1.0])]),
                np.vstack([f, fs + len(v)]), units="m", axes=AXES)


def test_disjoint_outward_components_accepted():
    v, f = _voxel_hull(_cells(range(0, 4), (-1, 0)) | _cells(range(6, 10), (-1, 0)), depth=2.0)
    r = compute_hydrostatics(TriMesh(v, f, units="m", axes=AXES), draft=1.0)
    assert r["V"].value == pytest.approx(2 * 4 * 2 * 1.0, rel=1e-12)


# ---- finding 3: physical-only section boundaries


def test_concave_waterplane_half_breadth_ignores_cap():
    # L-shaped waterplane: narrow aft part y in [-1, 1], wide forward part y in [-1, 7]
    cells = _cells(range(0, 4), (-1, 0)) | _cells(range(4, 8), range(-1, 7))
    v, f = _voxel_hull(cells, depth=2.0)
    mesh = TriMesh(v, f, units="m", axes=AXES)
    r = compute_hydrostatics(mesh, draft=1.0, grid_stations=[1.5, 2.5], grid_waterlines=[0.5, 1.0],
                             transom_station=1.5)
    assert np.allclose(np.asarray(r["half_breadth"].value), 1.0, atol=1e-12)
    assert r["A_T"].value == pytest.approx(2.0, rel=1e-12)
    assert r["V"].value == pytest.approx((4 * 2 + 4 * 8) * 1.0, rel=1e-12)
    clipped = clip_at_waterline(mesh, draft=1.0)
    assert volume_divergence(clipped.vertices, clipped.faces) == pytest.approx(
        volume_tetra(clipped.vertices, clipped.faces), rel=1e-9)


def test_tandem_twin_station_through_gap():
    cells = _cells(range(0, 4), (-1, 0)) | _cells(range(6, 10), (-1, 0))
    v, f = _voxel_hull(cells, depth=2.0)
    mesh = TriMesh(v, f, units="m", axes=AXES)
    r = compute_hydrostatics(mesh, draft=1.0, grid_stations=[1.5, 5.0, 8.5],
                             grid_waterlines=[0.5, 1.0])
    grid = np.asarray(r["half_breadth"].value)
    assert np.allclose(grid[[0, 2]], 1.0, atol=1e-12)
    assert np.allclose(grid[1], 0.0, atol=1e-12)
    # default midship (x = 5) lies in the gap: the midship section is dry
    assert r["A_M"].value == pytest.approx(0.0, abs=1e-12)
    for name in ("C_M", "C_P"):
        assert r[name].value is None and r[name].provenance == "not_computed"
        assert "midship section" in r[name].reason
    assert r["C_B"].value is not None


def test_side_by_side_twin_half_breadth():
    cells = _cells(range(0, 8), (-4, -3)) | _cells(range(0, 8), (2, 3))
    v, f = _voxel_hull(cells, depth=2.0)
    r = compute_hydrostatics(TriMesh(v, f, units="m", axes=AXES), draft=1.0,
                             grid_stations=[3.5], grid_waterlines=[0.5, 1.0])
    assert np.allclose(np.asarray(r["half_breadth"].value), 4.0, atol=1e-12)
    assert r["B_wl"].value == pytest.approx(8.0)
    assert r["A_M"].value == pytest.approx(4.0 * 1.0, rel=1e-12)


# ---- finding 4: immutability


def test_mesh_arrays_are_read_only_and_hash_is_stable():
    v, f = box_mesh(L_B, B_B, D_B)
    mesh = TriMesh(v, f, units="m", axes=AXES)
    digest = mesh.source_digest
    with pytest.raises(ValueError):
        mesh.vertices[0, 0] = 99.0
    with pytest.raises(ValueError):
        mesh.faces[0, 0] = 1
    v[0, 0] = 99.0  # mutating the caller's array must not reach the validated mesh
    f[0] = f[0][::-1]
    assert mesh.vertices[0, 0] == -L_B / 2
    assert mesh.source_digest == digest
    assert compute_hydrostatics(mesh, draft=T_B)["V"].value == pytest.approx(L_B * B_B * T_B)


# ---- finding 6: denominators and midship


def test_out_of_range_midship_refused():
    with pytest.raises(MeshContractError, match="midship"):
        compute_hydrostatics(_box(), draft=T_B, x_midship=100.0)


def test_near_degenerate_waterplane_refused():
    # V-section prism, keel line on the baseline: at a draft of 1e-10 m the waterplane
    # beam is ~1e-10 m and the coefficients are not meaningful.
    L, B, D = 20.0, 4.0, 3.0
    v = np.array([[-L / 2, 0, 0], [L / 2, 0, 0], [-L / 2, B / 2, D], [L / 2, B / 2, D],
                  [-L / 2, -B / 2, D], [L / 2, -B / 2, D]], float)
    f = np.array([[0, 1, 3], [0, 3, 2], [0, 4, 5], [0, 5, 1], [2, 3, 5], [2, 5, 4],
                  [0, 2, 4], [1, 5, 3]], np.int64)
    if volume_divergence(v, f) < 0:
        f = f[:, ::-1].copy()
    mesh = TriMesh(v, f, units="m", axes=AXES)
    assert compute_hydrostatics(mesh, draft=1.0)["C_B"].value == pytest.approx(0.5, rel=1e-12)
    with pytest.raises(MeshContractError, match="degenerate"):
        compute_hydrostatics(mesh, draft=1e-10)


# ---- finding 7: cuts through and near vertices


@pytest.mark.parametrize("delta", [0.0, 1e-13, -1e-13, 1e-9, -1e-9, 1e-6, -1e-6])
def test_cut_through_and_near_vertices(delta):
    # aft draft T + 1 = D: the trimmed waterline runs through the aft deck edge
    mesh = _box()
    trim = -2.0 * (D_B - T_B) + delta
    clipped = clip_at_waterline(mesh, draft=T_B, trim=trim)
    v1 = volume_divergence(clipped.vertices, clipped.faces)
    assert v1 == pytest.approx(volume_tetra(clipped.vertices, clipped.faces), rel=1e-9)
    assert v1 == pytest.approx(L_B * B_B * T_B, rel=1e-9)
    assert np.all(clipped.face_areas()[~clipped.artificial] > 0)


@pytest.mark.parametrize("delta", [0.0, 1e-13, -1e-13, 1e-7, -1e-7])
def test_cut_through_wigley_grid_row(delta):
    v, f = wigley_mesh(L_W, B_W, T_W, nx=40, nz=10)
    mesh = TriMesh(v, f, units="m", axes=AXES)
    draft = 0.6 * T_W + delta  # grid row j = 6 of 10
    clipped = clip_at_waterline(mesh, draft=draft)
    v1 = volume_divergence(clipped.vertices, clipped.faces)
    assert v1 == pytest.approx(volume_tetra(clipped.vertices, clipped.faces), rel=1e-9)
    base = compute_hydrostatics(mesh, draft=0.6 * T_W)["V"].value
    assert v1 == pytest.approx(base, rel=1e-6)


# ---- finding 9: large mesh smoke test


def test_large_mesh_section_grid_runtime():
    import time

    v, f = wigley_mesh(L_W, B_W, T_W, nx=400, nz=120)
    assert len(f) >= 100_000
    t0 = time.perf_counter()
    mesh = TriMesh(v, f, units="m", axes=AXES)
    r = compute_hydrostatics(mesh, draft=T_W, grid_stations=np.linspace(-45, 45, 10),
                             grid_waterlines=[1.0, 3.0, T_W])
    elapsed = time.perf_counter() - t0
    print(f"\n[large-mesh smoke] faces={len(f)} validate+hydrostatics+10 stations: {elapsed:.2f} s")
    assert elapsed < 30.0
    assert r["V"].value == pytest.approx(V_W, rel=1e-4)
