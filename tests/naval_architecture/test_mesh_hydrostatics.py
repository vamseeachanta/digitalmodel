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
