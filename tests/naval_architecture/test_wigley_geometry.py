"""Wigley analytic hull: exact offsets, a closed outward-facing tessellation,
and a manifest the hull-resistance case builder accepts."""

from __future__ import annotations

import json
import struct
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.naval_architecture import wigley_geometry as wg


def read_binary_stl(path: Path) -> np.ndarray:
    data = path.read_bytes()
    n = struct.unpack("<I", data[80:84])[0]
    tris = np.frombuffer(data[84:], dtype=np.dtype([("n", "<3f"), ("v", "<9f"), ("a", "<u2")]), count=n)
    return tris["v"].reshape(n, 3, 3).astype(float)


def signed_volume(tris: np.ndarray) -> float:
    a, b, c = tris[:, 0], tris[:, 1], tris[:, 2]
    return float(np.einsum("ij,ij->i", a, np.cross(b, c)).sum() / 6.0)


def test_offsets_are_the_wigley_formula():
    h = wg.WigleyHull()
    assert wg.wigley_offsets(h, h.length / 2, h.draft) == pytest.approx(h.beam / 2)  # midships, waterline
    assert wg.wigley_offsets(h, 0.0, h.draft) == pytest.approx(0.0)  # AP
    assert wg.wigley_offsets(h, h.length / 2, 0.0) == pytest.approx(0.0)  # keel
    # quarter length, half draft: (1 - 0.25) * (1 - 0.25) * B/2
    assert wg.wigley_offsets(h, h.length / 4, h.draft / 2) == pytest.approx(0.75 * 0.75 * h.beam / 2)
    # extruded above the waterline
    assert wg.wigley_offsets(h, h.length / 2, h.height) == pytest.approx(h.beam / 2)


def test_analytic_displacement_is_eight_ninths_of_the_box():
    h = wg.WigleyHull()
    assert h.displacement == pytest.approx((8 / 9) * (h.beam / 2) * h.length * h.draft)
    # wetted surface of the classic hull is a little above 2*L*(T + B/2 * 2/3)... sanity band only
    assert 0.9 * h.length * h.draft * 2 < h.wetted_surface < 1.6 * h.length * h.draft * 2


def test_stl_is_closed_outward_and_matches_the_analytic_volume(tmp_path):
    h = wg.WigleyHull(nx=120, nz_wet=24, nz_dry=6)
    n = wg.wigley_stl(h, tmp_path / "hull.stl")
    tris = read_binary_stl(tmp_path / "hull.stl")
    assert len(tris) == n > 0
    # closed: every edge is shared by exactly two triangles
    edges = {}
    q = np.round(tris, 9)
    for t in q:
        for i in range(3):
            e = tuple(sorted((tuple(t[i]), tuple(t[(i + 1) % 3]))))
            edges[e] = edges.get(e, 0) + 1
    assert set(edges.values()) == {2}, "open or non-manifold edges"
    # outward: positive signed volume equal to displacement + extruded freeboard volume
    vol = signed_volume(tris)
    waterplane = (2 / 3) * h.beam * h.length
    expected = h.displacement + waterplane * h.freeboard
    assert vol == pytest.approx(expected, rel=2e-3)


def test_manifest_loads_in_the_case_builder(tmp_path):
    from digitalmodel.solvers.openfoam.hull_manifest import load_hull_manifest

    stl, manifest = wg.write_wigley(tmp_path)
    data = json.loads(manifest.read_text())
    m = load_hull_manifest(manifest)
    assert m.lpp_m == pytest.approx(3.014)
    assert m.draft_m == pytest.approx(0.1884)
    assert data["bbox_min_m"][2] == 0.0
    assert data["source_sha256"] == __import__("hashlib").sha256(stl.read_bytes()).hexdigest()
