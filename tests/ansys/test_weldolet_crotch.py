"""Crotch-plane root flaw model (#2157 P0b, owner card G15).

A semicircular flaw on the bore surface in the plane y = 0 at the +x crotch,
normal to the run-pipe hoop stress; half model with y = 0 symmetry. The flaw
shape and its growth rule are fixed in the design-data register (F-06, F-07)
before any crotch solve.
"""

from __future__ import annotations

import json
import math
from functools import lru_cache
from pathlib import Path

import pytest

from digitalmodel.ansys import cint_parser
from digitalmodel.ansys import weldolet_crack as wc
from digitalmodel.ansys import weldolet_crotch as cr

REPO = Path(__file__).resolve().parents[2]
REGISTER = REPO / "examples" / "workflows" / "crack-fe-weldolet" / "design-data-register.json"


@lru_cache(maxsize=None)
def _mesh(level: int = 0, depth: float = 2.35):
    return cr.build_mesh(cr.CrotchSpec(crack_depth_mm=depth, mesh_level=level))


def _register() -> dict:
    return {d["id"]: d for d in json.loads(REGISTER.read_text("utf-8"))["design_data"]}


# --------------------------------------------------------------------------- #
# Flaw definition (fixed before the run)
# --------------------------------------------------------------------------- #
def test_flaw_shape_and_growth_rule():
    s = cr.CrotchSpec()
    geo = cr.flaw_geometry(s)
    assert geo["a_mm"] == 2.35 and geo["c_mm"] == 2.35  # semicircular, a/c = 1
    assert geo["top_tip_v_mm"] == pytest.approx(0.5)  # 0.5 mm above the fusion line
    assert geo["centre_v_mm"] == pytest.approx(0.5 - 2.35)
    assert geo["bottom_tip_v_mm"] == pytest.approx(0.5 - 4.70)
    # limit state: the lower tip reaches the run-pipe bore
    assert cr.remaining_ligament_mm(s) == pytest.approx(7.61 - 2 * 2.35, abs=1e-12)
    assert cr.LIGAMENT_EXHAUSTION_DEPTH_MM == pytest.approx(3.805, abs=1e-12)


def test_declared_crotch_depths_fit_and_deeper_are_rejected():
    assert cr.CROTCH_DEPTH_STATES_MM == (2.35, 2.8, 3.2)
    assert set(cr.CROTCH_DEPTH_STATES_MM) <= set(wc.DEPTH_STATES_MM)
    for a in cr.CROTCH_DEPTH_STATES_MM:
        assert cr.CrotchSpec(crack_depth_mm=a).validate() == []
    with pytest.raises(ValueError, match="tube"):
        cr.build_mesh(cr.CrotchSpec(crack_depth_mm=3.6))


def test_register_records_the_crotch_flaw():
    reg = _register()
    assert reg["F-06"]["value"] == 1.0  # a/c
    assert reg["F-07"]["value"] == cr.CROTCH_TOP_TIP_MM
    assert reg["F-08"]["value"] == list(cr.CROTCH_DEPTH_STATES_MM)
    assert reg["F-09"]["value"] == pytest.approx(cr.LIGAMENT_EXHAUSTION_DEPTH_MM)
    for key in ("F-06", "F-07", "F-08", "F-09", "N-04"):
        assert reg[key]["status_label"] == "ASSUMED - to be confirmed"
        assert reg[key]["note"].startswith("ASSUMED - to be confirmed: ")


# --------------------------------------------------------------------------- #
# Mesh
# --------------------------------------------------------------------------- #
_FACES = [(0, 1, 2, 3), (4, 5, 6, 7), (0, 1, 5, 4), (1, 2, 6, 5), (2, 3, 7, 6), (3, 0, 4, 7)]


def test_half_model_conforming_with_declared_free_faces():
    mesh = _mesh()
    assert min(y for _, y, _ in mesh.nodes) > -1e-9
    faces: dict[frozenset, int] = {}
    for el in mesh.elements:
        for f in _FACES:
            key = frozenset(el[i] for i in f)
            if len(key) < 3:
                continue
            faces[key] = faces.get(key, 0) + 1
    assert max(faces.values()) == 2
    bnd = mesh.boundary_nodes
    for key, count in faces.items():
        if count == 1:
            assert any(key <= nodes for nodes in bnd.values()), sorted(key)


def test_positive_jacobians():
    mesh = _mesh()
    assert min(mesh.jacobian_at_centre(el) for el in mesh.elements) > 0.0


def test_front_on_the_semicircle_in_the_symmetry_plane():
    mesh = _mesh()
    s = cr.CrotchSpec()
    geo = cr.flaw_geometry(s)
    rw = wc.derived_geometry(s.base)["hole_radius_mm"]
    ro = wc.derived_geometry(s.base)["run_outer_radius_mm"]
    assert mesh.n_front == 2 * cr.mesh_parameters(s)["front_divisions_quarter"] + 1
    assert mesh.n_front_all == 2 * mesh.n_front - 1  # open front
    phis = []
    for n in range(1, mesh.n_front_all + 1):
        x, y, z = mesh.nodes[n - 1]
        assert abs(y) < 1e-12
        r = ((x - rw) / geo["a_mm"]) ** 2 + ((z - ro - geo["centre_v_mm"]) / geo["c_mm"]) ** 2
        assert r == pytest.approx(1.0, abs=1e-7)
        if n <= mesh.n_front:
            phis.append(cint_parser.front_angle(cr.front_geometry(s), x, y, z))
    assert phis == sorted(phis)
    assert phis[0] == pytest.approx(0.0, abs=1e-9)
    assert phis[-1] == pytest.approx(180.0, abs=1e-9)
    assert 90.0 in [round(p, 9) for p in phis]


def test_crack_face_nodes_lie_inside_the_flaw():
    mesh = _mesh()
    s = cr.CrotchSpec()
    geo = cr.flaw_geometry(s)
    rw = wc.derived_geometry(s.base)["hole_radius_mm"]
    ro = wc.derived_geometry(s.base)["run_outer_radius_mm"]
    lo, hi = mesh.crack_interior
    assert hi > lo > mesh.n_front_all
    for n in range(lo, hi + 1):
        x, y, z = mesh.nodes[n - 1]
        assert abs(y) < 1e-12
        r = ((x - rw) / geo["a_mm"]) ** 2 + ((z - ro - geo["centre_v_mm"]) / geo["c_mm"]) ** 2
        assert r < 1.0


def test_refinement_halves_crack_front_element_size():
    p0, p1, p2 = (cr.mesh_parameters(cr.CrotchSpec(mesh_level=lv)) for lv in (0, 1, 2))
    assert p1["first_ring_mm"] == pytest.approx(p0["first_ring_mm"] / 2.0)
    assert p2["first_ring_mm"] == pytest.approx(p1["first_ring_mm"] / 2.0)
    assert p1["front_divisions_quarter"] == 2 * p0["front_divisions_quarter"]


# --------------------------------------------------------------------------- #
# Deck
# --------------------------------------------------------------------------- #
def test_deck_deterministic_and_contents():
    spec = cr.CrotchSpec()
    deck = cr.generate_crotch_apdl(spec)
    assert deck == cr.generate_crotch_apdl(spec)
    for token in ("ET,1,SOLID186", "CM,CRKTIP,NODE", "CINT,CTNC,CRKTIP", "CINT,NORM,0,2",
                  "CINT,SYMM,ON", "CINT,NCON,6", "CINT,TYPE,SIFS", "CINT,TYPE,JINT",
                  "D,ALL,UY,0.0", "*CFOPEN,weldolet_cint_L0,txt",
                  "*CFOPEN,weldolet_reac_L0,txt", "! crack-face pressure ON",
                  "! crotch-plane root flaw"):
        assert token in deck, token
    ri = wc.derived_geometry(spec.base)["run_inner_radius_mm"]
    assert f"AREAAPP = {math.pi * ri * ri * 0.5!r}" in deck  # half model
    assert cint_parser.find_host_tokens(deck) == []


def test_crack_face_pressure_off_and_level_decks_differ():
    on = cr.generate_crotch_apdl(cr.CrotchSpec())
    off = cr.generate_crotch_apdl(cr.CrotchSpec(crack_face_pressure=False))
    assert "! crack-face pressure OFF" in off
    assert wc.deck_sha256(on) != wc.deck_sha256(off)
    l1 = cr.generate_crotch_apdl(cr.CrotchSpec(mesh_level=1))
    assert wc.deck_sha256(on) != wc.deck_sha256(l1)


def test_deck_regenerates_from_receipt_spec():
    spec = cr.CrotchSpec()
    receipt = {"spec": cr.spec_dict(spec), "plane": "crotch"}
    assert cr.deck_sha256_for_receipt(receipt, 0) == wc.deck_sha256(
        cr.generate_crotch_apdl(spec)
    )
    assert cr.state_name(spec) == "p0b_crotch_a2p35"
