"""Weldolet root-flaw crack model generator (#2157 P0b).

The generator writes APDL text and needs no licence. Committed receipts are
validated in ``test_crack_receipts.py``. Frozen closed-form values below were
fixed before any weldolet solve.
"""

from __future__ import annotations

import json
import math
from functools import lru_cache
from pathlib import Path

import pytest

from digitalmodel.ansys import cint_parser
from digitalmodel.ansys import weldolet_crack as wc

REPO = Path(__file__).resolve().parents[2]
REGISTER = REPO / "examples" / "workflows" / "crack-fe-weldolet" / "design-data-register.json"
LABEL = "ASSUMED - to be confirmed"

# Frozen closed forms (assumed basis: NPS 6 Sch 40S, p = 4.7 MPa)
BARLOW_ID_MPA = 4.7 * 154.08 / (2.0 * 7.11)  # = 50.929... MPa, the comparator
HOOP_TOL = 0.02  # pre-stated plausibility tolerance on the far-field hoop stress
END_THRUST_N = 4.7 * math.pi * 77.04**2  # p * pi * r_i^2 = 87,635.x N
LIGAMENT_A0_MM = 6.21  # 8.56 - 2.35


def _register() -> dict:
    return json.loads(REGISTER.read_text(encoding="utf-8"))


@lru_cache(maxsize=None)
def _mesh(level: int, cracked: bool = True):
    depth = 2.35 if cracked else None
    return wc.build_mesh(wc.WeldoletSpec(crack_depth_mm=depth, mesh_level=level))


# --------------------------------------------------------------------------- #
# Design-data register
# --------------------------------------------------------------------------- #
def test_register_items_follow_the_riser_schema_and_label():
    reg = _register()
    keys = {"id", "parameter", "value", "unit", "source_class", "reference_ids",
            "note", "status_label"}
    ref_ids = {r["id"] for r in reg["references"]}
    item_ids = {d["id"] for d in reg["design_data"]}
    for item in reg["design_data"]:
        assert keys <= set(item), item["id"]
        assert item["status_label"] == LABEL, item["id"]
        assert item["note"].startswith(LABEL + ": "), item["id"]
        assert ". Confirm with " in item["note"], item["id"]
        assert item["source_class"] in reg["source_class_values"], item["id"]
        for ref in item["reference_ids"]:
            assert ref in ref_ids or ref in item_ids or ref.startswith("decision:"), ref
    counts: dict[str, int] = {}
    for item in reg["design_data"]:
        counts[item["source_class"]] = counts.get(item["source_class"], 0) + 1
    assert counts == reg["summary_counts"]


def test_register_sources_are_docs_literature_paths_and_host_free():
    reg = _register()
    for ref in reg["references"]:
        assert ref["source"].startswith("docs:literature/"), ref["id"]
        assert ref["source"].endswith("SOURCES.md")
    assert cint_parser.find_host_tokens(REGISTER.read_text(encoding="utf-8")) == []


def test_spec_defaults_match_register():
    reg = {d["id"]: d["value"] for d in _register()["design_data"]}
    spec = wc.WeldoletSpec()
    assert spec.run_od_mm == reg["G-01"]
    assert spec.run_wall_mm == reg["G-02"]
    assert spec.branch_od_mm == reg["G-03"]
    assert spec.branch_wall_mm == reg["G-04"]
    assert spec.branch_od_mm - 2.0 * spec.branch_wall_mm == pytest.approx(reg["G-05"])
    assert spec.weldolet_a_mm == reg["G-06"]
    assert spec.weldolet_b_mm == reg["G-07"]
    assert spec.weldolet_c_mm == reg["G-08"]
    assert 2.0 * wc.derived_geometry(spec)["hole_radius_mm"] == reg["G-09"]
    assert spec.root_gap_mm == reg["G-10"]
    assert spec.bevel_deg == reg["G-11"]
    assert spec.fillet_leg_mm == reg["G-12"]
    assert wc.derived_geometry(spec)["inner_taper_end_mm"] == reg["G-13"]
    assert wc.derived_geometry(spec)["groove_top_mm"] == pytest.approx(reg["G-14"])
    assert spec.youngs_modulus_mpa == reg["M-01"]
    assert spec.poisson == reg["M-02"]
    assert spec.yield_mpa == reg["M-03"]
    assert spec.pressure_mpa == reg["L-01"]
    assert spec.crack_face_pressure is True  # L-03 base case ON
    assert spec.crack_depth_mm == reg["F-01"]
    assert wc.derived_geometry(spec)["footprint_mm"] == pytest.approx(reg["F-03"])
    assert list(wc.DEPTH_STATES_MM) == reg["F-04"]
    assert spec.run_half_length_mm == reg["N-01"]
    assert spec.branch_length_mm == reg["N-02"]


# --------------------------------------------------------------------------- #
# Closed forms and derived geometry
# --------------------------------------------------------------------------- #
def test_ligament_and_limit_state():
    spec = wc.WeldoletSpec()
    assert wc.remaining_ligament_mm(spec) == pytest.approx(LIGAMENT_A0_MM, abs=1e-9)
    for a in wc.DEPTH_STATES_MM:
        s = wc.WeldoletSpec(crack_depth_mm=a)
        assert s.validate() == []
        assert 0.0 < wc.remaining_ligament_mm(s) < 8.56


def test_end_length_exceeds_shell_decay_length():
    geo = wc.derived_geometry(wc.WeldoletSpec())
    decay = 2.5 * math.sqrt(80.595 * 7.11)
    assert geo["shell_decay_length_mm"] == pytest.approx(decay, rel=1e-12)
    assert wc.WeldoletSpec().run_half_length_mm - geo["toe_radius_mm"] >= decay


def test_frozen_closed_form_comparators():
    spec = wc.WeldoletSpec()
    cf = wc.closed_forms(spec)
    assert cf["barlow_id_hoop_mpa"] == pytest.approx(BARLOW_ID_MPA, rel=1e-12)
    assert cf["barlow_id_hoop_mpa"] == pytest.approx(50.93, abs=0.005)
    assert cf["lame_hoop_mean_radius_mpa"] == pytest.approx(50.88, abs=0.01)
    assert cf["end_thrust_n"] == pytest.approx(END_THRUST_N, rel=1e-12)
    assert cf["hoop_tolerance"] == HOOP_TOL


def test_linearisation_of_a_linear_distribution_is_exact():
    s = [0.0, 1.0, 2.5, 4.0]
    sig = [10.0 + 3.0 * x for x in s]  # membrane 16, bending at s=0: -6
    m, b = wc.linearise(s, sig)
    assert m == pytest.approx(16.0, rel=1e-12)
    assert b == pytest.approx(-6.0, rel=1e-12)


def test_deterministic_trig_matches_libm():
    for k in range(-40, 41):
        x = k * 0.173
        s, c = wc._sincos(x)
        assert s == pytest.approx(math.sin(x), abs=1e-15)
        assert c == pytest.approx(math.cos(x), abs=1e-15)
    for t in (-1.0, -0.4, 0.0, 0.03, 0.5, 1.0):
        assert wc._atan(t) == pytest.approx(math.atan(t), abs=1e-15)


# --------------------------------------------------------------------------- #
# Deck text
# --------------------------------------------------------------------------- #
def test_deck_is_deterministic_and_level_dependent():
    a = wc.generate_weldolet_apdl(wc.WeldoletSpec())
    b = wc.generate_weldolet_apdl(wc.WeldoletSpec())
    assert a == b
    c = wc.generate_weldolet_apdl(wc.WeldoletSpec(mesh_level=1))
    assert wc.deck_sha256(a) != wc.deck_sha256(c)


def test_deck_contents_cracked():
    mesh = _mesh(0)
    deck = wc.generate_weldolet_apdl(wc.WeldoletSpec())
    assert "Units: length = mm, force = N, stress = MPa" in deck
    assert "ET,1,SOLID186" in deck
    # SIFS (from node 1) + JINT + SIFS (from node 2), one crack-extension
    # definition per front node, mid-sides included
    assert deck.count("CINT,CENC,") == 3 * mesh.n_front_all
    assert "*CFOPEN,weldolet_sifs_start_L0,txt" in deck
    for token in ("CINT,TYPE,SIFS", "CINT,TYPE,JINT", "CINT,NCON,6",
                  "DTYPE,K1", "DTYPE,K2", "DTYPE,K3", "DTYPE,JINT",
                  "*CFOPEN,weldolet_cint_L0,txt", "*CFOPEN,weldolet_reac_L0,txt",
                  "reaction_sum", "! crack-face pressure ON"):
        assert token in deck, token
    assert "CINT,SYMM,ON" not in deck
    assert cint_parser.find_host_tokens(deck) == []


def test_crack_face_pressure_option_and_uncracked_deck():
    off = wc.generate_weldolet_apdl(wc.WeldoletSpec(crack_face_pressure=False))
    assert "! crack-face pressure OFF" in off
    unc = wc.generate_weldolet_apdl(wc.WeldoletSpec(crack_depth_mm=None))
    assert "\nCINT," not in unc
    assert "*CFOPEN,weldolet_path_L0,txt" in unc
    assert "*CFOPEN,weldolet_hoop_L0,txt" in unc
    assert "PRSECT" in unc


@pytest.mark.parametrize(
    "kwargs, match",
    [
        ({"crack_depth_mm": 6.8}, "tube"),
        ({"crack_depth_mm": 1.0}, "tube"),
        ({"mesh_level": 3}, "mesh_level"),
        ({"run_half_length_mm": 60.0}, "half-length"),
    ],
)
def test_invalid_spec_rejected(kwargs, match):
    with pytest.raises(ValueError, match=match):
        wc.generate_weldolet_apdl(wc.WeldoletSpec(**kwargs))


# --------------------------------------------------------------------------- #
# Mesh integrity
# --------------------------------------------------------------------------- #
_FACES = [(0, 1, 2, 3), (4, 5, 6, 7), (0, 1, 5, 4), (1, 2, 6, 5), (2, 3, 7, 6), (3, 0, 4, 7)]


@pytest.mark.parametrize("cracked", [True, False])
def test_mesh_is_conforming_and_free_faces_are_declared_boundaries(cracked):
    mesh = _mesh(0, cracked)
    faces: dict[frozenset, int] = {}
    for el in mesh.elements:
        for f in _FACES:
            key = frozenset(el[i] for i in f)
            if len(key) < 3:
                continue  # collapsed edge of a crack-tip wedge
            faces[key] = faces.get(key, 0) + 1
    assert max(faces.values()) == 2
    boundary = mesh.boundary_nodes
    for key, count in faces.items():
        if count == 2:
            continue
        assert any(key <= nodes for nodes in boundary.values()), (
            f"free face off the declared boundaries: {sorted(key)}"
        )
    assert ("crack_lower" in boundary) is cracked


@pytest.mark.parametrize("cracked", [True, False])
def test_all_elements_have_positive_jacobian(cracked):
    mesh = _mesh(0, cracked)
    assert min(mesh.jacobian_at_centre(el) for el in mesh.elements) > 0.0


def test_front_is_a_closed_circle_on_the_fusion_face():
    mesh = _mesh(0)
    spec = wc.WeldoletSpec()
    geo = wc.derived_geometry(spec)
    rho_f = geo["hole_radius_mm"] + spec.crack_depth_mm
    ro = spec.run_od_mm / 2.0
    assert mesh.n_front == wc.mesh_parameters(spec)["front_divisions"]
    assert mesh.n_front_all == 2 * mesh.n_front  # closed: as many mid-sides as corners
    angles = []
    for n in range(1, mesh.n_front_all + 1):
        x, y, z = mesh.nodes[n - 1]
        assert math.hypot(x, y) == pytest.approx(rho_f, abs=2e-8)
        assert math.hypot(y, z) == pytest.approx(ro, abs=2e-8)
        if n <= mesh.n_front:
            angles.append(cint_parser.front_angle({"type": "polar_z"}, x, y))
    assert angles == sorted(angles)
    assert angles[0] == 0.0


def test_crack_faces_are_duplicated_only_when_cracked():
    cracked, uncracked = _mesh(0, True), _mesh(0, False)
    low, up = cracked.boundary_nodes["crack_lower"], cracked.boundary_nodes["crack_upper"]
    assert low and up
    assert low & up == set(range(1, cracked.n_front_all + 1))  # only the front
    pos_low = {cracked.keys[n - 1][:3] for n in low}
    pos_up = {cracked.keys[n - 1][:3] for n in up}
    assert pos_low == pos_up
    assert len(uncracked.nodes) < len(cracked.nodes)


def test_front_sequence_walks_the_closed_front():
    mesh = _mesh(0)
    seq = wc.front_sequence(mesh)
    assert seq[:3] == [1, mesh.n_front + 1, 2]
    assert seq[-1] == 2 * mesh.n_front
    assert sorted(seq) == list(range(1, mesh.n_front_all + 1))
    seq2 = wc.front_sequence(mesh, 2)
    assert seq2[0] == 2 and seq2[-2:] == [1, mesh.n_front + 1]


_SIFS_AUDIT = """# digitalmodel weldolet_crack start-node SIFS audit
# sign_factors  -1.0   1.0
# columns: node contour K1A K2A K3A K1B K2B K3B
      1.   6.  0.4111E+02  0.1198E+02 -0.7700E-01  0.4075E+02 -0.1200E+02  0.0000E+00
      3.   6.  0.3920E+02  0.1207E+02  0.4307E+00  0.3920E+02 -0.1207E+02  0.4307E+00
"""


def test_start_node_audit():
    audit = wc.start_node_audit(_SIFS_AUDIT)
    assert audit["sign_factors_k2_k3"] == [-1.0, 1.0]
    assert audit["ref_max_rel_diff"] == 0.0
    assert audit["within_tolerance"] is True
    assert audit["node1_k1_perturbation_rel"] == pytest.approx(41.11 / 40.75 - 1.0)
    bad = _SIFS_AUDIT.replace("0.3920E+02 -0.1207E+02", "0.3950E+02 -0.1207E+02")
    assert wc.start_node_audit(bad)["within_tolerance"] is False


def test_refinement_halves_crack_front_element_size():
    p0, p1, p2 = (wc.mesh_parameters(wc.WeldoletSpec(mesh_level=lv)) for lv in (0, 1, 2))
    assert p1["first_ring_mm"] == pytest.approx(p0["first_ring_mm"] / 2.0)
    assert p2["first_ring_mm"] == pytest.approx(p1["first_ring_mm"] / 2.0)
    assert p1["front_divisions"] == 2 * p0["front_divisions"]
    assert p2["front_divisions"] == 2 * p1["front_divisions"]
