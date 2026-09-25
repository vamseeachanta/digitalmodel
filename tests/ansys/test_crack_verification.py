"""Semi-elliptical surface-crack verification deck generator (#2157 P0a).

The generator writes APDL text and needs no licence. The committed receipt is
validated in ``test_crack_verification_receipt.py``. Only
``test_solve_verification_deck`` needs the licensed MAPDL host, and it is the
only P0a test that may skip.
"""

from __future__ import annotations

import math
import os
from functools import lru_cache
from pathlib import Path

import pytest

from digitalmodel.ansys import cint_parser
from digitalmodel.ansys.crack_verification import (
    CrackPlateSpec,
    build_mesh,
    deck_sha256,
    finite_width_effect,
    generate_crack_verification_apdl,
    mesh_parameters,
    write_crack_verification_inp,
)
from digitalmodel.asset_integrity.assessment.crack_fad import newman_raju_k


# Frozen comparator values (plan #2157, TDD list P0).
NR_DEEPEST = 7.2896  # MPa*sqrt(m), phi = 90 deg
NR_SURFACE = 5.7422  # MPa*sqrt(m), phi = 0 deg
NR_BAND = 0.05  # +/- 5 %, the stated accuracy of the Newman-Raju fit


@lru_cache(maxsize=None)
def _mesh(level: int):
    return build_mesh(CrackPlateSpec(mesh_level=level))


# --------------------------------------------------------------------------- #
# Comparator
# --------------------------------------------------------------------------- #
def test_frozen_comparator_values_reproduce():
    assert newman_raju_k(2.0, 4.0, 10.0, 100.0, phi_deg=90.0) == pytest.approx(
        NR_DEEPEST, abs=5e-5
    )
    assert newman_raju_k(2.0, 4.0, 10.0, 100.0, phi_deg=0.0) == pytest.approx(
        NR_SURFACE, abs=5e-5
    )


# --------------------------------------------------------------------------- #
# Generator: deck text
# --------------------------------------------------------------------------- #
def test_deck_is_deterministic():
    a = generate_crack_verification_apdl(CrackPlateSpec())
    b = generate_crack_verification_apdl(CrackPlateSpec())
    assert a == b
    assert deck_sha256(a) == deck_sha256(b)
    assert len(deck_sha256(a)) == 64


def test_deck_declares_units_in_header():
    deck = generate_crack_verification_apdl(CrackPlateSpec())
    header = deck.splitlines()[:12]
    assert any("Units: length = mm, force = N, stress = MPa" in ln for ln in header)
    assert "/UNITS,MPA" in deck


def test_deck_has_cint_sifs_and_j_over_six_contours():
    deck = generate_crack_verification_apdl(CrackPlateSpec())
    for token in (
        "ET,1,SOLID186",
        "NSEL,S,NODE,,1,NFRALL",  # front corners + mid-sides (CINT needs both)
        "CM,CRKTIP,NODE",
        "CINT,TYPE,SIFS",
        "CINT,TYPE,JINT",
        "CINT,CTNC,CRKTIP",
        "CINT,NORM,0,3",
        "CINT,SYMM,ON",
        "CINT,NCON,6",
        "DTYPE,K1",
        "DTYPE,K2",
        "DTYPE,K3",
        "DTYPE,JINT",
        "*CFOPEN,crack_cint_L0,txt",
        "SOLVE",
    ):
        assert token in deck, f"missing APDL token: {token}"


def test_deck_writes_solved_reaction_sums():
    deck = generate_crack_verification_apdl(CrackPlateSpec())
    assert "*CFOPEN,crack_reac_L0,txt" in deck
    assert "RF,FZ" in deck
    assert "RF,FX" in deck
    assert "RF,FY" in deck
    assert "fz_ligament" in deck


def test_mesh_levels_select_different_decks():
    d0 = generate_crack_verification_apdl(CrackPlateSpec(mesh_level=0))
    d1 = generate_crack_verification_apdl(CrackPlateSpec(mesh_level=1))
    assert deck_sha256(d0) != deck_sha256(d1)
    assert "*CFOPEN,crack_cint_L1,txt" in d1


def test_fine_level_halves_crack_front_element_size():
    p0, p1, p2 = (mesh_parameters(CrackPlateSpec(), lv) for lv in (0, 1, 2))
    assert p1["first_ring_mm"] == pytest.approx(p0["first_ring_mm"] / 2.0)
    assert p2["first_ring_mm"] == pytest.approx(p1["first_ring_mm"] / 2.0)
    assert p1["front_divisions_quarter"] == 2 * p0["front_divisions_quarter"]
    assert p2["front_divisions_quarter"] == 2 * p1["front_divisions_quarter"]


def test_deck_has_no_host_specific_content():
    deck = generate_crack_verification_apdl(CrackPlateSpec())
    assert cint_parser.find_host_tokens(deck) == []
    assert "/CWD" not in deck.upper()


def test_edge_effects_negligible():
    # Newman-Raju finite-width correction for the modelled half-width.
    assert finite_width_effect(CrackPlateSpec()) < 0.002


@pytest.mark.parametrize(
    "kwargs, match",
    [
        ({"crack_depth_mm": 12.0}, "a < t"),
        ({"crack_depth_mm": 5.0}, "a <= c"),
        ({"mesh_level": -1}, "mesh_level"),
        ({"half_width_mm": 10.0}, "half-width"),
        ({"tube_radius_mm": 1.5}, "tube"),
    ],
)
def test_invalid_spec_rejected(kwargs, match):
    with pytest.raises(ValueError, match=match):
        generate_crack_verification_apdl(CrackPlateSpec(**kwargs))


def test_write_inp(tmp_path: Path):
    out = write_crack_verification_inp(CrackPlateSpec(), tmp_path / "d" / "p.inp")
    assert out.is_file()
    text = out.read_bytes().decode("ascii")
    assert deck_sha256(text) == deck_sha256(
        generate_crack_verification_apdl(CrackPlateSpec())
    )


# --------------------------------------------------------------------------- #
# Generator: mesh integrity
# --------------------------------------------------------------------------- #
@pytest.mark.parametrize("level", [0, 1])
def test_mesh_is_conforming(level):
    """Every interior face is shared by two elements; free faces lie on the
    six bounding planes only (so no node merge was missed)."""
    mesh = _mesh(level)
    spec = CrackPlateSpec()
    faces: dict[frozenset, int] = {}
    loc = [(0, 1, 2, 3), (4, 5, 6, 7), (0, 1, 5, 4), (1, 2, 6, 5), (2, 3, 7, 6), (3, 0, 4, 7)]
    for el in mesh.elements:
        for f in loc:
            key = frozenset(el[i] for i in f)
            if len(key) < 3:
                continue  # collapsed edge of a crack-tip wedge
            faces[key] = faces.get(key, 0) + 1
    assert max(faces.values()) == 2
    b, t, h = spec.half_width_mm, spec.thickness_mm, spec.half_height_mm
    for key, count in faces.items():
        if count == 2:
            continue
        pts = [mesh.nodes[n - 1] for n in key]
        on_plane = (
            all(abs(p[0] - b) < 1e-9 for p in pts)
            or all(abs(p[0] + b) < 1e-9 for p in pts)
            or all(abs(p[1]) < 1e-9 for p in pts)
            or all(abs(p[1] - t) < 1e-9 for p in pts)
            or all(abs(p[2]) < 1e-9 for p in pts)
            or all(abs(p[2] - h) < 1e-9 for p in pts)
        )
        assert on_plane, f"free face off the boundary: {pts}"


@pytest.mark.parametrize("level", [0, 1])
def test_all_elements_have_positive_jacobian(level):
    mesh = _mesh(level)
    for el in mesh.elements:
        assert mesh.jacobian_at_centre(el) > 0.0


@pytest.mark.parametrize("level", [0, 1])
def test_crack_front_nodes_on_ellipse_and_ligament_constrained(level):
    mesh = _mesh(level)
    spec = CrackPlateSpec()
    a, c = spec.crack_depth_mm, spec.crack_half_length_mm
    front = [mesh.nodes[n - 1] for n in range(1, mesh.n_front + 1)]
    assert len(front) == 2 * mesh_parameters(spec, level)["front_divisions_quarter"] + 1
    for x, y, z in front:
        assert z == 0.0
        assert (x / c) ** 2 + (y / a) ** 2 == pytest.approx(1.0, abs=1e-7)
    # ligament block: z = 0 and outside or on the crack front
    for n in range(1, mesh.n_ligament + 1):
        x, y, z = mesh.nodes[n - 1]
        assert z == 0.0
        assert (x / c) ** 2 + (y / a) ** 2 >= 1.0 - 1e-7
    # every other z = 0 node is on the crack face (inside the front)
    for n in range(mesh.n_ligament + 1, len(mesh.nodes) + 1):
        x, y, z = mesh.nodes[n - 1]
        if z == 0.0:
            assert (x / c) ** 2 + (y / a) ** 2 < 1.0 - 1e-7


@pytest.mark.parametrize("level", [0, 1])
def test_front_midside_nodes_follow_corners(level):
    mesh = _mesh(level)
    spec = CrackPlateSpec()
    a, c = spec.crack_depth_mm, spec.crack_half_length_mm
    assert mesh.n_front_all == 2 * mesh.n_front - 1
    for n in range(mesh.n_front + 1, mesh.n_front_all + 1):
        x, y, z = mesh.nodes[n - 1]
        assert z == 0.0
        assert (x / c) ** 2 + (y / a) ** 2 == pytest.approx(1.0, abs=1e-7)


def test_front_contains_deepest_and_both_surface_points():
    mesh = _mesh(0)
    front = [mesh.nodes[n - 1] for n in range(1, mesh.n_front + 1)]
    assert (4.0, 0.0, 0.0) in front
    assert (-4.0, 0.0, 0.0) in front
    assert (0.0, 2.0, 0.0) in front


# --------------------------------------------------------------------------- #
# Licensed solve: the only test that may skip
# --------------------------------------------------------------------------- #
def _mapdl_available() -> bool:
    from digitalmodel.ansys.runner import ANSYSRunner

    return ANSYSRunner()._detect_executable() is not None


@pytest.mark.slow
@pytest.mark.skipif(
    os.environ.get("DIGITALMODEL_MAPDL_SOLVE") != "1" or not _mapdl_available(),
    reason="licensed MAPDL solve: needs an MAPDL executable and DIGITALMODEL_MAPDL_SOLVE=1",
)
def test_solve_verification_deck(tmp_path: Path):
    from digitalmodel.ansys.crack_verification import run_verification

    receipt = run_verification(tmp_path / "run", tmp_path / "receipt.json")
    assert cint_parser.validate_receipt_schema(receipt) == []
    for name, result in receipt["guards"].items():
        assert result["status"] == "pass", f"guard {name}: {result}"
    deep = receipt["comparator"]["deepest"]
    assert math.isfinite(deep["fe_mpa_sqrt_m"])
