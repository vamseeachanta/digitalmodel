"""Tests for building OrcaWave diffraction decks from vessel-DB records (#896)."""

from __future__ import annotations

import json

from digitalmodel.hydrodynamics.diffraction.vessel_deck_builder import (
    MESH_FILE,
    PLACEHOLDER_MESH,
    build_deck,
    clean_vessel_name,
    parametric_hull_profile,
    select_form,
)
from digitalmodel.marine_ops.vessel_db.loader import Record


def _rec(name, vtype, loa=250.0, beam=46.0, draft=12.0, depth=24.0, displ=120000.0):
    fields = {"loa_m": loa, "beam_m": beam, "draft_m": draft, "depth_m": depth}
    if displ is not None:
        fields["displacement_t"] = displ
    return Record(
        name=name,
        scope="floating",
        layer="particulars",
        vessel_type=vtype,
        raw_fields=fields,
        citations=[{"fields": ["all"], "source": "test", "access": "public"}],
    )


def test_clean_vessel_name():
    assert (
        clean_vessel_name("Alaskan Frontier (VLCC; sisters X/Y/Z)")
        == "Alaskan Frontier"
    )
    assert "/" not in clean_vessel_name("A/B (x)")
    assert clean_vessel_name("()") == "vessel"


def test_select_form():
    assert select_form(_rec("X", "Turret-moored FPSO")) == "box"
    assert select_form(_rec("X", "VLCC crude tanker")) == "full"
    assert select_form(_rec("X", "7th-gen drillship")) == "ship"
    assert select_form(_rec("X", "Semi-submersible crane vessel")) == "placeholder"
    assert select_form(_rec("X", "deepwater construction vessel")) == "ship"


def test_parametric_hull_profile_valid_and_tapered():
    prof = parametric_hull_profile("T", 200.0, 40.0, 12.0, 24.0, form="ship")
    assert prof.length_bp == 200.0 and prof.beam == 40.0
    assert len(prof.stations) >= 2
    half = [max(y for _, y in s.waterline_offsets) for s in prof.stations]
    # ends are finer than midship (plan-form taper)
    assert half[0] < max(half) and half[-1] < max(half)
    # never exceeds half-beam
    assert max(half) <= 20.0 + 1e-6


def test_build_deck_parametric(tmp_path):
    res = build_deck(_rec("Test FPSO", "FPSO turret"), tmp_path)
    assert res is not None
    assert res.mesh_kind == "parametric" and res.n_panels and res.n_panels > 0
    assert res.mass_basis == "explicit"
    assert res.deck_path.exists()
    assert (res.deck_path.parent / MESH_FILE).exists()
    # the generated file is a valid OrcaWave deck referencing our mesh
    deck = res.deck_path.read_text()
    assert "UnitsSystem: SI" in deck
    assert MESH_FILE in deck
    # manifest carries provenance + swap instructions
    man = json.loads((res.deck_path.parent / "manifest.json").read_text())
    assert man["mesh"]["kind"] == "parametric"
    assert "swap" in man["mesh"]


def test_build_deck_placeholder_for_semisub(tmp_path):
    res = build_deck(_rec("SSCV Test", "Semi-submersible crane vessel"), tmp_path)
    assert res is not None
    assert res.mesh_kind == "placeholder"
    assert res.n_panels is None
    # deck references the mesh-to-supply path; no gdf written
    man = json.loads((res.deck_path.parent / "manifest.json").read_text())
    assert man["mesh"]["file"] == PLACEHOLDER_MESH
    assert not (res.deck_path.parent / MESH_FILE).exists()


def test_build_deck_free_floating_without_displacement(tmp_path):
    res = build_deck(_rec("NoMass Ship", "drillship", displ=None), tmp_path)
    assert res is not None
    assert res.mass_basis == "free_floating"


def test_build_deck_none_without_dimensions(tmp_path):
    bare = Record(
        name="Bare",
        scope="floating",
        layer="particulars",
        vessel_type="fpso",
        raw_fields={"displacement_t": 1000.0},
    )
    assert build_deck(bare, tmp_path) is None


def test_swappable_mesh_path_is_single_field(tmp_path):
    res = build_deck(_rec("Swap Test", "FPSO"), tmp_path)
    text = res.deck_path.read_text()
    assert MESH_FILE in text  # mesh referenced by the single swappable filename
