"""Tests for building OrcaWave diffraction decks from vessel-DB records (#896)."""

from __future__ import annotations

import json

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
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


def test_canonical_spec_written_and_loadable(tmp_path):
    """build_deck writes spec.yml (canonical DiffractionSpec) alongside the
    OrcaWave-native deck — the batch runner loads jobs via
    DiffractionSpec.from_yaml, which cannot parse the native deck."""
    res = build_deck(_rec("Spec Test", "FPSO"), tmp_path)
    assert res.spec_path.is_file()
    assert res.spec_path != res.deck_path
    spec = DiffractionSpec.from_yaml(res.spec_path)
    assert spec.vessel.geometry.mesh_file == MESH_FILE
    man = json.loads((res.deck_path.parent / "manifest.json").read_text())
    assert man["spec"] == res.spec_path.name


def test_explicit_cog_converted_to_mesh_frame(tmp_path):
    """cog_m from mass_properties (x=0 amidships, z=0 keel) must be converted
    to the parametric mesh/deck frame (x=0 at aft perpendicular spanning
    0..LOA, z=0 at waterline; hull_library.mesh_generator) before emission."""
    rec = _rec(
        "Datum FPSO", "FPSO", loa=300.0, beam=50.0, draft=20.0, depth=32.0,
        displ=250000.0,
    )
    rec.raw_fields["vcg_m"] = 15.0   # KG: 15 m above keel
    rec.raw_fields["lcg_m"] = -10.0  # 10 m aft of amidships

    res = build_deck(rec, tmp_path)
    assert res.mass_basis == "explicit"

    expected = [300.0 / 2.0 - 10.0, 0.0, 15.0 - 20.0]  # [140, 0, -5]
    spec = DiffractionSpec.from_yaml(res.spec_path)
    assert spec.vessel.inertia.centre_of_gravity == pytest.approx(expected)

    # And the OrcaWave deck emits it verbatim (BodyMeshPosition frame).
    deck = yaml.safe_load(res.deck_path.read_text())
    assert deck["Bodies"][0]["BodyCentreOfMass"] == pytest.approx(expected)


def test_explicit_cog_defaults_land_amidships_at_keel(tmp_path):
    """No cited LCG/VCG: amidships default (x=0) must land at LOA/2 in the
    mesh frame, and the keel-datum VCG placeholder at z=-draft (not the
    waterline)."""
    res = build_deck(_rec("Default CoG", "FPSO", loa=250.0, draft=12.0), tmp_path)
    spec = DiffractionSpec.from_yaml(res.spec_path)
    assert spec.vessel.inertia.centre_of_gravity == pytest.approx(
        [125.0, 0.0, -12.0]
    )
    # the keel-datum VCG gap is still flagged for the user
    assert any("VCG unknown" in n for n in res.notes)
