# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise the generic column-and-pontoon deck branch without a solver."""

import json

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.vessel_deck_builder import build_deck, select_form
from digitalmodel.hydrodynamics.diffraction import vessel_deck_builder as builder
from digitalmodel.hydrodynamics.hull_library.generic_semi import generic_semi_parameters
from digitalmodel.marine_ops.vessel_db.loader import Record


def record(vessel_type="drilling semi-submersible", beam=90.0):
    return Record(
        name="SEMI-G1", scope="floating", layer="particulars",
        vessel_type=vessel_type,
        raw_fields={"loa_m": 96.2, "beam_m": beam, "draft_m": 22.75},
    )


def test_drilling_semi_routes_to_column_pontoon():
    assert select_form(record()) == "column_pontoon"


@pytest.mark.parametrize("vessel_type", [
    "spar", "tlp", "semi-submersible crane vessel", "semi-submersible SSCV",
    "semi-submersible heavy-lift vessel", "semi-submersible heavy lift vessel",
])
def test_excluded_types_stay_placeholder(vessel_type):
    assert select_form(record(vessel_type)) == "placeholder"


def test_column_deck_gdf_preserves_volume(tmp_path, monkeypatch):
    captured = {}
    generate = builder.generate_column_pontoon

    def capture(parameters):
        mesh, report = generate(parameters)
        captured.update(parameters=parameters, report=report)
        return mesh, report

    monkeypatch.setattr(builder, "generate_column_pontoon", capture)
    result = build_deck(record(), tmp_path)
    assert result is not None and result.mesh_kind == "parametric"
    assert result.form == "column_pontoon"
    assert result.mesh_path.name == "hull.gdf"
    lines = result.mesh_path.read_text().splitlines()
    assert int(lines[3]) == result.n_panels
    vertices = np.loadtxt(lines[4:]).reshape(-1, 4, 3)
    # Same signed divergence volume as column_pontoon_form._hydrostatics;
    # each quad is split into two oriented triangles.
    volume = abs(sum(
        np.einsum("ij,ij->i", vertices[:, 0], np.cross(vertices[:, j], vertices[:, j+1])).sum()/6
        for j in (1, 2)
    ))
    assert captured["parameters"] == generic_semi_parameters(96.2, 90.0, 22.75, 4.0)
    assert volume == pytest.approx(captured["report"].displacement, rel=0.01)
    manifest = json.loads((result.mesh_path.parent / "manifest.json").read_text())
    assert manifest["mesh"]["kind"] == "parametric"


def test_invalid_semi_proportions_use_noted_placeholder(tmp_path):
    result = build_deck(record(beam=10.0), tmp_path)
    assert result.mesh_kind == "placeholder"
    assert result.n_panels is None
    assert any("invalid column-and-pontoon parameters" in note for note in result.notes)
