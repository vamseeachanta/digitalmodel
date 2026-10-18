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


def assumed_source_record():
    """Reuse the established volume-control dimensions for output checks."""
    return Record(
        name="synthetic source record", scope="floating", layer="particulars",
        vessel_type="drilling semi-submersible",
        raw_fields=record().raw_fields,
    )


def test_assumed_geometry_labels_and_repeatable_outputs(tmp_path):
    """All output labels identify assumptions; independent builds match bytewise."""
    from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec

    source = assumed_source_record()
    result = build_deck(source, tmp_path / "first")
    assert "assumed proportions" in result.vessel
    assert source.name not in result.vessel
    spec = DiffractionSpec.from_yaml(result.spec_path)
    assert spec.vessel.name == result.vessel
    assert "assumed proportions" in spec.vessel.type
    assert result.vessel in result.deck_path.stem
    assert result.vessel in result.deck_path.read_text()
    assert result.vessel in result.mesh_path.read_text().splitlines()[0]
    manifest = json.loads((result.mesh_path.parent / "manifest.json").read_text())
    assert manifest["vessel"] == result.vessel
    assert manifest["geometry_basis"] == "assumed proportions"
    assert manifest["source_record_id"] == source.vessel_id()
    assert "vessel_id" not in manifest
    assert any("assumed proportions" in note for note in result.notes)
    repeated = build_deck(source, tmp_path / "second")
    first = {p.name: p.read_bytes() for p in result.deck_path.parent.iterdir()}
    second = {p.name: p.read_bytes() for p in repeated.deck_path.parent.iterdir()}
    assert set(first) == {"hull.gdf", "spec.yml", "manifest.json", result.deck_path.name}
    assert first == second


def test_demo_index_uses_output_label_and_is_repeatable(tmp_path, monkeypatch, capsys):
    """The thin index writer preserves the builder's assumed-proportions label."""
    import importlib.util
    from pathlib import Path
    from types import SimpleNamespace

    path = Path(__file__).resolve().parents[3] / "examples/demos/diffraction/prepare_vessel_decks.py"
    spec = importlib.util.spec_from_file_location("prepared_decks", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    source = assumed_source_record()
    label = builder.ASSUMED_SEMI_NAME

    def prepared_result(record, out_dir, **kwargs):
        return SimpleNamespace(
            vessel=label, form="column_pontoon", mesh_kind="parametric",
            n_panels=1, mass_basis="free_floating", notes=["assumed proportions"],
            deck_path=out_dir / "source_record" / f"{label}.yml",
        )

    monkeypatch.setattr(module, "build_deck", prepared_result)
    monkeypatch.setattr(module, "select_representative", lambda: [("semisubmersible", source)])
    outputs = []
    for name in ("first", "second"):
        out = tmp_path / name
        monkeypatch.setattr("sys.argv", ["prepare_vessel_decks.py", "--out", str(out)])
        module.main()
        outputs.append((out / "index.json").read_bytes())
        assert json.loads(outputs[-1])[0]["vessel"] == label
        assert source.name not in capsys.readouterr().out
    assert outputs[0] == outputs[1]
