# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Bulk fleet diffraction-deck generation (digitalmodel #929)."""

from __future__ import annotations

import importlib.util
import json
import sys
from pathlib import Path, PureWindowsPath

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction import fleet_decks
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_batch_runner import (
    OrcaWaveBatchConfig,
)
from digitalmodel.marine_ops.vessel_db.loader import Record


def _record(name: str, vessel_type: str, **dims) -> Record:
    return Record(
        name=name,
        scope="floating",
        layer="particulars",
        vessel_type=vessel_type,
        raw_fields=dims,
    )


@pytest.fixture
def fleet(monkeypatch):
    records = [
        _record(
            "Test FPSO", "FPSO", loa_m=258, beam_m=46, draft_m=14, displacement_t=160000
        ),
        _record(
            "Test VLCC",
            "VLCC tanker",
            loa_m=330,
            beam_m=60,
            draft_m=22,
            displacement_t=320000,
        ),
        _record("Test Semi", "semisubmersible", loa_m=120, beam_m=78, draft_m=20),
        _record("Synthetic spar", "spar", loa_m=80, beam_m=30, draft_m=20),
        _record(
            "No Draft Barge", "barge", loa_m=100, beam_m=30
        ),  # missing draft -> skip
    ]
    monkeypatch.setattr(fleet_decks, "iter_fleet_records", lambda scopes=None: records)
    return records


def test_generate_fleet_decks_builds_and_skips(tmp_path: Path, fleet):
    result = fleet_decks.generate_fleet_decks(tmp_path, target_panels=120)

    # Four complete records = three meshed + one placeholder; one lacks draft.
    assert result.n_built == 4
    assert result.n_placeholder == 1
    assert result.n_parametric == 3
    assert [name for name, _ in result.skipped] == ["No Draft Barge"]

    # Each built deck wrote its dir with a spec .yml + manifest.
    for deck in result.decks:
        if deck.mesh_kind == "placeholder":
            assert not deck.deck_path.exists()
        else:
            assert deck.deck_path.is_file()
        assert (deck.deck_path.parent / "manifest.json").is_file()

    index = json.loads((tmp_path / "index.json").read_text(encoding="utf-8"))
    assert index["n_built"] == 4
    assert index["n_placeholder"] == 1
    assert index["n_skipped"] == 1
    assert len(index["decks"]) == 4
    for entry, deck in zip(index["decks"], result.decks):
        expected = None if deck.mesh_kind == "placeholder" else deck.deck_path.name
        assert entry["deck"] == expected


@pytest.mark.parametrize("stale_deck", [False, True])
def test_placeholder_index_does_not_advertise_native_deck(
    tmp_path, monkeypatch, stale_deck
):
    record = _record("Synthetic spar", "spar", loa_m=80, beam_m=30, draft_m=20)
    monkeypatch.setattr(
        fleet_decks, "iter_fleet_records", lambda scopes=None: [record]
    )
    deck_dir = tmp_path / record.vessel_id()
    deck_dir.mkdir()
    old_deck = deck_dir / f"{record.name}.yml"
    if stale_deck:
        old_deck.write_text("prior input\n", encoding="utf-8", newline="\n")

    result = fleet_decks.generate_fleet_decks(tmp_path)

    index = json.loads(result.index_path.read_text(encoding="utf-8"))
    assert index["decks"][0]["deck"] is None
    assert index["decks"][0]["mesh_kind"] == "placeholder"
    assert result.decks[0].spec_path.is_file()
    assert old_deck.exists() == stale_deck
    if stale_deck:
        assert old_deck.read_text(encoding="utf-8") == "prior input\n"


@pytest.mark.parametrize("stale_deck", [False, True])
def test_demo_index_distinguishes_generated_and_placeholder_decks(
    tmp_path, monkeypatch, capsys, stale_deck
):
    script = (
        Path(__file__).resolve().parents[3]
        / "examples/demos/diffraction/prepare_vessel_decks.py"
    )
    spec = importlib.util.spec_from_file_location("prepared_decks", script)
    demo = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(demo)
    generated = _record("Synthetic hull", "barge", loa_m=20, beam_m=8, draft_m=4)
    placeholder = _record("Synthetic spar", "spar", loa_m=80, beam_m=30, draft_m=20)
    monkeypatch.setattr(
        demo, "select_representative",
        lambda: [("generated", generated), ("placeholder", placeholder)],
    )
    monkeypatch.setattr(
        sys, "argv", [script.name, "--out", tmp_path.as_posix(), "--target-panels", "20"]
    )
    old_deck = tmp_path / placeholder.vessel_id() / f"{placeholder.name}.yml"
    if stale_deck:
        old_deck.parent.mkdir()
        old_deck.write_text("prior input\n", encoding="utf-8", newline="\n")

    demo.main()

    index = json.loads((tmp_path / "index.json").read_text(encoding="utf-8"))
    assert len(index) == 2
    assert index[1]["mesh_kind"] == "placeholder"
    assert index[1]["deck"] is None
    assert index[0]["deck"] == f"{generated.name}.yml"
    assert (tmp_path / index[0]["deck_dir"] / index[0]["deck"]).is_file()
    assert old_deck.exists() == stale_deck
    if stale_deck:
        assert old_deck.read_text(encoding="utf-8") == "prior input\n"
    output = capsys.readouterr().out
    assert "2 preparation records" in output
    assert "non-null deck entries in index.json" in output
    assert "Run each <vessel>.yml" not in output


def test_emit_batch_config_excludes_placeholder_and_parses(tmp_path: Path, fleet):
    result = fleet_decks.generate_fleet_decks(tmp_path, target_panels=120)
    cfg_path = fleet_decks.emit_orcawave_batch_config(result, tmp_path / "batch.yml")

    raw = yaml.safe_load(cfg_path.read_text(encoding="utf-8"))
    # The drilling semi-submersible now has a parametric mesh and is included.
    assert len(raw["jobs"]) == 3
    job_names = {j["job_name"] for j in raw["jobs"]}
    assert any("semi" in n.lower() for n in job_names)
    for job in raw["jobs"]:
        assert Path(job["spec_path"]).name.endswith(".yml")
        assert "output_dir" in job

    # The emitted YAML must parse back through the real batch-runner schema
    # (proves dispatch-readiness, not just a hand-rolled dict).
    parsed = OrcaWaveBatchConfig.from_yaml(cfg_path)
    assert len(parsed.jobs) == 3
    assert str(parsed.execution_mode.value) == "parallel"

    # Every job's spec_path must round-trip through DiffractionSpec.from_yaml —
    # that is exactly how orcawave_batch_runner loads jobs. Pointing jobs at the
    # OrcaWave-native deck .yml (top-level Units/Bodies keys) fails schema
    # validation and errors 100% of jobs on the licensed-run lane.
    for job in parsed.jobs:
        spec = DiffractionSpec.from_yaml(job.spec_path)
        assert spec.vessel.name  # canonical spec, loadable by the batch runner
        assert spec.frequencies is not None and spec.wave_headings is not None


def test_emit_batch_config_can_include_placeholder(tmp_path: Path, fleet):
    result = fleet_decks.generate_fleet_decks(tmp_path, target_panels=120)
    cfg_path = fleet_decks.emit_orcawave_batch_config(
        result, tmp_path / "batch_all.yml", include_placeholder=True
    )
    raw = yaml.safe_load(cfg_path.read_text(encoding="utf-8"))
    assert len(raw["jobs"]) == 4
    placeholder = next(d for d in result.decks if d.mesh_kind == "placeholder")
    assert any(j["job_name"] == placeholder.spec_path.parent.name for j in raw["jobs"])


def test_batch_config_uses_forward_slashes_for_windows_paths(tmp_path):
    prepared = PureWindowsPath("prepared") / "synthetic"
    deck = fleet_decks.DeckResult(
        vessel="Synthetic hull", form="box", deck_path=prepared / "native.yml",
        spec_path=prepared / "spec.yml", mesh_path=prepared / "hull.gdf",
        mesh_kind="parametric", n_panels=4, mass_basis="explicit",
    )
    result = fleet_decks.FleetResult(out_dir=PureWindowsPath("outputs"), decks=[deck])

    path = fleet_decks.emit_orcawave_batch_config(result, tmp_path / "batch.yml")

    config = yaml.safe_load(path.read_text(encoding="utf-8"))
    assert config["base_output_dir"] == "outputs/batch_output"
    assert config["jobs"][0]["spec_path"] == "prepared/synthetic/spec.yml"
    assert config["jobs"][0]["output_dir"] == "outputs/batch_output/synthetic"


def test_iter_fleet_records_dedups_across_scopes(monkeypatch):
    rec_floating = _record("Shared Vessel", "FPSO", loa_m=200, beam_m=40, draft_m=12)
    rec_install = _record("shared vessel", "FPSO", loa_m=200, beam_m=40, draft_m=12)
    rec_install.scope = "install"

    monkeypatch.setattr(
        fleet_decks,
        "datasets",
        lambda: [("floating", "particulars"), ("install", "particulars")],
    )

    def fake_iter(scope, layer, base=None):
        return {"floating": [rec_floating], "install": [rec_install]}[scope]

    monkeypatch.setattr(fleet_decks, "iter_records", fake_iter)

    out = fleet_decks.iter_fleet_records()
    assert len(out) == 1  # same name across two scopes -> deduped


def test_scope_filter_passed_through(monkeypatch):
    seen_scopes = []

    monkeypatch.setattr(
        fleet_decks,
        "datasets",
        lambda: [("floating", "particulars"), ("install", "particulars")],
    )

    def fake_iter(scope, layer, base=None):
        seen_scopes.append(scope)
        return []

    monkeypatch.setattr(fleet_decks, "iter_records", fake_iter)
    fleet_decks.iter_fleet_records(scopes=["install"])
    assert seen_scopes == ["install"]
