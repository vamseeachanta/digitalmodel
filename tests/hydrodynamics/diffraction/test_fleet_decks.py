"""Bulk fleet diffraction-deck generation (digitalmodel #929)."""

from __future__ import annotations

import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction import fleet_decks
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
        _record(
            "No Draft Barge", "barge", loa_m=100, beam_m=30
        ),  # missing draft -> skip
    ]
    monkeypatch.setattr(fleet_decks, "iter_fleet_records", lambda scopes=None: records)
    return records


def test_generate_fleet_decks_builds_and_skips(tmp_path: Path, fleet):
    result = fleet_decks.generate_fleet_decks(tmp_path, target_panels=120)

    assert result.n_built == 3
    assert result.n_placeholder == 1  # the semisub
    assert result.n_parametric == 2
    assert [name for name, _ in result.skipped] == ["No Draft Barge"]

    # Each built deck wrote its dir with a spec .yml + manifest.
    for deck in result.decks:
        assert deck.deck_path.is_file()
        assert (deck.deck_path.parent / "manifest.json").is_file()

    index = json.loads((tmp_path / "index.json").read_text())
    assert index["n_built"] == 3
    assert index["n_skipped"] == 1
    assert len(index["decks"]) == 3


def test_emit_batch_config_excludes_placeholder_and_parses(tmp_path: Path, fleet):
    result = fleet_decks.generate_fleet_decks(tmp_path, target_panels=120)
    cfg_path = fleet_decks.emit_orcawave_batch_config(result, tmp_path / "batch.yml")

    raw = yaml.safe_load(cfg_path.read_text())
    # Placeholder (semisub) excluded by default -> 2 jobs from 3 decks.
    assert len(raw["jobs"]) == 2
    job_names = {j["job_name"] for j in raw["jobs"]}
    assert not any("semi" in n.lower() for n in job_names)
    for job in raw["jobs"]:
        assert Path(job["spec_path"]).name.endswith(".yml")
        assert "output_dir" in job

    # The emitted YAML must parse back through the real batch-runner schema
    # (proves dispatch-readiness, not just a hand-rolled dict).
    parsed = OrcaWaveBatchConfig.from_yaml(cfg_path)
    assert len(parsed.jobs) == 2
    assert str(parsed.execution_mode.value) == "parallel"


def test_emit_batch_config_can_include_placeholder(tmp_path: Path, fleet):
    result = fleet_decks.generate_fleet_decks(tmp_path, target_panels=120)
    cfg_path = fleet_decks.emit_orcawave_batch_config(
        result, tmp_path / "batch_all.yml", include_placeholder=True
    )
    raw = yaml.safe_load(cfg_path.read_text())
    assert len(raw["jobs"]) == 3


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
