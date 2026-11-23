"""Offline smoke test for the floating-wind-sizing workflow (issue #1028).

Dispatches the committed example ``input.yml`` through the digitalmodel engine
(basename ``floating_wind_sizing``) and asserts a sane screening summary.
Closed-form / solver-free: no OrcaFlex or OrcaWave licence required.
"""

import json
from pathlib import Path

import pytest

from digitalmodel.engine import engine
from digitalmodel.usecase_registry import load_usecases, validate_registry

REPO_ROOT = Path(__file__).resolve().parents[2]
INPUT_YML = (
    REPO_ROOT / "examples" / "workflows" / "floating-wind-sizing" / "input.yml"
)
SUMMARY_JSON = (
    REPO_ROOT
    / "examples"
    / "workflows"
    / "floating-wind-sizing"
    / "results"
    / "floating_wind_sizing_summary.json"
)


def test_floating_wind_sizing_workflow_offline():
    assert INPUT_YML.exists(), f"missing example input: {INPUT_YML}"

    cfg = engine(inputfile=str(INPUT_YML))

    result = cfg["floating_wind_sizing"]["result"]
    assert result["n_concepts"] == 3
    assert result["total_variants"] == 12  # 6 semi + 3 spar + 3 barge
    assert 0 < result["total_passed"] <= result["total_variants"]

    archetypes = {c["archetype"] for c in result["concepts"]}
    assert archetypes == {"semi", "spar", "barge"}

    # The spar concept should produce a passing shortlist; the barge (resonant
    # with the wave band) should not.
    by_arch = {c["archetype"]: c for c in result["concepts"]}
    assert by_arch["spar"]["n_passed"] >= 1
    assert by_arch["barge"]["n_passed"] == 0

    # Summary JSON is written and round-trips.
    assert SUMMARY_JSON.exists()
    on_disk = json.loads(SUMMARY_JSON.read_text())
    assert on_disk["total_variants"] == 12


def test_registry_entry_is_valid_and_resolves():
    # The floating-wind-sizing use-case is registered and its template resolves.
    assert validate_registry() == []
    fw = [u for u in load_usecases() if u.id == "floating-wind-sizing"]
    assert len(fw) == 1
    assert fw[0].template_path().exists()
