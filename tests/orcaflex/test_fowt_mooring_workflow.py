"""Offline smoke test for the FOWT watch-circle vs dynamic-cable workflow.

Dispatches the committed ``examples/workflows/fowt-mooring/input.yml`` through
the digitalmodel engine (basename ``fowt_mooring``) and asserts a sane result.
Closed-form / solver-free: no OrcaFlex licence required.
"""

import json
from pathlib import Path

import pytest

from digitalmodel.engine import engine

REPO_ROOT = Path(__file__).resolve().parents[2]
INPUT_YML = REPO_ROOT / "examples" / "workflows" / "fowt-mooring" / "input.yml"
SUMMARY_JSON = (
    REPO_ROOT
    / "examples"
    / "workflows"
    / "fowt-mooring"
    / "results"
    / "fowt_mooring_summary.json"
)


def test_fowt_mooring_workflow_offline(tmp_path, monkeypatch):
    assert INPUT_YML.exists(), f"missing example input: {INPUT_YML}"

    cfg = engine(inputfile=str(INPUT_YML))

    assert isinstance(cfg, dict)
    assert cfg["basename"] == "fowt_mooring"

    result = cfg["fowt_mooring"]["result"]
    # Healthy feasibility screen: governing bend radius well above the MBR limit.
    assert result["passes"] is True
    assert result["governing_bend_radius_m"] == pytest.approx(136.554, abs=1e-2)
    assert result["mbr_limit_m"] == pytest.approx(4.5)
    assert result["margin_m"] == pytest.approx(132.054, abs=1e-2)
    assert result["governing_offset_m"] == pytest.approx(25.0)
    # Watch-circle drift toward touchdown shrinks the chord below the still-water span.
    assert (
        result["governing_chord_m"]
        < cfg["fowt_mooring"]["cable"]["nominal_horizontal_span"]
    )
    assert result["mooring_type"] == "hybrid"
    assert result["floater_type"] == "semi"

    # Summary JSON written to the committed results dir.
    assert SUMMARY_JSON.exists()
    written = json.loads(SUMMARY_JSON.read_text())
    assert written["result"]["passes"] is True
