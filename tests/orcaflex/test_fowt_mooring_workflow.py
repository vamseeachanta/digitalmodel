# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline smoke test for the FOWT watch-circle vs dynamic-cable workflow.

Dispatches the committed ``examples/workflows/fowt-mooring/input.yml`` through
the digitalmodel engine (basename ``fowt_mooring``) and asserts a sane result.
Closed-form / solver-free: no OrcaFlex licence required.
"""

import json
import logging
from pathlib import Path

import pytest

from digitalmodel.engine import engine

REPO_ROOT = Path(__file__).resolve().parents[2]
INPUT_YML = REPO_ROOT / "examples" / "workflows" / "fowt-mooring" / "input.yml"


def test_fowt_mooring_workflow_offline(tmp_path, monkeypatch):
    assert INPUT_YML.exists(), f"missing example input: {INPUT_YML}"
    input_path = tmp_path / INPUT_YML.relative_to(REPO_ROOT)
    input_path.parent.mkdir(parents=True)
    input_path.write_bytes(INPUT_YML.read_bytes())
    monkeypatch.chdir(tmp_path)

    page = tmp_path / "wikis/engineering-standards/wiki/standards/dnv-st-0359.md"
    page.parent.mkdir(parents=True)
    page.write_text(
        "---\ncode_id: DNV-ST-0359\npublisher: DNV\nrevision: 2025-12\n---\n",
        encoding="utf-8",
    )
    monkeypatch.setenv("LLM_WIKI_PATH", str(tmp_path))
    # Detach and close file streams before the engine replaces its handlers.
    for handler in list(logging.root.handlers):
        if isinstance(handler, logging.FileHandler):
            logging.root.removeHandler(handler)
            handler.close()
    cfg = engine(inputfile=str(input_path))

    assert isinstance(cfg, dict)
    assert cfg["basename"] == "fowt_mooring"

    result = cfg["fowt_mooring"]["result"]
    assert len(result["citations"]) == 1
    assert result["citations"][0]["code_id"] == "DNV-ST-0359"
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

    summary_json = tmp_path / "examples/workflows/fowt-mooring/results/fowt_mooring_summary.json"
    assert summary_json.exists()
    written = json.loads(summary_json.read_text())
    assert written["result"]["passes"] is True
