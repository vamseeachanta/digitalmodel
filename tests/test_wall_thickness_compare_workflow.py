# ABOUTME: Tests for the wall_thickness_compare engine route + packaged calc note.
# ABOUTME: Verifies multi-code comparison runs, ranks, and emits MD/JSON artifacts.

import json

import pytest

from digitalmodel.structural.analysis.wall_thickness_compare_workflow import (
    build_compare_note,
    run_wall_thickness_compare,
)
from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_comparison import compare_codes


def _cfg(output_dir):
    return {
        "wall_thickness_compare": {
            "label": "test_pipeline",
            "output_dir": str(output_dir),
            "safety_class": "medium",
            "codes": ["DNV-ST-F101", "API-RP-1111", "PD-8010-2"],
            "geometry": {
                "outer_diameter": 0.3239,
                "wall_thickness": 0.0191,
                "corrosion_allowance": 0.003,
            },
            "material": {"grade": "X65", "smys": 448e6, "smts": 531e6},
            "loads": {
                "internal_pressure": 25e6,
                "external_pressure": 15e6,
                "bending_moment": 120e3,
                "effective_tension": 800e3,
            },
        }
    }


def test_run_compare_emits_artifacts(tmp_path):
    cfg = _cfg(tmp_path)
    out = run_wall_thickness_compare(cfg)
    result = out["wall_thickness_compare"]["result"]

    assert result["overall_status"] in {"PASS", "FAIL"}
    assert result["governing_code"] is not None
    assert len(result["codes"]) == 3

    md = tmp_path / "test_pipeline_calcnote.md"
    js = tmp_path / "test_pipeline_results.json"
    html = tmp_path / "test_pipeline_calcnote.html"
    assert md.exists() and js.exists() and html.exists()

    saved = json.loads(js.read_text())
    assert saved["governing_code"] == result["governing_code"]


def test_run_compare_deterministic(tmp_path):
    r1 = run_wall_thickness_compare(_cfg(tmp_path / "a"))["wall_thickness_compare"]["result"]
    r2 = run_wall_thickness_compare(_cfg(tmp_path / "b"))["wall_thickness_compare"]["result"]
    assert r1 == r2


def test_compare_note_contains_matrix():
    geometry = PipeGeometry(outer_diameter=0.3239, wall_thickness=0.0191)
    material = PipeMaterial(grade="X65", smys=448e6, smts=531e6)
    loads = DesignLoads(internal_pressure=25e6, external_pressure=15e6)
    codes = [DesignCode.DNV_ST_F101, DesignCode.API_RP_1111, DesignCode.PD_8010_2]
    results = compare_codes(geometry, material, loads, codes, DesignFactors())

    note = build_compare_note(geometry, material, loads, results, "Test")
    assert "Code x check utilisation matrix" in note
    assert "DNV-ST-F101" in note
    assert "API-RP-1111" in note
    assert "PD-8010-2" in note
