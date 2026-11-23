"""Tests for scripts/build_results_workbook.py — multi-tab xlsx from a results dir."""

from __future__ import annotations

import importlib.util
import json
from pathlib import Path

import pandas as pd
import pytest

REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT = REPO_ROOT / "scripts" / "build_results_workbook.py"


@pytest.fixture
def builder():
    spec = importlib.util.spec_from_file_location("build_results_workbook", SCRIPT)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    mod._used_names.clear()
    return mod


def _orcawave_dir(tmp_path: Path) -> Path:
    d = tmp_path / "owv"
    d.mkdir()
    (d / "RAOs.csv").write_text(
        "Frequency_radps,Heading_deg,Roll,Yaw\n0.3,90,66.44,66.79\n")
    (d / "AddedMass.csv").write_text("Frequency_radps,A33\n0.3,2.41e6\n")
    (d / "UnitBoxRAO_validation.json").write_text(json.dumps({
        "overall_status": "WARNING",
        "schema_validation": {"added_mass": ["Matrix at frequency 0.3000 is not symmetric"]},
        "physical_validity": {"raos": ["Roll: Maximum RAO magnitude 66.44 deg/m exceeds typical range"]},
    }))
    (d / "UnitBoxRAO.owr").write_bytes(b"BINARY-RESULT")  # heavy: must NOT be embedded
    return d


def test_workbook_has_expected_tabs_and_keeps_heavy_local(tmp_path, builder):
    out = tmp_path / "report.xlsx"
    result_json = tmp_path / "result.json"
    result_json.write_text(json.dumps({"run_id": "lr_acma_x", "state": "finished",
                                        "returncode": 0, "audit": {"scope": "acma"}}))

    builder.main([str(_orcawave_dir(tmp_path)),
                  "--result-json", str(result_json), "--out", str(out)])

    xl = pd.ExcelFile(out)
    assert "Index" in xl.sheet_names
    assert "Run Metadata" in xl.sheet_names
    assert "RAOs" in xl.sheet_names
    assert "AddedMass" in xl.sheet_names
    assert "Validation" in xl.sheet_names
    assert "Artifacts (local)" in xl.sheet_names

    # heavy .owr is referenced (path+size), never embedded as a data tab
    artifacts = pd.read_excel(out, "Artifacts (local)")
    assert artifacts["suffix"].str.contains(".owr").any()
    assert "UnitBoxRAO" not in [s for s in xl.sheet_names]  # no tab made from the binary

    # detailed numbers survive the round-trip
    raos = pd.read_excel(out, "RAOs")
    assert raos["Roll"].max() == pytest.approx(66.44)
    val = pd.read_excel(out, "Validation")
    assert (val["section"] == "OVERALL").any()


def test_sheet_names_are_sanitized_and_unique(builder):
    a = builder._sheet_name("this/is:a*very[long]name-that-exceeds-31-characters")
    b = builder._sheet_name("this/is:a*very[long]name-that-exceeds-31-characters")
    assert len(a) <= 31 and len(b) <= 31
    assert a != b
    for ch in "[]:*?/\\":
        assert ch not in a


def test_missing_dir_fails_closed(tmp_path, builder):
    with pytest.raises(SystemExit):
        builder.main([str(tmp_path / "nope"), "--out", str(tmp_path / "x.xlsx")])
