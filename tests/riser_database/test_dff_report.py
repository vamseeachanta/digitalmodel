# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the DFF example; independent hand result is 1/4=.25, .2*4=.8.

All inputs are synthetic caller assumptions. The comparison checks software
arithmetic, not DNV-OS-F201 class factors or a physical fatigue analysis.
"""
import importlib.util
import json
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "dff_report", ROOT / "scripts/reports/build_riser_dff_example.py"
)
BUILDER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(BUILDER)


def test_rebuild_matches_committed_artifacts(tmp_path):
    BUILDER.build(tmp_path)
    for name in ("fatigue-factor-analysis.html", "changes.json"):
        assert (tmp_path / name).read_bytes() == (BUILDER.DESTINATION / name).read_bytes(), (
            "Rebuild with PYTHONPATH=src python scripts/reports/build_riser_dff_example.py"
        )


def test_example_calls_production_arithmetic():
    result = BUILDER.example()
    # Synthetic F=4; hand allowable=1/4=.25 and usage=.2*4=.8.
    assert result["factor"] == pytest.approx(4.0)
    assert result["allowable_damage"] == pytest.approx(0.25)
    assert result["usage_factor"] == pytest.approx(0.8)
    assert result["passes_dff"] is True
    assert result["standard_status"] == "NOT_EVALUATED"
    assert "class value 4.0" in result["citation_note"]
    assert "normal" in result["citation_note"]


def test_report_qualifies_its_verdict_and_records_changes(tmp_path):
    BUILDER.build(tmp_path)
    report = (tmp_path / "fatigue-factor-analysis.html").read_text(encoding="utf-8")
    assert "Arithmetic verdict: PASS" in report
    assert "Standards verdict: NOT_EVALUATED" in report
    assert report.index("Summary") < report.index("Revision history") < report.index("Inputs")
    changes = json.loads((tmp_path / "changes.json").read_text(encoding="utf-8"))
    assert changes["revision"] == "1"
    assert changes["changes"]


@pytest.mark.parametrize("platform_newline", ["\n", "\r\n"])
def test_report_writes_utf8_lf_with_platform_text_defaults(tmp_path, monkeypatch, platform_newline):
    original = Path.write_text

    def platform_write(path, data, encoding=None, errors=None, newline=None):
        return original(
            path, data, encoding=encoding, errors=errors,
            newline=platform_newline if newline is None else newline,
        )

    monkeypatch.setattr(Path, "write_text", platform_write)
    BUILDER.build(tmp_path)
    for name in ("fatigue-factor-analysis.html", "changes.json"):
        raw = (tmp_path / name).read_bytes()
        assert b"\r" not in raw
        assert raw.decode("utf-8").endswith("\n")
