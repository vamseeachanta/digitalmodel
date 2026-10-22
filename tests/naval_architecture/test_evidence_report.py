# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the evidence supplement and check independent arithmetic anchors."""

import importlib.util
import json
import math
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reporting/build_naval_evidence_report.py"
REPORT = ROOT / "docs/reports/modules/navalarchitecture/evidence"


@pytest.fixture
def builder():
    spec = importlib.util.spec_from_file_location("evidence_builder", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_rebuild_matches_committed_deliverables(builder, tmp_path):
    paths = builder.build(tmp_path)
    assert {p.name for p in paths} == {"report.html", "results.json", "changes.json"}
    for path in paths:
        assert path.read_bytes() == (REPORT / path.name).read_bytes()


def test_reference_answers_do_not_come_from_the_builder(builder):
    """Mechanics Map (2021), section 15.5; independent substitutions.

    At r=0.5,zeta=0: H=4/3. At r=0.5,zeta=0.05: H=sqrt(200/113).
    At r=1,zeta=0.05: H=10. At r=1,zeta=0: no finite H exists.
    The ratio input example has T/D=7/10=0.7. No rule limit is qualified.
    """
    data = builder.calculate()
    expected = [4 / 3, math.sqrt(200 / 113), 10.0, None]
    for row, value in zip(data["oscillator"], expected, strict=True):
        if value is None:
            assert row["value"] is None
            assert row["status"] == "UNBOUNDED"
        else:
            assert row["value"] == pytest.approx(value, rel=1e-14)
    assert data["partial"]["status"] == "NOT_EVALUATED"
    assert data["partial"]["checks"][4]["value"] == pytest.approx(0.7)
    assert data["empty_selection"]["status"] == "NOT_EVALUATED"
    assert data["missing_depth"]["missing_inputs"] == ["depth_m"]
    assert data["missing_modulus"]["missing_inputs"] == ["actual_z_cm3"]


def test_report_structure_and_bound_outputs(builder, tmp_path):
    paths = builder.build(tmp_path)
    page = paths[0].read_text()
    assert page.index('id="summary"') < page.index('id="revision-history"')
    assert page.index('id="revision-history"') < page.index('id="scope"')
    for section in builder.SECTIONS:
        assert f'id="{section}"' in page
    assert '<svg' in page and '<polyline' in page
    assert "100.0" in page  # Historical behavior documented by issue #2597.
    assert "ValueError" in page and "NOT_EVALUATED" in page
    assert "UNVERIFIED" in page and "Part A 2.3" in page
    assert 'src="http' not in page and '<link' not in page
    data = json.loads(paths[1].read_text())
    assert len(data["curves"]) > 1
    assert any(row["value"] is None for row in data["curves"])
    assert all(row["line"] > 0 for row in data["sweep"])


def test_figure_uses_module_results(builder, monkeypatch, tmp_path):
    first = builder.build(tmp_path / "first")[0].read_text()
    monkeypatch.setattr(builder, "simple_heave_rao", lambda *args: 2.0)
    second = builder.build(tmp_path / "second")[0].read_text()
    assert first.split("<svg", 1)[1].split("</svg>", 1)[0] != second.split("<svg", 1)[1].split("</svg>", 1)[0]
    assert builder.calculate()["curves"][0]["value"] == 2.0


def test_cli_rebuild_and_stale_output_refusal(tmp_path):
    command = [sys.executable, str(SCRIPT), "--output", str(tmp_path)]
    subprocess.run(command, check=True, capture_output=True)
    subprocess.run([*command, "--check"], check=True, capture_output=True)
    page = tmp_path / "report.html"
    page.write_text(page.read_text().replace("NOT_EVALUATED", "PASS"))
    result = subprocess.run([*command, "--check"], capture_output=True)
    assert result.returncode != 0


def test_sweep_includes_all_candidate_patterns(builder):
    rows = builder.calculate()["sweep"]
    assert any(r["path"].endswith("maneuvering_envelope_workflow.py") for r in rows)
    assert any(r["path"].endswith("hull_ingest.py") for r in rows)
    assert any(r["kind"] == "all" for r in rows)
    assert any(r["kind"] == "mapping_default" for r in rows)
    assert any(r["kind"] == "broad_handler" for r in rows)
