"""Report data, analytical anchors and portable rebuild contracts."""

import importlib.util
import json
import re
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
FOLDER = ROOT / "docs/reports/modules/riserconfigurationscreen"
REBUILD = "python scripts/reports/build_riser_configuration_report.py"


def builder():
    spec = importlib.util.spec_from_file_location(
        "riser_report", ROOT / "scripts/reports/build_riser_configuration_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_report_rebuild():
    rebuilt = builder().render_report()
    assert rebuilt == (FOLDER / "report.html").read_text(
        encoding="utf-8"), f"Report is stale; rebuild with: {REBUILD}"
    assert b"\r\n" not in rebuilt.encode("utf-8")


def test_all_example_numbers_match_module():
    module = builder()
    data = module.calculate()
    text = (FOLDER / "report.html").read_text(encoding="utf-8")
    embedded = json.loads(re.search(
        r'<script id="report-data" type="application/json">(.*?)</script>',
        text, re.S).group(1))
    assert embedded == module.display_data(data), f"Rebuild with: {REBUILD}"
    for path, digits, shown in re.findall(
            r'data-number="([^"]+)" data-digits="(\d+)">([^<]+)', text):
        value = data
        for key in path.split("."):
            value = value[int(key)] if isinstance(value, list) else value[key]
        assert shown == module.number(value, int(digits)), path


def test_hand_calculation_anchors():
    row = builder().calculate()["anchor"]
    # Assumed depth 100 m and angle 30 degrees: sin(angle)=1/2;
    # a = depth*sin(angle)/(1-sin(angle)) = 100 m.
    assert row["touchdown_curvature_per_m"] == pytest.approx(0.01, rel=1e-7)
    # Weight 10 N/m gives H=w*a = 1000 N.
    assert row["horizontal_tension_n"] == pytest.approx(1000.0, rel=1e-7)
    # T_top = w*(a+depth) = 2000 N, from vertical equilibrium.
    assert row["top_tension_n"] == pytest.approx(2000.0, rel=1e-7)


def test_structure_and_public_wording():
    text = (FOLDER / "report.html").read_text(encoding="utf-8")
    assert 'class="hs-doc"' in text
    assert len(re.findall(r'<h2 id="s\d+">', text)) == 10
    assert 'id="comments"' in text and "showSaveFilePicker" in text
    assert "Draft for maintainer review" in text
    assert "ASSUMED - to be confirmed" in text
    assert "<script src=" not in text and '<link rel="stylesheet"' not in text
    visible = re.sub(r"<(script|style)\b.*?</\1>", "", text, flags=re.S)
    visible = re.sub(r"<[^>]+>", " ", visible)
    assert not re.search(r"\b(owner|client|lane|seat|session|could|should|would)\b", visible, re.I)
    assert "<strong" not in text and "<b>" not in text


def test_style_and_changes_are_embedded():
    text = (FOLDER / "report.html").read_text(encoding="utf-8")
    changes = json.loads((FOLDER / "changes.json").read_text(encoding="utf-8"))
    assert changes["changes"][0]["description"] in text
    assert "--brand:" in text
    assert "Limit" in text and "<title>" in text
