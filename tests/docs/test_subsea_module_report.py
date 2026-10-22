# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the client report and reconcile its numbers to the public API."""

import importlib.util
import json
import math
from pathlib import Path

import pytest
from bs4 import BeautifulSoup

ROOT = Path(__file__).resolve().parents[2]
BUILDER = ROOT / "scripts/generate_subsea_module_report.py"
REPORT = ROOT / "docs/reports/modules/subsea/subsea-module-report.html"


def builder():
    spec = importlib.util.spec_from_file_location("subsea_report_builder", BUILDER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_report_rebuild_and_every_example_number(tmp_path):
    module = builder()
    target = tmp_path / "subsea-module-report.html"
    module.build_report(target)
    assert target.read_bytes() == REPORT.read_bytes()
    soup = BeautifulSoup(target.read_text(encoding="utf-8"), "html.parser")
    inputs, riser, result, weight = module.calculate_example()
    # Re-evaluate through public APIs, independently of template substitutions.
    from digitalmodel.subsea.catenary_riser import (
        EffectiveWeightCalculator, SimpleCatenaryAnalyzer,
    )
    direct = SimpleCatenaryAnalyzer().analyze_riser(
        riser, water_depth=inputs["water_depth"],
        horizontal_offset=inputs["horizontal_offset"],
    )
    direct_weight = EffectiveWeightCalculator().calculate(riser)
    expected = module.example_numbers(inputs, riser, direct, direct_weight)
    cells = soup.select("[data-number]")
    assert {cell["data-number"] for cell in cells} == set(expected)
    for cell in cells:
        key = cell["data-number"]
        assert float(cell["data-value"]) == expected[key]
        assert cell.get_text() == module.format_number(key, expected[key])
    # No hand-entered numerical literals can enter worked-example prose.
    worked = soup.select_one("#worked-example")
    for cell in worked.select("[data-number]"):
        cell.decompose()
    assert not any(char.isdigit() for char in worked.get_text())
    assert result.converged and weight.effective_weight > 0


def test_public_reference_and_curve_arrays_match_module():
    module = builder()
    reference = module.public_reference()
    assert reference["offset_m"] == pytest.approx(reference["expected_offset_m"], abs=1e-7)
    assert reference["length_m"] == pytest.approx(reference["expected_length_m"], abs=1e-7)
    maximum_difference = max(abs(reference["offset_difference_pct"]), abs(reference["length_difference_pct"]))
    assert reference["difference_bound_pct"] == math.ceil(maximum_difference * 10) / 10
    assert maximum_difference <= reference["difference_bound_pct"]
    data = module.chart_data()
    inputs, riser, result, _ = module.calculate_example()
    from digitalmodel.subsea.catenary_riser import SimpleCatenaryAnalyzer
    analyzer = SimpleCatenaryAnalyzer()
    arc, tension = analyzer.calculate_tension_along_riser(result, len(data["arc_m"]))
    assert data["arc_m"] == list(arc)
    assert data["tension_kN"] == list(tension / 1000)
    assert data["length_limit_m"] == riser.length
    assert max(data["required_length_m"]) == pytest.approx(riser.length)
    for offset, required in zip(data["offset_m"], data["required_length_m"], strict=True):
        checked = analyzer.analyze_riser(riser, water_depth=inputs["water_depth"], horizontal_offset=offset)
        assert required == checked.arc_length
    assert math.isfinite(data["offset_limit_m"])


def test_house_style_sections_sources_and_offline_assets():
    module = builder()
    html = REPORT.read_text(encoding="utf-8")
    assert all(line.rstrip() == line for line in html.splitlines())
    soup = BeautifulSoup(html, "html.parser")
    assert "hs-doc" in soup.body["class"]
    assert len(soup.select("main > section")) == 10
    assert not soup.select("h4, h5, h6, b, strong, script[src], link[rel=stylesheet]")
    assert soup.select_one("#revision-history")
    assert soup.select_one("#comment-text") and soup.select_one("#save-comments")
    assert soup.select_one("#changes-panel")
    assert "showDirectoryPicker" in soup.get_text() or "showDirectoryPicker" in str(soup)
    for link in soup.select('a[href^="#"]'):
        assert soup.find(id=link["href"][1:]), link
    exemplar = (ROOT / module.EXEMPLAR).read_text(encoding="utf-8")
    import re
    style = re.search(r"<style>(.*?)</style>", exemplar, re.S).group(1)
    assert style.strip() in str(soup)
    changes = json.loads((REPORT.parent / "changes.json").read_text())
    assert changes["revisions"][0]["revision"] == "00"


def test_visible_client_text_uses_engineering_register():
    soup = BeautifulSoup(REPORT.read_text(encoding="utf-8"), "html.parser")
    for node in soup.select("script, style"):
        node.decompose()
    text = soup.get_text(" ", strip=True)
    import re
    assert not re.search(r"\b(owner|agent|lane|job|could|should|would|certified|compliant)\b", text, re.I)
    assert not re.search(r"(?:/(?:home|mnt)/|[\w.+-]+@[\w.-]+\.[A-Za-z]{2,})", text)


def test_missing_design_limits_never_appear_as_acceptance():
    soup = BeautifulSoup(REPORT.read_text(encoding="utf-8"), "html.parser")
    assert "Strength and fatigue acceptance are not established" in soup.get_text()
    assert "NOT_EVALUATED" in soup.get_text()
    assert "ASSUMED - to be confirmed" in soup.get_text()


@pytest.mark.parametrize("replacement", ["OTHER_MATERIAL", "factory()"])
def test_changed_documented_material_is_rejected(tmp_path, monkeypatch, replacement):
    module = builder()
    original = (ROOT / module.README).read_text()
    example = tmp_path / "example.md"
    example.write_text(original.replace("material=STEEL_API_5L_X65,", f"material={replacement},"))
    monkeypatch.setattr(module, "README", example)
    with pytest.raises(ValueError, match="documented input"):
        module.example_inputs()


def test_missing_house_style_is_reported(tmp_path, monkeypatch):
    module = builder()
    exemplar = tmp_path / "exemplar.html"
    exemplar.write_text("<html><body>No stylesheet.</body></html>")
    monkeypatch.setattr(module, "EXEMPLAR", str(exemplar))
    with pytest.raises(ValueError, match="stylesheet"):
        module.build_report(tmp_path / "report.html")
