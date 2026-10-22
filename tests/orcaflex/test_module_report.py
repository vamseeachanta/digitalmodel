"""Rebuild and numerical provenance checks for the analytical module report."""
import importlib.util
import json
import re
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/orcaflex"


def load_builder():
    spec = importlib.util.spec_from_file_location("module_report", REPORT / "build_report.py")
    builder = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(builder)
    return builder


class Numbers(HTMLParser):
    def __init__(self):
        super().__init__()
        self.values = []
        self.key = None

    def handle_starttag(self, tag, attrs):
        self.key = dict(attrs).get("data-value")

    def handle_data(self, data):
        if self.key:
            self.values.append((self.key, data))

    def handle_endtag(self, tag):
        self.key = None


def test_rebuild_is_exact_and_self_contained(tmp_path):
    builder = load_builder()
    builder.build(tmp_path)
    for name in ("report.html", "changes.json"):
        assert (tmp_path / name).read_bytes() == (REPORT / name).read_bytes()
    html = (tmp_path / "report.html").read_text(encoding="utf-8")
    assert '<body class="hs-doc">' in html
    markup = re.sub(r"<style>.*?</style>", "", html, flags=re.S)
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)=', markup)
    assert "showSaveFilePicker" in html
    assert "report.comments.json" in html
    assert len(re.findall(r'<h2 id="s\d+">', html)) == 10
    assert "OrcFxAPI" not in sys.modules


def test_all_example_numbers_are_live_module_values():
    builder = load_builder()
    from digitalmodel.orcaflex.riser_config import SCRDesignInput
    data = builder.calculate()
    model = SCRDesignInput(**data["inputs"])
    assert data["geometry"] == model.calculate_geometry()
    assert data["weight"] == model.pipe.submerged_weight_per_m
    for case in data["chart"]:
        candidate = model.model_copy(update={"hang_off_angle_from_vertical": case["angle"]})
        assert case["tension"] == candidate.calculate_geometry()["top_tension_kN"]
    html = (REPORT / "report.html").read_text(encoding="utf-8")
    embedded = re.search(r'<script id="evidence" type="application/json">(.*?)</script>', html, re.S)
    assert json.loads(embedded.group(1)) == data
    assert f'<td>{data["inputs"]["pipe"]["grade"]}</td>' in html
    parser = Numbers()
    parser.feed(html)
    expected = builder.numeric_values(data)
    assert set(expected) == {key for key, _ in parser.values}
    for key, value in parser.values:
        assert value == builder.display_value(key, expected[key])


def test_numerical_mutation_changes_report():
    builder = load_builder()
    data = builder.calculate()
    before = builder.render(data)
    data["geometry"]["top_tension_kN"] += 1
    after = builder.render(data)
    assert before != after
    assert builder.display_value("geometry.top_tension_kN", data["geometry"]["top_tension_kN"]) in after


def test_style_is_copied_from_existing_exemplar():
    builder = load_builder()
    source = (ROOT / builder.STYLE_SOURCE).read_text(encoding="utf-8")
    style = re.search(r"<style>(.*?)</style>", source, re.S).group(1)
    assert style in builder.render(builder.calculate())


def test_reference_links_and_public_text():
    html = (REPORT / "report.html").read_text(encoding="utf-8")
    ids = re.findall(r'\bid="([^"]+)"', html)
    assert len(ids) == len(set(ids))
    for anchor in re.findall(r'href="#([^"]+)"', html):
        assert anchor in ids
    assert not re.search(r"\b(?:owner|lane|seat|hostname|session)\b", html, re.I)
    assert not re.search(r"\b[A-Za-z]:[/\\]", html)
    assert "<b>" not in html and "<strong>" not in html


def test_reference_calculation_agrees_at_module_precision():
    data = load_builder().calculate()
    for key, reference in data["reference"].items():
        assert data["geometry"][key] == round(reference, 1)


def test_checked_in_report_matches_read_only_rebuild():
    builder = load_builder()
    assert builder.render(builder.calculate()) == (REPORT / "report.html").read_text(encoding="utf-8")
    assert json.loads((REPORT / "changes.json").read_text(encoding="utf-8")) == builder.CHANGES


def test_source_links_resolve_inside_repository():
    html = (REPORT / "report.html").read_text(encoding="utf-8")
    markup = re.sub(r"<(style|script)[^>]*>.*?</\1>", "", html, flags=re.S)
    for target in re.findall(r'href="([^"#]+)"', markup):
        resolved = (REPORT / target).resolve()
        assert resolved.is_relative_to(ROOT)
        assert resolved.is_file()


def test_offline_controls_and_document_structure():
    html = (REPORT / "report.html").read_text(encoding="utf-8")
    markup = re.sub(r"<style>.*?</style>", "", html, flags=re.S)
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)=', markup)
    assert '<body class="hs-doc">' in html
    assert len(re.findall(r'<h2 id="s\d+">', html)) == 10
    assert "showSaveFilePicker" in html
    assert "report.comments.json" in html
    assert "OrcFxAPI" not in sys.modules


def test_invalid_geometry_is_rejected():
    from digitalmodel.orcaflex.riser_config import SCRDesignInput
    with pytest.raises(ValueError, match="above the seabed"):
        SCRDesignInput(water_depth=1, hang_off_elevation=-1).calculate_geometry()
