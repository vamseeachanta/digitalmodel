"""Rebuild and verify the public field-development worked example."""

import importlib.util
import json
import re
from dataclasses import replace
from html.parser import HTMLParser
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/fielddevelopment"


def builder():
    spec = importlib.util.spec_from_file_location("field_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_example_matches_public_workflow():
    from digitalmodel.field_development.workflow import evaluate_field_development

    build = builder()
    data = build.compute()
    spec = build.example_spec()
    result = evaluate_field_development(spec)
    assert data["selected"] == result.evaluated_host
    assert data["costs"] == {
        "capital": result.economics.costs.capex_usd_mm,
        "operating": result.economics.costs.opex_usd_mm_per_yr,
        "abandonment": result.economics.costs.abex_usd_mm,
    }
    assert data["npv"] == result.economics.metrics.npv_usd_mm
    assert data["ranking"] == [
        {"host": option.host_type.value, "score": option.score}
        for option in result.concept.ranked_options
    ]
    for point in data["sensitivity"]:
        actual = evaluate_field_development(
            replace(spec, oil_price_usd_per_bbl=point["price"])
        )
        assert point["npv"] == actual.economics.metrics.npv_usd_mm


def test_rebuild_and_all_visible_values():
    # In-memory rebuilding needs no temporary-directory ACLs or output mutation.
    build = builder()
    html, changes = build.render()
    data = json.loads(re.search(
        r'<script id="evidence" type="application/json">(.*?)</script>', html, re.S
    ).group(1))
    assert data == build.compute()
    expected = build.display_values(data)
    displayed = re.findall(r'<span data-value="([^"]+)">([^<]+)</span>', html)
    assert set(key for key, _ in displayed) == set(expected)
    for key, value in displayed:
        assert value == build.format_value(expected[key])
    assert html == (REPORT / "report.html").read_text(encoding="utf-8")
    assert changes == (REPORT / "changes.json").read_text(encoding="utf-8")


def test_report_contract():
    html = (REPORT / "report.html").read_text(encoding="utf-8")
    assert '<body class="hs-doc">' in html
    assert len(re.findall(r'<h2\b', html)) == 10
    class Resources(HTMLParser):
        def handle_starttag(self, tag, attrs):
            if tag in {"script", "link", "img"}:
                assert not ({"src", "href"} & dict(attrs).keys())

    Resources().feed(html)
    assert "showSaveFilePicker" in html
    assert "textContent" in html
    assert "NOT_EVALUATED" in html
    assert 'id="zero-limit"' in html
    assert 'data-point=' in html
    assert "ASSUMED - to be confirmed" in html


def test_economic_reconciliation():
    build = builder()
    data = build.compute()
    assert data["reconciled_npv"] == pytest.approx(data["npv"], rel=1e-12)
    assert data["production_mmbbl"] > data["inputs"]["reservoir_size_mmbbl"]
    assert data["tax_applied"] is False
    assert data["selected"] == "Spar"  # The physical schematic depicts this host.


def test_sources_and_style_are_bound():
    build = builder()
    html = (REPORT / "report.html").read_text(encoding="utf-8")
    for record in build.source_records():
        assert (ROOT / record["path"]).is_file()
        assert record["digest"] in html
    exemplar = (ROOT / build.EXEMPLAR).read_text(encoding="utf-8")
    assert re.search(r'<style>.*?</style>', exemplar, re.S).group(0) in html


def test_plot_readback_and_public_text():
    build = builder()
    data = build.compute()
    html = (REPORT / "report.html").read_text(encoding="utf-8")

    class Readback(HTMLParser):
        def __init__(self):
            super().__init__()
            self.hidden = False
            self.text = []
            self.points = []

        def handle_starttag(self, tag, attrs):
            attrs = dict(attrs)
            if tag in {"script", "style"}:
                self.hidden = True
            if "data-point" in attrs:
                self.points.append(attrs)

        def handle_endtag(self, tag):
            if tag in {"script", "style"}:
                self.hidden = False

        def handle_data(self, text):
            if not self.hidden:
                self.text.append(text)

    parser = Readback()
    parser.feed(html)
    assert len(parser.points) == len(data["sensitivity"])
    for mark, point in zip(parser.points, data["sensitivity"]):
        assert build.format_value(point["price"]) in mark["aria-label"]
        assert build.format_value(point["npv"]) in mark["aria-label"]
        assert "limit zero million USD" in mark["aria-label"]
    text = " ".join(parser.text)
    assert not re.search(r"\b(owner|lane|seat|session|could|should|would)\b", text, re.I)
    assert not re.search(r"[A-Za-z]:[/\\]|\\\\[A-Za-z]", html)
