"""Rebuild and verify the public field-development worked example."""

import importlib.util
import json
import math
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
    assert_numeric_evidence(data, build.compute())
    expected = build.display_values(data)
    displayed = re.findall(r'<span data-value="([^"]+)">([^<]+)</span>', html)
    assert set(key for key, _ in displayed) == set(expected)
    for key, value in displayed:
        assert value == build.format_value(expected[key], key)
    committed = (REPORT / "report.html").read_text(encoding="utf-8")
    assert_numeric_evidence(evidence(html), evidence(committed))
    assert without_evidence(html) == without_evidence(committed)
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
    # Independent hand values, rather than another call to module helpers.
    assert data["costs"] == pytest.approx({
        "capital": 5450.00,  # 5.0 * 1.09 * 1000
        "operating": 175.00,
        "abandonment": 381.50,
    })
    assert data["production_mmbbl"] == pytest.approx(662.71289396, abs=1e-8)
    assert data["npv"] == pytest.approx(16045.8153, abs=0.00005)
    assert [row["score"] for row in data["ranking"]] == [92.5, 92.5, 92.0, 53.9, 0.0]
    assert data["tax_applied"] is False
    assert data["selected"] == data["ranking"][0]["host"]
    assert data["payback_years"] == 3


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


EVIDENCE = r'(<script id="evidence" type="application/json">)(.*?)(</script>)'


def evidence(html):
    return json.loads(re.search(EVIDENCE, html, re.S).group(2))


def without_evidence(html):
    return re.sub(EVIDENCE, r"\1\3", html, flags=re.S)


def assert_numeric_evidence(actual, expected):
    if isinstance(expected, dict):
        assert actual.keys() == expected.keys()
        for key in expected:
            assert_numeric_evidence(actual[key], expected[key])
    elif isinstance(expected, list):
        assert len(actual) == len(expected)
        for a, e in zip(actual, expected):
            assert_numeric_evidence(a, e)
    elif isinstance(expected, (int, float)) and not isinstance(expected, bool):
        assert actual == pytest.approx(expected, rel=1e-12, abs=0)
    else:
        assert actual == expected


def test_embedded_precision_ignores_platform_ulps(monkeypatch):
    build = builder()
    data = build.compute()
    monkeypatch.setattr(build, "compute", lambda: data)
    original, _ = build.render()
    data["npv"] = math.nextafter(data["npv"], math.inf)
    for point in data["sensitivity"]:
        point["npv"] = math.nextafter(point["npv"], math.inf)
    perturbed, _ = build.render()
    assert original == perturbed
    assert evidence(original)["npv"] == 16045.815300532


def test_tie_fallback_and_presentation():
    build = builder()
    data = build.compute()
    assert data["tied_alternatives"][0]["host"] == "Semi"
    assert data["tied_alternatives"][0]["npv"] == pytest.approx(13743.8978, abs=0.00005)
    assert data["eur_mmbbl"] == 60.0
    assert data["plateau_mmbbl"] == 657.0
    assert data["decline_rate"] == 2.0
    assert data["eur_multiple"] == pytest.approx(11.0452149)
    html, changes = build.render()
    assert html.count("stable sort") >= 3
    assert "fallback constant" in html
    assert "14 significant digits" in html
    assert "Draft for technical review" in html
    assert "Draft for client review" not in html
    assert "2026-10-22" in html
    assert build.format_value(30, "field_life_years") == "30"
    assert build.format_value(100000.0, "production_capacity_bopd") == "100,000"
    assert build.format_value(data["excess_percent"], "excess_percent") == "65.7"
    zero = re.search(r'<line id="zero-limit"[^>]*y1="([^"]+)"', html)
    assert float(zero.group(1)) < 300
    assert json.loads(changes)["revision"] == "1"


def test_independent_price_sensitivity():
    from decimal import Decimal, localcontext

    data = builder().compute()
    with localcontext() as context:
        context.prec = 40
        factors = [Decimal(0)] + [Decimal(1)] * 18 + [
            (-Decimal(2) * year).exp() for year in range(1, 13)
        ]
        for point in data["sensitivity"]:
            cash = [-Decimal("5450") * Decimal("0.30")]
            for year in range(1, 31):
                net = (Decimal("36.5") * Decimal(str(point["price"]))
                       - Decimal("175")) * factors[year]
                if year == 1:
                    net -= Decimal("5450") * Decimal("0.50")
                if year == 2:
                    net -= Decimal("5450") * Decimal("0.20")
                if year == 30:
                    net -= Decimal("381.50")
                cash.append(net)
            expected = sum(value / Decimal("1.1") ** year
                           for year, value in enumerate(cash))
            assert point["npv"] == pytest.approx(float(expected), rel=1e-12)


def test_builder_has_no_private_module_imports():
    import ast

    tree = ast.parse((REPORT / "build_report.py").read_text(encoding="utf-8"))
    for node in ast.walk(tree):
        if isinstance(node, ast.ImportFrom):
            assert all(not name.name.startswith("_") for name in node.names)


def test_optional_backend_is_an_explicit_skip(monkeypatch):
    from unittest import SkipTest

    build = builder()
    monkeypatch.setattr(build.importlib.util, "find_spec", lambda name: object())
    with pytest.raises(SkipTest, match="local fallback"):
        build.compute()


@pytest.mark.parametrize("path", [
    "docs/reports/modules/fielddevelopment/report.html",
    "docs/reports/modules/fielddevelopment/build_report.py",
    "docs/reports/modules/fielddevelopment/report_body.html.in",
    "docs/reports/modules/fielddevelopment/interaction.js",
    "docs/reports/modules/fielddevelopment/changes.json",
    "docs/api/cfd/turbulent-flat-plate-verification.html",
    "docs/api/_assets/brand.css",
    "docs/domains/references/field_development/catalog/README.md",
])
def test_report_dependencies_route_to_field_development(path):
    import sys

    spec = importlib.util.spec_from_file_location(
        "report_domain_detector", ROOT / "scripts/ci/detect_touched_domains.py"
    )
    detector = importlib.util.module_from_spec(spec)
    monkeypatch = pytest.MonkeyPatch()
    with monkeypatch.context() as patch:
        patch.setitem(sys.modules, spec.name, detector)
        spec.loader.exec_module(detector)
        domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
        names = {domain.name for domain in detector.touched_domains([path], domains)}
    assert "field-development" in names
