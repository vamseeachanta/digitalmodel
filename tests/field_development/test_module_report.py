# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the public report and check its independently worked example."""

import ast
import importlib.util
import json
import math
import re
from copy import deepcopy
from dataclasses import replace
from decimal import Decimal, localcontext
from html.parser import HTMLParser
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/fielddevelopment"
EVIDENCE = re.compile(
    r'<script id="evidence" type="application/json">(.*?)</script>', re.S
)
SENSITIVITY_LITERALS = [
    (37.5, 4785.5950237263), (45.0, 7037.6390790873),
    (52.5, 9289.6831344484), (60.0, 11541.7271898094),
    (67.5, 13793.7712451705), (75.0, 16045.8153005315),
    (82.5, 18297.8593558926), (90.0, 20549.9034112537),
    (97.5, 22801.9474666147), (105.0, 25053.9915219758),
    (112.5, 27306.0355773368),
]


def read_text(path):
    with path.open(encoding="utf-8", newline=None) as stream:
        return stream.read()


def builder():
    spec = importlib.util.spec_from_file_location("field_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def calculation():
    build = builder()
    try:
        return build, build.compute()
    except build.BackendUnavailable as exc:
        pytest.skip(str(exc))


def independent_schedule(price="75", capital="5450", operating="175"):
    """Decimal reference: 18 plateau years, 12 decline years, 30/50/20 CAPEX."""
    with localcontext() as context:
        context.prec = 50
        capital, operating = Decimal(capital), Decimal(operating)
        cashflows, production = [], []
        for year in range(31):
            factor = (Decimal(0) if year == 0 else Decimal(1) if year <= 18
                      else (-Decimal(2) * (year - 18)).exp())
            volume = Decimal("36.5") * factor
            cash = volume * Decimal(price) - operating * factor
            if year < 3:
                cash -= capital * [Decimal(".30"), Decimal(".50"), Decimal(".20")][year]
            if year == 30:
                cash -= capital * Decimal(".07")
            production.append(volume)
            cashflows.append(cash / Decimal("1.10") ** year)
        return float(sum(production)), float(sum(cashflows))


def assert_evidence_equal(actual, expected):
    """Compare evidence values numerically, preserving structure and booleans."""
    if isinstance(expected, dict):
        assert actual.keys() == expected.keys()
        for key in expected:
            assert_evidence_equal(actual[key], expected[key])
    elif isinstance(expected, list):
        assert len(actual) == len(expected)
        for left, right in zip(actual, expected):
            assert_evidence_equal(left, right)
    elif isinstance(expected, (int, float)) and not isinstance(expected, bool):
        assert actual == pytest.approx(expected, rel=1e-12, abs=0)
    else:
        assert actual == expected


def test_example_matches_public_workflow(calculation):
    from digitalmodel.field_development.workflow import evaluate_field_development

    build, data = calculation
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


def test_headlines_have_independent_worked_literals(calculation):
    _, data = calculation
    assert Decimal("5.0") * Decimal("1.09") * Decimal("1000") == Decimal("5450")
    assert data["costs"] == pytest.approx(
        {"capital": 5450.0, "operating": 175.0, "abandonment": 381.5}, rel=1e-12
    )
    assert data["production_mmbbl"] == pytest.approx(662.71289396, abs=1e-8, rel=0)
    assert data["npv"] == pytest.approx(16045.8153, abs=0.0001, rel=0)
    assert [row["score"] for row in data["ranking"]] == pytest.approx(
        [92.5, 92.5, 92.0, 53.9, 0.0]
    )
    production, npv = independent_schedule()
    assert data["production_mmbbl"] == pytest.approx(production, rel=1e-12)
    assert data["npv"] == pytest.approx(npv, rel=1e-12)
    assert data["reconciled_npv"] == pytest.approx(npv, rel=1e-12)
    assert data["tax_applied"] is False


@pytest.mark.parametrize("price, expected", SENSITIVITY_LITERALS)
def test_all_sensitivity_points_have_independent_literals(calculation, price, expected):
    _, data = calculation
    points = {point["price"]: point["npv"] for point in data["sensitivity"]}
    assert len(points) == 11
    _, decimal_npv = independent_schedule(str(price))
    assert decimal_npv == pytest.approx(expected, rel=1e-12)
    assert points[price] == pytest.approx(expected, rel=1e-12)


def test_tied_alternative_has_separate_economics(calculation):
    _, data = calculation
    assert data["ranking"][:2] == [
        {"host": "Spar", "score": 92.5}, {"host": "Semi", "score": 92.5}
    ]
    assert len(data["tied_alternatives"]) == 1
    alternative = data["tied_alternatives"][0]
    assert alternative["host"] == "Semi"
    assert alternative["npv"] == pytest.approx(13743.90, abs=0.005, rel=0)
    assert alternative["costs"] == pytest.approx(
        {"capital": 7630.0, "operating": 210.0, "abandonment": 534.1}, rel=1e-12
    )
    _, npv = independent_schedule(capital="7630", operating="210")
    assert alternative["npv"] == pytest.approx(npv, rel=1e-12)
    assert alternative["npv"] != pytest.approx(data["npv"])


def test_fallback_decline_and_assumptions_are_explicit(calculation):
    _, data = calculation
    assert data["eur_mmbbl"] == 60.0
    assert data["plateau_mmbbl"] == 657.0
    assert data["plateau_years"] == 18
    assert data["decline_rate"] == 2.0
    assert data["recovery_factor"] == 0.15
    assert data["plateau_fraction"] == 0.60
    assert data["abandonment_fraction"] == 0.07
    assert data["ranking_weights"] == {
        "depth": 0.4, "reservoir": 0.3, "distance": 0.2, "fluid": 0.1
    }
    assert data["production_to_eur"] == pytest.approx(11.0452148993, rel=1e-10)


def test_rebuild_evidence_and_all_visible_values(calculation):
    build, computed = calculation
    html, changes = build.render()
    committed = read_text(REPORT / "report.html")
    evidence = json.loads(EVIDENCE.search(html).group(1))
    assert_evidence_equal(evidence, computed)
    assert_evidence_equal(json.loads(EVIDENCE.search(committed).group(1)), computed)
    assert EVIDENCE.sub("", html) == EVIDENCE.sub("", committed)
    expected = build.display_values(computed)
    displayed = re.findall(r'<span data-value="([^"]+)">([^<]+)</span>', html)
    assert {key for key, _ in displayed} == set(expected)
    for key, value in displayed:
        assert value == build.format_value(expected[key], key)
    assert changes == read_text(REPORT / "changes.json")


def test_platform_ulps_do_not_change_rendered_bytes(calculation, monkeypatch):
    build, data = calculation
    original = build.render()
    perturbed = deepcopy(data)
    for key in ("npv", "reconciled_npv"):
        perturbed[key] = math.nextafter(perturbed[key], math.inf)
    for point in perturbed["sensitivity"]:
        point["npv"] = math.nextafter(point["npv"], math.inf)
    monkeypatch.setattr(build, "compute", lambda: perturbed)
    assert build.render() == original
    evidence = json.loads(EVIDENCE.search(original[0]).group(1))
    assert_evidence_equal(evidence, perturbed)
    perturbed["npv"] += 0.01
    with pytest.raises(AssertionError):
        assert_evidence_equal(evidence, perturbed)


def test_report_roundtrip_is_utf8_with_lf(calculation, tmp_path):
    build, _ = calculation
    expected = dict(zip(("report.html", "changes.json"), build.render()))
    build.build(tmp_path)
    for name, text in expected.items():
        raw = (tmp_path / name).read_bytes()
        assert b"\r" not in raw
        assert raw.decode("utf-8") == text
        assert raw.endswith(b"\n")


def test_crlf_sources_rebuild_identically(calculation, monkeypatch, tmp_path):
    build, _ = calculation
    expected = build.render()
    paths = [path for path, _ in build.SOURCES]
    paths += ["docs/reports/modules/fielddevelopment/" + name
              for name in ("report_body.html.in", "interaction.js")]
    for relative in paths:
        destination = tmp_path / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_text(read_text(ROOT / relative), encoding="utf-8", newline="\r\n")
    monkeypatch.setattr(build, "ROOT", tmp_path)
    monkeypatch.setattr(build, "HERE", tmp_path / REPORT.relative_to(ROOT))
    assert build.render() == expected


def test_builder_imports_only_public_model_symbols():
    tree = ast.parse(read_text(REPORT / "build_report.py"))
    for node in ast.walk(tree):
        if isinstance(node, ast.ImportFrom) and (node.module or "").startswith("digitalmodel"):
            assert all(not alias.name.startswith("_") for alias in node.names)


def test_optional_backend_is_an_explicit_unavailable_case(monkeypatch):
    build = builder()
    original = build.importlib.util.find_spec
    monkeypatch.setattr(
        build.importlib.util, "find_spec",
        lambda name, *args: object() if name == "worldenergydata" else original(name, *args),
    )
    with pytest.raises(build.BackendUnavailable, match="fallback"):
        build.compute()


def test_report_contract():
    html = read_text(REPORT / "report.html")
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
    assert "ASSUMED - to be confirmed" in html
    for tag in ("header", "footer"):
        current = re.search(rf"<{tag}\b.*?</{tag}>", html, re.S).group(0)
        assert "client review" not in current.lower()
    assert "production-volume limitation are presented for client review." in html
    history = re.search(r"Revision history.*?<table>(.*?)</table>", html, re.S).group(1)
    assert "2026-10-21" in history
    assert "2026-10-22" in history
    assert "30.00 years" not in html
    assert "100,000.00" not in html


def test_tie_and_fallback_disclosed_in_result_sections():
    html = read_text(REPORT / "report.html")
    sections = {match.group(1): match.group(2) for match in re.finditer(
        r'<h2 id="([^"]+)">.*?</h2>(.*?)(?=<h2|$)', html, re.S
    )}
    for section in ("summary", "results"):
        text = re.sub(r"<[^>]+>", " ", sections[section]).lower()
        assert "tie" in text and "spar" in text and "semi" in text
        assert "order" in text
    assert "selected Spar" in sections["summary"]
    caption = re.search(r'<p class="sub">Table B\.(.*?)</p>', html, re.S).group(1).lower()
    assert "tie" in caption and "order" in caption
    method = re.sub(r"<[^>]+>", " ", sections["method"]).lower()
    assert "fallback" in method
    assert "derived decline rate" not in method
    for marker in ("eur", "plateau"):
        assert marker in method
    assert "13,743.90" in html


def test_tables_cite_model_sources_without_circular_locators():
    html = read_text(REPORT / "report.html")
    tables = re.findall(r"<table>.*?</table>", html, re.S)
    numeric_tables = [table for table in tables if "data-value=" in table]
    assert len(numeric_tables) >= 3
    for table in numeric_tables:
        assert "§9.2" not in table
        assert ".py" in table


def test_sources_and_style_are_bound():
    build = builder()
    html = read_text(REPORT / "report.html")
    for record in build.source_records():
        assert (ROOT / record["path"]).is_file()
        assert record["digest"] in html
    exemplar = read_text(ROOT / build.EXEMPLAR)
    assert re.search(r"<style>.*?</style>", exemplar, re.S).group(0) in html


def test_plot_readback_zero_line_and_public_text(calculation):
    build, data = calculation
    html = read_text(REPORT / "report.html")

    class Readback(HTMLParser):
        def __init__(self):
            super().__init__()
            self.hidden = False
            self.text = []
            self.points = []
            self.zero = None

        def handle_starttag(self, tag, attrs):
            attrs = dict(attrs)
            if tag in {"script", "style"}:
                self.hidden = True
            if "data-point" in attrs:
                self.points.append(attrs)
            if attrs.get("id") == "zero-limit":
                self.zero = attrs

        def handle_endtag(self, tag):
            if tag in {"script", "style"}:
                self.hidden = False

        def handle_data(self, text):
            if not self.hidden:
                self.text.append(text)

    parser = Readback()
    parser.feed(html)
    assert len(parser.points) == len(data["sensitivity"])
    assert 60 < float(parser.zero["y1"]) < 300
    assert parser.zero["y1"] == parser.zero["y2"]
    for mark, point in zip(parser.points, data["sensitivity"]):
        assert build.format_value(point["price"]) in mark["aria-label"]
        assert build.format_value(point["npv"]) in mark["aria-label"]
        assert "limit zero million USD" in mark["aria-label"]
    text = " ".join(parser.text)
    assert not re.search(r"\b(owner|lane|seat|session|could|should|would)\b", text, re.I)
    assert not re.search(r"[A-Za-z]:[/\\]|\\\\[A-Za-z]", html)
