"""Report evidence checks; run only the uncertainty test directory."""
import importlib.util
import json
import re
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from digitalmodel.uncertainty import (
    Prior, percentile_summary, run_matrix, sample_lhs, spearman_ranks, tornado,
)

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/uncertainty"
COMMAND = "python docs/reports/modules/uncertainty/build_report.py"


def builder():
    spec = importlib.util.spec_from_file_location("uncertainty_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_hand_calculated_anchors():
    data = builder().calculate()
    # y = a*x+b with a=3, x=4, and b=1 gives 12+1=13.
    assert data["anchors"][0]["y"] == 13.0
    # With b=1.5, y=12+1.5=13.5.
    assert data["anchors"][1]["y"] == 13.5
    # With b=2, y=12+2=14.
    assert data["anchors"][2]["y"] == 14.0
    # E[y]=4*3+(1+2)/2=13.5; Var[y]=16/4+1/12=49/12.
    assert data["reference_mean"] == 13.5
    assert data["reference_variance"] == pytest.approx(49 / 12)


def test_every_example_number_matches_module():
    data = builder().calculate()
    samples = sample_lhs([
        Prior(name="a", dist="normal", mean=3, std=0.5),
        Prior(name="b", dist="uniform", low=1, high=2),
    ], n_samples=2000, seed=2026)
    result = run_matrix(lambda row: {"y": 4 * row["a"] + row["b"]}, samples)
    np.testing.assert_allclose(data["rows"], result[["a", "b", "y"]], rtol=0, atol=1e-12)
    assert data["mean"] == pytest.approx(result.y.mean(), abs=1e-12)
    assert data["variance"] == pytest.approx(result.y.var(ddof=0), abs=1e-12)
    assert data["percentiles"] == percentile_summary(result, "y")
    # JSON-style records do not retain platform-specific integer widths.
    pd.testing.assert_frame_equal(pd.DataFrame(data["tornado"]), tornado(result, ["a", "b"], "y"), check_dtype=False)
    pd.testing.assert_frame_equal(pd.DataFrame(data["spearman"]), spearman_ranks(result, ["a", "b"], "y"))
    assert abs(data["mean_difference_pct"]) <= data["mean_limit_pct"]
    assert abs(data["variance_difference_pct"]) <= data["variance_limit_pct"]


def test_rebuild_and_displayed_numbers(tmp_path):
    module = builder()
    module.build(tmp_path)
    actual = (tmp_path / "index.html").read_text(encoding="utf-8")
    saved = (REPORT / "index.html").read_text(encoding="utf-8")
    assert actual == saved, f"Report is stale; rebuild with: {COMMAND}"
    data = module.calculate()
    for key, text in re.findall(r'data-value="([a-z_]+)">([^<]+)</span>', actual):
        assert float(text) == pytest.approx(data[key], abs=0.00051)
    assert len(re.findall(r'<section id="section-', actual)) == 10
    assert '<body class="hs-doc">' in actual
    assert "showDirectoryPicker" in actual
    assert "createWritable" in actual
    assert '<script src=' not in actual
    assert (tmp_path / "changes.json").read_text(encoding="utf-8") == (
        REPORT / "changes.json").read_text(encoding="utf-8")


def test_built_public_text_and_links():
    from html.parser import HTMLParser

    class Visible(HTMLParser):
        hidden = False
        text = []

        def handle_starttag(self, tag, attrs):
            if tag in {"script", "style"}:
                self.hidden = True

        def handle_endtag(self, tag):
            if tag in {"script", "style"}:
                self.hidden = False

        def handle_data(self, value):
            if not self.hidden:
                self.text.append(value)

    html = (REPORT / "index.html").read_text(encoding="utf-8")
    parser = Visible()
    parser.feed(html)
    text = " ".join(parser.text)
    assert not re.search(r"\b(owner|client|lane|agent|session|solver|could|should|would)\b", text, re.I)
    assert "<strong>" not in html and "<b>" not in html
    ids = set(re.findall(r'id="([^"]+)"', html))
    assert set(re.findall(r'href="#([^"]+)"', html)) <= ids
    payload = json.loads(re.search(r'<script id="evidence" type="application/json">(.*?)</script>', html, re.S)[1])
    assert len(payload["rows"]) == 2000
    expected = builder().rounded(builder().calculate())
    for key, value in expected.items():
        assert payload[key] == value, f"Stale numerical evidence; rebuild with: {COMMAND}"
    for key, value in re.findall(r'data-value="([a-z_]+)">([^<]+)</span>', html):
        assert float(value) == pytest.approx(expected[key], abs=0.00051)
    for name, value in expected["percentiles"].items():
        assert f"{name.upper()}: {value:.3f}" in html
    for row in expected["anchors"]:
        assert f'<td>{row["b"]:.1f}</td><td>{row["y"]:.1f}</td>' in html
    for href in re.findall(r'href="([^"]+)"', html):
        if not href.startswith(("https://", "#")):
            assert (REPORT / href).is_file(), href
