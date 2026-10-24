"""Report evidence checks; run only the uncertainty test directory."""
import ast
from copy import deepcopy
import hashlib
import importlib.util
import os
import subprocess
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
    assert_independent_summaries(data)
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
    evidence_pattern = r'<script id="evidence" type="application/json">(.*?)</script>'
    actual_evidence = json.loads(re.search(evidence_pattern, actual, re.S)[1])
    saved_evidence = json.loads(re.search(evidence_pattern, saved, re.S)[1])
    assert_nested_close(actual_evidence, saved_evidence)
    def structure(html):
        html = re.sub(evidence_pattern, '<script id="evidence"></script>', html, flags=re.S)
        return re.sub(r'(data-value="[a-z_]+">)[^<]+', r"\1", html)
    assert structure(actual) == structure(saved), f"Report is stale; rebuild with: {COMMAND}"
    for relative, digest in saved_evidence["sources"].items():
        source = (ROOT / relative).read_text(encoding="utf-8").encode("utf-8")
        assert hashlib.sha256(source).hexdigest() == digest, f"Source changed; rebuild with: {COMMAND}"
    data = module.calculate()
    for key, text in re.findall(r'data-value="([a-z_]+)">([^<]+)</span>', actual):
        assert float(text) == pytest.approx(data[key], abs=0.051 if key.endswith("_difference_pct") else 0.00051)
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
        assert_nested_close(payload[key], value)
    for key, value in re.findall(r'data-value="([a-z_]+)">([^<]+)</span>', html):
        assert float(value) == pytest.approx(expected[key], abs=0.051 if key.endswith("_difference_pct") else 0.00051)
    for name, value in expected["percentiles"].items():
        assert f"{name.upper()}: {value:.3f}" in html
    for row in expected["anchors"]:
        assert f'<td>{row["b"]:.1f}</td><td>{row["y"]:.1f}</td>' in html
    for href in re.findall(r'href="([^"]+)"', html):
        if not href.startswith(("https://", "#")):
            assert (REPORT / href).is_file(), href


def assert_independent_summaries(data):
    """Oracles avoid the analytics functions used by the report generator."""
    a, b, y = np.asarray(data["rows"]).T
    scales = [a.std(ddof=0), b.std(ddof=0)]
    # y=4a+b: beta_a=4*s_a/s_y and beta_b=s_b/s_y exactly.
    for row, slope, scale in zip(data["tornado"], [4.0, 1.0], scales):
        assert row["coefficient"] == pytest.approx(slope * scale / y.std(ddof=0), abs=1e-12)
    n = len(y)
    for row, values in zip(data["spearman"], [a, b]):
        assert len(np.unique(values)) == n and len(np.unique(y)) == n
        ranks = np.argsort(np.argsort(values))
        y_ranks = np.argsort(np.argsort(y))
        # No ties: rho=1-6*sum((rank_x-rank_y)^2)/(n*(n^2-1)).
        rho = 1 - 6 * np.sum((ranks - y_ranks)**2) / (n * (n**2 - 1))
        assert row["coefficient"] == pytest.approx(rho, abs=1e-12)
    ordered = sorted(y)
    for name, p in [("p10", 0.1), ("p50", 0.5), ("p90", 0.9)]:
        position = (n - 1) * p
        lower = int(position)
        expected = ordered[lower] + (position - lower) * (ordered[lower + 1] - ordered[lower])
        assert data["percentiles"][name] == pytest.approx(expected, abs=1e-12)
    # Normal/uniform convolution: F(q)=2*[G((q-13)/2)-G((q-14)/2)],
    # G(z)=z*Phi(z)+phi(z). Solving F(q)=0.1/0.5/0.9 gives these literals.
    # A 0.10 dimensionless absolute tolerance allows seeded sample tail error.
    for name, expected in zip(["p10", "p50", "p90"], [10.910, 13.500, 16.090]):
        assert data["percentiles"][name] == pytest.approx(expected, abs=0.10)


@pytest.mark.parametrize("summary", ["percentiles", "tornado", "spearman"])
def test_independent_oracles_reject_incorrect_summaries(summary):
    data = deepcopy(builder().calculate())
    if summary == "percentiles":
        data[summary]["p10"] += 1.0
    else:
        data[summary][0]["coefficient"] += 0.2
    with pytest.raises(AssertionError):
        assert_independent_summaries(data)


def test_small_nonzero_evidence_keeps_significant_figures():
    data = builder().rounded({"pvalue": [2.52391234e-7], "zero": 0.0})
    assert data["pvalue"][0] == pytest.approx(2.52391234e-7, rel=5e-7, abs=0)
    assert data["zero"] == 0.0


def test_criteria_match_roundtrip_tolerances():
    source = ROOT / "tests/uncertainty/test_roundtrip.py"
    tree = ast.parse(source.read_text(encoding="utf-8"))
    calls = [node for node in ast.walk(tree) if isinstance(node, ast.Call)
             and isinstance(node.func, ast.Attribute) and node.func.attr == "assert_allclose"]
    tolerances = [next(ast.literal_eval(arg.value) for arg in call.keywords
                       if arg.arg == "rtol") for call in calls]
    data = builder().calculate()
    assert len(tolerances) == 2
    assert data["mean_limit_pct"] == 100 * tolerances[0]
    assert data["variance_limit_pct"] == 100 * tolerances[1]


@pytest.mark.parametrize("key,value", [
    ("mean_difference_pct", float("nan")), ("mean", float("nan")),
    ("mean_limit_pct", -1.0), ("variance", float("inf")),
    ("reference_variance", 0.0), ("variance_limit_pct", None),
])
def test_report_refuses_invalid_comparison_inputs(tmp_path, monkeypatch, key, value):
    module = builder()
    data = module.calculate()
    if value is None:
        data.pop(key)
    else:
        data[key] = value
    monkeypatch.setattr(module, "calculate", lambda: data)
    with pytest.raises(ValueError, match="numerical comparison"):
        module.build(tmp_path)


def test_rebuild_does_not_read_layout_exemplar(tmp_path, monkeypatch):
    module = builder()
    monkeypatch.setattr(module, "ROOT", tmp_path)
    module.build(tmp_path / "report")
    saved = (tmp_path / "report/index.html").read_text(encoding="utf-8")
    assert re.search(r"<style>.+?</style>", saved, re.S)


def test_report_structure_and_current_evidence():
    html = (REPORT / "index.html").read_text(encoding="utf-8")
    assert len(re.findall(r"<table>\s*<caption>", html)) == 3
    label = re.search(r'href="#section-8">([^<]+)', html)[1]
    heading = re.search(r'<section id="section-8"><h2>8. ([^<]+)', html)[1]
    assert label == heading
    assert "temporary-folder access was denied" not in html
    assert "test_rebuild_and_displayed_numbers" in html
    assert "1.14.1" not in html and "SciPy 1.15.3" in html
    assert 'data-value="mean_difference_pct">-0.000<' not in html
    assert "corr(a,b)" in html and "covariance" in html
    assert "test_roundtrip.py:23-24" in html



@pytest.mark.parametrize("marker", [
    "!docs/reports/2026-09-13-ansys-evidence-stage-a.html\n",
    "# Retain the uncertainty module report and its reproducible source only.\n",
])
def test_report_ignore_rules_preserve_sibling_reports(tmp_path, marker):
    # Disposable repository; inherited Git bindings must not select a caller's checkout.
    env = {key: value for key, value in os.environ.items() if not key.startswith("GIT_")}
    subprocess.run(["git", "init", "-q", tmp_path.as_posix()], check=True, env=env)
    rules = (ROOT / ".gitignore").read_text(encoding="utf-8")
    earlier = "!docs/reports/modules/alpha/\n!docs/reports/modules/alpha/index.html\n"
    later = "\n!docs/reports/modules/beta/\n!docs/reports/modules/beta/index.html\n"
    # Cover an exception near the first report opt-in, before the old exclusion.
    assert rules.count(marker) == 1
    rules = rules.replace(marker, marker + earlier) + later
    (tmp_path / ".gitignore").write_text(rules, encoding="utf-8", newline="\n")
    for name in ["alpha/index.html", "beta/index.html", "uncertainty/index.html"]:
        result = subprocess.run(["git", "check-ignore", "--no-index", "docs/reports/modules/" + name],
                                cwd=tmp_path, env=env, capture_output=True, text=True, encoding="utf-8")
        assert result.returncode == 1, name
    # Positive controls establish that arbitrary outputs still remain ignored.
    for name in ["alpha/output.bin", "beta/output.bin", "unlisted/index.html"]:
        result = subprocess.run(["git", "check-ignore", "--no-index", "docs/reports/modules/" + name],
                                cwd=tmp_path, env=env, capture_output=True, text=True, encoding="utf-8")
        assert result.returncode == 0, name


def assert_nested_close(actual, expected):
    message = f"Stale numerical evidence; rebuild with: {COMMAND}"
    if isinstance(expected, dict):
        assert actual.keys() == expected.keys(), message
        for key in expected:
            assert_nested_close(actual[key], expected[key])
    elif isinstance(expected, list):
        assert len(actual) == len(expected), message
        for left, right in zip(actual, expected):
            assert_nested_close(left, right)
    elif isinstance(expected, float):
        assert actual == pytest.approx(expected, rel=5e-7, abs=1e-12), message
    else:
        assert actual == expected, message
