# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Report evidence checks; run only the uncertainty test directory."""

import ast
import importlib.util
import json
import math
import shlex
import re
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.uncertainty import (
    Prior,
    run_matrix,
    sample_lhs,
)

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/uncertainty"
COMMAND = "python docs/reports/modules/uncertainty/build_report.py"


def read_lf(path):
    with path.open(encoding="utf-8", newline="\n") as handle:
        return handle.read()


def assert_display_precision(key, text, reference):
    value = float(text)
    if key.endswith("_pct"):
        if 0 < abs(reference) < 0.05:
            assert value != 0
            tolerance = abs(reference) * 0.05
        else:
            tolerance = 0.0500001
    else:
        tolerance = 0.0005001
    assert value == pytest.approx(reference, rel=0, abs=tolerance)


def builder():
    spec = importlib.util.spec_from_file_location(
        "uncertainty_report", REPORT / "build_report.py"
    )
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
    samples = sample_lhs(
        [
            Prior(name="a", dist="normal", mean=3, std=0.5),
            Prior(name="b", dist="uniform", low=1, high=2),
        ],
        n_samples=2000,
        seed=2026,
    )
    result = run_matrix(lambda row: {"y": 4 * row["a"] + row["b"]}, samples)
    np.testing.assert_allclose(
        data["rows"], result[["a", "b", "y"]], rtol=0, atol=1e-12
    )
    assert data["mean"] == pytest.approx(result.y.mean(), abs=1e-12)
    assert data["variance"] == pytest.approx(result.y.var(ddof=0), abs=1e-12)
    assert abs(data["mean_difference_pct"]) <= data["mean_limit_pct"]
    assert abs(data["variance_difference_pct"]) <= data["variance_limit_pct"]


def test_rebuild_and_displayed_numbers(tmp_path):
    module = builder()
    module.build(tmp_path)
    actual = read_lf(tmp_path / "index.html")
    saved = read_lf(REPORT / "index.html")
    assert actual == saved, f"Report is stale; rebuild with: {COMMAND}"
    data = module.calculate()
    for key, text in re.findall(r'data-value="([a-z_]+)">([^<]+)</span>', actual):
        assert_display_precision(key, text, data[key])
    assert len(re.findall(r'<section id="section-', actual)) == 10
    assert '<body class="hs-doc">' in actual
    assert "showDirectoryPicker" in actual
    assert "createWritable" in actual
    assert "<script src=" not in actual
    assert read_lf(tmp_path / "changes.json") == read_lf(REPORT / "changes.json")


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

    html = read_lf(REPORT / "index.html")
    parser = Visible()
    parser.feed(html)
    text = " ".join(parser.text)
    assert not re.search(r"\b(owner|client|lane|agent|session|solver|could|should|would)\b", text, re.I)
    assert "<strong>" not in html and "<b>" not in html
    ids = set(re.findall(r'id="([^"]+)"', html))
    assert set(re.findall(r'href="#([^"]+)"', html)) <= ids
    payload = json.loads(
        re.search(
            r'<script id="evidence" type="application/json">(.*?)</script>', html, re.S
        )[1]
    )
    assert len(payload["rows"]) == 2000
    expected = builder().rounded(builder().calculate())
    for key, value in expected.items():
        assert (
            payload[key] == value
        ), f"Stale numerical evidence; rebuild with: {COMMAND}"
    for key, value in re.findall(r'data-value="([a-z_]+)">([^<]+)</span>', html):
        assert_display_precision(key, value, expected[key])
    for name, value in expected["percentiles"].items():
        assert f"{name.upper()}: {value:.3f}" in html
    for row in expected["anchors"]:
        assert f'<td>{row["b"]:.1f}</td><td>{row["y"]:.1f}</td>' in html
    for href in re.findall(r'href="([^"]+)"', html):
        if not href.startswith(("https://", "#")):
            assert (REPORT / href.split("#", 1)[0]).is_file(), href


def test_independent_response_statistics():
    data = builder().calculate()
    a, b, y = np.asarray(data["rows"]).T
    np.testing.assert_allclose(y, 4 * a + b, rtol=0, atol=1e-12)
    # For the exact linear response, standardized beta_i = slope_i*s_i/s_y.
    scale_y = math.sqrt(sum((y - y.mean()) ** 2) / len(y))
    coefficients = {row["input"]: row["coefficient"] for row in data["tornado"]}
    for name, values, slope in (("a", a, 4), ("b", b, 1)):
        scale = math.sqrt(sum((values - values.mean()) ** 2) / len(values))
        assert coefficients[name] == pytest.approx(slope * scale / scale_y, abs=1e-12)
    # No ties: Spearman rho = 1 - 6*sum(rank differences squared)/(n*(n²-1)).
    rank_y = np.argsort(np.argsort(y))
    for row in data["spearman"]:
        values = {"a": a, "b": b}[row["input"]]
        assert len(set(values)) == len(y) == len(set(y))
        ranks = np.argsort(np.argsort(values))
        rho = 1 - 6 * sum((ranks - rank_y) ** 2) / (len(y) * (len(y) ** 2 - 1))
        assert row["coefficient"] == pytest.approx(rho, abs=1e-12)
    ordered = sorted(y)
    for percentile in (10, 50, 90):
        position = (len(y) - 1) * percentile / 100
        lower = math.floor(position)
        value = ordered[lower] + (position - lower) * (
            ordered[lower + 1] - ordered[lower]
        )
        assert data["percentiles"][f"p{percentile}"] == pytest.approx(value, abs=1e-12)


def test_analytical_percentile_literals():
    data = builder().calculate()

    def integrated_normal(t):
        # Integral of Phi(t): t*Phi(t) + phi(t).
        return t * (1 + math.erf(t / math.sqrt(2))) / 2 + math.exp(
            -t * t / 2
        ) / math.sqrt(2 * math.pi)

    for probability, reference in ((0.1, 10.910), (0.5, 13.500), (0.9, 16.090)):
        # Integrate Phi((y - 12 - b)/2) over the uniform offset 1 <= b <= 2.
        cdf = 2 * (
            integrated_normal((reference - 13) / 2)
            - integrated_normal((reference - 14) / 2)
        )
        assert cdf == pytest.approx(probability, abs=0.0001)
        # A 0.1 absolute demonstration tolerance covers this seeded finite sample;
        # it is not a convergence guarantee or an application acceptance limit.
        assert data["percentiles"][f"p{round(probability * 100)}"] == pytest.approx(
            reference, abs=0.1
        )


def test_limits_match_roundtrip_relative_tolerances():
    data = builder().calculate()
    with (ROOT / "tests/uncertainty/test_roundtrip.py").open(
        encoding="utf-8", newline="\n"
    ) as handle:
        tree = ast.parse(handle.read())
    calls = [
        node
        for node in ast.walk(tree)
        if isinstance(node, ast.Call)
        and isinstance(node.func, ast.Attribute)
        and node.func.attr == "assert_allclose"
    ]
    assert len(calls) == 2
    for moment, call in zip(("mean", "variance"), calls):
        rtol = next(
            ast.literal_eval(kw.value) for kw in call.keywords if kw.arg == "rtol"
        )
        assert data[moment + "_limit_pct"] == 100 * rtol


def test_sample_dependence_explains_variance_difference():
    data = builder().calculate()
    a, b, y = np.asarray(data["rows"]).T
    centered_a, centered_b = a - a.mean(), b - b.mean()
    covariance = sum(centered_a * centered_b) / len(y)
    correlation = covariance / math.sqrt(
        sum(centered_a**2) * sum(centered_b**2) / len(y) ** 2
    )
    cross_pct = 100 * 2 * 4 * covariance / (49 / 12)
    assert data["sample_correlation"] == pytest.approx(correlation, abs=1e-12)
    assert data["covariance_difference_pct"] == pytest.approx(cross_pct, abs=1e-12)
    marginal_pct = 100 * (16 * np.var(a) + np.var(b) - 49 / 12) / (49 / 12)
    assert data["variance_difference_pct"] == pytest.approx(cross_pct + marginal_pct)


def test_significant_figures_preserve_small_nonzero_evidence():
    module = builder()
    assert module.rounded(2.520278162451213e-7) == 2.520278e-7
    assert module.rounded(-0.00047827112905136145) == -0.0004782711
    html = read_lf(REPORT / "index.html")
    payload = json.loads(
        re.search(
            r'<script id="evidence" type="application/json">(.*?)</script>', html, re.S
        )[1]
    )
    assert payload["spearman"][1]["pvalue"] > 0
    assert 'data-value="mean_difference_pct">-0.000</span>' not in html


def test_template_is_self_contained_and_html_structure_matches():
    template = read_lf(REPORT / "report.html.in")
    assert "<style>" in template
    assert "@@STYLE@@" not in template
    assert "pipeline-ffs-assessment.html" not in template
    assert (
        "remains not evaluated because temporary-folder access was denied"
        not in template
    )
    assert "test_rebuild_and_displayed_numbers" in template
    assert "1.14.1" not in template
    assert "1.15.3" in template
    for table in re.findall(r"<table>(.*?)</table>", template, re.S):
        assert table.lstrip().startswith("<caption>")
    nav_label = re.search(r'href="#section-8">([^<]+)', template)[1]
    heading = re.search(r"<h2>8\. ([^<]+)", template)[1]
    assert nav_label == heading


@pytest.mark.parametrize(
    "workflow", ["quality-gates-by-domain.yml", "full-matrix-sweep.yml"]
)
@pytest.mark.parametrize(
    "gate_name", ["tests-infrastructure-core", "tests-infrastructure-other"]
)
def test_workflow_gate_command_includes_uncertainty_directory(
    workflow, gate_name, monkeypatch, capsys
):
    import yaml

    config = yaml.safe_load(read_lf(ROOT / ".claude/quality-gates.yaml"))
    original = shlex.split(config["gates"][gate_name]["command"])
    document = yaml.safe_load(read_lf(ROOT / ".github/workflows" / workflow))
    steps = [step for job in document["jobs"].values() for step in job.get("steps", [])]
    script = next(
        step["run"] for step in steps if step.get("name") == "Read domain gate command"
    )
    code = script.split("<<'PY'\n", 1)[1].split("\nPY", 1)[0]
    monkeypatch.setenv("GATE_NAME", gate_name)
    monkeypatch.chdir(ROOT)
    exec(compile(code, workflow, "exec"), {})
    command = shlex.split(capsys.readouterr().out.strip())
    assert command[: len(original)] == original
    if gate_name == "tests-infrastructure-core":
        assert command.count("tests/uncertainty/") == 1
        assert command == original + (
            [] if "tests/uncertainty/" in original else ["tests/uncertainty/"]
        )
    else:
        assert command == original


def test_report_sources_and_outputs_are_utf8_lf():
    for name in ("build_report.py", "report.html.in", "index.html", "changes.json"):
        raw = (REPORT / name).read_bytes()
        raw.decode("utf-8")
        assert b"\r" not in raw
        assert raw.endswith(b"\n")
