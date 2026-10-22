# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check report values against hand calculations, stored oracles and public calls."""

import copy
import importlib.util
import json
import re
import shlex
from html.parser import HTMLParser
from pathlib import Path

import numpy as np
import pytest
import yaml

from digitalmodel.hydrodynamics.wang_passing_ship import WangVessel, wang_passing_forces

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/passingshipforces"
spec = importlib.util.spec_from_file_location("force_report", REPORT / "generate_report.py")
report = importlib.util.module_from_spec(spec)
CHART = r'<script id="chart-data" type="application/json">(.*?)</script>'


class Document(HTMLParser):
    def __init__(self, text):
        super().__init__()
        self.ids = []
        self.links = []
        self.numbers = {}
        self.rows = []
        self.row = None
        self.cell = None
        self.current = None
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if attrs.get("href", "").startswith("#"):
            self.links.append(attrs["href"][1:])
        if "data-number" in attrs:
            self.current = attrs["data-number"]
            assert self.current not in self.numbers
        if tag == "tr":
            self.row = []
        if tag == "td":
            self.cell = []

    def handle_data(self, data):
        if self.current:
            self.numbers[self.current] = data
        if self.cell is not None:
            self.cell.append(data)

    def handle_endtag(self, tag):
        if tag == "span":
            self.current = None
        if tag == "td":
            self.row.append("".join(self.cell).strip())
            self.cell = None
        if tag == "tr" and self.row:
            self.rows.append(self.row)
            self.row = None


def split_document(text):
    payload = json.loads(re.search(CHART, text, re.S).group(1))
    return re.sub(CHART, "<chart-data/>", text, flags=re.S), payload


def numeric(text):
    return float(text.replace(",", "").replace("−", "-"))


@pytest.fixture(scope="module")
def generated(tmp_path_factory):
    spec.loader.exec_module(report)
    work = tmp_path_factory.mktemp("force-report")
    model = report.calculate(work / "calculation")
    return model, report.render(model), work


# Independent raw-kernel double-quadrature hand calculations for the public
# example: inner S2' kernels integrated directly, then the outer S1' / moment
# kernels; no production antiderivatives or report formatting are used here.
# The signed extrema are sampled, rather than continuous optimized peaks.
@pytest.mark.parametrize("label, expected, unit, key", [
    ("Minimum surge", -34484.7068, "lbf", "min_surge_force"),
    ("Maximum surge", 34484.7068, "lbf", "max_surge_force"),
    ("Minimum sway", -36095.3412, "lbf", "min_sway_force"),
    ("Maximum sway", 87637.7742, "lbf", "max_sway_force"),
    ("Minimum yaw moment", -14304244.21, "ft·lbf", "min_yaw_moment"),
    ("Maximum yaw moment", 14304244.21, "ft·lbf", "max_yaw_moment"),
])
def test_reported_extrema_against_hand_calculation(generated, label, expected, unit, key):
    _, text, _ = generated
    doc = Document(text)
    row = next(row for row in doc.rows if row[0] == label)
    assert row[2] == unit
    assert numeric(row[1]) == pytest.approx(expected, abs=0.051, rel=0)
    assert numeric(doc.numbers[key]) == pytest.approx(expected, abs=0.051, rel=0)
    summary_key = key.replace("min_", "summary-min-").replace("max_", "summary-max-")
    assert doc.numbers[summary_key] == row[1]


def test_chart_readback_against_stored_workbook_and_library(generated):
    model, text, _ = generated
    _, payload = split_document(text)
    for mode in ("finite", "deep"):
        settings = model["settings"]
        result = wang_passing_forces(
            [row["stagger"] for row in model[mode]["forces"]],
            settings["separation_distance"],
            WangVessel(**settings["moored_vessel"]),
            WangVessel(**settings["passing_vessel"]),
            settings["passing_velocity"], settings["water_density"],
            depth=settings["water_depth"] if mode == "finite" else None,
        )
        for field, values in (("stagger", result.stagger), ("surge_force", result.surge),
                              ("sway_force", result.sway), ("yaw_moment", result.yaw)):
            np.testing.assert_allclose(
                [row[field] for row in payload[mode]], values, atol=1e-6, rtol=0
            )
        abeam = min(payload[mode], key=lambda row: abs(row["stagger"]))
        assert abeam["stagger"] == 0
        # Stored rounded workbook row at abeam; inputs differ only by their
        # documented rounding. These tolerances do not qualify physical accuracy.
        expected = 87640.0 if mode == "finite" else 15020.0
        assert abeam["sway_force"] == pytest.approx(expected, rel=1e-3)
    assert numeric(Document(text).numbers["max_sway_force_stagger"]) == 0


def test_rebuild_uses_tolerant_chart_comparison_and_exact_narrative(generated):
    _, text, work = generated
    out = work / "rebuilt"
    report.build(out, work / "rebuild-calculation")
    rebuilt = (out / "report.html").read_text(encoding="utf-8")
    assert rebuilt == text
    narrative, payload = split_document(text)
    issued_narrative, issued_payload = split_document(
        (REPORT / "report.html").read_text(encoding="utf-8")
    )
    assert narrative == issued_narrative
    for mode in ("finite", "deep"):
        assert len(payload[mode]) == len(issued_payload[mode]) == 201
        for row, issued_row in zip(payload[mode], issued_payload[mode]):
            assert row.keys() == issued_row.keys()
            for key in row:
                np.testing.assert_allclose(row[key], issued_row[key], atol=1e-6, rtol=1e-12)
    assert (out / "changes.json").read_bytes() == (REPORT / "changes.json").read_bytes()
    assert report.house_style() in text
    assert '<body class="hs-doc">' in text
    assert '<script src=' not in text and '<link rel="stylesheet"' not in text
    assert "showDirectoryPicker" in text and "createWritable" in text
    assert "report.comments.json" in text and 'type="file"' in text
    assert "localStorage" in text and "textContent" in text and "changes-panel" in text


def test_report_ignores_signed_peak_ties_and_ulp_noise(generated):
    model, text, _ = generated
    perturbed = copy.deepcopy(model)
    for mode in ("finite", "deep"):
        for component in ("surge_force", "sway_force", "yaw_moment"):
            perturbed[mode]["peak_" + component] *= -1
            for row in perturbed[mode]["forces"]:
                row[component] = float(np.nextafter(row[component], np.inf))
    changed, payload = split_document(report.render(perturbed))
    original, _ = split_document(text)
    assert changed == original
    for mode in payload.values():
        for row in mode:
            for value in row.values():
                assert value == round(value, 6)


def test_image_truncation_is_quantified_at_fixed_staggers(generated):
    model, text, _ = generated
    rows = model["image_sensitivity"]
    assert len(rows) == 4
    # Independent quadrature, 21 versus 801 image terms, rounded to one
    # decimal percent. Denominator: the 801-term reference, not the legacy sum.
    assert [row["difference_percent"] for row in rows] == pytest.approx(
        [-3.4, -1.2, -0.4, 19.2], abs=0.1
    )
    for row in rows:
        expected = 100 * (row["legacy"] / row["reference"] - 1)
        assert row["difference_percent"] == pytest.approx(expected)
    assert "801 terms" in text and "legacy tool" in text
    assert "infinite-sum convergence is NOT_EVALUATED" in text


def test_utf8_reads_and_lf_writes(generated, monkeypatch):
    _, _, work = generated
    original_read, original_write = Path.read_text, Path.write_text
    read_paths, write_paths = [], []

    def read(path, *args, **kwargs):
        assert kwargs.get("encoding") == "utf-8"
        read_paths.append(path)
        return original_read(path, *args, **kwargs)

    def write(path, *args, **kwargs):
        assert kwargs.get("encoding") == "utf-8"
        assert kwargs.get("newline") == "\n"
        write_paths.append(path)
        return original_write(path, *args, **kwargs)

    with monkeypatch.context() as patch:
        patch.setattr(Path, "read_text", read)
        patch.setattr(Path, "write_text", write)
        report.build(work / "portable", work / "portable-calculation")
    assert report.EXAMPLE in read_paths and report.STYLE_SOURCE in read_paths
    assert {path.name for path in write_paths} == {"report.html", "changes.json"}
    for path in write_paths:
        assert b"\r" not in path.read_bytes()


def test_hosted_harness_and_contracts_gate_include_report_tests():
    workflow = yaml.safe_load(
        (ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text(encoding="utf-8")
    )
    harness = workflow["jobs"]["ci-harness-tests"]
    assert "if" not in harness
    step = next(row for row in harness["steps"] if row["name"] == "Run harness tests")
    assert "if" not in step
    command = shlex.split(step["run"])
    for path in ("tests/docs/test_passing_ship_forces_report.py",
                 "tests/hydrodynamics/test_wang_passing_ship.py",
                 "tests/hydrodynamics/test_passing_ship_forces_workflow.py"):
        assert path in command
    gate = yaml.safe_load(
        (ROOT / ".claude/quality-gates.yaml").read_text(encoding="utf-8")
    )["gates"]["tests-contracts"]
    assert gate["enabled"] and gate["failure_action"] == "block"
    assert "tests/docs/" in shlex.split(gate["command"])


def test_document_structure_numbers_and_public_wording(generated):
    _, text, _ = generated
    doc = Document(text)
    assert len(doc.ids) == len(set(doc.ids))
    assert set(doc.links) <= set(doc.ids)
    assert len(re.findall(r"<h2[ >]", text)) == 10
    assert not re.search(r"<h[4-6][ >]|<strong|<b[ >]", text)
    visible = re.sub(r"<(script|style)\b.*?</\1>", "", text, flags=re.S)
    visible = re.sub(r"<[^>]+>", " ", visible)
    assert not re.search(r"\b(could|should|would|own" + r"er|validated|certified|compliant)\b",
                         visible, re.I)
    for required in ("NOT_EVALUATED", "ASSUMED - to be confirmed", "Revision history",
                     "9.2 Source locators", "Field validation", "Limit: NOT_EVALUATED",
                     "X > 0 acts toward +x", "N > 0 turns the +x end toward the passing hull",
                     "ASCE", "101", "WW3", "247", "MARSIM'03"):
        assert required in text
    assert "Draft for technical review" in text
    assert "Draft for client review" not in text
    assert 'href="#source-calculator"' in text and 'href="#source-workflow"' in text
    assert 'id="source-calculator"' in text and 'id="source-workflow"' in text
    worked = text.split('id="worked-example"', 1)[1].split('</section>', 1)[0]
    worked = re.sub(r'<h2>.*?</h2>', "", worked)
    worked = re.sub(r'<span data-number="[^"]+">.*?</span>', "", worked)
    worked = re.sub(r"<[^>]+>", "", worked)
    assert not re.search(r"\d", worked)
