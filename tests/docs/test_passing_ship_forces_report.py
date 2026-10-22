# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check report reproduction, numerical readback and the offline document contract."""

import copy
import importlib.util
import json
import re
from html.parser import HTMLParser
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.wang_passing_ship import WangVessel, wang_passing_forces
from digitalmodel.passing_ship_forces import router


ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/passingshipforces"
spec = importlib.util.spec_from_file_location("force_report", REPORT / "generate_report.py")
report = importlib.util.module_from_spec(spec)


class Document(HTMLParser):
    def __init__(self, text):
        super().__init__()
        self.ids = []
        self.links = []
        self.numbers = {}
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

    def handle_data(self, data):
        if self.current:
            self.numbers[self.current] = data

    def handle_endtag(self, tag):
        if tag == "span":
            self.current = None


@pytest.fixture(scope="module")
def generated(tmp_path_factory):
    spec.loader.exec_module(report)
    work = tmp_path_factory.mktemp("force-report")
    model = report.calculate(work / "calculation")
    text = report.render(model)
    return model, text, work


def test_every_worked_number_matches_public_module(generated, tmp_path):
    model, text, _ = generated
    cfg = report.example_config(tmp_path)
    actual = router(copy.deepcopy(cfg))["passing_ship_forces"]
    settings = cfg["passing_ship_forces"]
    expected = report.worked_numbers(settings, actual)
    parsed = Document(text)
    assert set(parsed.numbers) == set(expected)
    for key, value in expected.items():
        assert parsed.numbers[key] == report.number(value, key)
    # Independent library call checks every plotted coordinate and component.
    for mode in ("finite", "deep"):
        result = wang_passing_forces(
            [row["stagger"] for row in model[mode]["forces"]],
            settings["separation_distance"],
            WangVessel(**settings["moored_vessel"]),
            WangVessel(**settings["passing_vessel"]),
            settings["passing_velocity"], settings["water_density"],
            depth=settings["water_depth"] if mode == "finite" else None,
        )
        for field, values in (("stagger", result.stagger),
                              ("surge_force", result.surge),
                              ("sway_force", result.sway),
                              ("yaw_moment", result.yaw)):
            np.testing.assert_array_equal(
                [row[field] for row in model[mode]["forces"]], values
            )
    payload = re.search(r'<script id="chart-data" type="application/json">(.*?)</script>',
                        text, re.S).group(1)
    assert json.loads(payload) == report.chart_data(model)


def test_rebuild_is_identical_and_uses_house_stylesheet(generated):
    model, text, work = generated
    out = work / "rebuilt"
    report.build(out, work / "rebuild-calculation")
    assert (out / "report.html").read_text() == text
    assert text == (REPORT / "report.html").read_text()
    assert (out / "changes.json").read_bytes() == (REPORT / "changes.json").read_bytes()
    assert report.house_style() in text
    assert '<body class="hs-doc">' in text
    assert '<script src=' not in text and '<link rel="stylesheet"' not in text
    assert "showDirectoryPicker" in text and "createWritable" in text
    assert "report.comments.json" in text and 'type="file"' in text
    assert "localStorage" in text and "textContent" in text
    assert "changes-panel" in text


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
    assert "NOT_EVALUATED" in visible
    assert "ASSUMED - to be confirmed" in visible
    assert "Revision history" in visible
    assert "9.2 Source locators" in visible
    assert "Field validation" in visible
    assert "Limit: NOT_EVALUATED" in text
    # No typed numerical claims in worked-example prose: all come from spans.
    worked = text.split('id="worked-example"', 1)[1].split('</section>', 1)[0]
    worked = re.sub(r'<h2>.*?</h2>', "", worked)
    worked = re.sub(r'<span data-number="[^"]+">.*?</span>', "", worked)
    worked = re.sub(r"<[^>]+>", "", worked)
    assert not re.search(r"\d", worked)
