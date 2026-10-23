# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the spectral fatigue report and verify its numerical bindings."""

import copy
import csv
import importlib.util
import json
import math
import re
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/spectralfatigue"


@pytest.fixture
def builder():
    spec = importlib.util.spec_from_file_location("spectral_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def record(builder, tmp_path):
    return builder.calculate(tmp_path / "calculation")


def test_rebuild_is_current_and_deterministic(builder, tmp_path):
    first = tmp_path / "first"
    second = tmp_path / "second"
    builder.build(first, tmp_path / "calc-first")
    builder.build(second, tmp_path / "calc-second")
    for name in ("report.html", "changes.json"):
        assert (first / name).read_bytes() == (second / name).read_bytes()
        assert (first / name).read_bytes() == (REPORT / name).read_bytes()


def test_public_example_is_used_without_numeric_overrides(record, tmp_path):
    from digitalmodel.spectral_fatigue.workflow import router

    source = yaml.safe_load((ROOT / "examples/workflows/spectral-fatigue/input.yml").read_text())
    assert record["input"] == source["spectral_fatigue"]
    source["spectral_fatigue"]["output_dir"] = str(tmp_path / "direct")
    actual = router(copy.deepcopy(source))["spectral_fatigue"]
    assert record["summary"] == actual["locations"]
    assert record["governing_location"] == actual["governing_location"]
    assert record["screening_status"] == actual["screening_status"]
    with Path(actual["results_csv"]).open() as stream:
        rows = list(csv.DictReader(stream))
    assert record["sea_states"] == rows


@pytest.mark.parametrize("index", [0, 1, 2])
def test_each_state_matches_library(record, index):
    from digitalmodel.fatigue.spectral_fatigue import compute_spectral_moments, dirlik_damage

    state = record["states"][index]
    location = record["input"]["locations"][state["location_index"]]
    inputs = location["sea_states"][state["state_index"]]
    moments = compute_spectral_moments(inputs["frequency_Hz"], inputs["stress_psd_MPa2_Hz"])
    curve = record["input"]["sn_curve"]
    result = dirlik_damage(moments, curve["slope"], curve["log_intercept"])
    assert state["moments"] == moments.model_dump()
    assert state["result"] == result.model_dump()
    assert state["rms"] == moments.rms
    assert state["alpha2"] == moments.irregularity_factor
    assert state["epsilon"] == moments.bandwidth_parameter
    assert state["peak_rate"] == moments.nu_p
    assert state["crossing_rate"] == moments.nu_0


def test_frequency_convention_audit_has_independent_reference(record):
    for state in record["states"]:
        m = state["moments"]
        assert state["hertz_peak_rate"] == pytest.approx(math.sqrt(m["m4"] / m["m2"]))
        assert state["peak_difference_percent"] == pytest.approx(
            100 * (state["peak_rate"] / state["hertz_peak_rate"] - 1)
        )
        assert state["peak_difference_percent"] < 0
        assert state["alpha2"] ** 2 + state["epsilon"] ** 2 == pytest.approx(1)


class Page(HTMLParser):
    """Collect numeric bindings, scripts, headings and external resources."""

    def __init__(self):
        super().__init__()
        self.bindings = []
        self.attrs = None
        self.bound_text = ""
        self.scripts = {}
        self.script_id = None
        self.resources = []
        self.headings = []
        self.ids = []
        self.links = []
        self.body_class = None
        self.worked = False
        self.unbound = []
        self.hidden = False
        self.in_heading = False

    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        if "id" in a:
            self.ids.append(a["id"])
        if tag == "body":
            self.body_class = a.get("class")
        if tag == "section":
            self.worked = a.get("id") == "detailed-results"
        if "data-number" in a:
            self.attrs, self.bound_text = a, ""
        if tag == "script":
            self.script_id = a.get("id")
            self.hidden = True
        if tag == "style":
            self.hidden = True
        if tag in {"h1", "h2", "h3", "h4", "h5", "h6"}:
            self.headings.append(tag)
            self.in_heading = True
        if tag in {"script", "img", "link", "iframe"} and (a.get("src") or a.get("href")):
            self.resources.append(a)
        if tag == "a" and a.get("href", "").startswith("#"):
            self.links.append(a["href"][1:])

    def handle_data(self, data):
        if self.attrs is not None:
            self.bound_text += data
        elif self.worked and not self.hidden and not self.in_heading and re.search(r"\d", data):
            self.unbound.append(data)
        if self.script_id:
            self.scripts[self.script_id] = self.scripts.get(self.script_id, "") + data

    def handle_endtag(self, tag):
        if tag == "span" and self.attrs is not None:
            self.bindings.append((self.attrs, self.bound_text))
            self.attrs = None
        if tag in {"script", "style"}:
            self.script_id = None
            self.hidden = False
        if tag == "section":
            self.worked = False
        if tag in {"h1", "h2", "h3"}:
            self.in_heading = False


def test_all_displayed_example_numbers_are_bound_to_module(record, builder):
    page = Page()
    page.feed((REPORT / "report.html").read_text())
    assert len(page.bindings) > 50
    assert not page.unbound
    for attrs, visible in page.bindings:
        value = builder.lookup(record, attrs["data-number"])
        assert float(attrs["data-value"]) == value
        assert visible == builder.format_number(value, attrs["data-format"])
    assert json.loads(page.scripts["report-data"]) == record


def test_chart_points_and_limits_are_returned_values(record):
    text = (REPORT / "report.html").read_text()
    data = json.loads(re.search(r'<script id="chart-data" type="application/json">(.*?)</script>', text, re.S)[1])
    assert len(data) == len(record["summary"])
    for series, summary in zip(data, record["summary"]):
        assert series["limit"] == summary["required_life_years"]
        assert series["points"][-1] == [record["input"]["design_life_years"], summary["fatigue_life_years"]]
        for year, life in series["points"]:
            assert life == summary["fatigue_life_years"]
        assert series["limit_points"][-1][1] == summary["required_life_years"]
    assert "e.setAttribute('data-limit','required-life')" in text
    assert "tabindex:0" in text


def test_house_style_structure_and_links(builder):
    text = (REPORT / "report.html").read_text()
    page = Page()
    page.feed(text)
    assert page.body_class == "hs-doc"
    assert not page.resources
    assert not set(page.headings) & {"h4", "h5", "h6"}
    assert len(re.findall(r'<section id="(?:executive-summary|introduction|design-data|methodology|detailed-results|findings|limitations|recommendations|references|acronyms)"', text)) == 10
    assert set(page.links) <= set(page.ids)
    assert len(page.ids) == len(set(page.ids))
    assert builder.house_style() in text
    assert text.index('id="revision-history"') < text.index('id="introduction"')
    assert "<strong>" not in text and "<b>" not in text


def test_change_panel_and_comment_persistence_contract():
    text = (REPORT / "report.html").read_text()
    changes = json.loads((REPORT / "changes.json").read_text())
    page = Page()
    page.feed(text)
    assert json.loads(page.scripts["change-data"]) == changes
    assert changes["revisions"][0]["revision"] == "0"
    assert "showDirectoryPicker" in text
    assert "createWritable" in text
    assert "report.comments.json" in text
    assert "textContent" in text


def test_visible_wording_and_required_registers():
    text = (REPORT / "report.html").read_text()
    visible = re.sub(r"<(script|style)\b.*?</\1>", "", text, flags=re.S)
    visible = re.sub(r"<[^>]+>", " ", visible)
    for word in ("could", "should", "would", "certified", "compliant"):
        assert not re.search(rf"\b{word}\b", visible, re.I)
    for required in ("Draft for client review", "ASSUMED - to be confirmed", "NOT_EVALUATED", "Data register", "QA pre-flight", "Not covered"):
        assert required in visible


def test_chart_interaction_and_comments_saved_beside_report(tmp_path):
    import os
    import shutil
    import subprocess

    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the offline interaction check")
    env = dict(os.environ)
    for key in ("UV_TOOL_DIR", "PRE_COMMIT_HOME", "XDG_CACHE_HOME", "npm_config_cache"):
        folder = tmp_path / key.lower()
        folder.mkdir()
        env[key] = str(folder)
    result = subprocess.run(
        [node, str(Path(__file__).with_name("spectralfatigue_interactions.cjs")), str(REPORT / "report.html")],
        cwd=tmp_path, env=env, capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stderr
