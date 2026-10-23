"""Rebuild and numerical provenance checks for the analytical module report."""
import importlib.util
import json
import math
import os
import re
import shlex
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/orcaflex"
REBUILD = "PYTHONPATH=src python docs/reports/modules/orcaflex/build_report.py"
SOLVER_FREE_BUILD = """
import importlib.abc
import runpy
import sys

class RejectSolver(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] == 'OrcFxAPI':
            raise AssertionError('Native solver import attempted')

sys.meta_path.insert(0, RejectSolver())
try:
    __import__('OrcFxAPI')
except AssertionError:
    pass
else:
    raise AssertionError('Import blocker was not exercised')
builder = runpy.run_path(sys.argv[1])
builder['build'](sys.argv[2])
"""


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
    subprocess.run(
        [sys.executable, "-c", SOLVER_FREE_BUILD,
         str(ROOT / "docs/reports/modules/orcaflex/build_report.py"), str(tmp_path)],
        cwd=ROOT, env={**os.environ, "PYTHONPATH": str(ROOT / "src")},
        check=True, capture_output=True, text=True,
    )
    for name in ("report.html", "changes.json"):
        assert (tmp_path / name).read_bytes() == (REPORT / name).read_bytes(), REBUILD
    html = (tmp_path / "report.html").read_text(encoding="utf-8")
    assert '<body class="hs-doc">' in html
    markup = re.sub(r"<style>.*?</style>", "", html, flags=re.S)
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)=', markup)
    assert "showSaveFilePicker" in html
    assert "report.comments.json" in html
    assert len(re.findall(r'<h2 id="s\d+">', html)) == 10


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
    assert json.loads(embedded.group(1)) == data, REBUILD
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


def test_displayed_input_register_reproduces_submerged_weight():
    html = (REPORT / "report.html").read_text(encoding="utf-8")
    rows = re.findall(
        r'<tr><td>[^<]+</td><td><span data-value="([^"]+)">([^<]+)</span></td><td>([^<]+)</td>',
        html,
    )
    register = {key: (float(value), unit) for key, value, unit in rows}
    for key in ("wall_thickness", "coating_thickness", "corrosion_allowance"):
        value, unit = register["pipe." + key]
        assert unit == "mm"
        assert f"{value:.3f}" in html
    outer = register["pipe.outer_diameter"][0]
    wall = register["pipe.wall_thickness"][0] / 1000
    coating = register["pipe.coating_thickness"][0] / 1000
    bore, coated = outer - 2 * wall, outer + 2 * coating
    parser = Numbers()
    parser.feed(html)
    displayed = dict(parser.values)
    mass = math.pi / 4 * (
        float(displayed["steel_density"]) * (outer**2 - bore**2)
        + register["pipe.coating_density"][0] * (coated**2 - outer**2)
        + register["pipe.contents_density"][0] * bore**2
        - register["pipe.seawater_density"][0] * coated**2
    )
    assert mass * float(displayed["gravity"]) == pytest.approx(1189.05, abs=0.005)
    assert round(mass * float(displayed["gravity"]), 1) == float(displayed["weight"])


def test_example_agrees_with_hand_calculated_literals():
    # D=0.2731 m, t=0.0254 m, coating=0.040 m; densities=7850/900/800/1025 kg/m³.
    # w=9.80665*pi/4*[7850*(D²-bore²)+900*(coated²-D²)+800*bore²-1025*coated²].
    # h=1490 m, theta=12 degrees: a=h*sin(theta)/(1-sin(theta)); T=w*(a+h)/1000.
    data = load_builder().calculate()
    assert data["weight"] == pytest.approx(1189.05, abs=0.005)
    assert data["geometry"]["catenary_parameter_m"] == pytest.approx(391.10, abs=0.005)
    assert data["geometry"]["top_tension_kN"] == pytest.approx(2236.7, abs=0.05)


@pytest.mark.parametrize("check_name", [
    "test_rebuild_is_exact_and_self_contained",
    "test_all_example_numbers_are_live_module_values",
    "test_checked_in_report_matches_read_only_rebuild",
])
def test_stale_report_failures_name_rebuild_command(tmp_path, monkeypatch, check_name):
    builder = load_builder()
    html = (REPORT / "report.html").read_text(encoding="utf-8")
    html = re.sub(r'("test_count": )\d+', r'\g<1>0', html)
    (tmp_path / "report.html").write_text(html, encoding="utf-8")
    (tmp_path / "changes.json").write_bytes((REPORT / "changes.json").read_bytes())
    monkeypatch.setattr(sys.modules[__name__], "load_builder", lambda: builder)
    monkeypatch.setattr(sys.modules[__name__], "REPORT", tmp_path)
    with pytest.raises(AssertionError, match=re.escape(REBUILD)):
        if check_name == "test_rebuild_is_exact_and_self_contained":
            globals()[check_name](tmp_path / "rebuilt")
        else:
            globals()[check_name]()


def test_ci_runs_report_checks_for_every_touched_domain():
    import yaml
    workflow = yaml.safe_load((ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text())
    assert not {"paths", "paths-ignore"} & workflow["on"]["pull_request"].keys()
    harness = workflow["jobs"]["ci-harness-tests"]
    assert "if" not in harness
    report_steps = [step for step in harness["steps"]
                    if "tests/orcaflex/test_module_report.py" in step.get("run", "")]
    assert report_steps, "Report checks must also run when shared material inputs change"
    for step in report_steps:
        assert "if" not in step
        assert "tests/orcaflex/test_module_report.py" in shlex.split(step["run"])


def test_draft_status_is_for_maintainer_review():
    assert load_builder().CHANGES["status"] == "Draft for maintainer review"
    assert "Draft for maintainer review" in (REPORT / "report.html").read_text(encoding="utf-8")


def test_checked_in_report_matches_read_only_rebuild():
    builder = load_builder()
    assert builder.render(builder.calculate()) == (REPORT / "report.html").read_text(encoding="utf-8"), REBUILD
    assert json.loads((REPORT / "changes.json").read_text(encoding="utf-8")) == builder.CHANGES, REBUILD


def test_source_links_resolve_inside_repository():
    html = (REPORT / "report.html").read_text(encoding="utf-8")
    markup = re.sub(r"<(style|script)[^>]*>.*?</\1>", "", html, flags=re.S)
    for target in re.findall(r'href="([^"#]+)"', markup):
        resolved = (REPORT / target).resolve()
        assert resolved.is_relative_to(ROOT)
        assert resolved.is_file()


def test_invalid_geometry_is_rejected():
    from digitalmodel.orcaflex.riser_config import SCRDesignInput
    with pytest.raises(ValueError, match="above the seabed"):
        SCRDesignInput(water_depth=1, hang_off_elevation=-1).calculate_geometry()
