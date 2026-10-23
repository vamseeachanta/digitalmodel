# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild and numerical binding checks for the cross-section module analysis."""

from html.parser import HTMLParser
import json
import math
from pathlib import Path
import runpy
import subprocess
import sys
import xml.etree.ElementTree as ET

import pytest

from digitalmodel.subsea.cross_sections.reporting import build_report_model, render_html_report
from digitalmodel.subsea.cross_sections.io import load_cross_section_fixture
from digitalmodel.subsea.cross_sections.reporting import CrossSectionReport, ReportFixture, fixture_paths
from digitalmodel.subsea.cross_sections.schema import CrossSectionDefinition, Provenance, RadialLayer, UnitValue
from digitalmodel.subsea.cross_sections.visualization import render_cross_section_svg

ROOT = Path(__file__).resolve().parents[3]
BUILDER = ROOT / "scripts/reporting/build_cross_sections_module_report.py"
REPORT = ROOT / "docs/reports/modules/cross_sections"


def hollow_definition():
    provenance = Provenance(source_id="arithmetic", source_type="project_assumption")
    return CrossSectionDefinition(
        id="hollow-circle", name="Hollow circle", family="rigid_pipeline_flowline", duty="static",
        provenance=[provenance], radial_layers=[RadialLayer(
            name="shell", role="pipe", material="unspecified", provenance=provenance,
            inner_diameter=UnitValue(value=1, unit="m"), outer_diameter=UnitValue(value=2, unit="m"),
        )],
    )


def test_radial_visual_preserves_bore_and_radius_ratio():
    root = ET.fromstring(render_cross_section_svg(hollow_definition()))
    circles = root.findall("{http://www.w3.org/2000/svg}circle")
    assert len(circles) == 2
    assert float(circles[-1].attrib["r"]) / float(circles[0].attrib["r"]) == 0.5
    assert circles[-1].attrib["fill"] == "white"
    # At the renderer's 48 drawing units per metre diameter, a .02 m core
    # inside a 2 m envelope has radius .48, not the former minimum radius 2.
    # This is a drawing-geometry oracle, not a material or acceptance constant.
    definition = hollow_definition()
    core = definition.radial_layers[0].model_copy(deep=True)
    core.inner_diameter.value = 0
    core.outer_diameter.value = 0.02
    definition.radial_layers[0].inner_diameter.value = 0.02
    definition.radial_layers.insert(0, core)
    circles = ET.fromstring(render_cross_section_svg(definition)).findall("{http://www.w3.org/2000/svg}circle")
    assert float(circles[-1].attrib["r"]) == 0.48


def test_explicit_empty_report_selection_is_empty():
    model = build_report_model([])
    assert model.fixtures == ()
    assert model.provenance == ()


def test_report_fixture_text_escapes_markup():
    definition = hollow_definition()
    definition.name = '<script>alert("example")</script>'
    definition.radial_layers[0].name = '<script>alert("layer")</script>'
    # Rendering itself is pure; no browser is started.
    svg = render_cross_section_svg(definition)
    assert "<script>" not in svg
    item = ReportFixture(definition, svg, ())
    html = render_html_report(CrossSectionReport((item,), (), []))
    assert "<script>" not in html
    assert "&lt;script&gt;" in html


def test_packed_envelope_does_not_compare_incompatible_units():
    definition = load_cross_section_fixture(next(p for p in fixture_paths() if p.stem.startswith("steel")))
    definition.packed_components[0].diameter = UnitValue(value=1, unit="m")
    item = ReportFixture(definition, render_cross_section_svg(definition), ())
    html = render_html_report(CrossSectionReport((item,), (), []))
    assert "mixed component diameter units" in html
    assert "component max 18 mm" not in html


def builder():
    return runpy.run_path(str(BUILDER))


class Bindings(HTMLParser):
    def __init__(self):
        super().__init__()
        self.values = []
        self.active = None

    def handle_starttag(self, tag, attrs):
        data = dict(attrs)
        if "data-result" in data:
            self.active = data["data-result"]

    def handle_data(self, data):
        if self.active is not None:
            self.values.append((self.active, data))

    def handle_endtag(self, tag):
        self.active = None


def test_module_report_hand_calculation_and_display_bindings():
    """Same independent hand arithmetic as test_section_values, not a stored output oracle.

    A/pi=.0525; mass/(L*pi)=117.875; sealed W/(L*pi)=553.75;
    I/pi=.0009515625; Z/pi=.00380625; EA/pi=2517522500; EI/pi=20586514.0625.
    Synthetic geometry, water rho=1000, g=10; sourced steel rho=7850, E=2e11.
    """
    module = builder()
    data = module["calculate"]()
    expected = {
        "area_m2": 0.0525 * math.pi, "mass_per_length_kg_m": 117.875 * math.pi,
        "submerged_weight_n_m": 553.75 * math.pi, "second_moment_m4": 0.0009515625 * math.pi,
        "section_modulus_m3": 0.00380625 * math.pi, "axial_stiffness_n": 2517522500 * math.pi,
        "bending_stiffness_n_m2": 20586514.0625 * math.pi,
    }
    for name, value in expected.items():
        assert data["results"][name] == pytest.approx(value, rel=1e-12)
    html = module["render"](data)
    parsed = Bindings()
    parsed.feed(html)
    assert {name for name, _ in parsed.values} >= expected.keys()
    for name, text in parsed.values:
        assert text == module["format_number"](data["results"][name])
    assert 'class="doc hs-doc"' in html
    for title in ["Summary", "Revision history", "Inputs", "Method", "Worked example",
                  "Fixture results", "Limits of use", "NOT_EVALUATED", "Sources"]:
        assert title in html
    assert html.index("Summary") < html.index("Revision history") < html.index("Inputs")
    assert "assumed, based on previous experience" in html
    assert "script src=" not in html and "<link" not in html


def test_report_changes_when_production_result_changes(monkeypatch):
    from dataclasses import replace

    module = builder()
    calculate = module["calculate"]
    original = calculate.__globals__["calculate_radial_section"]

    def changed(*args, **kwargs):
        result = original(*args, **kwargs)
        return replace(result, submerged_weight_n_m=result.submerged_weight_n_m * 2)

    before = calculate()
    monkeypatch.setitem(calculate.__globals__, "calculate_radial_section", changed)
    after = calculate()
    assert after["results"]["submerged_weight_n_m"] == before["results"]["submerged_weight_n_m"] * 2
    assert module["render"](after) != module["render"](before)


def test_mass_figure_tracks_calculated_layer_ratios():
    module = builder()
    data = module["calculate"]()
    svg = ET.fromstring(module["mass_chart"](data))
    bars = svg.findall("{http://www.w3.org/2000/svg}rect")
    assert len(bars) == len(data["layers"])
    widths = [float(bar.attrib["width"]) for bar in bars]
    masses = [item.mass_per_length_kg_m for item in data["layers"]]
    # The renderer rounds SVG coordinates to 0.1 drawing units. This is a
    # display precision bound, not an engineering acceptance tolerance.
    for width, mass in zip(widths, masses, strict=True):
        assert width == pytest.approx(widths[0] * mass / masses[0], abs=0.1)


def test_rebuild_matches_committed_artifacts_and_is_repeatable(tmp_path):
    command = [sys.executable, str(BUILDER), "--output-dir", str(tmp_path)]
    subprocess.run(command, check=True, cwd=ROOT, capture_output=True, text=True)
    first = {p.name: p.read_bytes() for p in tmp_path.iterdir()}
    assert set(first) == {"index.html", "changes.json"}
    subprocess.run(command, check=True, cwd=ROOT, capture_output=True, text=True)
    assert {p.name: p.read_bytes() for p in tmp_path.iterdir()} == first
    for name, content in first.items():
        assert (REPORT / name).read_bytes() == content
        relative = str((REPORT / name).relative_to(ROOT))
        assert subprocess.run(["git", "check-ignore", "-q", relative], cwd=ROOT).returncode == 1
    changes = json.loads(first["changes.json"])
    assert changes["revision"] == 0
    assert changes["changes"]


def test_check_mode_detects_stale_output(tmp_path):
    module = builder()
    module["build"](tmp_path)
    (tmp_path / "index.html").write_text("stale report\n")
    result = subprocess.run(
        [sys.executable, str(BUILDER), "--output-dir", str(tmp_path), "--check"],
        cwd=ROOT, capture_output=True, text=True,
    )
    assert result.returncode != 0
    assert "stale" in result.stderr.lower()
