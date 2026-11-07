# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild and numerical binding checks for the cross-section module analysis."""

from html.parser import HTMLParser
import json
import math
from pathlib import Path
import re
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


def test_module_report_has_reasoned_output_ownership():
    from scripts import check_generated_html as checker

    relative = BUILDER.relative_to(ROOT).as_posix()
    reason = checker.DISCOVERY_FALSE_POSITIVES[relative]
    for name in ("index.html", "changes.json"):
        assert (REPORT / name).relative_to(ROOT).as_posix() in reason
    assert "test_module_report.py" in reason
    assert checker.validate_registry(ROOT) == []


@pytest.mark.parametrize("unit", ["m", "inch"])
def test_fixture_inventory_rejects_non_mm_outer_diameter(monkeypatch, unit):
    module = builder()
    report = build_report_model()
    fixture = next(item for item in report.fixtures if item.definition.radial_layers)
    fixture.definition.radial_layers[-1].outer_diameter.unit = unit
    monkeypatch.setitem(module["calculate"].__globals__, "build_report_model", lambda: report)
    with pytest.raises(ValueError, match="millimetres"):
        module["calculate"]()


@pytest.mark.parametrize("value,expected", [
    (0.0525 * math.pi, "0.1649"), (0.0625 * math.pi, "0.1963"),
    (0.0009515625 * math.pi, "0.002989"),
])
def test_report_numbers_retain_four_significant_figures(value, expected):
    assert builder()["format_number"](value) == expected


@pytest.mark.parametrize("value,expected", [
    (553.75 * math.pi, "1,739.7"), (653.75 * math.pi, "2,053.8"),
    (2517522500 * math.pi, "7.909 × 10⁹"),
    (20586514.0625 * math.pi, "6.467 × 10⁷"),
    (0, "0.0"), (-1739.7, "-1,739.7"),
    (0.001, "0.001000"), (999999.1, "999,999.1"),
    (1e6, "1.000 × 10⁶"), (1e-4, "1.000 × 10⁻⁴"),
])
def test_report_numbers_use_grouping_and_readable_exponents(value, expected):
    assert builder()["format_number"](value) == expected


def test_report_inputs_preserve_entered_moduli():
    module = builder()
    html = module["render"](module["calculate"]())
    for value in ("200,000,000,000", "1,000,000,000", "1,000,000"):
        assert f"<td>{value}</td>" in html


@pytest.mark.parametrize("water_density,gravity", [(1000, 10), (1025.5, 9.81)])
def test_report_environment_inputs_preserve_entered_values(water_density, gravity):
    module = builder()
    data = module["calculate"]()
    data["environment"] = {
        "water_density_kg_m3": water_density, "gravity_m_s2": gravity,
    }
    html = module["render"](data)
    assert f"Water density is {water_density:,} kg/m³" in html
    assert f"Gravity is {gravity:,} m/s²" in html


def test_worked_modulus_provenance_distinguishes_reference_and_assumptions(monkeypatch):
    module = builder()
    captured = []
    original = module["calculate"].__globals__["calculate_elastic_section"]

    def capture(items):
        captured.extend(items)
        return original(items)

    monkeypatch.setitem(module["calculate"].__globals__, "calculate_elastic_section", capture)
    module["calculate"]()
    steel, coating, insulation = [item.modulus_provenance for item in captured]
    assert steel.source_type == "reference"
    assert steel.derived_from == []
    assert steel.citation == module["SOURCES"]["elasticity"][0]
    assert steel.url_or_path == module["SOURCES"]["elasticity"][1]
    assert "Table 12.1" in steel.note
    assert steel.note == steel.note.rstrip()
    for source in (coating, insulation):
        assert source.source_type == "project_assumption"
        assert source.note == "assumed, based on previous experience"
        assert source.citation is None
        assert source.url_or_path is None


def test_worked_density_provenance_distinguishes_reference_and_assumptions():
    module = builder()
    definition = module["example_definition"]()
    steel, coating, insulation = [layer.provenance for layer in definition.radial_layers]
    assert steel.source_type == "reference"
    assert steel.citation == module["SOURCES"]["steel"][0]
    assert steel.url_or_path == module["SOURCES"]["steel"][1]
    assert "geometry" in steel.note.lower() and "assumed" in steel.note.lower()
    assert steel in definition.provenance
    for record in (coating, insulation):
        assert record.source_type == "project_assumption"
        assert "assumed, based on previous experience" in record.note
        assert record.citation is None and record.url_or_path is None


@pytest.mark.parametrize("property_name", ["density", "modulus"])
def test_report_material_bases_follow_independent_provenance(monkeypatch, property_name):
    module = builder()
    original = module["calculate"].__globals__["calculate_elastic_section"]

    def swap_basis(items):
        steel, coating, _ = items
        assumed = Provenance(source_id="assumed-property", source_type="project_assumption")
        referenced = Provenance(
            source_id="referenced-property", source_type="reference", citation="Illustrative reference",
        )
        if property_name == "density":
            steel.layer.provenance = assumed
            coating.layer.provenance = referenced
        else:
            steel.modulus_provenance = assumed
            coating.modulus_provenance = referenced
        return original(items)

    monkeypatch.setitem(module["calculate"].__globals__, "calculate_elastic_section", swap_basis)
    data = module["calculate"]()
    html = module["render"](data)
    assert '<th scope="col">Density basis</th>' in html
    assert '<th scope="col">E basis</th>' in html
    rows = re.findall(r"<tr><td>(steel|coating)</td>(.*?)</tr>", html)
    input_rows = [re.findall(r"<td>(.*?)</td>", cells) for _, cells in rows if cells.count("<td>") == 6]
    assumed_basis = "assumed, based on previous experience"
    reference_basis = "public reference"
    expected = [[assumed_basis, reference_basis], [reference_basis, assumed_basis]]
    if property_name == "modulus":
        expected = [list(reversed(pair)) for pair in expected]
    assert [cells[-2:] for cells in input_rows] == expected


def test_report_writes_explicit_lf_newlines(tmp_path, monkeypatch):
    written_newlines = []
    original = Path.write_text

    def record_write(path, text, **kwargs):
        written_newlines.append(kwargs.get("newline"))
        return original(path, text, **kwargs)

    monkeypatch.setattr(Path, "write_text", record_write)
    builder()["build"](tmp_path)
    assert written_newlines == ["\n", "\n"]
    assert all(b"\r\n" not in path.read_bytes() for path in tmp_path.iterdir())


def test_module_retention_preserves_existing_main_reports():
    retained = (
        "gis/report.html", "passing_ship/build_worked_example.py",
        "pipe_capacity/build_worked_example.py", "riserfatigue/report.html",
        "vesselseakeeping/index.html", "cross_sections/index.html",
        "cross_sections/changes.json",
    )
    for relative in retained:
        path = "docs/reports/modules/" + relative
        result = subprocess.run(
            ["git", "check-ignore", "--no-index", "-q", path], cwd=ROOT,
        )
        assert result.returncode == 1, path
    assert subprocess.run(
        ["git", "check-ignore", "--no-index", "-q",
         "docs/reports/modules/cross_sections/scratch.html"], cwd=ROOT,
    ).returncode == 0


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
    assert changes["revision"] == 5
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
