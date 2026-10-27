# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent preparation anchors; no published settlement FEA qualification."""
import copy
import json
import math
import os
import shlex
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

from digitalmodel.asset_integrity.tank_settlement_level3 import (
    main,
    prepare_assessment,
    pressure_at_elevation,
    settlement_displacement,
)

ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = ROOT / "examples/workflows/api579-tank-settlement-level3/input.json"


@pytest.fixture
def settings():
    return json.loads(EXAMPLE.read_text(encoding="utf-8"))


def test_harmonic_hand_calculation():
    """NIST DLMF (2010), 1.8.1, finite Fourier series, mean=a0/2.

    Synthetic coefficients: mean=-0.010 m, a1=0.002 m, b2=0.003 m.
    At 0 degrees: -0.010+0.002=-0.008 m; at 90: -0.010 m;
    at 45: -0.010+0.002/sqrt(2)+0.003=-0.005585786437626905 m.
    Positive displacement is upward; negative displacement is settlement.
    """
    harmonics = [{"order": 1, "cos_m": 0.002, "sin_m": 0.0},
                 {"order": 2, "cos_m": 0.0, "sin_m": 0.003}]
    assert settlement_displacement(0, -0.010, harmonics) == pytest.approx(-0.008)
    assert settlement_displacement(90, -0.010, harmonics) == pytest.approx(-0.010)
    assert settlement_displacement(45, -0.010, harmonics) == pytest.approx(
        -0.005585786437626905)


def test_published_hydrostatic_anchor():
    """OpenStax University Physics Vol. 1 (2016), 14.1, Example 14.1.

    Gauge pressure at 40 m depth: 1000 kg/m3 * 9.80 m/s2 * 40 m
    = 392000 Pa. At/above the free surface gauge pressure is zero.
    """
    assert pressure_at_elevation(40, 80, 1000, 9.80) == pytest.approx(392000)
    assert pressure_at_elevation(80, 80, 1000, 9.80) == 0
    assert pressure_at_elevation(90, 80, 1000, 9.80) == 0


def test_component_and_load_hand_calculation(settings):
    """Synthetic mid-surface areas times thickness, then W=rho*g*volume.

    R=10 m, two 5 m courses t=0.010,0.008 m: V_shell=1.8*pi m3.
    Bottom disk radius 9 m, t=0.006: V=0.486*pi m3.
    Annulus radii 9,10 m, t=0.010: V=0.190*pi m3.
    Top angle A=0.001 m2: V=0.020*pi m3; total V=2.496*pi m3.
    Synthetic density=7800 kg/m3, g=10 m/s2: W=194688*pi N.
    Bottom pressure=800*10*8=64000 Pa; at z=5: 24000 Pa.
    """
    result = prepare_assessment(settings)
    assert result["loads"]["self_weight_n"] == pytest.approx(194688 * math.pi)
    assert result["loads"]["bottom_pressure_pa"] == pytest.approx(64000)
    courses = result["model_recipe"]["courses"]
    assert [row["z_top_m"] for row in courses] == [5, 10]
    assert courses[0]["pressure_top_pa"] == pytest.approx(24000)
    assert courses[1]["pressure_top_pa"] == 0
    assert result["loads"]["bottom_edge_samples"][0]["uz_m"] == pytest.approx(-0.008)
    assert result["input_sha256"] == prepare_assessment(copy.deepcopy(settings))["input_sha256"]
    json.dumps(result, allow_nan=False)


def test_missing_physics_never_accepts(settings):
    result = prepare_assessment(settings)
    assert result["status"] == "handoff_required"
    assert result["decision"]["verdict"] == "ESCALATE"
    assert set(result["checks"]) == {"strain_limit", "buckling", "plastic_collapse", "weld_strain"}
    assert all(check["status"] == "NOT_EVALUATED" for check in result["checks"].values())
    assert all(check["required_evidence"] for check in result["checks"].values())
    assert result["checks"]["plastic_collapse"]["role"] == "secondary"
    assert result["qualification"] == "preparation_hand_calculations_only"
    assert "NOT_EVALUATED" in result["report_html"]
    assert "Revision history" in result["report_html"]
    assert "Annex 2D" in result["report_html"]
    assert result["model_recipe"]["mesh"]["status"] == "NOT_EVALUATED"


@pytest.mark.parametrize("section,key,value", [
    ("geometry", "radius_m", 0),
    ("geometry", "radius_m", True),
    ("geometry", "radius_m", float("nan")),
    ("loads", "fill_height_m", 11),
    ("loads", "gravity_m_s2", 0),
    ("loads", "product_density_kg_m3", -1),
    ("loads", "steel_density_kg_m3", "7800"),
    ("mesh", "junction_size_m", 2),
])
def test_invalid_scalars(settings, section, key, value):
    settings[section][key] = value
    with pytest.raises(ValueError):
        prepare_assessment(settings)


@pytest.mark.parametrize("mutate", [
    lambda s: s["geometry"].update(courses=[]),
    lambda s: s["geometry"]["courses"][0].update(thickness_m=-0.01),
    lambda s: s["geometry"]["annular_ring"].update(width_m=10),
    lambda s: s["geometry"]["bottom"].update(material_ref=""),
    lambda s: s["settlement"].update(angles_deg=[0, 360]),
    lambda s: s["settlement"].update(angles_deg=[0, 0]),
    lambda s: s["settlement"].update(source_ref=""),
    lambda s: s["settlement"]["harmonics"][0].update(order=True),
    lambda s: s["settlement"]["harmonics"].append(s["settlement"]["harmonics"][0]),
    lambda s: s.update(unknown_solver_result={"verdict": "ACCEPT"}),
])
def test_invalid_structures(settings, mutate):
    mutate(settings)
    with pytest.raises(ValueError):
        prepare_assessment(settings)


def test_optional_roof_girder_wind(settings):
    """Added flat roof volume=pi*100*0.004; girder volume=20*pi*0.002.

    Extra weight=(0.400+0.040)*pi*7800*10=34320*pi N.
    Wind is retained as a referenced load intent and is never a solved load.
    """
    settings["geometry"]["roof"] = {"type": "flat", "thickness_m": 0.004,
                                     "material_ref": "synthetic steel"}
    settings["geometry"]["wind_girders"] = [
        {"elevation_m": 7, "area_m2": 0.002, "material_ref": "synthetic steel"}]
    settings["loads"]["wind"] = {"pressure_pa": 100, "source_ref": "synthetic wind case"}
    result = prepare_assessment(settings)
    assert result["loads"]["self_weight_n"] == pytest.approx(229008 * math.pi)
    assert result["loads"]["wind"]["status"] == "NOT_EVALUATED"


def test_cli_and_registry(tmp_path):
    """Standalone route writes and reads a handoff and report without any solver."""
    row = next(row for row in yaml.safe_load((ROOT / "docs/registry/workflows.yaml").read_text(encoding="utf-8"))[
        "workflows"] if row["id"] == "api579-tank-settlement-level3")
    assert row["runtime"] == "standalone"
    assert (ROOT / row["input"]).resolve() == EXAMPLE
    assert row["invocation"].startswith("uv run --no-sources python -m ")
    assert row["test"] == (
        "tests/asset_integrity/test_tank_settlement_level3.py::test_cli_and_registry")
    assert row["outputs"] == ["{output_dir}/handoff.json", "{output_dir}/report.html"]
    arguments = shlex.split(row["invocation"])[4:]
    arguments = [arg.format(input=EXAMPLE.as_posix(), output_dir=tmp_path.as_posix())
                 for arg in arguments]
    completed = subprocess.run(
        [sys.executable, *arguments],
        capture_output=True, text=True, encoding="utf-8", check=False,
        env={**os.environ, "PYTHONPATH": str(ROOT / "src")})
    assert completed.returncode == 0, completed.stderr
    handoff, report = [Path(output.format(output_dir=tmp_path.as_posix()))
                       for output in row["outputs"]]
    result = json.loads(handoff.read_text(encoding="utf-8"))
    assert result["decision"]["verdict"] == "ESCALATE"
    assert report.read_text(encoding="utf-8") == result["report_html"]
    assert b"\r" not in handoff.read_bytes()
    assert b"\r" not in report.read_bytes()
    assert "handoff_required" in completed.stdout


def test_digest_changes_without_mutating_inputs(settings):
    before = copy.deepcopy(settings)
    first = prepare_assessment(settings)
    assert settings == before
    settings["settlement"]["mean_m"] = -0.020
    second = prepare_assessment(settings)
    assert first["input_sha256"] != second["input_sha256"]
    assert first["input"]["settlement"]["mean_m"] == -0.010


@pytest.mark.parametrize("mutate", [
    lambda s: s["geometry"].update(radius_m=1e200),
    lambda s: s["settlement"]["harmonics"][0].update(order=10**400),
])
def test_arithmetic_overflow_rejected(settings, mutate):
    mutate(settings)
    with pytest.raises(ValueError, match="finite|range"):
        prepare_assessment(settings)


def test_report_escapes_input(settings):
    settings["geometry"]["bottom"]["material_ref"] = "<script>bad()</script>"
    result = prepare_assessment(settings)
    # Input is preserved in JSON, but cannot become executable HTML.
    assert "<script>bad()" not in result["report_html"]
    assert "&lt;script&gt;bad()&lt;/script&gt;" in result["report_html"]


@pytest.mark.parametrize("key,value", [
    ("basis", "conflicting basis"),
    ("fill_height_m", 0.5),
    ("fill_height_m", 8.0),
    ("thickness_m", 0.0254),
    ("cos_m", 0.1),
])
def test_duplicate_json_keys_rejected_before_writing(settings, tmp_path, key, value):
    raw = json.dumps(settings)
    marker = json.dumps(key) + ":"
    raw = raw.replace(marker, marker + json.dumps(value) + ", " + marker, 1)
    input_path = tmp_path / "input.json"
    input_path.write_text(raw, encoding="utf-8", newline="\n")
    output = tmp_path / "output"
    with pytest.raises(ValueError, match="Duplicate JSON key"):
        main([input_path.as_posix(), "--output-dir", output.as_posix()])
    assert not output.exists()


def test_escaped_duplicate_json_key_rejected(settings, tmp_path):
    raw = json.dumps(settings).replace(
        '"fill_height_m":', '"fill_\\u0068eight_m": 0.5, "fill_height_m":')
    input_path = tmp_path / "input.json"
    input_path.write_text(raw, encoding="utf-8", newline="\n")
    with pytest.raises(ValueError, match="Duplicate JSON key"):
        main([input_path.as_posix(), "--output-dir", (tmp_path / "output").as_posix()])
    assert not (tmp_path / "output").exists()


def test_required_evidence_is_independent_per_handoff(settings):
    first = prepare_assessment(settings)
    expected = copy.deepcopy(first["checks"])
    for check in first["checks"].values():
        check["required_evidence"].clear()
    assert prepare_assessment(settings)["checks"] == expected


def test_unevaluated_applicability_is_flagged(settings):
    decision = prepare_assessment(settings)["decision"]
    assert decision["verdict"] == "ESCALATE"
    assert decision["applicability"]["ok"] is False
    assert decision["applicability"]["flags"]
    assert any("not evaluated" in note.lower()
               for note in decision["applicability"]["notes"])
    assert all(decision[key] is None for key in (
        "margin", "margin_allowable", "remaining_life_yr", "rsf", "rsf_a"))


@pytest.mark.parametrize("component,missing", [("roof", "type"), ("wind_girders", "elevation_m")])
def test_component_required_fields(settings, component, missing):
    block = {"material_ref": "synthetic steel"}
    block.update({"thickness_m": 0.004} if component == "roof" else {"area_m2": 0.002})
    settings["geometry"][component] = block if component == "roof" else [block]
    with pytest.raises(ValueError, match=f"Missing keys.*{missing}"):
        prepare_assessment(settings)


def test_zero_girder_elevation_is_valid(settings):
    settings["geometry"]["wind_girders"] = [
        {"elevation_m": 0, "area_m2": 0.002, "material_ref": "synthetic steel"}]
    assert prepare_assessment(settings)["model_recipe"]["wind_girders"][0]["elevation_m"] == 0


class ReportTables(HTMLParser):
    """Read actual table cells, including escaped text, without a browser."""

    def __init__(self):
        super().__init__()
        self.rows = []
        self.cells = []
        self.cell = None

    def handle_starttag(self, tag, attrs):
        if tag == "tr":
            self.cells = []
        elif tag == "td":
            self.cell = ""

    def handle_data(self, data):
        if self.cell is not None:
            self.cell += data

    def handle_endtag(self, tag):
        if tag == "td":
            self.cells.append(self.cell)
            self.cell = None
        elif tag == "tr" and self.cells:
            self.rows.append(self.cells)


def test_report_tables_show_load_basis_and_nested_data(settings):
    settings["geometry"]["courses"][0]["thickness_m"] = 0.0254
    settings["loads"]["wind"] = {"pressure_pa": 100, "source_ref": "synthetic wind case"}
    result = prepare_assessment(settings)
    tables = ReportTables()
    tables.feed(result["report_html"])
    rows = dict(tables.rows)
    for key, value in settings["loads"].items():
        if key != "wind":
            assert float(rows[key]) == pytest.approx(value)
    assert rows["source_ref"] == settings["settlement"]["source_ref"]
    assert float(rows["mean_m"]) == pytest.approx(-0.010)
    for index, harmonic in enumerate(settings["settlement"]["harmonics"], 1):
        for key, value in harmonic.items():
            assert float(rows[f"harmonics[{index}].{key}"]) == pytest.approx(value)
    assert rows["courses[1].thickness_m"] == "0.0254"
    assert rows["courses[2].material_ref"] == "synthetic steel"
    assert rows["wind.pressure_pa"] == "100"
    assert rows["wind.source_ref"] == "synthetic wind case"
    for check in result["checks"].values():
        for evidence in check["required_evidence"]:
            assert any(value == evidence for _key, value in tables.rows)
    for requirement in result["handoff_required"]:
        assert any(value == requirement for _key, value in tables.rows)
    assert all(not value.startswith(("{", "[")) for _key, value in tables.rows)


def test_report_numeric_anchors(settings):
    tables = ReportTables()
    tables.feed(prepare_assessment(settings)["report_html"])
    rows = dict(tables.rows)
    # W=2.496*pi*7800*10=611630.3905 N; p=800*10*8=64000 Pa;
    # u(0)=-0.010+0.002=-0.008 m; the displayed results use three significant figures.
    assert float(rows["self_weight_n"]) == pytest.approx(611630.3905, rel=0.001)
    assert float(rows["bottom_pressure_pa"]) == pytest.approx(64000)
    assert float(rows["0"]) == pytest.approx(-0.008)
    assert rows["self_weight_n"] == "6.12e+05"


def test_report_derived_inventory_precision(settings):
    settings["geometry"]["courses"][0]["height_m"] = 5.1
    settings["geometry"]["courses"][1]["height_m"] = 4.9
    settings["loads"]["gravity_m_s2"] = 9.81
    tables = ReportTables()
    tables.feed(prepare_assessment(settings)["report_html"])
    rows = dict(tables.rows)
    # p(5.1)=800*9.81*(8-5.1)=22759.2 Pa, displayed to three significant figures.
    assert rows["courses[1].pressure_top_pa"] == "2.28e+04"
    assert float(rows["courses[1].pressure_top_pa"]) == pytest.approx(22759.2, rel=0.002)
    assert rows["gravity_m_s2"] == "9.81"
