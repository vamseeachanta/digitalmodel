# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Test report arithmetic independently of regeneration and serializer rounding.

Basis: structural/pipe_cross_section/docs/worked-layered-pipe-buoyancy.md,
Inputs and Derivation. Exact rational radii are integrated here, independently
of both the calculator and report builder. No rounded presentation value is
an expected value. Force checks use the implementation conversion convention
in calculator.py (not an adopted value of local gravitational acceleration).
"""

import ast
import copy
import hashlib
import importlib.util
import json
import math
import os
import re
import subprocess
import sys
from fractions import Fraction
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_pipe_cross_section_module_report.py"
REPORT = ROOT / "docs/reports/modules/pipe_cross_section"
EXAMPLE = ROOT / "src/digitalmodel/structural/pipe_cross_section/docs/worked-layered-pipe-buoyancy.md"
DELIVERABLES = {"index.html", "results.json", "changes.json", "manifest.json", "README.md"}
# Same binary-roundoff allowances as the executable worked example: relative
# 1e-12, absolute 1e-15 m² and 1e-10 kg/m. Force floor is the mass floor
# converted with the implementation's 9.81 / 1000 kN/N convention.
REL = 1e-12
AREA_ABS = 1e-15
MASS_ABS = 1e-10
FORCE_ABS = MASS_ABS * 9.81 / 1000


def builder():
    spec = importlib.util.spec_from_file_location("pipe_module_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def independent_cases():
    """Integrate s ds dtheta using exact metre radii from the cited example."""
    bore, steel, coated = Fraction(9, 100), Fraction(801, 8000), Fraction(843, 8000)
    steel_coefficient = steel**2 - bore**2
    coating_coefficient = coated**2 - steel**2
    # These identities check the hand derivation, rather than production data.
    assert steel_coefficient == Fraction(123201, 64000000)
    assert coating_coefficient == Fraction(8631, 8000000)
    assert steel**2 == Fraction(641601, 64000000)
    assert coated**2 == Fraction(710649, 64000000)
    steel_density, coating_density, water_density = (
        Fraction("7800.25"), Fraction("1200.75"), Fraction("1020.5")
    )
    expected = {}
    for name, outer in (("bare", steel), ("coated", coated)):
        layers = [(bore, steel, steel_density)]
        if name == "coated":
            layers.append((steel, coated, coating_density))
        areas = [math.pi * float(high**2 - low**2) for low, high, _ in layers]
        masses = [area * float(layer[2]) for area, layer in zip(areas, layers, strict=True)]
        mass = sum(masses)
        displaced = math.pi * float(outer**2)
        water_mass = displaced * float(water_density)
        expected[name] = {
            "inner_diameter_mm": float(2 * bore * 1000),
            "outer_diameter_mm": float(2 * outer * 1000),
            # Exact international inch conversion, exposed by the reviewed
            # mm_to_inch public API through digitalmodel.units/Pint.
            "outer_diameter_inch": float(2 * outer * 1000 / Fraction("25.4")),
            "mass_in_air_kg_m": mass,
            "displaced_area_m2": displaced,
            "displaced_water_mass_kg_m": water_mass,
            "submerged_mass_kg_m": mass - water_mass,
            "air_force_kN_m": mass * 9.81 / 1000,
            "buoyancy_force_kN_m": water_mass * 9.81 / 1000,
            "submerged_force_kN_m": (mass - water_mass) * 9.81 / 1000,
            "is_buoyant": mass - water_mass < 0,
            "layers": [
                {
                    "inner_diameter_mm": float(2 * low * 1000),
                    "outer_diameter_mm": float(2 * high * 1000),
                    "thickness_mm": float((high - low) * 1000),
                    "density_kg_m3": float(density),
                    "area_m2": area,
                    "mass_kg_m": layer_mass,
                }
                for (low, high, density), area, layer_mass in zip(layers, areas, masses, strict=True)
            ],
        }
    return expected


def assert_numeric(actual, expected, field):
    if isinstance(expected, bool):
        assert actual is expected, field
    else:
        floor = AREA_ABS if field.endswith("m2") else MASS_ABS
        if field.endswith("kN_m"):
            floor = FORCE_ABS
        assert actual == pytest.approx(expected, rel=REL, abs=floor), field


def assert_cases_match_oracle(actual):
    """Require every recorded result to have an independently derived oracle."""
    expected = independent_cases()
    assert actual.keys() == expected.keys()
    for case, reference in expected.items():
        assert actual[case].keys() == reference.keys()
        for field, value in reference.items():
            if field == "layers":
                assert len(actual[case][field]) == len(value)
                for layer, reference_layer in zip(actual[case][field], value, strict=True):
                    assert layer.keys() == reference_layer.keys()
                    for key, target in reference_layer.items():
                        assert_numeric(layer[key], target, key)
            else:
                assert_numeric(actual[case][field], value, field)


def public_cases():
    """Replay public APIs separately from both builder and integral arithmetic."""
    from digitalmodel.structural.pipe_cross_section import PipeCrossSection

    results = {}
    for case, coating in (("bare", 0.0), ("coated", 5.25)):
        pipe = PipeCrossSection(
            steel_od_mm=200.25, steel_wt_mm=10.125, steel_density=7800.25,
            lpp_thickness_mm=coating, lpp_density=1200.75,
            concrete_thickness_mm=0.0, seawater_density=1020.5,
            internal_contents="air", internal_fluid_density=0.0,
        )
        results[case] = {
            "inner_diameter_mm": pipe.inner_diameter_mm,
            "outer_diameter_mm": pipe.outer_diameter_mm,
            "outer_diameter_inch": pipe.outer_diameter_inch,
            "mass_in_air_kg_m": pipe.total_weight_in_air_kg_m,
            "displaced_area_m2": pipe.displaced_volume_m3_m,
            "displaced_water_mass_kg_m": pipe.buoyancy_force_kg_m,
            "submerged_mass_kg_m": pipe.submerged_weight_kg_m,
            "air_force_kN_m": pipe.total_weight_in_air_kN_m,
            "buoyancy_force_kN_m": pipe.buoyancy_force_kN_m,
            "submerged_force_kN_m": pipe.submerged_weight_kN_m,
            "is_buoyant": pipe.is_buoyant,
            "layers": [
                {
                    "inner_diameter_mm": layer.inner_diameter_mm,
                    "outer_diameter_mm": layer.outer_diameter_mm,
                    "thickness_mm": layer.thickness_mm,
                    "density_kg_m3": layer.density_kg_m3,
                    "area_m2": layer.cross_sectional_area_m2,
                    "mass_kg_m": layer.weight_per_meter_kg,
                } for layer in pipe.layers
            ],
        }
    return results


def test_layered_pipe_report_matches_independent_polar_integrals():
    """Test normal package imports and unrounded public outputs, in SI units."""
    assert_cases_match_oracle(public_cases())


def build_delivery(folder):
    output = folder / "output"
    environment = os.environ.copy()
    environment["PYTHONPATH"] = str(ROOT / "src")
    subprocess.run(
        [sys.executable, str(SCRIPT), "--scratch", str(folder / "scratch"),
         "--output-dir", str(output)],
        cwd=folder, env=environment, check=True, capture_output=True, text=True, timeout=30,
    )
    return output


@pytest.fixture(scope="module")
def fresh_pair(tmp_path_factory):
    return tuple(build_delivery(tmp_path_factory.mktemp("pipe-report")) for _ in range(2))


def test_rebuild_is_byte_identical(fresh_pair):
    for output in (*fresh_pair, REPORT):
        assert {p.name for p in output.iterdir()} == DELIVERABLES
    for name in sorted(DELIVERABLES):
        assert (fresh_pair[0] / name).read_bytes() == (fresh_pair[1] / name).read_bytes()
        assert (REPORT / name).read_bytes() == (fresh_pair[0] / name).read_bytes(), name


def test_report_values_have_independent_oracles(fresh_pair):
    for output in (*fresh_pair, REPORT):
        data = json.loads((output / "results.json").read_text())
        assert_cases_match_oracle(data["cases"])
        assert data["source_revision"] == "09089a121a16069c4b657d1a320a630a23ab1dd5"
        for name, inputs in data["inputs"].items():
            assert inputs == {
                "steel_od_mm": 200.25, "steel_wt_mm": 10.125, "steel_density": 7800.25,
                "lpp_thickness_mm": 0.0 if name == "bare" else 5.25,
                "lpp_density": 1200.75, "concrete_thickness_mm": 0.0,
                "seawater_density": 1020.5, "internal_contents": "air",
                "internal_fluid_density": 0.0,
            }


def test_visible_results_match_independent_arithmetic():
    """Check rendered cells as well as JSON, without builder formatting helpers."""
    parsed = Page()
    parsed.feed((REPORT / "index.html").read_text())
    expected = independent_cases()
    for key, label, unit, spec in (
        ("inner_diameter_mm", "Bore diameter", "mm", "g"),
        ("outer_diameter_mm", "Exterior diameter", "mm", "g"),
        ("outer_diameter_inch", "Exterior diameter", "inch", ".4f"),
        ("displaced_area_m2", "Displaced volume per length", "m³/m (= m²)", ".4e"),
        ("mass_in_air_kg_m", "Material mass per length", "kg/m", ".1f"),
        ("displaced_water_mass_kg_m", "Displaced water mass per length", "kg/m", ".1f"),
        ("submerged_mass_kg_m", "Submerged mass equivalent per length", "kg/m", ".1f"),
    ):
        cells = [label, *(format(expected[case][key], spec) for case in ("bare", "coated")), unit]
        assert any(row[:4] == cells for row in parsed.rows), key
    for key, label in (
        ("air_force_kN_m", "Material gravitational force per length"),
        ("buoyancy_force_kN_m", "Buoyant force per length"),
        ("submerged_force_kN_m", "Submerged gravitational force equivalent per length"),
    ):
        cells = [label, *(f'{expected[case][key]:.3f}' for case in ("bare", "coated"))]
        assert any(row[:3] == cells for row in parsed.rows), key
    for case, values in expected.items():
        for index, layer in enumerate(values["layers"]):
            cells = [case + " / " + ("steel" if index == 0 else "coating")]
            cells += [format(layer[key], "g") for key in (
                "inner_diameter_mm", "outer_diameter_mm", "thickness_mm", "density_kg_m3"
            )]
            cells += [f'{layer["area_m2"]:.4e}', f'{layer["mass_kg_m"]:.1f}']
            assert any(row[:7] == cells for row in parsed.rows)


def test_production_replay_matches_executable_example(fresh_pair, tmp_path):
    """Freshness against public APIs and the document is not the numeric oracle."""
    data = json.loads((fresh_pair[0] / "results.json").read_text())
    assert data["cases"] == public_cases()
    blocks = re.findall(r"```python\n(.*?)\n```", EXAMPLE.read_text(), re.S)
    assert len(blocks) == 1
    script = tmp_path / "example.py"
    script.write_text(blocks[0])
    environment = os.environ.copy()
    environment["PYTHONPATH"] = str(ROOT / "src")
    subprocess.run([sys.executable, str(script), str(tmp_path)], cwd=tmp_path,
                   env=environment, check=True, capture_output=True, timeout=30)
    documented = json.loads((tmp_path / "layered-pipe-buoyancy.json").read_text())
    for case in documented:
        for field, value in documented[case]["live"].items():
            assert data["cases"][case][field] == value


def assert_manifest(directory):
    """Independently read delivered bytes; do not use a builder hash helper."""
    manifest = json.loads((directory / "manifest.json").read_text())
    assert set(manifest["artifacts"]) == DELIVERABLES - {"manifest.json"}
    assert "scripts/reports/build_pipe_cross_section_module_report.py" in manifest["sources"]
    assert "tests/reporting/test_pipe_cross_section_module_report.py" in manifest["sources"]
    for category, root in (("artifacts", directory), ("sources", ROOT)):
        for relative, digest in manifest[category].items():
            path = Path(relative)
            assert not path.is_absolute() and ".." not in path.parts
            assert hashlib.sha256((root / path).read_bytes()).hexdigest() == digest, relative


def test_manifest_binds_delivered_bytes(fresh_pair):
    for output in (*fresh_pair, REPORT):
        assert_manifest(output)


@pytest.mark.parametrize("fault", ["value", "sign"])
def test_tampered_result_is_detected(fresh_pair, tmp_path, fault):
    for path in fresh_pair[0].iterdir():
        (tmp_path / path.name).write_bytes(path.read_bytes())
    data = json.loads((tmp_path / "results.json").read_text())
    original = data["cases"]["coated"]["submerged_mass_kg_m"]
    data["cases"]["coated"]["submerged_mass_kg_m"] = original + 1 if fault == "value" else -original
    (tmp_path / "results.json").write_text(json.dumps(data))
    with pytest.raises(AssertionError, match="results.json"):
        assert_manifest(tmp_path)
    # Even a consistently rehashed delivery cannot defeat the numeric oracle.
    manifest = json.loads((tmp_path / "manifest.json").read_text())
    manifest["artifacts"]["results.json"] = hashlib.sha256((tmp_path / "results.json").read_bytes()).hexdigest()
    (tmp_path / "manifest.json").write_text(json.dumps(manifest))
    assert_manifest(tmp_path)
    with pytest.raises(AssertionError, match="submerged_mass_kg_m"):
        assert_cases_match_oracle(data["cases"])


@pytest.mark.parametrize("fault", ["value", "sign"])
def test_consistent_regeneration_still_fails_oracle(tmp_path, monkeypatch, fault):
    build = builder()
    original = build.calculate

    def changed(scratch):
        data = copy.deepcopy(original(scratch))
        value = data["cases"]["bare"]["submerged_mass_kg_m"]
        data["cases"]["bare"]["submerged_mass_kg_m"] = value + 1 if fault == "value" else -value
        return data

    monkeypatch.setattr(build, "calculate", changed)
    output = tmp_path / "output"
    build.build(tmp_path / "scratch", output)
    data = json.loads((output / "results.json").read_text())
    page = (output / "index.html").read_text()
    payload = re.search(r'<script id="calculation" type="application/json">(.*?)</script>', page, re.S)
    assert json.loads(payload[1]) == data
    assert_manifest(output)
    value = data["cases"]["bare"]["submerged_mass_kg_m"]
    assert f"{value:.1f}" in page
    with pytest.raises(AssertionError, match="submerged_mass_kg_m"):
        assert_cases_match_oracle(data["cases"])


class Page(HTMLParser):
    def __init__(self):
        super().__init__()
        self.ids = set()
        self.links = []
        self.scripts = []
        self.headings = []
        self.tag = None
        self.rows = []
        self.row = None
        self.cell = None

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "tr":
            self.row = []
        if tag in {"td", "th"}:
            self.cell = ""
        if "id" in attrs:
            assert attrs["id"] not in self.ids
            self.ids.add(attrs["id"])
        for attribute in ("href", "src"):
            if attribute in attrs:
                self.links.append((tag, attrs[attribute]))
        if tag == "script":
            self.scripts.append(attrs)
        self.tag = tag

    def handle_endtag(self, tag):
        if tag in {"td", "th"} and self.cell is not None:
            self.row.append(" ".join(self.cell.split()))
            self.cell = None
        if tag == "tr" and self.row is not None:
            self.rows.append(self.row)
            self.row = None
        if self.tag == tag:
            self.tag = None

    def handle_data(self, value):
        if self.cell is not None:
            self.cell += value
        if self.tag in {"h2", "h3"}:
            self.headings.append(value)


def test_report_links_and_required_sections():
    page = (REPORT / "index.html").read_text()
    parsed = Page()
    parsed.feed(page)
    assert {f"s{number}" for number in range(1, 11)} <= parsed.ids
    expected = [
        "1. Summary", "Revision history", "2. Context and scope", "3. Input basis",
        "4. Method", "5. Worked calculation", "6. Checks", "7. Results",
        "8. Limits / NOT_EVALUATED", "9. Way forward", "10. References and evidence",
    ]
    assert parsed.headings == expected
    for tag, link in parsed.links:
        parts = urlsplit(link)
        assert not parts.scheme and not parts.netloc, link
        if parts.path:
            assert (REPORT / unquote(parts.path)).resolve().is_file(), link
        elif parts.fragment:
            assert parts.fragment in parsed.ids, link
    assert all("src" not in script and script.get("type") == "application/json"
               for script in parsed.scripts)
    payload = re.search(r'<script id="calculation" type="application/json">(.*?)</script>', page, re.S)
    assert json.loads(payload[1]) == json.loads((REPORT / "results.json").read_text())
    changes = json.loads((REPORT / "changes.json").read_text())
    for change in changes:
        for value in change.values():
            assert value in page
    assert '<svg ' in page and 'class="hs-doc"' in page
    assert "sealed" in page and "closure" in page and "NOT_EVALUATED" in page
    assert "/" + "home/" not in page and "/" + "mnt/" not in page


def test_text_and_json_are_escaped(tmp_path):
    build = builder()
    data = build.calculate(tmp_path / "scratch")
    data["inputs"]["bare"]["internal_contents"] = '</script><img src="bad" onerror="bad">&'
    page = build.render(data)
    assert '&lt;/script&gt;&lt;img' in page
    assert '</script><img' not in page
    payload = re.search(r'<script id="calculation" type="application/json">(.*?)</script>', page, re.S)
    assert json.loads(payload[1]) == data


def test_scratch_must_be_external():
    with pytest.raises(ValueError, match="outside"):
        builder().calculate(ROOT / "scratch")


def test_inventory_covers_public_functions_and_classes():
    build = builder()
    listed = {}
    for row in build.inventory():
        listed.setdefault(row["source"], {}).update(row["symbols"])
    for module in ("models", "calculator", "visualization", "cli"):
        source = f"{build.PACKAGE}/{module}.py"
        expected = {}
        for node in ast.parse((ROOT / source).read_text()).body:
            if isinstance(node, (ast.ClassDef, ast.FunctionDef)) and not node.name.startswith("_"):
                expected[node.name] = node.lineno
                if isinstance(node, ast.ClassDef):
                    for method in node.body:
                        if isinstance(method, ast.FunctionDef) and not method.name.startswith("_"):
                            expected[f"{node.name}.{method.name}"] = method.lineno
        assert listed[source] == expected
