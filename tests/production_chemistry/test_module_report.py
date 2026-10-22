# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild and numerical contracts for the production chemistry analysis."""

import ast
from dataclasses import asdict
import hashlib
import importlib.util
import json
import math
import os
from html.parser import HTMLParser
from pathlib import Path
import subprocess
import sys

import pytest

from digitalmodel.production_chemistry import default_coefficients, saturation_indices, si_profile
from digitalmodel.production_chemistry.scale_si import ION_PROPERTIES

ROOT = Path(__file__).resolve().parents[2]
BUILDER = ROOT / "scripts/production_chemistry/build_module_report.py"
REPORT = ROOT / "docs/reports/modules/productionchemistry/report.html"


class ReportParser(HTMLParser):
    """Read the rendered document, including every annotated numeric value."""

    def __init__(self):
        super().__init__()
        self.json_id = None
        self.json_text = {}
        self.number = None
        self.numbers = []
        self.sections = []
        self.ids = []
        self.links = []
        self.visible = []
        self.hidden = 0

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if attrs.get("id"):
            self.ids.append(attrs["id"])
        if tag == "section":
            self.sections.append(attrs.get("id"))
        if attrs.get("href", "").startswith("#"):
            self.links.append(attrs["href"][1:])
        if tag in {"script", "style"}:
            self.hidden += 1
        if tag == "script" and attrs.get("type") == "application/json":
            self.json_id = attrs["id"]
            self.json_text[self.json_id] = ""
        if "data-number" in attrs:
            self.number = [attrs["data-number"], int(attrs["data-places"]), ""]

    def handle_data(self, data):
        if self.json_id:
            self.json_text[self.json_id] += data
        if self.number is not None:
            self.number[2] += data
        if not self.hidden:
            self.visible.append(data)

    def handle_endtag(self, tag):
        if tag in {"script", "style"}:
            self.hidden -= 1
        if tag == "script":
            self.json_id = None
        if self.number is not None and tag in {"span", "td", "tspan"}:
            self.numbers.append(self.number)
            self.number = None


@pytest.fixture(scope="module")
def rebuilt(tmp_path_factory):
    output = tmp_path_factory.mktemp("productionchemistry")
    assert BUILDER.is_file(), "The report generator has not been implemented"
    env = dict(os.environ, PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    subprocess.run(
        [sys.executable, str(BUILDER), "--output-dir", str(output)],
        cwd=ROOT, env=env, capture_output=True, text=True, check=True,
    )
    parser = ReportParser()
    parser.feed((output / "report.html").read_text())
    return output, parser, json.loads(parser.json_text["report-data"])


def load_example():
    spec = importlib.util.spec_from_file_location(
        "public_scale_example", ROOT / "scripts/production_chemistry/build_scale_si_explorer.py"
    )
    example = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(example)
    return example


def test_rebuild_matches_published_document_and_changes(rebuilt):
    output, _, _ = rebuilt
    assert (output / "report.html").read_bytes() == REPORT.read_bytes()
    assert (output / "changes.json").read_bytes() == REPORT.with_name("changes.json").read_bytes()


def test_every_worked_result_equals_direct_module_calls(rebuilt):
    _, _, data = rebuilt
    example = load_example()
    points = [
        (example.T_BH + (example.T_WH - example.T_BH) * i / (example.N_STEPS - 1),
         example.P_BH + (example.P_WH - example.P_BH) * i / (example.N_STEPS - 1))
        for i in range(example.N_STEPS)
    ]
    expected_profile = si_profile(example.FORMATION_WATER, points)
    assert data["profile"] == expected_profile.to_dict(orient="records")
    for i, (temperature, pressure) in enumerate(points):
        expected = saturation_indices(example.FORMATION_WATER, temperature, pressure)
        for family, result in expected.items():
            record = data["states"][i][family]
            for field in ("si", "log10_iap", "pk_cond"):
                assert record[field] == getattr(result, field)
            assert record["details"] == result.details
    assert data["ionic_strength"] == example.FORMATION_WATER.ionic_strength()
    assert data["inputs"]["brine"] == vars(example.FORMATION_WATER)
    assert data["criterion"] == math.log10(1.0)
    assert data["coefficients"] == {k: asdict(v) for k, v in default_coefficients().items()}
    assert data["ions"] == {k: {"molecular_mass": v[0], "charge": v[1]}
                            for k, v in ION_PROPERTIES.items()}
    tree = ast.parse((ROOT / "tests/production_chemistry/test_scale_si.py").read_text())
    assert data["evidence"]["test_count"] == sum(
        isinstance(item, ast.FunctionDef) and item.name.startswith("test_") for item in tree.body
    )
    for source, digest in data["sources"].items():
        assert hashlib.sha256((ROOT / source).read_bytes()).hexdigest() == digest


def test_every_displayed_number_is_resolved_from_computations(rebuilt):
    _, parser, data = rebuilt
    assert len(parser.numbers) > 180
    for key, places, text in parser.numbers:
        value = data["numbers"][key]
        origin = data
        for field in key.split("."):
            origin = origin[int(field)] if isinstance(origin, list) else origin[field]
        assert value == origin, key
        expected = f"{value:.{places}f}" if places >= 0 else f"{value:.1e}"
        assert text == expected.replace("-", "−"), key
        assert value == 0 or float(text.replace("−", "-")) != 0, key


def test_house_style_sections_and_offline_controls(rebuilt):
    output, parser, _ = rebuilt
    html = (output / "report.html").read_text()
    assert 'class="doc hs-doc"' in html
    assert parser.sections == [f"section-{i}" for i in range(1, 11)]
    assert len(set(parser.ids)) == len(parser.ids)
    assert set(parser.links) <= set(parser.ids)
    assert '<script src=' not in html and '<link rel="stylesheet"' not in html
    assert 'id="comment-text"' in html and 'showSaveFilePicker' in html
    assert 'id="change-panel"' in html and 'type="application/json"' in html
    assert 'SI = 0' in html
    assert 'ASSUMED - to be confirmed' in html
    assert 'NOT_EVALUATED' in html
    text = " ".join(parser.visible)
    for forbidden in ("owner", "would", "could", "should", "validated", "certified"):
        assert forbidden not in text.lower().split()
