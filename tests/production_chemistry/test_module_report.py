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
import shutil
import subprocess
import sys

import pytest

from digitalmodel.production_chemistry import default_coefficients, saturation_indices, si_profile
from digitalmodel.production_chemistry.scale_si import ION_PROPERTIES

ROOT = Path(__file__).resolve().parents[2]
BUILDER = ROOT / "scripts/production_chemistry/build_module_report.py"
REPORT = ROOT / "docs/reports/modules/productionchemistry/index.html"


@pytest.fixture
def builder():
    spec = importlib.util.spec_from_file_location("module_report", BUILDER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_report_outputs_are_registered_for_isolated_regeneration():
    from scripts import check_generated_html as checker

    entries = [entry for entry in checker.GENERATORS
               if entry.script == "scripts/production_chemistry/build_module_report.py"]
    assert len(entries) == 1
    assert set(entries[0].outputs) == {
        "docs/reports/modules/productionchemistry/index.html",
        "docs/reports/modules/productionchemistry/changes.json",
    }
    assert {"pyproject.toml", "tests/production_chemistry/test_scale_si.py"} <= set(entries[0].inputs)


def test_embedded_values_have_portable_precision(builder):
    assert builder.build_data()["ionic_strength"] == 1.59318959253


def test_builder_uses_the_registered_index_output_name(builder, tmp_path):
    builder.build(tmp_path)
    assert (tmp_path / "index.html").is_file()
    assert (tmp_path / "changes.json").is_file()


@pytest.mark.parametrize("family,correlation", [("calcite", "calcite_co2"), ("barite", "barite")])
def test_printed_coefficients_reproduce_wellhead_indices(builder, family, correlation):
    data = builder.build_data()
    parser = ReportParser()
    parser.feed("".join(builder.render_body(data)))
    coefficients = {
        key.rsplit(".", 1)[1]: float(text.replace("−", "-"))
        for key, _, text in parser.numbers if key.startswith(f"coefficients.{correlation}.")
    }
    temperature, pressure = data["profile"][-1]["t_f"], data["profile"][-1]["p_psia"]
    strength = data["ionic_strength"]
    terms = dict(a0=1, a_t=temperature, a_t2=temperature**2, a_t3=temperature**3,
                 a_p=pressure, a_sqrt_i=math.sqrt(strength), a_i=strength)
    constant = sum(coefficients[key] * factor for key, factor in terms.items())
    result = data["states"][-1][family]
    assert constant + result["log10_iap"] == pytest.approx(result["si"], abs=1e-6)


def test_build_is_independent_of_windows_text_defaults(builder, monkeypatch, tmp_path):
    read_text, write_text = Path.read_text, Path.write_text

    def windows_read(path, encoding=None, errors=None):
        return read_text(path, encoding=encoding or "cp1252", errors=errors)

    def windows_write(path, data, encoding=None, errors=None, newline=None):
        return write_text(path, data, encoding=encoding or "cp1252", errors=errors,
                          newline="\r\n" if newline is None else newline)

    monkeypatch.setattr(Path, "read_text", windows_read)
    monkeypatch.setattr(Path, "write_text", windows_write)
    builder.build(tmp_path)
    html = next(tmp_path.glob("*.html")).read_bytes()
    assert "°F" in html.decode("utf-8")
    assert b"\r\n" not in html
    assert b"\r\n" not in (tmp_path / "changes.json").read_bytes()


def test_source_records_ignore_checkout_line_endings(builder, monkeypatch, tmp_path):
    expected = builder.build_data()["sources"]
    for relative in expected:
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes((ROOT / relative).read_bytes().replace(b"\r\n", b"\n").replace(b"\n", b"\r\n"))
    monkeypatch.setattr(builder, "ROOT", tmp_path)
    monkeypatch.setattr(builder, "read_example", load_example)
    assert builder.build_data()["sources"] == expected


@pytest.mark.parametrize("index", [math.nan, math.inf, -math.inf])
def test_nonfinite_endpoint_is_not_evaluated(builder, index):
    data = builder.build_data()
    data["states"][-1]["barite"]["si"] = index
    parser = ReportParser()
    parser.feed(builder.render_body(data)[4])
    row = " ".join(parser.visible).split("Barite", 1)[1].split("Celestite", 1)[0]
    assert "NOT_EVALUATED" in row
    assert "Supersaturation is not indicated" not in row


def test_module_ignore_rules_preserve_earlier_sibling_exceptions(tmp_path):
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}}
    subprocess.run(["git", "init", "-q", str(tmp_path)], env=env, check=True)
    rules = (ROOT / ".gitignore").read_text(encoding="utf-8")
    marker = "# Retain the production chemistry module analysis and its change register."
    sibling = "!docs/reports/modules/\ndocs/reports/modules/*\n!docs/reports/modules/example/\n!docs/reports/modules/example/index.html\n"
    (tmp_path / ".gitignore").write_text(rules.replace(marker, sibling + marker), encoding="utf-8")
    result = subprocess.run(
        ["git", "check-ignore", "--no-index", "docs/reports/modules/example/index.html"],
        cwd=tmp_path, env=env, capture_output=True, check=False,
    )
    assert result.returncode == 1, result.stdout


def test_offline_script_regressions_run_in_node():
    assert shutil.which("node"), "Node is required for the offline interaction checks"
    subprocess.run(["node", "--test", str(Path(__file__).with_suffix(".js"))],
                   cwd=ROOT, check=True, capture_output=True, text=True, encoding="utf-8")


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
    parser.feed((output / "index.html").read_text(encoding="utf-8"))
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
    assert (output / "index.html").read_bytes() == REPORT.read_text(encoding="utf-8").encode("utf-8")
    published_changes = REPORT.with_name("changes.json").read_text(encoding="utf-8")
    assert (output / "changes.json").read_bytes() == published_changes.encode("utf-8")


def test_every_worked_result_equals_direct_module_calls(rebuilt):
    _, _, data = rebuilt
    example = load_example()
    points = [
        (example.T_BH + (example.T_WH - example.T_BH) * i / (example.N_STEPS - 1),
         example.P_BH + (example.P_WH - example.P_BH) * i / (example.N_STEPS - 1))
        for i in range(example.N_STEPS)
    ]
    expected_profile = si_profile(example.FORMATION_WATER, points)
    for actual, expected in zip(data["profile"], expected_profile.to_dict(orient="records"), strict=True):
        assert actual == pytest.approx(expected, rel=5e-12, abs=1e-14)
    for i, (temperature, pressure) in enumerate(points):
        expected = saturation_indices(example.FORMATION_WATER, temperature, pressure)
        for family, result in expected.items():
            record = data["states"][i][family]
            for field in ("si", "log10_iap", "pk_cond"):
                assert record[field] == pytest.approx(getattr(result, field), rel=5e-12, abs=1e-14)
            assert record["details"] == pytest.approx(result.details, rel=5e-12, abs=1e-14)
    assert data["ionic_strength"] == pytest.approx(example.FORMATION_WATER.ionic_strength(), rel=5e-12)
    assert data["inputs"]["brine"] == vars(example.FORMATION_WATER)
    assert data["criterion"] == math.log10(1.0)
    assert data["coefficients"] == {k: asdict(v) for k, v in default_coefficients().items()}
    assert data["ions"] == {k: {"molecular_mass": v[0], "charge": v[1]}
                            for k, v in ION_PROPERTIES.items()}
    tree = ast.parse((ROOT / "tests/production_chemistry/test_scale_si.py").read_text(encoding="utf-8"))
    assert data["evidence"]["test_count"] == sum(
        isinstance(item, ast.FunctionDef) and item.name.startswith("test_") for item in tree.body
    )
    for source, digest in data["sources"].items():
        text = (ROOT / source).read_text(encoding="utf-8")
        assert hashlib.sha256(text.encode("utf-8")).hexdigest() == digest


def test_every_displayed_number_is_resolved_from_computations(rebuilt):
    _, parser, data = rebuilt
    assert len(parser.numbers) > 180
    for key, places, text in parser.numbers:
        value = data["numbers"][key]
        origin = data
        for field in key.split("."):
            origin = origin[int(field)] if isinstance(origin, list) else origin[field]
        assert value == origin, key
        expected = f"{value:.{places}f}" if places >= 0 else f"{value:.5e}"
        assert text == expected.replace("-", "−"), key
        assert value == 0 or float(text.replace("−", "-")) != 0, key


def test_house_style_sections_and_offline_controls(rebuilt):
    output, parser, _ = rebuilt
    html = (output / "index.html").read_text(encoding="utf-8")
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
