# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent geometry, visible bindings and isolated rebuild contracts."""

from __future__ import annotations

import copy
import importlib.util
import json
import os
import re
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.bemrosetta import PanelMesh, STLHandler

ROOT = Path(__file__).resolve().parents[3]
REPORT = ROOT / "docs/reports/modules/bemrosetta"
# Source: worked-ascii-triangle-round-trip.md, Numerical comparison criterion.
# Small integer coordinates and their .6e serialization are exact here. The
# absolute allowance is 1e-12 u (bounds), u² (area), or 1 (normal components);
# relative allowance is zero. This is software roundoff, not a design tolerance.
ATOL = 1e-12


@pytest.fixture(scope="module")
def builder():
    spec = importlib.util.spec_from_file_location(
        "bemrosetta_module_report", REPORT / "build_report.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def oracle():
    """Literal hand calculation, independent of builder and mesh algorithms.

    Basis: synthetic A=(1,2,3), B=(5,2,3), C=(1,5,3), t=(10,-4,2).
    STL assigns no length unit: u is an arbitrary common length unit.
    B-A=(4,0,0), C-A=(0,3,0); cross=(0,0,12) u², area=4*3/2=6 u²,
    signed normal=cross/12=(0,0,1). Coordinate extrema give the bounds.
    Translation cancels in edge differences; translated extrema are computed
    by adding t component by component. No vertex numbering is asserted.
    """
    expected = {
        "input.a": ([1, 2, 3], "u"),
        "input.b": ([5, 2, 3], "u"),
        "input.c": ([1, 5, 3], "u"),
        "input.translation": ([10, -4, 2], "u"),
        "edge.ab": ([4, 0, 0], "u"),
        "edge.ac": ([0, 3, 0], "u"),
        "cross": ([0, 0, 12], "u²"),
    }
    for case, minimum, maximum in (
        ("original", [1, 2, 3], [5, 5, 3]),
        ("translated", [11, -2, 5], [15, 1, 5]),
    ):
        for stage in ("before", "after"):
            for quantity, value, unit in (
                ("area", 6, "u²"),
                ("normal", [0, 0, 1], "1"),
                ("minimum", minimum, "u"),
                ("maximum", maximum, "u"),
            ):
                key = f"{case}.{stage}.{quantity}"
                expected[key] = (value, unit)
                expected[f"error.{key}"] = (0, unit)
                expected[f"reference.{case}.{quantity}"] = (value, unit)
    return expected


class Page(HTMLParser):
    """Read visible quantities and units rather than trusting embedded JSON."""

    def __init__(self, text):
        super().__init__()
        self.quantities = {}
        self.ids = []
        self.links = []
        self.resources = []
        self.binding = None
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a":
            self.links.append(attrs["href"])
        if tag in {"img", "iframe", "link", "script"}:
            self.resources.extend(attrs[k] for k in ("src", "href") if k in attrs)
        if "data-value" in attrs:
            assert self.binding is None
            self.binding = (attrs, "")

    def handle_data(self, data):
        if self.binding is not None:
            attrs, text = self.binding
            self.binding = (attrs, text + data)

    def handle_endtag(self, tag):
        if tag == "span" and self.binding is not None:
            attrs, text = self.binding
            key = attrs["data-value"]
            assert key not in self.quantities
            self.quantities[key] = (attrs, text)
            self.binding = None


def fresh_geometry(directory):
    """Separate public module calls; geometry comparison tolerates renumbering."""
    directory.mkdir()
    fresh = {}
    for case, offset in (("original", None), ("translated", [10, -4, 2])):
        mesh = PanelMesh(
            vertices=np.array([[1, 2, 3], [5, 2, 3], [1, 5, 3]], dtype=float),
            panels=np.array([[0, 1, 2, -1]], dtype=np.int32),
        )
        if offset is not None:
            mesh.translate(offset)
        handler = STLHandler()
        path = directory / f"{case}.stl"
        handler.write(mesh, path, binary=False)
        assert path.read_text(encoding="ascii").startswith("solid ")
        for stage, obj in (("before", mesh), ("after", handler.read(path))):
            for quantity, value in (
                ("area", obj.total_area),
                ("normal", obj.normals[0]),
                ("minimum", obj.bounding_box[0]),
                ("maximum", obj.bounding_box[1]),
            ):
                fresh[f"{case}.{stage}.{quantity}"] = value
    return fresh


def assert_visible(page, expected):
    assert set(page.quantities) == set(expected)
    for key, (value, unit) in expected.items():
        attrs, text = page.quantities[key]
        assert attrs["data-unit"] == unit
        assert attrs["data-source"].startswith("#source-")
        assert attrs["data-equation"]
        values = value if isinstance(value, list) else [value]
        assert text == ", ".join(f"{v:.1f}" for v in values) + f" {unit}", key


def test_worked_values_match_independent_calculation(builder, tmp_path):
    record = builder.calculate(tmp_path / "calculation")
    expected = oracle()
    assert set(record) == set(expected)
    fresh = fresh_geometry(tmp_path / "fresh")
    for key, (value, unit) in expected.items():
        assert record[key]["unit"] == unit
        np.testing.assert_allclose(record[key]["value"], value, rtol=0, atol=ATOL)
        if key in fresh:
            np.testing.assert_allclose(record[key]["value"], fresh[key], rtol=0, atol=ATOL)
    page = Page((REPORT / "report.html").read_text(encoding="utf-8"))
    assert_visible(page, expected)
    # Every visible module result is checked against a fresh module call too.
    for key, value in fresh.items():
        attrs, text = page.quantities[key]
        numbers = np.atleast_1d(value)
        assert text == ", ".join(f"{v:.1f}" for v in numbers) + f" {attrs['data-unit']}"


def test_visible_values_follow_computed_results(builder, tmp_path, monkeypatch):
    original = builder.calculate(tmp_path / "original")
    assert_visible(Page(builder.build(original)), oracle())
    real_area = PanelMesh.total_area.fget
    # Deliberate synthetic fault of one square unit, larger than display rounding.
    monkeypatch.setattr(PanelMesh, "total_area", property(lambda m: real_area(m) + 1))
    mutated = builder.calculate(tmp_path / "mutation")
    page = Page(builder.build(mutated))
    key = "original.after.area"
    assert mutated[key]["value"] == original[key]["value"] + 1
    assert page.quantities[key][1] == "7.0 u²"
    assert page.quantities[f"error.{key}"][1] == "1.0 u²"
    assert builder.build(mutated) != builder.build(original)
    # The same visible-output oracle must reject the injected fault.
    with pytest.raises(AssertionError):
        assert_visible(page, oracle())


def test_report_rebuild_is_deterministic(builder, tmp_path):
    environment = dict(os.environ, PYTHONPATH=str(ROOT / "src"))
    for name in ("first", "second"):
        output, scratch = tmp_path / name, tmp_path / f"{name}-scratch"
        subprocess.run(
            [sys.executable, str(REPORT / "build_report.py"),
             "--output-dir", str(output), "--scratch-dir", str(scratch)],
            cwd=tmp_path, env=environment, check=True, capture_output=True,
        )
        assert {p.name for p in output.iterdir()} == {
            "report.html", "sources.json", "changes.json"
        }
        assert {p.name for p in scratch.iterdir()} == {"original.stl", "translated.stl"}
    for filename in ("report.html", "sources.json", "changes.json"):
        assert (tmp_path / "first" / filename).read_bytes() == (
            tmp_path / "second" / filename
        ).read_bytes() == (REPORT / filename).read_bytes()


def test_scope_limits_and_report_pointers(builder):
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    page = Page(text)
    assert re.findall(r'<section id="section-(\d+)"', text) == [str(i) for i in range(1, 11)]
    assert len(page.ids) == len(set(page.ids))
    assert text.index('id="section-1"') < text.index('id="revision-history"') < text.index('id="section-2"')
    assert re.search(r'</section>\s*<aside id="revision-history"', text)
    assert 'id="engineering-changes"' in text
    for item in ("coefficient correctness", "conversion into a commercial solver",
                 "general mesh equivalence", "physical solver qualification"):
        assert f"{item}: NOT_EVALUATED" in text
    assert "Reproducibility is not physical validation" in text
    assert not page.resources
    for link in page.links:
        if link.startswith("#"):
            assert link[1:] in page.ids
        else:
            assert not Path(link).is_absolute()
            assert (REPORT / link.split("#")[0]).is_file()
    sources = json.loads((REPORT / "sources.json").read_text(encoding="utf-8"))
    for source in sources["sources"]:
        path = ROOT / source["path"]
        assert path.is_file()
        assert source["locator"] in path.read_text(encoding="utf-8")
        assert f"source-{source['id']}" in page.ids
    for path, target in (
        (REPORT / "README.md", "report.html"),
        (ROOT / "src/digitalmodel/hydrodynamics/bemrosetta/README.md",
         "../../../../docs/reports/modules/bemrosetta/README.md"),
    ):
        assert f"]({target})" in path.read_text(encoding="utf-8")
        assert (path.parent / target).is_file()


def test_html_and_embedded_json_are_escaped(builder, tmp_path):
    record = builder.calculate(tmp_path / "scratch")
    hostile = '</script><img src="unexpected">&'
    changed = copy.deepcopy(record)
    changed["input.a"]["equation"] = hostile
    text = builder.build(changed)
    assert hostile not in text
    page = Page(text)
    assert not page.resources
    assert page.quantities["input.a"][0]["data-equation"] == hostile
    embedded = re.search(r'<script type="application/json" id="calculation-record">(.*?)</script>', text, re.S)
    assert json.loads(embedded[1]) == changed


def test_builder_does_not_probe_or_run_optional_tools(builder, tmp_path, monkeypatch):
    from digitalmodel.hydrodynamics.bemrosetta.core import runner

    def forbidden(*args, **kwargs):
        pytest.fail("Optional executable detection or execution was attempted")

    for name in ("is_bemrosetta_available", "get_runner"):
        monkeypatch.setattr(runner, name, forbidden)
    monkeypatch.setattr(runner.BEMRosettaRunner, "__init__", forbidden)
    builder.calculate(tmp_path / "offline")
