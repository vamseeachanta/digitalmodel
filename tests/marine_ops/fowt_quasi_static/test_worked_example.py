# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Check the rendered synthetic tensor against independent axis distances."""

from html.parser import HTMLParser
import os
from pathlib import Path
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[3]
BUILDER = ROOT / "docs/reports/modules/fowt_quasi_static/build_worked_example.py"


class Values(HTMLParser):
    """Read visible numeric cells and their declared units from the report."""

    def __init__(self):
        super().__init__()
        self.cells = {}
        self.visible = []
        self.captions = []
        self.in_caption = False
        self.key = None
        self.unit = None
        self.parts = []

    def handle_starttag(self, tag, attrs):
        if tag == "caption":
            self.in_caption = True
        if tag == "td":
            attributes = dict(attrs)
            self.key = attributes.get("data-key")
            self.unit = attributes.get("data-unit")
            self.parts = []

    def handle_data(self, data):
        self.visible.append(data)
        if self.in_caption:
            self.captions.append(data)
        if self.key:
            self.parts.append(data)

    def handle_endtag(self, tag):
        if tag == "caption":
            self.in_caption = False
        if tag == "td" and self.key:
            assert self.key not in self.cells
            self.cells[self.key] = (float("".join(self.parts)), self.unit)
            self.key = None


def run_builder(output_dir, cwd):
    """Execute the documented entry point from outside the source tree."""
    environment = os.environ.copy()
    environment["PYTHONPATH"] = str(ROOT / "src")
    environment["PYTHONDONTWRITEBYTECODE"] = "1"
    return subprocess.run(
        [sys.executable, str(BUILDER), "--output-dir", str(output_dir)],
        cwd=cwd, env=environment, check=True, capture_output=True, text=True,
    )


def test_inertia_example_matches_independent_tensor(tmp_path):
    output = tmp_path / "report"
    run_builder(output, tmp_path)
    html = (output / "worked-example.html").read_text(encoding="utf-8")
    values = Values()
    values.feed(html)

    # Input specification: README.md, Inputs, method and answer section,
    # in docs/reports/modules/fowt_quasi_static; SI units.
    mass = 2
    x, y, z = 1, 2, 0
    # Independent hand calculation: squared distances to each axis, then
    # negative coordinate products. No parallel_axis import or tensor identity.
    expected = {
        "xx": mass * (y*y + z*z), "yy": mass * (x*x + z*z),
        "zz": mass * (x*x + y*y), "xy": -mass*x*y, "yx": -mass*y*x,
        "xz": -mass*x*z, "zx": -mass*z*x, "yz": -mass*y*z, "zy": -mass*z*y,
    }
    assert (expected["xx"], expected["yy"], expected["zz"], expected["xy"]) == (8, 2, 10, -4)
    assert values.cells["mass"] == (mass, "kg")
    for axis, coordinate in zip("xyz", (x, y, z)):
        assert values.cells[f"offset-{axis}"] == (coordinate, "m")
        assert values.cells[f"control-offset-{axis}"] == (0, "m")
    for component, answer in expected.items():
        assert values.cells[f"shift-{component}"] == (answer, "kg m^2")
        # At the origin, every distance and coordinate product is zero.
        assert values.cells[f"control-{component}"] == (0, "kg m^2")
    assert len(values.cells) == 25  # Mass, both offsets, and both full tensors.
    # Units must also be visible, rather than present only in data attributes.
    assert values.visible.count("kg") == 1
    assert values.visible.count("m") == 6  # Cartesian coordinates for both offsets.
    assert sum("[kg m^2]" in caption for caption in values.captions) == 2
    visible_text = " ".join(" ".join(values.visible).split())
    assert "Input basis: README.md, Inputs, method and answer section." in visible_text
    assert "#3318 requests a worked example but does not prescribe these values." in visible_text
    assert "Source: issue #3318" not in html
    assert "synthetic" in html.lower()
    assert "NOT_EVALUATED" in html
    assert "mooring qualification" in html
    assert html.index('<h2>Summary</h2>') < html.index('<h2>Revision history</h2>') < html.index('<h2>Inputs</h2>')
    assert "Ixx = m(y^2 + z^2)" in html
    assert "Ixy = Iyx = -mxy" in html


def test_inertia_example_rebuild_is_deterministic(tmp_path):
    working = tmp_path / "working"
    working.mkdir()
    first, second = tmp_path / "first", tmp_path / "second"
    environment = os.environ.copy()
    environment["PYTHONPATH"] = str(ROOT / "src")
    environment["PYTHONDONTWRITEBYTECODE"] = "1"
    # Importing the builder must not generate files or parse command arguments.
    subprocess.run(
        [sys.executable, "-c", "import runpy, sys; runpy.run_path(sys.argv[1])", str(BUILDER)],
        cwd=working, env=environment, check=True, capture_output=True, text=True,
    )
    assert list(working.iterdir()) == []
    run_builder(first, working)
    original = {p.name: p.read_bytes() for p in first.iterdir()}
    assert set(original) == {"worked-example.html"}
    run_builder(first, working)
    assert {p.name: p.read_bytes() for p in first.iterdir()} == original
    run_builder(second, working)
    assert {p.name: p.read_bytes() for p in second.iterdir()} == original
    assert list(working.iterdir()) == []
    assert set(tmp_path.iterdir()) == {working, first, second}
