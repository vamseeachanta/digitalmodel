# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check the rendered beam example against independent synthetic inputs."""

import math
import os
from html.parser import HTMLParser
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[3]
BUILDER = ROOT / "docs/reports/modules/structural_fe/build_worked_example.py"

# [S1] Synthetic admission fixture, independently specified here in SI units.
# Identifiers are zero-based; node coordinates define the prescribed 3-4-5 beam.
EXPECTED = {
    "node_0_x": (0.0, "m"),
    "node_0_y": (0.0, "m"),
    "node_1_x": (3.0, "m"),
    "node_1_y": (4.0, "m"),
    "element_id": (0, "identifier"),
    "node1": (0, "identifier"),
    "node2": (1, "identifier"),
    "material_E": (200e9, "Pa"),
    "element_E": (200e9, "Pa"),
    "I": (8e-6, "m^4"),
    "A": (0.01, "m^2"),
    "nu": (0.3, "dimensionless"),
    "rho": (7800.0, "kg/m^3"),
    "sigma_y": (250e6, "Pa"),
    "load_node": (1, "identifier"),
    "load_magnitude": (-1000.0, "N"),
}
TEXT_EXPECTED = {
    "fixed_nodes": ("[0]", "node identifiers"),
    "load_direction": ("y", "global axis"),
}


class ReportRows(HTMLParser):
    """Read visible table cells rather than hidden metadata."""

    def __init__(self):
        super().__init__()
        self.rows = {}
        self.key = None
        self.cells = []
        self.cell = None

    def handle_starttag(self, tag, attrs):
        if tag == "tr":
            self.key = dict(attrs).get("data-field")
            self.cells = []
        elif tag in {"td", "th"} and self.key:
            self.cell = []

    def handle_data(self, data):
        if self.cell is not None:
            self.cell.append(data)

    def handle_endtag(self, tag):
        if tag in {"td", "th"} and self.cell is not None:
            self.cells.append("".join(self.cell).strip())
            self.cell = None
        elif tag == "tr" and self.key:
            assert self.key not in self.rows
            self.rows[self.key] = self.cells
            self.key = None


def run_builder(cwd, output_dir):
    """Execute the documented CLI from an unrelated working directory."""
    env = dict(os.environ, PYTHONPATH=str(ROOT / "src"))
    result = subprocess.run(
        [sys.executable, str(BUILDER), "--output-dir", str(output_dir)],
        cwd=cwd,
        env=env,
        capture_output=True,
        text=True,
        check=True,
    )
    assert not result.stderr
    assert set(path.name for path in output_dir.iterdir()) == {"worked-example.html"}
    return (output_dir / "worked-example.html").read_bytes()


def test_beam_builder_example_preserves_hand_geometry(tmp_path):
    """[H1] Pythagoras gives sqrt(3^2 + 4^2) = 5 m, independently of the model."""
    report = run_builder(tmp_path, tmp_path / "report").decode("utf-8")
    parser = ReportRows()
    parser.feed(report)
    rows = parser.rows

    for prefix in ("input", "admitted"):
        for field, (expected, unit) in EXPECTED.items():
            _, actual, actual_unit, basis = rows[f"{prefix}_{field}"]
            assert float(actual) == pytest.approx(expected)
            assert actual_unit == unit
            assert "[S1]" in basis
        for field, (expected, unit) in TEXT_EXPECTED.items():
            _, actual, actual_unit, basis = rows[f"{prefix}_{field}"]
            assert (actual, actual_unit) == (expected, unit)
            assert "[S1]" in basis

    hand_length = math.sqrt(3.0**2 + 4.0**2)  # [H1], SI geometry above.
    for field in ("hand_length", "admitted_length"):
        _, actual, unit, basis = rows[field]
        assert float(actual) == pytest.approx(hand_length)
        assert unit == "m"
        assert "[H1]" in basis
    assert "sqrt(3^2 + 4^2) = 5 m" in report
    for field in ("buckling", "stress", "displacement"):
        assert rows[field][1] == "NOT_EVALUATED"
        assert rows[field][2] == "not calculated"
    assert "synthetic" in report.lower()
    assert report.index("Summary</h2>") < report.index("Revision history</h2>")
    assert report.index("Revision history</h2>") < report.index("Inputs</h2>")
    assert "Limits</h2>" in report
    assert "model construction only" in report.lower()
    assert set(path.name for path in tmp_path.iterdir()) == {"report"}


def test_beam_builder_example_rebuild_is_deterministic(tmp_path):
    """Identical inputs give identical bytes across rebuilds and output paths."""
    # Importing the script must neither parse arguments nor write an output.
    result = subprocess.run(
        [sys.executable, "-c", "import runpy, sys; runpy.run_path(sys.argv[1])", str(BUILDER)],
        cwd=tmp_path,
        capture_output=True,
        check=True,
    )
    assert not result.stdout and not result.stderr
    assert not list(tmp_path.iterdir())

    first = run_builder(tmp_path, tmp_path / "first")
    assert first == (BUILDER.parent / "worked-example.html").read_bytes()
    assert run_builder(tmp_path, tmp_path / "first") == first
    assert run_builder(tmp_path, tmp_path / "second") == first
    assert set(path.name for path in tmp_path.iterdir()) == {"first", "second"}
    assert str(tmp_path).encode() not in first
