# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check the rendered synthetic example against independent arithmetic."""

import os
from html.parser import HTMLParser
from pathlib import Path
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[3]
REPORT_DIR = Path("docs/reports/modules/offshore_resilience")
BUILDER = ROOT / REPORT_DIR / "build_worked_example.py"


class RenderedValues(HTMLParser):
    """Read visible text from identified elements, rather than hidden data."""

    def __init__(self):
        super().__init__()
        self.values = {}
        self.current = None

    def handle_starttag(self, tag, attrs):
        identifier = dict(attrs).get("id")
        if identifier:
            self.current = identifier
            self.values[identifier] = ""

    def handle_data(self, data):
        if self.current:
            self.values[self.current] += data

    def handle_endtag(self, tag):
        self.current = None


def run_builder(cwd, output_dir):
    """Run the documented CLI from outside the source checkout."""
    env = dict(os.environ, PYTHONPATH=str(ROOT / "src"))
    subprocess.run(
        [sys.executable, str(BUILDER), "--output-dir", str(output_dir)],
        cwd=cwd,
        env=env,
        check=True,
        capture_output=True,
        text=True,
    )
    return (output_dir / "worked_example.html").read_text(encoding="utf-8")


def test_cost_example_matches_hand_sum(tmp_path):
    """Issue #3308 hand sum: (100 - 70) + 10 = 40; 40 / 100 = 0.4."""
    html = run_builder(tmp_path, tmp_path / "selected-output")
    rendered = RenderedValues()
    rendered.feed(html)
    # Independent expectations from the synthetic scenario in issue #3308.
    expected = {
        "conventional-cost": (100, "currency units"),
        "minimum-facility-cost": (70, "currency units"),
        "avoided-installation-cost": (10, "currency units"),
        "net-savings": ((100 - 70) + 10, "currency units"),
        "savings-fraction": (((100 - 70) + 10) / 100, "dimensionless"),
    }
    for identifier, (expected_value, expected_unit) in expected.items():
        value, unit = rendered.values[identifier].split(maxsplit=1)
        assert float(value) == expected_value
        assert unit == expected_unit
    normalized_html = " ".join(html.split())
    assert "(100.0 - 70.0) + 10.0 = 40.0 currency units" in normalized_html
    assert "40.0 / 100.0 = 0.4 dimensionless" in normalized_html
    assert "synthetic" in html.lower()
    assert "not market estimates" in html
    assert "NOT_EVALUATED" in html
    assert "structural adequacy" in html
    assert html.index("Summary</h2>") < html.index("Revision history</h2>")
    assert html.index("Revision history</h2>") < html.index("Inputs</h2>")


def test_cost_example_rebuild_is_deterministic(tmp_path):
    """Repeated builds and a different output location yield identical bytes."""
    output_dir = tmp_path / "selected-output"
    run_builder(tmp_path, output_dir)
    first = (output_dir / "worked_example.html").read_bytes()
    run_builder(tmp_path, output_dir)
    assert (output_dir / "worked_example.html").read_bytes() == first
    other_dir = tmp_path / "other-output"
    run_builder(tmp_path, other_dir)
    assert (other_dir / "worked_example.html").read_bytes() == first
    assert set(tmp_path.iterdir()) == {output_dir, other_dir}
    for directory in (output_dir, other_dir):
        assert {p.name for p in directory.iterdir()} == {"worked_example.html"}


def test_import_has_no_output(tmp_path):
    """Loading the builder defines functions without writing a report."""
    subprocess.run(
        [sys.executable, "-c", "import runpy, sys; runpy.run_path(sys.argv[1])",
         str(BUILDER)],
        cwd=tmp_path,
        env=dict(os.environ, PYTHONPATH=str(ROOT / "src")),
        check=True,
        capture_output=True,
        text=True,
    )
    assert list(tmp_path.iterdir()) == []


def test_delivered_files_are_not_ignored():
    """Check exceptions even for files that become tracked after publication."""
    for path in (
        Path(".gitignore"),
        REPORT_DIR / "build_worked_example.py",
        REPORT_DIR / "README.md",
        Path(__file__).relative_to(ROOT),
    ):
        result = subprocess.run(
            ["git", "check-ignore", "--no-index", "--", str(path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
        )
        assert result.returncode == 1, f"{path}: {result.stdout}{result.stderr}"
