# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Read the generated section example against an independent polynomial."""

import os
from fractions import Fraction
from html.parser import HTMLParser
from pathlib import Path
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[3]
BUILDER = ROOT / "docs/reports/modules/passing_ship/build_worked_example.py"


class Tables(HTMLParser):
    """Read visible table cells, retaining each table's identifier."""

    def __init__(self):
        super().__init__()
        self.tables = {}
        self.table = None
        self.row = None
        self.cell = None

    def handle_starttag(self, tag, attrs):
        if tag == "table":
            self.table = dict(attrs)["id"]
            self.tables[self.table] = []
        elif tag == "tr":
            self.row = []
        elif tag in {"td", "th"}:
            self.cell = []

    def handle_data(self, data):
        if self.cell is not None:
            self.cell.append(data)

    def handle_endtag(self, tag):
        if tag in {"td", "th"}:
            self.row.append("".join(self.cell).strip())
            self.cell = None
        elif tag == "tr":
            self.tables[self.table].append(self.row)
        elif tag == "table":
            self.table = None


def run_builder(output, cwd):
    """Execute the public command with all transient state in the test folder."""
    env = dict(os.environ, PYTHONDONTWRITEBYTECODE="1")
    subprocess.run(
        [sys.executable, str(BUILDER), "--output-dir", str(output)],
        cwd=cwd,
        env=env,
        check=True,
        capture_output=True,
        text=True,
    )
    return (output / "worked_example.html").read_bytes()


def test_section_shape_example_matches_hand_polynomial(tmp_path):
    """H1: solve a*x^2+b*x+c with S(-2)=S(2)=0 and S(0)=1.

    Subtraction gives b=0; the midpoint gives c=1; 4*a+1=0 gives
    a=-1/4 per square metre. Thus S=1-x^2/4 and S'=-x/2 per metre.
    Endpoint slopes below are the one-sided polynomial limits, not the
    two-sided derivative of the implementation's zero-extended shape.
    All expected numbers follow from this hand calculation, independently
    of the builder and the formulation helpers. See local README, H1.
    """
    output = tmp_path / "selected"
    page = run_builder(output, tmp_path).decode("utf-8")
    parsed = Tables()
    parsed.feed(page)
    assert parsed.tables["inputs"] == [
        ["Input", "Value", "Unit", "Basis"],
        ["Length L", "4", "m", "H1 synthetic definition"],
        ["Stations x", "-2, -1, 0, 1, 2", "m", "H1 synthetic definition"],
    ]
    results = parsed.tables["results"]
    assert results[0] == [
        "x [m]", "S [dimensionless]", "Polynomial slope [m^-1]", "Slope meaning"
    ]
    # H1 direct substitution at the five specified stations.
    expected = [
        (-2, 0, 1, "one-sided from interior"),
        (-1, Fraction(3, 4), Fraction(1, 2), "interior derivative"),
        (0, 1, 0, "interior derivative"),
        (1, Fraction(3, 4), Fraction(-1, 2), "interior derivative"),
        (2, 0, -1, "one-sided from interior"),
    ]
    assert len(results[1:]) == len(expected)
    for cells, (x, shape, slope, meaning) in zip(results[1:], expected):
        assert [Fraction(cell) for cell in cells[:3]] == [x, shape, slope]
        assert cells[3] == meaning
    assert "Synthetic normalized-section worked example" in page
    assert "NOT_EVALUATED" in page
    assert "hydrodynamic force qualification remains unevaluated" in page
    assert "two-sided derivative does not exist at the endpoints" in page
    assert "S(x) = a*x^2 + b*x + c" in page
    assert "S(x) = 1 - x^2/4" in page
    assert "dS/dx = -x/2" in page
    assert "s1_function" in page and "ds1_dx" in page
    assert page.index('id="summary"') < page.index('id="revision-history"')
    assert page.index('id="revision-history"') < page.index('id="inputs-section"')
    assert {p.relative_to(tmp_path).as_posix() for p in tmp_path.rglob("*")} == {
        "selected", "selected/worked_example.html"
    }


def test_section_shape_example_rebuild_is_deterministic(tmp_path):
    """Require identical bytes on rebuild and at a different output location."""
    # Importing the builder must have no output or command-line side effects.
    subprocess.run(
        [sys.executable, "-c", "import runpy, sys; runpy.run_path(sys.argv[1])", str(BUILDER)],
        cwd=tmp_path,
        check=True,
        capture_output=True,
        text=True,
    )
    assert list(tmp_path.iterdir()) == []
    first_dir = tmp_path / "first"
    first = run_builder(first_dir, tmp_path)
    assert run_builder(first_dir, tmp_path) == first
    assert run_builder(tmp_path / "second", tmp_path) == first
    assert {p.relative_to(tmp_path).as_posix() for p in tmp_path.rglob("*")} == {
        "first", "first/worked_example.html", "second", "second/worked_example.html"
    }
