# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check rendered arithmetic against the independent hand derivation [H].

[H] Linear free-surface gravity waves: omega^2 = g*k*tanh(k*h).
In deep water tanh(k*h) tends to unity, hence k = omega^2/g.
Synthetic arithmetic probes g=4 m/s^2 and omega=(1, 2) rad/s give
k=(1/4, 1) 1/m. These expected values do not use the helper under test.
"""

from __future__ import annotations

import importlib.util
import os
import subprocess
import sys
from fractions import Fraction
from html.parser import HTMLParser
from pathlib import Path


BUILDER = (
    Path(__file__).resolve().parents[3]
    / "docs/reports/modules/parametric_hull_analysis/build_worked_example.py"
)


class _Tables(HTMLParser):
    """Read visible table cells rather than hidden metadata."""

    def __init__(self):
        super().__init__()
        self.tables = {}
        self.table = None
        self.row = []
        self.cell = None

    def handle_starttag(self, tag, attrs):
        if tag == "table":
            self.table = dict(attrs).get("id")
            self.tables[self.table] = []
        elif tag == "tr":
            self.row = []
        elif tag in ("th", "td"):
            self.cell = []

    def handle_data(self, data):
        if self.cell is not None:
            self.cell.append(data)

    def handle_endtag(self, tag):
        if tag in ("th", "td") and self.cell is not None:
            self.row.append(" ".join("".join(self.cell).split()))
            self.cell = None
        elif tag == "tr" and self.table is not None:
            self.tables[self.table].append(self.row)
        elif tag == "table":
            self.table = None


def _build(output_dir, working_dir):
    result = subprocess.run(
        [sys.executable, str(BUILDER), "--output-dir", str(output_dir)],
        cwd=working_dir,
        env={**os.environ, "PYTHONDONTWRITEBYTECODE": "1"},
        capture_output=True,
        text=True,
        timeout=60,  # Short arithmetic example; no solver or network is needed.
    )
    assert result.returncode == 0, result.stderr
    return (output_dir / "worked-example.html").read_text(encoding="utf-8")


def test_wave_number_example_matches_hand_dispersion(tmp_path):
    """Read back values and units and compare with hand calculation [H]."""
    working_dir = tmp_path / "working"
    working_dir.mkdir()
    output_dir = tmp_path / "selected-output"
    html = _build(output_dir, working_dir)
    parser = _Tables()
    parser.feed(html)

    # [H]: the specified synthetic gravity is an arithmetic probe.
    gravity = parser.tables["inputs"][1]
    assert gravity[:3] == ["g", "4", "m/s^2"]
    assert "synthetic" in gravity[3].lower()
    rows = parser.tables["results"][1:]
    expected = [(Fraction(1), Fraction(1, 4)), (Fraction(2), Fraction(1))]
    assert len(rows) == len(expected)
    for row, (omega, k) in zip(rows, expected):
        assert Fraction(row[0]) == omega
        assert row[1] == "rad/s"
        assert Fraction(row[2]) == k
        assert row[3] == "1/m"
        assert Fraction(row[4]) == k
        assert row[5] == "1/m"

    for text in (
        "arithmetic probes",
        "environmental data",
        "angular frequency",
        "deep water",
        "omega^2 = g*k*tanh(k*h)",
        "k = omega^2/g",
        "Hand derivation [H]",
        "NOT_EVALUATED",
        "heading conventions",
        "forward-speed RAO corrections",
        "sweeps",
        "BEM solver execution",
        "full-module qualification",
    ):
        assert text in html
    assert html.index('id="summary"') < html.index('id="revision-history"')
    assert html.index('id="revision-history"') < html.index('id="inputs"')
    assert len(parser.tables["revisions"]) == len(["header", "initial issue"])
    assert not list(working_dir.iterdir())
    assert {path.relative_to(output_dir) for path in output_dir.rglob("*")} == {
        Path("worked-example.html")
    }


def test_wave_number_example_rebuild_is_deterministic(tmp_path):
    """Import without execution; compare rebuilds and independent locations."""
    working_dir = tmp_path / "working"
    working_dir.mkdir()
    previous_cwd = Path.cwd()
    try:
        os.chdir(working_dir)
        spec = importlib.util.spec_from_file_location("worked_example", BUILDER)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
    finally:
        os.chdir(previous_cwd)
    assert not list(working_dir.iterdir())

    first_output = tmp_path / "first"
    second_output = tmp_path / "second"
    _build(first_output, working_dir)
    first = (first_output / "worked-example.html").read_bytes()
    _build(first_output, working_dir)
    assert first == (first_output / "worked-example.html").read_bytes()
    _build(second_output, working_dir)
    assert first == (second_output / "worked-example.html").read_bytes()
    assert not list(working_dir.iterdir())
    assert {path.name for path in tmp_path.iterdir()} == {
        "working", "first", "second"
    }
