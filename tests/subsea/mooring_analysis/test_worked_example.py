# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Read back the synthetic touchdown example from a separate process.

Independent basis: test_catenary_geometry.py::test_vertical_span_closed_form,
case A; General Physics I: Classical Mechanics, section 19.3, The Catenary
(2024 web edition), https://phys.libretexts.org/@go/page/92208.
For z=a=100 m and w=9.81 kN/m, s=100*sqrt(3) m,
x=100*ln(2+sqrt(3)) m, and T=H+wz=1962 kN.
"""

import os
from math import log, sqrt
from pathlib import Path
import subprocess
import sys
import xml.etree.ElementTree as ET

import pytest


ROOT = Path(__file__).resolve().parents[3]
BUILDER = ROOT / "docs/reports/modules/mooring_analysis/build_worked_example.py"


def run_builder(cwd, output):
    """Use the checkout source with all outputs confined to the test directory."""
    env = dict(os.environ, PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    subprocess.run(
        [sys.executable, str(BUILDER), "--output-dir", str(output)],
        cwd=cwd, env=env, check=True, capture_output=True, text=True,
    )
    return output / "worked_example.html"


def test_touchdown_example_matches_independent_geometry(tmp_path):
    """Compare visible values and units to the cited case-A hand calculation."""
    cwd = tmp_path / "working directory"
    cwd.mkdir()
    output = tmp_path / "selected output"
    report = run_builder(cwd, output)
    page = ET.fromstring(report.read_text(encoding="utf-8"))

    def value(row_id, unit):
        row = page.find(f".//tr[@id='{row_id}']")
        assert row is not None, row_id
        assert row.find("td[@class='unit']").text == unit
        return float(row.find("td[@class='value']").text)

    # Inputs are explicit synthetic case-A values, not production fixtures.
    assert value("height", "m") == 100.0
    assert value("line-length", "m") == 500.0
    assert value("weight-water", "kg/m") == 1000.0
    assert value("gravity", "m/s²") == 9.81
    assert value("horizontal-tension", "kN") == 981.0
    assert value("unit-weight", "kN/m") == 9.81
    assert value("parameter", "m") == 100.0

    # Half of the 0.1-unit display increment permits rounding only.
    display_tolerance = 0.05
    assert value("suspended-length", "m") == pytest.approx(
        100.0 * sqrt(3.0), abs=display_tolerance,
    )
    assert value("horizontal-reach", "m") == pytest.approx(
        100.0 * log(2.0 + sqrt(3.0)), abs=display_tolerance,
    )
    assert value("fairlead-tension", "kN") == pytest.approx(
        981.0 + 9.81 * 100.0, abs=display_tolerance,
    )
    text = " ".join(page.itertext())
    for statement in (
        "synthetic", "inextensible", "positive weight", "horizontal touchdown tangent",
        "sufficient line length", "NOT_EVALUATED", "stiffness", "design-factor",
        "model export", "historical port", "s² = z² + 2az", "T = H + wz",
    ):
        assert statement in text
    assert page.find(".//a[@href='https://phys.libretexts.org/@go/page/92208']") is not None
    headings = [heading.text for heading in page.findall(".//h2")]
    assert headings[:2] == ["Summary", "Revision history"]
    assert not list(cwd.iterdir())
    assert list(output.iterdir()) == [report]


def test_touchdown_example_rebuild_is_deterministic(tmp_path):
    """Imports have no outputs; repeated and relocated builds are byte identical."""
    cwd = tmp_path / "working"
    cwd.mkdir()
    env = dict(os.environ, PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    subprocess.run(
        [sys.executable, "-c", "import runpy, sys; runpy.run_path(sys.argv[1])", str(BUILDER)],
        cwd=cwd, env=env, check=True, capture_output=True, text=True,
    )
    assert not list(cwd.iterdir())

    output = tmp_path / "first"
    first = run_builder(cwd, output).read_bytes()
    assert run_builder(cwd, output).read_bytes() == first
    relocated = tmp_path / "second"
    assert run_builder(cwd, relocated).read_bytes() == first
    assert sorted(p.relative_to(tmp_path).as_posix() for p in tmp_path.rglob("*")) == [
        "first", "first/worked_example.html", "second", "second/worked_example.html", "working",
    ]
