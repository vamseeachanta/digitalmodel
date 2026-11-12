# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Check the synthetic section example against independent polar integrals."""

import math
import os
from pathlib import Path
import subprocess
import sys
import xml.etree.ElementTree as ET

import pytest

from digitalmodel.structural.pipe_capacity.common.PipeSizing import PipeSizing


ROOT = Path(__file__).resolve().parents[3]
REPORT_DIR = Path("docs/reports/modules/pipe_capacity")
BUILDER = ROOT / REPORT_DIR / "build_worked_example.py"


def run_example(output_dir, cwd):
    """Execute the public command from an isolated working directory."""
    env = dict(os.environ, PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    return subprocess.run(
        [sys.executable, str(BUILDER), "--output-dir", str(output_dir)],
        cwd=cwd,
        env=env,
        capture_output=True,
        text=True,
        check=True,
    )


def test_pipe_section_example_matches_annulus_integrals(tmp_path):
    """Derive expected values without the diameter formulas under test.

    Synthetic arithmetic probes: outside diameter 4 m, inside diameter 2 m;
    thus the annulus has inner radius 1 m and outer radius 2 m.
    With dA = r dr dtheta and theta from 0 to 2*pi:
      A = integral integral r dr dtheta = 2*pi * [r**2/2]_1^2 = 3*pi m^2.
      Ix = integral integral (r*sin(theta))**2 r dr dtheta
         = pi * [r**4/4]_1^2 = 15*pi/4 m^4; Iy = Ix by symmetry.
      J = integral integral r**2 r dr dtheta
        = 2*pi * [r**4/4]_1^2 = 15*pi/2 m^4.
    The radial powers and coefficients below follow these integrals, not the
    implementation. A relative tolerance of 1e-12 permits floating arithmetic;
    displayed values use one decimal place (half-step error at most 0.05).
    """
    expected = {"A": 3 * math.pi, "I": 15 * math.pi / 4, "J": 15 * math.pi / 2}
    geometry = {"Nominal_OD": 4.0, "Nominal_ID": 2.0}
    cfg = {"Outer_Pipe": {"Geometry": geometry}}
    PipeSizing(cfg).pipe_section_properties("Outer_Pipe")
    properties = cfg["Outer_Pipe"]["section_properties"]["pipe"]
    for key, value in expected.items():
        assert properties[key] == pytest.approx(value, rel=1e-12)

    output_dir = tmp_path / "selected-output"
    run_example(output_dir, tmp_path)
    page = ET.parse(output_dir / "worked_example.html").getroot()
    for key, value in {"Nominal_OD": 4.0, "Nominal_ID": 2.0, **expected}.items():
        row = page.find(f".//tr[@id='{key}']")
        assert row is not None
        assert row.find("td[@class='value']").text == f"{value:.1f}"
        unit = "m" if key.startswith("Nominal_") else "m^2" if key == "A" else "m^4"
        assert row.find("td[@class='unit']").text == unit

    text = " ".join(page.itertext())
    assert "synthetic" in text.lower()
    assert "arithmetic probes" in text
    assert "product specification" in text
    assert "NOT_EVALUATED" in text
    for limit in ("pressure containment", "collapse", "combined loading"):
        assert limit in text
    assert "dA = r dr dtheta" in text
    assert "[r^2/2]" in text
    assert "[r^4/4]" in text
    assert "15*pi/4" in text and "15*pi/2" in text
    assert "centroidal" in text
    headings = [heading.text for heading in page.findall(".//h2")]
    assert headings[:2] == ["Summary", "Revision history"]
    assert page.findall(".//script") == []
    assert set(tmp_path.rglob("*")) == {output_dir, output_dir / "worked_example.html"}


def test_pipe_section_example_rebuild_is_deterministic(tmp_path):
    """Rebuild in place and at another path without changing page bytes."""
    first_dir = tmp_path / "first"
    run_example(first_dir, tmp_path)
    original = (first_dir / "worked_example.html").read_bytes()
    run_example(first_dir, tmp_path)
    assert (first_dir / "worked_example.html").read_bytes() == original
    second_dir = tmp_path / "second"
    run_example(second_dir, first_dir)
    assert (second_dir / "worked_example.html").read_bytes() == original
    assert set(tmp_path.rglob("*.html")) == {
        first_dir / "worked_example.html", second_dir / "worked_example.html"
    }


@pytest.mark.parametrize(
    "path",
    [
        Path(".gitignore"),
        REPORT_DIR / "build_worked_example.py",
        REPORT_DIR / "README.md",
        Path(__file__).resolve().relative_to(ROOT),
    ],
)
def test_worked_example_deliverables_are_not_ignored(path):
    """Keep each deliverable visible despite later report wildcard rules."""
    result = subprocess.run(
        ["git", "check-ignore", "--no-index", "--", str(path)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    assert result.returncode == 1, result.stdout + result.stderr


def test_worked_example_import_does_not_write(tmp_path):
    """Import the builder without invoking its command or writing a page."""
    env = dict(os.environ, PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    subprocess.run(
        [sys.executable, "-c", "import runpy, sys; runpy.run_path(sys.argv[1])", str(BUILDER)],
        cwd=tmp_path,
        env=env,
        capture_output=True,
        check=True,
    )
    assert list(tmp_path.iterdir()) == []
