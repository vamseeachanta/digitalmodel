# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Read back the standalone mathematical demonstration, using hand calculations."""

import os
from html.parser import HTMLParser
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[3]
LOCAL_DOCS = Path("docs/reports/modules/river_currents")
BUILDER = ROOT / LOCAL_DOCS / "build_worked_example.py"
DELIVERED = (
    ".gitignore",
    str(LOCAL_DOCS / "build_worked_example.py"),
    str(LOCAL_DOCS / "README.md"),
    "tests/hydrodynamics/river_currents/test_worked_example.py",
)


class RenderedValues(HTMLParser):
    """Collect text from identified cells without relying on builder internals."""

    def __init__(self, content):
        super().__init__()
        self.values = {}
        self.current = None
        self.feed(content)

    def handle_starttag(self, tag, attrs):
        self.current = dict(attrs).get("id")
        if self.current:
            assert self.current not in self.values, "duplicate rendered identifier"
            self.values[self.current] = ""

    def handle_data(self, data):
        if self.current:
            self.values[self.current] += data

    def handle_endtag(self, tag):
        self.current = None


def build(output_dir, cwd):
    """Run the documented CLI from an unrelated working directory."""
    env = dict(os.environ, PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    subprocess.run(
        [sys.executable, str(BUILDER), "--output-dir", str(output_dir)],
        cwd=cwd, env=env, check=True, capture_output=True, text=True,
    )
    assert sorted(p.name for p in output_dir.iterdir()) == ["worked-example.html"]
    return (output_dir / "worked-example.html").read_bytes()


def test_river_example_matches_linear_profile_integral(tmp_path):
    """Independent basis: u=z/2; triangle area=(4 m)(2 m/s)/2=4 m²/s.

    Dividing that area by the 4 m depth gives 1 m/s. Heights 0, 2, 4 m
    give speeds 0, 1, 2 m/s. All constants are the synthetic prescription,
    documented in the local README's Hand calculation section.
    """
    content = build(tmp_path / "result", tmp_path).decode("utf-8")
    values = RenderedValues(content).values
    expected = {
        "depth": (4.0, "m"),
        "surface-speed": (2.0, "m/s"),
        "exponent": (1.0, "dimensionless"),
        "height-bed": (0.0, "m"),
        "height-mid": (2.0, "m"),
        "height-surface": (4.0, "m"),
        "speed-bed": (0.0, "m/s"),
        "speed-mid": (1.0, "m/s"),
        "speed-surface": (2.0, "m/s"),
        "triangle-integral": (4.0, "m²/s"),
        "hand-mean": (1.0, "m/s"),
        "helper-mean": (1.0, "m/s"),
    }
    for key, (number, unit) in expected.items():
        assert float(values[key]) == pytest.approx(number)
        assert values[f"{key}-unit"] == unit
    assert "mathematical demonstration" in content
    assert "not a measured river" in content
    for scope in ("bank effects", "force calibration", "extreme-value fitting"):
        assert f"{scope}: NOT_EVALUATED" in content
    assert content.index("Summary") < content.index("Revision history") < content.index("Inputs")
    assert sorted(p.name for p in tmp_path.iterdir()) == ["result"]


def test_river_example_rebuild_is_deterministic(tmp_path):
    first = build(tmp_path / "first", tmp_path)
    assert build(tmp_path / "first", tmp_path) == first
    assert build(tmp_path / "second", tmp_path) == first
    assert sorted(p.name for p in tmp_path.iterdir()) == ["first", "second"]


def test_river_example_import_has_no_output(tmp_path):
    env = dict(os.environ, PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    result = subprocess.run(
        [sys.executable, "-c", "import runpy, sys; runpy.run_path(sys.argv[1])", str(BUILDER)],
        cwd=tmp_path, env=env, check=True, capture_output=True, text=True,
    )
    assert result.stdout == result.stderr == ""
    assert list(tmp_path.iterdir()) == []


@pytest.mark.parametrize("path", DELIVERED)
def test_river_example_delivered_file_is_not_ignored(path):
    result = subprocess.run(
        ["git", "check-ignore", "--no-index", "--", path],
        cwd=ROOT, check=False, capture_output=True, text=True,
    )
    assert result.returncode == 1, result.stdout + result.stderr
    assert result.stdout == result.stderr == ""
