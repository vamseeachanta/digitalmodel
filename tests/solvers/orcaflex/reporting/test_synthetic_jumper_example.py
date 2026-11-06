# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Exercise the synthetic example through its command-line entry point."""

import json
import os
from html.parser import HTMLParser
from pathlib import Path
import re
import subprocess
import sys

import pytest


REPO_ROOT = Path(__file__).resolve().parents[4]
EXAMPLE = REPO_ROOT / "examples/solvers/orcaflex/reporting/synthetic_jumper.py"

# The child guards the entire example, including its imports. Audit writes as
# well as network operations so a new dependency cannot silently widen its I/O.
OFFLINE_RUNNER = """
import os
from pathlib import Path
import runpy
import sys

output = Path(sys.argv[-1]).resolve()

class NoSolver:
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] == 'OrcFxAPI':
            raise AssertionError('Solver import attempted')

def guard(event, args):
    if event.startswith('socket.'):
        raise AssertionError('Network operation attempted')
    if event == 'open':
        path, mode, flags = args
        if not flags & (os.O_WRONLY | os.O_RDWR | os.O_CREAT):
            return
    elif event == 'os.mkdir':
        path = args[0]
    else:
        return
    if not Path(path).resolve().is_relative_to(output):
        raise AssertionError('Write outside output directory attempted')

sys.meta_path.insert(0, NoSolver())
sys.addaudithook(guard)
sys.argv = sys.argv[1:]
runpy.run_path(sys.argv[0], run_name='__main__')
"""


class ExampleHTML(HTMLParser):
    """Collect sections, visible text, assets and inline chart definitions."""

    def __init__(self):
        super().__init__()
        self.sections = {}
        self.assets = []
        self.scripts = []
        self.text = []
        self.rows = {}
        self.section = None
        self.script = None
        self.row = None

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "section":
            self.section = attrs["id"]
            self.sections[self.section] = []
            self.rows[self.section] = []
        if tag == "tr":
            self.row = []
        if tag == "script":
            self.script = []
        if tag in {"script", "img", "iframe", "link"}:
            self.assets.extend(attrs[key] for key in ("src", "href") if key in attrs)

    def handle_endtag(self, tag):
        if tag == "tr" and self.section:
            self.rows[self.section].append(self.row)
            self.row = None
        if tag == "section":
            self.section = None
        if tag == "script":
            self.scripts.append("".join(self.script))
            self.script = None

    def handle_data(self, data):
        if self.script is not None:
            self.script.append(data)
        else:
            self.text.append(data)
            if self.section:
                self.sections[self.section].append(data)
            if self.row is not None and data.strip():
                self.row.append(data.strip())

    def chart(self, chart_id):
        pattern = r'Plotly\.newPlot\(\s*"' + re.escape(chart_id) + r'"\s*,\s*'
        for script in self.scripts:
            match = re.search(pattern, script)
            if match:
                decoder = json.JSONDecoder()
                traces, end = decoder.raw_decode(script[match.end() :])
                layout, _ = decoder.raw_decode(
                    script[match.end() + end :].lstrip(", \n")
                )
                return traces, layout
        raise AssertionError(f"Missing chart: {chart_id}")


def test_synthetic_jumper_html_example(tmp_path):
    """Compare rendered data to the hand calculation in the companion note."""
    output = tmp_path / "chosen-output"
    env = {
        **os.environ,
        "PYTHONDONTWRITEBYTECODE": "1",
        "PYTHONPATH": str(REPO_ROOT / "src"),
    }
    result = subprocess.run(
        [
            sys.executable,
            "-c",
            OFFLINE_RUNNER,
            str(EXAMPLE),
            "--output-dir",
            str(output),
        ],
        cwd=tmp_path,
        env=env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert list(tmp_path.iterdir()) == [output]
    assert [path.name for path in output.iterdir()] == ["synthetic_jumper.html"]
    html = (output / "synthetic_jumper.html").read_text(encoding="utf-8")
    parsed = ExampleHTML()
    parsed.feed(html)
    assert not parsed.assets
    assert any("plotly.js" in script for script in parsed.scripts)
    assert "Error rendering section" not in html

    # MoorDyn-C 2.3.1, tests/Mooring/lifting.txt, POINT PROPERTIES:
    # the input chord is abs(-100 - 0) = 100 m, not the 100.5 m
    # unstretched length in LINES. No equilibrium shape is calculated here.
    traces, layout = parsed.chart("geometry-3d")
    assert traces[0]["x"] == pytest.approx([0.0, 0.0])
    assert traces[0]["y"] == pytest.approx([0.0, 0.0])
    assert traces[0]["z"] == pytest.approx([0.0, -100.0])
    for axis, label in (("xaxis", "X [m]"), ("yaxis", "Y [m]"), ("zaxis", "Z [m]")):
        assert layout["scene"][axis]["title"]["text"] == label
    geometry = " ".join(parsed.sections["geometry"])
    for value in ("Fixed end", "Free end", "500.0 m", "Arc Length [m]"):
        assert value in geometry
    rows = parsed.rows["geometry"]
    assert rows[0] == ["Label", "Arc Length [m]", "X [m]", "Y [m]", "Z [m]"]
    assert [row[0] for row in rows[1:]] == ["Fixed end", "Free end"]
    for row, values in zip(rows[1:], ([0, 0, 0, 0], [100, 0, 0, -100])):
        assert [float(value) for value in row[1:]] == pytest.approx(values)

    # MoorDyn-C 2.3.1, tests/lifting.cpp: length starts at 100.5 m and
    # shortens by 1 m each second. OpenStax University Physics Volume 1,
    # first edition (2016), section 3.4, equation 3.10: L = L0 + rate*t.
    # Hand extrema over 0..4 s: 100.5 - 1*0 = 100.5 m (maximum),
    # 100.5 - 1*4 = 96.5 m (minimum); rate = (96.5 - 100.5)/4 = -1 m/s.
    histories = (
        (
            "length",
            "Prescribed unstretched length",
            "m",
            [100.5, 99.5, 98.5, 97.5, 96.5],
            96.5,
            100.5,
        ),
        ("length-rate", "Prescribed length rate", "m/s", [-1] * 5, -1, -1),
    )
    for chart_id, label, units, values, low, high in histories:
        traces, layout = parsed.chart(f"dynamic-{chart_id}")
        assert traces[0]["x"] == pytest.approx([0, 1, 2, 3, 4])
        assert traces[0]["y"] == pytest.approx(values)
        assert min(traces[0]["y"]) == pytest.approx(low)
        assert max(traces[0]["y"]) == pytest.approx(high)
        assert layout["xaxis"]["title"]["text"] == "Time [s]"
        assert layout["yaxis"]["title"]["text"] == f"{label} [{units}]"

    for section in ("other-structures", "extreme-results", "fatigue"):
        assert section not in parsed.sections
    for section in (
        "executive-summary",
        "materials",
        "boundary-conditions",
        "mesh",
        "loads",
        "analysis-setup",
        "static-results",
        "design-checks",
    ):
        assert "No data available" in " ".join(parsed.sections[section])
    dynamic = " ".join(parsed.sections["dynamic-results"])
    assert "No envelope data provided" in dynamic
    assert "Statistical summary not provided" in dynamic
    visible = " ".join(parsed.text)
    assert "NO CHECKS" in visible
    assert "No fitness verdict is made" in visible
    assert "MoorDyn-C 2.3.1" in visible
    assert "Initial input chord" in geometry
    assert "not solver results" in visible
    assert "ALL PASS" not in visible
    assert "COMPLIANT" not in visible
    assert "completed successfully" not in visible


def test_synthetic_jumper_requires_output_directory(tmp_path):
    """An omitted destination must fail without producing report files."""
    result = subprocess.run(
        [sys.executable, str(EXAMPLE)],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        encoding="utf-8",
        env={
            **os.environ,
            "PYTHONDONTWRITEBYTECODE": "1",
            "PYTHONPATH": str(REPO_ROOT / "src"),
        },
    )
    assert result.returncode != 0
    assert "--output-dir" in result.stderr
    assert not list(tmp_path.iterdir())
