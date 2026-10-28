# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent pressure-integral oracle and deterministic report delivery checks.

Basis: test_planing_hull_hydrostatics.py, module note and _synthetic_strip.
For z=abs(x), b=h and F'=2*rho*g*(h*b-b*b/2); A=integral(2*z dz).
Geometry/density/depths are synthetic. Gravity is the test's rounded NIST
convention. 64 binary64 epsilons cover angle conversion and short arithmetic,
without discretisation or empirical tolerance. Plot dimensions are presentation
coordinates, not engineering constants.
"""
import copy
import hashlib
import importlib.util
import json
import math
import os
import re
import shlex
import subprocess
import sys
import xml.etree.ElementTree as ET
from fractions import Fraction
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit

import pytest

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/planing_hull"
SCRIPT = REPORT / "build_report.py"
REL_TOL = 64 * math.ulp(1.0)  # Existing hydrostatic test's roundoff rationale.
# SVG uses three decimal places: half a display unit plus binary64 roundoff
# at the documented 760-coordinate view-box width (presentation only).
PIXEL_TOL = 0.5 * 10**-3 + REL_TOL * 760


def builder():
    assert SCRIPT.is_file(), "The local report builder has not been implemented"
    spec = importlib.util.spec_from_file_location("planing_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class Page(HTMLParser):
    """Read visible, individually identified values and SVG coordinates."""

    def __init__(self, text):
        super().__init__()
        self.values = {}
        self.attributes = {}
        self.links = []
        self.ids = set()
        self.active = None
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attr = dict(attrs)
        if "id" in attr:
            self.ids.add(attr["id"])
        if "data-value" in attr:
            self.active = attr["data-value"]
            self.values[self.active] = ""
            self.attributes[self.active] = attr
        if "href" in attr:
            self.links.append(attr["href"])

    def handle_data(self, data):
        if self.active is not None:
            self.values[self.active] += data

    def handle_endtag(self, tag):
        self.active = None


def payload(page):
    return json.loads(re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>',
        page, re.S,
    )[1])


def pressure_oracle(depth):
    """Exact rational integration of both side pressures, no production calls."""
    h = max(Fraction(str(depth)), Fraction(0))
    b = h  # Unit slope from the elementary right triangle, not math.tan.
    return float(2 * 1000 * Fraction("9.81") * (h * b - b * b / 2))


def expected_inputs():
    # Synthetic inputs in the task and the existing independent test fixture.
    return {
        "length": (3.0, "m", "3.000"),
        "beam": (2.0, "m", "2.000"),
        "deadrise": (45.0, "deg", "45.0"),
        "chine_height": (1.0, "m", "1.000"),
        "lcg": (1.5, "m", "1.500"),
        "n_strips": (4, "1", "4"),
        "density": (1000.0, "kg/m^3", "1000.0"),
        "gravity": (9.81, "m/s^2", "9.81"),
    }


def assert_visible(page, key, text, unit):
    assert page.values[key] == text
    assert page.attributes[key]["data-unit"] == unit


def test_report_values_have_independent_oracle(tmp_path):
    build = builder()
    html = build.render(tmp_path / "oracle")
    data, page = payload(html), Page(html)
    expected_keys = set()
    assert set(data["inputs"]) == set(expected_inputs())
    for key, (value, unit, display) in expected_inputs().items():
        assert data["inputs"][key]["value"] == value
        assert data["inputs"][key]["unit"] == unit
        assert data["inputs"][key]["source"]
        assert_visible(page, key, display, unit)
        expected_keys.add(key)
    # Layout values are independent arithmetic from specified L and strip count.
    assert data["strip_length"] == 0.75
    assert data["strip_centres"] == [0.375, 1.125, 1.875, 2.625]
    assert_visible(page, "strip_length", "0.750", "m")
    expected_keys.add("strip_length")
    for i, centre in enumerate([0.375, 1.125, 1.875, 2.625]):
        assert_visible(page, f"centre_{i}", f"{centre:.3f}", "m")
        expected_keys.add(f"centre_{i}")
    depths = [-0.25, 0.0, 0.25, 0.5]  # Synthetic dry and below-chine anchors.
    assert [row["depth"] for row in data["rows"]] == depths
    for i, row in enumerate(data["rows"]):
        depth = depths[i]
        area = float(max(Fraction(str(depth)), Fraction(0)) ** 2)
        force = pressure_oracle(depth)
        assert row["area"] == pytest.approx(area, rel=REL_TOL, abs=0.0)
        for key in ("force", "hand_force"):
            assert row[key] == pytest.approx(force, rel=REL_TOL, abs=0.0)
        assert row["units"] == {"depth": "m", "area": "m^2", "force": "N/m"}
        for key, text, unit in (
            ("depth", f"{depth:.3f}", "m"),
            ("area", f"{area:.4f}", "m^2"),
            ("force", f"{force:.3f}", "N/m"),
            ("hand_force", f"{force:.3f}", "N/m"),
            ("difference", "0.0", "N/m"),
        ):
            name = f"row_{i}_{key}"
            assert_visible(page, name, text, unit)
            expected_keys.add(name)
        assert row["passes"] is True
        if force:
            assert abs(row["difference"]) <= REL_TOL * force
        else:
            assert row["difference"] == 0
    assert_visible(page, "summary_force", "613.125", "N/m")
    expected_keys.add("summary_force")
    # Every sample of the computed curve has an independent pressure oracle.
    assert len(data["curve"]) == 25  # Presentation grid: 24 equal intervals.
    assert [r["depth"] for r in data["curve"]] == [float(Fraction(-1, 4) + Fraction(i, 32)) for i in range(25)]
    for point in data["curve"]:
        assert point["force"] == pytest.approx(pressure_oracle(point["depth"]), rel=REL_TOL, abs=0.0)
    # No-browser namespace check: HTML spans inside SVG text are invalid.
    svg_text = re.search(r"<svg\b.*?</svg>", html, re.S)[0]
    svg = ET.fromstring(svg_text)
    namespace = "{http://www.w3.org/2000/svg}"
    allowed = {namespace + tag for tag in (
        "svg", "path", "polyline", "circle", "title", "text", "tspan",
    )}
    assert all(element.tag in allowed for element in svg.iter())
    labels = [element for element in svg.iter() if "data-value" in element.attrib]
    assert len(labels) == 7  # Four depth and three force-axis labels.
    assert all(element.tag == namespace + "tspan" for element in labels)
    # SVG projection is tested without calling a production plotting helper.
    polyline = re.search(r'<polyline id="force-curve" points="([^"]+)"', html)[1]
    points = [tuple(map(float, item.split(","))) for item in polyline.split()]
    for (x, y), row in zip(points, data["curve"], strict=True):
        assert x == pytest.approx(80 + (row["depth"] + 0.25) / 0.75 * 600, abs=PIXEL_TOL)
        assert y == pytest.approx(300 - pressure_oracle(row["depth"]) / 2452.5 * 240, abs=PIXEL_TOL)
    circles = re.findall(r'<circle class="hand-anchor" cx="([^"]+)" cy="([^"]+)"', html)
    assert len(circles) == len(depths)
    for (x, y), depth in zip(circles, depths, strict=True):
        assert float(x) == pytest.approx(80 + (depth + 0.25) / 0.75 * 600, abs=PIXEL_TOL)
        assert float(y) == pytest.approx(300 - pressure_oracle(depth) / 2452.5 * 240, abs=PIXEL_TOL)
    # Axis labels too are generated and independently checked.
    for i, depth in enumerate(depths):
        key = f"axis_depth_{i}"
        assert_visible(page, key, f"{depth:.2f}", "m")
        expected_keys.add(key)
    for i, force in enumerate([0.0, 1226.25, 2452.5]):
        key = f"axis_force_{i}"
        assert_visible(page, key, f"{force:.1f}", "N/m")
        expected_keys.add(key)
    assert set(page.values) == expected_keys  # No unasserted visible worked value.


def cli_build(scratch, output):
    # Clear inherited bindings even with an explicit cwd (fixture isolation).
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    subprocess.run(
        [sys.executable, str(SCRIPT), "--scratch", str(scratch), "--output", str(output)],
        cwd=ROOT, env=env, check=True, capture_output=True, text=True,
    )
    return {name: (output / name).read_bytes() for name in ("index.html", "changes.json")}


def test_rebuild_is_deterministic(tmp_path):
    builder()
    first = cli_build(tmp_path / "scratch-a", tmp_path / "output-a")
    second = cli_build(tmp_path / "scratch-b", tmp_path / "output-b")
    # No volatile metadata exists; no bytes are removed or normalised.
    assert first == second
    assert first == {name: (REPORT / name).read_bytes() for name in first}
    assert sorted(p.name for p in (tmp_path / "output-a").iterdir()) == sorted(first)
    assert sorted(p.name for p in (tmp_path / "scratch-a").iterdir()) == ["calculation.json"]
    assert payload(first["index.html"].decode()) == json.loads((tmp_path / "scratch-a/calculation.json").read_text())


def test_report_rejects_stale_calculation(tmp_path, monkeypatch):
    build = builder()
    before = build.render(tmp_path / "before")
    original = build.calculate

    def changed(scratch):
        data = copy.deepcopy(original(scratch))
        data["rows"][2]["force"] += 100.0  # Deliberate synthetic stale-result probe.
        data["curve"][16]["force"] += 100.0
        return data

    monkeypatch.setattr(build, "calculate", changed)
    try:
        after = build.render(tmp_path / "after")
    except ValueError as error:
        assert "stale" in str(error).lower() or "oracle" in str(error).lower()
    else:
        assert_visible(Page(after), "row_2_force", "713.125", "N/m")
        assert_visible(Page(after), "summary_force", "713.125", "N/m")
        assert re.search(r'<polyline id="force-curve"[^>]+>', before)[0] != re.search(r'<polyline id="force-curve"[^>]+>', after)[0]
        assert after != (REPORT / "index.html").read_text()


def test_links_sections_history_sources_and_limits(tmp_path):
    build = builder()
    html = build.render(tmp_path / "structure")
    page = Page(html)
    assert re.findall(r'<h2 id="s(\d+)">', html) == [str(i) for i in range(1, 11)]
    assert html.index("Revision history") < html.index('<h2 id="s2">')
    assert all(line == line.rstrip() for line in html.splitlines())
    assert 'class="hs-doc"' in html
    assert "changes-panel" in page.ids
    assert "showSaveFilePicker" in html
    changes = json.loads((REPORT / "changes.json").read_text())
    assert changes == build.CHANGES
    for revision in changes:
        for value in revision.values():
            assert value in html
    for href in page.links:
        parsed = urlsplit(href)
        if parsed.scheme:
            assert parsed.scheme == "https"
        elif parsed.path:
            target = (REPORT / unquote(parsed.path)).resolve()
            assert target.is_file(), href
            if parsed.fragment and target.suffix == ".html":
                assert parsed.fragment in Page(target.read_text()).ids
        else:
            assert parsed.fragment in page.ids
    for path, digest in payload(html)["source_digests"].items():
        assert hashlib.sha256((ROOT / path).read_bytes()).hexdigest() == digest
    for method in ("Impact added mass", "Wetting", "Dynamic damping", "Time integration", "Phase extraction", "Response amplitudes"):
        assert f"{method}: NOT_EVALUATED" in html
    assert "No volatile metadata" in (REPORT / "README.md").read_text()
    assert not re.search(r'(?:src|href)="http:', html)
    assert not re.search(r'src="https:', html)


def test_per_length_result_is_independent_of_strip_count(tmp_path):
    build = builder()
    from digitalmodel.hydrodynamics.planing_hull import PlaningHullGeometry, PlaningStripModel

    data = build.calculate(tmp_path)
    geometry = PlaningHullGeometry(**{key: data["inputs"][key]["value"] for key in (
        "length", "beam", "deadrise", "chine_height", "lcg",
    )})
    # Alternate synthetic strip count; no expected result comes from this call.
    model = PlaningStripModel(geometry, n_strips=8, rho=1000.0)
    assert model.buoyancy_force_per_length(0.25) == pytest.approx(pressure_oracle(0.25), rel=REL_TOL, abs=0.0)


def test_scratch_must_be_external_and_separate_from_output(tmp_path):
    build = builder()
    with pytest.raises(ValueError, match="outside"):
        build.render(REPORT / "scratch")
    with pytest.raises(ValueError, match="overlap"):
        build.build(tmp_path, tmp_path / "output")


def test_documented_commands_include_runtime_dependencies():
    commands = re.findall(r"^PYTHONPATH=src uv .+$", (REPORT / "README.md").read_text(), re.M)
    assert len(commands) == 3
    commands.append(builder().REBUILD)
    for command in commands:
        tokens = shlex.split(command)
        dependencies = {tokens[i + 1] for i, token in enumerate(tokens) if token == "--with"}
        assert {"pytest", "numpy", "scipy"} <= dependencies, command


def test_pressure_integral_uses_horizontal_projection(tmp_path):
    html = builder().render(tmp_path / "projection")
    assert "horizontal projection of both straight sides" in html
    assert "dFz = p cos(beta) ds = p dx" in html
    assert "vertical projection of both straight sides" not in html
