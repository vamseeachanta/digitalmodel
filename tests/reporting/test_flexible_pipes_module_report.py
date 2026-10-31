# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check the delivered geometry report with a radius-based hand calculation."""

import copy
import hashlib
from html.parser import HTMLParser
import importlib.util
import json
import math
import os
from pathlib import Path
import re
import subprocess
import sys
from urllib.parse import urlsplit

import pytest

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/flexible_pipes"
SCRIPT = REPORT / "build_report.py"
# Source: flexible_pipe_geometry.md, independent hand calculation. This is a
# software arithmetic tolerance, not a material or engineering acceptance limit.
RELATIVE_TOLERANCE = 1e-12


def builder():
    assert SCRIPT.is_file(), "The local report builder is required"
    spec = importlib.util.spec_from_file_location("flexible_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def hand_calculation():
    """Derive every value without calling the production section or builder.

    Source: examples/domains/subsea/flexible_pipe_geometry.md, Synthetic inputs
    and Independent hand calculation. Areas use radii, not mean-diameter mass.
    Display precision follows that note; density is shown to one decimal place.
    """
    values = {}

    def add(key, value, unit, decimals):
        values[key] = (value, unit, decimals)

    rows = [(0.100, 0.010, 1000.0), (0.120, 0.020, 2000.0),
            (0.160, 0.020, 500.0)]
    areas, masses = [], []
    for index, (diameter, thickness, density) in enumerate(rows, start=1):
        prefix = f"layer_{index}."
        ri = diameter / 2
        ro = ri + thickness
        area = math.pi * (ro**2 - ri**2)
        mass = density * area
        for name, value in (("inner_diameter", diameter), ("thickness", thickness),
                            ("outer_diameter", 2 * ro), ("inner_radius", ri),
                            ("outer_radius", ro)):
            add(prefix + name, value, "m", 3)
        add(prefix + "density", density, "kg/m^3", 1)
        add(prefix + "annular_area", area, "m^2", 9)
        add(prefix + "dry_mass", mass, "kg/m", 6)
        areas.append(area)
        masses.append(mass)
    bore_radius = rows[0][0] / 2
    outer_radius = rows[-1][0] / 2 + rows[-1][1]
    add("external_density", 800.0, "kg/m^3", 1)
    add("bore_density", 600.0, "kg/m^3", 1)
    add("bore_diameter", 2 * bore_radius, "m", 3)
    add("outer_diameter", 2 * outer_radius, "m", 3)
    add("wall_area", sum(areas), "m^2", 9)
    add("envelope_area", math.pi * outer_radius**2, "m^2", 9)
    add("bore_area", math.pi * bore_radius**2, "m^2", 9)
    add("dry_mass", sum(masses), "kg/m", 6)
    add("displaced_mass", 800 * math.pi * outer_radius**2, "kg/m", 6)
    add("content_mass", 600 * math.pi * bore_radius**2, "kg/m", 6)
    return values


class ReportParser(HTMLParser):
    """Read visible measurements, geometry, and links independently."""

    def __init__(self, page):
        super().__init__()
        self.cells, self.circles, self.links, self.ids = {}, {}, [], set()
        self.current = None
        self.feed(page)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.add(attrs["id"])
        if tag == "a":
            self.links.append(attrs["href"])
        if tag == "td" and "data-key" in attrs:
            self.current = attrs["data-key"]
            assert self.current not in self.cells
            self.cells[self.current] = {**attrs, "text": ""}
        if tag == "circle" and "data-key" in attrs:
            self.circles[attrs["data-key"]] = attrs

    def handle_data(self, data):
        if self.current:
            self.cells[self.current]["text"] += data

    def handle_endtag(self, tag):
        if tag == "td":
            self.current = None


def calculation(page):
    return json.loads(re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>',
        page, re.S,
    )[1])


def test_report_values_have_independent_oracle(tmp_path):
    """Catch altered inputs, mass formulas, units, rounding, and diagram radii."""
    build = builder()
    page = build.render(tmp_path / "oracle")
    data, parsed, expected = calculation(page), ReportParser(page), hand_calculation()
    assert set(data["values"]) == set(expected) == set(parsed.cells)
    assert set(data["input_keys"]) == {
        f"layer_{index}.{name}" for index in range(1, 4)
        for name in ("inner_diameter", "thickness", "density")
    } | {"external_density", "bore_density"}
    assert [layer["type"] for layer in data["layers"]] == [
        "pressure_sheath", "anti_wear", "outer_sheath",
    ]
    for key, (value, unit, decimals) in expected.items():
        actual = data["values"][key]
        assert actual["value"] == pytest.approx(value, rel=RELATIVE_TOLERANCE, abs=0)
        assert (actual["unit"], actual["decimals"], actual["source"]) == (unit, decimals, "G")
        cell = parsed.cells[key]
        assert cell["data-unit"] == unit
        assert cell["text"] == f"{value:.{decimals}f}"
        assert abs(float(cell["text"]) - value) <= 0.5 * 10**-decimals
    # Source: note's exact area and density-times-area expressions.
    for index, coefficient in enumerate((0.0011, 0.0028, 0.0036), start=1):
        assert expected[f"layer_{index}.annular_area"][0] == pytest.approx(coefficient * math.pi, rel=RELATIVE_TOLERANCE)
    for key, coefficient in (("dry_mass", 8.5), ("displaced_mass", 8), ("content_mass", 1.5)):
        assert data["values"][key]["value"] == pytest.approx(coefficient * math.pi, rel=RELATIVE_TOLERANCE)
    checks = {check["key"]: check for check in data["checks"]}
    assert set(checks) == {key for key in expected if key not in data["input_keys"]}
    for key, check in checks.items():
        assert check["expected"] == pytest.approx(expected[key][0], rel=RELATIVE_TOLERANCE, abs=0)
        assert check["passes"] is True
        assert check["relative_error"] <= RELATIVE_TOLERANCE
    assert set(parsed.circles) == {"bore_diameter", "outer_diameter", "layer_1.outer_radius", "layer_2.outer_radius", "layer_3.outer_radius"}
    for key, circle in parsed.circles.items():
        radius = expected[key][0] / 2 if key.endswith("diameter") else expected[key][0]
        assert float(circle["r"]) / float(circle["data-scale"]) == pytest.approx(radius, rel=RELATIVE_TOLERANCE)


def run_builder(scratch, output):
    env = os.environ.copy()
    env["PYTHONPATH"] = str(ROOT / "src")
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(key, None)
    subprocess.run([sys.executable, str(SCRIPT), "--scratch", str(scratch),
                    "--output", str(output)], cwd=ROOT, env=env, check=True,
                   capture_output=True, text=True)


def test_rebuild_is_deterministic(tmp_path):
    """Catch timestamps, path leakage, stale delivery, and broken local links."""
    builder()
    outputs = [tmp_path / "first", tmp_path / "second"]
    for index, output in enumerate(outputs):
        run_builder(tmp_path / f"scratch-{index}", output)
    names = {"index.html", "changes.json", "source_register.json"}
    assert {p.name for p in outputs[0].iterdir()} == names
    assert {p.name for p in outputs[1].iterdir()} == names
    for name in names:
        # No metadata is volatile; no content is stripped before comparison.
        assert (outputs[0] / name).read_bytes() == (outputs[1] / name).read_bytes()
        assert (outputs[0] / name).read_bytes() == (REPORT / name).read_bytes()
    page = (outputs[0] / "index.html").read_text()
    assert all(line == line.rstrip() for line in page.splitlines())
    parsed = ReportParser(page)
    assert len(re.findall(r'<h2 id="s\d+">', page)) == 10
    assert page.index("Revision history") < page.index('<h2 id="s2">')
    assert 'class="hs-doc"' in page and "NOT_EVALUATED" in page
    for href in parsed.links:
        url = urlsplit(href)
        if url.scheme:
            continue
        if url.path:
            assert (outputs[0] / url.path).is_file()
        if url.fragment:
            assert url.fragment in parsed.ids
    changes = json.loads((outputs[0] / "changes.json").read_text())
    for item in changes:
        assert all(str(item[key]) in page for key in ("revision", "date", "change"))
    register = json.loads((outputs[0] / "source_register.json").read_text())
    assert register["readiness"] == "synthetic arithmetic only"
    assert register["sources"]
    for source in register["sources"]:
        assert source["sha256"] == hashlib.sha256((ROOT / source["path"]).read_bytes()).hexdigest()
    for forbidden in (str(ROOT), str(tmp_path), "src=\"http"):
        assert forbidden not in page


def test_report_rejects_stale_calculation(tmp_path, monkeypatch):
    """Catch a hardcoded table even if the embedded JSON is refreshed."""
    build = builder()
    original = build.calculate
    baseline = build.render(tmp_path / "before")
    # Synthetic perturbation: add one kg/m to the radius-derived dry total.
    def changed(scratch):
        result = copy.deepcopy(original(scratch))
        result["values"]["dry_mass"]["value"] += 1
        return result

    monkeypatch.setattr(build, "calculate", changed)
    page = build.render(tmp_path / "after")
    value = hand_calculation()["dry_mass"][0] + 1
    assert calculation(page)["values"]["dry_mass"]["value"] == pytest.approx(value, rel=RELATIVE_TOLERANCE)
    assert ReportParser(page).cells["dry_mass"]["text"] == f"{value:.6f}"
    assert page != baseline
    check = next(row for row in calculation(page)["checks"] if row["key"] == "dry_mass")
    assert check["passes"] is False
    assert "FAIL" in page


def test_scratch_cannot_overwrite_checkout():
    """Catch accidental calculation scratch in the delivered report folder."""
    with pytest.raises(ValueError, match="outside the checkout"):
        builder().render(REPORT / "scratch")


def test_local_readme_links_resolve():
    """Catch a rebuild entry point that points outside the repository."""
    for target in re.findall(r"\]\(([^)]+)\)", (REPORT / "README.md").read_text()):
        if not urlsplit(target).scheme:
            assert (REPORT / target).is_file(), target
