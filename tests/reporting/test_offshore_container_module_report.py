# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Separate analytical oracles from report replay and deterministic rebuilds."""

import copy
import hashlib
import importlib.util
import json
import math
import os
import re
import shutil
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit

import pytest

from digitalmodel.structural.offshore_container import (
    SHS,
    DesignFactors,
    allowable_stress,
    sling_angle_to_vertical,
    utilization,
)

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_offshore_container_module_report.py"
REPORT = ROOT / "docs/reports/modules/offshore_container"
DELIVERABLES = {"index.html", "results.json", "changes.json", "manifest.json", "README.md"}


def synthetic_factors():
    """Arithmetic assumptions, deliberately independent of standard defaults."""
    return DesignFactors(
        structure_load_factor=1.6,
        padeye_load_factor=1.2,
        single_padeye_load_factor=1.7,
        usage_factor=0.5,
        default_sling_angle_to_vertical_deg=12.0,
        tare_fraction=0.1,
    )


def assert_geometry_and_allowable(data):
    """Use D1/D2 integrals in the package worked example, never API replay."""
    # Outer square 0.2 m, inner square 0.1 m: subtract their integrals.
    # A = 3/100 m²; I = (0.2**4 - 0.1**4)/12 = 1/8000 m⁴;
    # Z = I/(0.2/2) = 1/800 m³. Both routes have the same SI dimensions.
    for route in ("metres", "millimetres"):
        assert data["sections"][route] == pytest.approx(
            {"area_m2": 3 / 100, "inertia_m4": 1 / 8000, "modulus_m3": 1 / 800},
            rel=1e-12, abs=0,
        )
    # Centred 6 by 8 m rectangle: half-diagonal = sqrt(3²+4²) = 5 m.
    # Height 5 m gives equal opposite/adjacent legs, hence pi/4 radians.
    assert data["angle_rad"] == pytest.approx(math.pi / 4, rel=0, abs=1e-12)
    assert data["allowable_stress_Pa"] == pytest.approx(100e6, rel=1e-12, abs=0)


def assert_numerical_oracle(data):
    """Closed arithmetic for the synthetic input, not a physical capacity oracle."""
    assert_geometry_and_allowable(data)
    # BIPM, third CGPM resolution 2: standard gravity 980.665 cm/s²;
    # centi = 10**-2, so 9.80665 m/s² (BIPM SI prefixes).
    # https://www.bipm.org/en/committees/cg/cgpm/3-1901/resolution-2
    # g is a conventional input, not a measured local acceleration.
    gravity = 9.80665
    # Explicit synthetic inputs: mass 1000 kg; four attachments; net area .02 m².
    # Per-attachment vertical load = (1.2*1000*g)/(4-1) = 400*g N.
    # Equal triangle legs give tan(v)=1 and 1/cos(v)=sqrt(2).
    # Fi=(1.6-.1)*1000*g=1500*g N. Each long rail gets half;
    # integrate uniform load on a simple span: Mmax=Fi*L/16=562.5*g N m.
    top = (400 * gravity / (3 / 100)) / 100e6
    floor = (562.5 * gravity / (1 / 800)) / 100e6
    pad = (400 * gravity * math.sqrt(2) / 0.02) / 100e6
    actual = dict(data["utilization"])
    assert actual.pop("sling_angle_to_vertical_deg") == pytest.approx(45.0, rel=0, abs=1e-12)
    assert actual == pytest.approx({
        "top_rail": top,
        "floor": floor,
        "pad_eye": pad,
        "governing": max(top, floor, pad),
        "rsl_kN": 400 * gravity * math.sqrt(2) / 1000,
        "FL_kN": 1600 * gravity / 1000,
    }, rel=1e-12, abs=0)


def test_section_sling_and_caller_factor_values_match_independent_geometry():
    """The metre and millimetre public constructors meet the same integrals."""
    from dataclasses import asdict

    sections = {}
    for route, section in (("metres", SHS(b=0.2, t=0.05)),
                           ("millimetres", SHS.from_mm(b_mm=200, t_mm=50))):
        sections[route] = {
            "area_m2": section.area, "inertia_m4": section.inertia,
            "modulus_m3": section.modulus,
        }
    actual = {
        "sections": sections,
        "angle_rad": sling_angle_to_vertical(6, 8, 5),
        "allowable_stress_Pa": allowable_stress(200e6, synthetic_factors()),
        "utilization": asdict(utilization(
            length=6, width=8, lift_height=5, rating_kg=1000,
            section=SHS(0.2, 0.05), fy=200e6, factors=synthetic_factors(),
            n_padeyes=4, padeye_area_m2=0.02,
        )),
    }
    assert_numerical_oracle(actual)


def builder():
    """Load only the builder; the calculation uses normal package imports."""
    spec = importlib.util.spec_from_file_location("offshore_container_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def build_delivery(base):
    """Exercise required CLI arguments from an external working directory."""
    base.mkdir(parents=True)
    output = base / "output"
    env = dict(os.environ, PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    subprocess.run(
        [sys.executable, str(SCRIPT), "--scratch", str(base / "scratch"),
         "--output-dir", str(output)], cwd=base, env=env,
        check=True, capture_output=True, text=True,
    )
    return output


def test_rebuild_is_byte_identical(tmp_path):
    first = build_delivery(tmp_path / "first")
    second = build_delivery(tmp_path / "second")
    assert {p.name for p in first.iterdir()} == DELIVERABLES
    assert {p.name for p in second.iterdir()} == DELIVERABLES
    assert {p.name for p in REPORT.iterdir()} == DELIVERABLES
    for name in sorted(DELIVERABLES):
        assert (first / name).read_bytes() == (second / name).read_bytes()
        assert (first / name).read_bytes() == (REPORT / name).read_bytes(), name


def test_report_values_have_independent_oracles():
    data = json.loads((REPORT / "results.json").read_text(encoding="utf-8"))
    assert_numerical_oracle(data)
    assert data["inputs"] == {
        "b_m": 0.2, "t_m": 0.05, "b_mm": 200, "t_mm": 50,
        "length_m": 6, "width_m": 8, "lift_height_m": 5,
        "rating_kg": 1000, "fy_Pa": 200e6, "n_padeyes": 4,
        "padeye_area_m2": 0.02, "gravity_m_s2": 9.80665,
        "factors": vars(synthetic_factors()),
    }
    assert all(item["status"] == "NOT_EVALUATED" for item in data["open_findings"])


def assert_manifest(directory):
    """Read every byte independently, without using the builder's hash helper."""
    manifest = json.loads((directory / "manifest.json").read_text(encoding="utf-8"))
    assert set(manifest["artifacts"]) == DELIVERABLES - {"manifest.json"}
    for name, digest in manifest["artifacts"].items():
        assert hashlib.sha256((directory / name).read_bytes()).hexdigest() == digest, name
    assert SCRIPT.relative_to(ROOT).as_posix() in manifest["sources"]
    assert Path(__file__).relative_to(ROOT).as_posix() in manifest["sources"]
    for path, digest in manifest["sources"].items():
        assert not Path(path).is_absolute() and ".." not in Path(path).parts
        assert hashlib.sha256((ROOT / path).read_bytes()).hexdigest() == digest, path


def test_manifest_binds_delivered_bytes():
    assert_manifest(REPORT)


def test_tampered_result_is_detected(tmp_path):
    delivered = tmp_path / "delivered"
    shutil.copytree(REPORT, delivered)
    path = delivered / "results.json"
    data = json.loads(path.read_text(encoding="utf-8"))
    data["utilization"]["floor"] = -data["utilization"]["floor"]
    path.write_text(json.dumps(data) + "\n", encoding="utf-8")
    with pytest.raises(AssertionError, match="results.json"):
        assert_manifest(delivered)
    # Even rebinding the tampered delivery cannot satisfy the analytical oracle.
    manifest_path = delivered / "manifest.json"
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    manifest["artifacts"]["results.json"] = hashlib.sha256(path.read_bytes()).hexdigest()
    manifest_path.write_text(json.dumps(manifest) + "\n", encoding="utf-8")
    assert_manifest(delivered)
    with pytest.raises(AssertionError):
        assert_numerical_oracle(data)


@pytest.mark.parametrize("quantity", ["area_m2", "angle_rad", "allowable_stress_Pa", "floor", "rsl_kN"])
@pytest.mark.parametrize("multiplier", [-1, 2])
def test_agreeing_builder_and_report_cannot_replace_oracle(tmp_path, monkeypatch, quantity, multiplier):
    """Corrupt the calculation upstream so that HTML and JSON agree on a fault."""
    build = builder()
    original = build.calculate

    def corrupted(scratch):
        data = copy.deepcopy(original(scratch))
        if quantity == "area_m2":
            data["sections"]["metres"][quantity] *= multiplier
        elif quantity in {"floor", "rsl_kN"}:
            data["utilization"][quantity] *= multiplier
        else:
            data[quantity] *= multiplier
        return data

    monkeypatch.setattr(build, "calculate", corrupted)
    data = build.calculate(tmp_path / "scratch")
    page = build.render(data)
    embedded = json.loads(re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>', page, re.S,
    )[1])
    assert embedded == data  # Internally consistent replay still has no authority.
    with pytest.raises(AssertionError):
        assert_numerical_oracle(embedded)


class ReportParser(HTMLParser):
    """Collect local links and required structural elements without a browser."""

    def __init__(self):
        super().__init__()
        self.ids = []
        self.links = []
        self.headings = []
        self.scripts = []
        self.heading = None

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        for key in ("href", "src"):
            if key in attrs:
                self.links.append(attrs[key])
        if tag == "h2":
            self.heading = attrs.get("id")
            self.headings.append(self.heading)
        if tag == "script":
            self.scripts.append(attrs)


def test_report_links_and_required_sections():
    page = (REPORT / "index.html").read_text(encoding="utf-8")
    parser = ReportParser()
    parser.feed(page)
    assert parser.headings == [f"s{i}" for i in range(1, 11)]
    assert len(parser.ids) == len(set(parser.ids))
    for link in parser.links:
        parts = urlsplit(link)
        if parts.scheme:
            assert parts.scheme == "https"
            continue
        target = REPORT / unquote(parts.path) if parts.path else REPORT / "index.html"
        assert target.is_file(), link
        if parts.fragment and target.suffix == ".html":
            target_parser = ReportParser()
            target_parser.feed(target.read_text(encoding="utf-8"))
            assert parts.fragment in target_parser.ids, link
    assert all(script.get("type") == "application/json" for script in parser.scripts)
    assert 'class="hs-doc"' in page and "<svg" in page
    assert 'id="changes-panel"' in page and "NOT_EVALUATED" in page
    assert page.index("Summary</h2>") < page.index("Revision history") < page.index('id="s2"')
    data = json.loads((REPORT / "results.json").read_text(encoding="utf-8"))
    embedded = json.loads(re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>', page, re.S,
    )[1])
    assert embedded == data
    for row in builder().number_rows(data):
        assert row in page
    for change in json.loads((REPORT / "changes.json").read_text(encoding="utf-8")):
        assert all(str(value) in page for value in change.values())
    assert "/" + "home/" not in page and "/" + "mnt/" not in page


def test_scratch_is_external_and_arguments_are_required(tmp_path):
    build = builder()
    with pytest.raises(ValueError, match="outside"):
        build.calculate(ROOT / "unused-scratch")
    result = subprocess.run([sys.executable, str(SCRIPT)], cwd=tmp_path,
                            capture_output=True, text=True)
    assert result.returncode != 0
    assert "--scratch" in result.stderr and "--output-dir" in result.stderr


def test_report_escapes_text_and_embedded_json(tmp_path):
    build = builder()
    data = build.calculate(tmp_path / "escape")
    unsafe = '</script><script src="bad">&'
    data["open_findings"][0]["finding"] = unsafe
    page = build.render(data)
    assert unsafe not in page
    assert "&lt;/script&gt;" in page
    embedded = json.loads(re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>', page, re.S,
    )[1])
    assert embedded["open_findings"][0]["finding"] == unsafe
