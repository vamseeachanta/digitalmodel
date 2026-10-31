# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic mechanics and delivery checks; no equipment qualification.

Hand basis: integrate dA=r dr dtheta over radii 1/20 to 1/10 m.
A=3*pi/400 m2; I=integral(y² dA)=3*pi/128000 m4; Z=I/(1/10).
Linear strain y/R and sigma=E*strain give 200e6 Pa at R=100 m,
E=200e9 Pa; M=E*I/R and sigma/(700e6 Pa)=2/7.
For the public nozzle, radius=0.0025 m and C=25.5*sqrt(1000).
At rho=1000, v=25.5 m/s and Q=pi*(0.0025**2)*25.5 m3/s.
Continuity retains 0.0005 m3/s; subtraction gives (5-1)*1e6=4e6 Pa.
The modulus and comparison limit have public source locators; geometry and
pressure remain illustrative. A plate yield datum is not a connector allowable.
"""

import ast
import copy
import hashlib
from html.parser import HTMLParser
import importlib.util
import json
import math
import os
from pathlib import Path
import subprocess
import sys
from urllib.parse import unquote, urlsplit

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_subsea_trees_connectors_module_report.py"
REPORT = ROOT / "docs/reports/modules/subsea_trees_connectors"
FILES = {"index.html", "changes.json", "results.json", "manifest.json", "README.md"}
REBUILD = (
    'Rebuild: PYTHONPATH=src uv run python '
    'scripts/reports/build_subsea_trees_connectors_module_report.py '
    '--scratch "$TMPDIR/subsea-report-scratch" '
    '--output-dir docs/reports/modules/subsea_trees_connectors'
)
# Binary64 arithmetic roundoff only; no absolute floor can hide a wrong sign/zero.
RELATIVE_ROUNDOFF = 1e-12


def builder():
    assert SCRIPT.is_file(), "The subsea module report builder is missing"
    spec = importlib.util.spec_from_file_location("subsea_module_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def assert_independent_outputs(data):
    """Reject wrong arithmetic even when HTML, JSON and manifest agree."""
    expected = {
        "geometry": {"inner_diameter_m": 0.1, "area_m2": 3 * math.pi / 400,
                     "bore_area_m2": math.pi / 400,
                     "second_moment_m4": 3 * math.pi / 128000,
                     "section_modulus_m3": 3 * math.pi / 12800},
        "bending": {"curvature_per_m": 0.01, "stress_pa": 200e6,
                    "moment_nm": 200e9 * (3 * math.pi / 128000) / 100,
                    "utilization": 0.2857142857142857, "within_supplied_limit": True},
        "tree": {"velocity_ceiling_m_s": 25.5, "rate_ceiling_m3_s": 0.0005006913291658733},
        "handoff": {"production_rate_m3_s": 0.0005,
                    "annulus_rate_m3_s": 0.0, "downstream_pressure_pa": 4e6,
                    "within_supplied_ceiling": True},
    }
    assert data["outputs"].keys() == expected.keys()
    for group, values in expected.items():
        assert data["outputs"][group].keys() == values.keys()
        for key, reference in values.items():
            actual = data["outputs"][group][key]
            if isinstance(reference, bool):
                assert actual is reference, (group, key)
            elif reference == 0:
                assert actual == 0, (group, key)
            else:
                assert actual == pytest.approx(reference, rel=RELATIVE_ROUNDOFF, abs=0), (group, key)


class Page(HTMLParser):
    """Read rendered cells, links and JSON independently of the renderer."""

    def __init__(self, text):
        super().__init__()
        self.ids, self.links, self.sections = set(), [], []
        self.results, self.payloads, self.cells = {}, {}, {}
        self.current = None
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.add(attrs["id"])
        for attr in ("href", "src"):
            if attr in attrs:
                self.links.append(attrs[attr])
        if tag == "h2":
            self.sections.append(attrs.get("id"))
        if tag == "script":
            assert attrs.get("type") == "application/json", "No executable scripts"
            self.current = (self.payloads, attrs["id"])
        elif "data-result" in attrs:
            self.current = (self.results, attrs["data-result"])
        elif "data-input" in attrs:
            self.current = (self.cells, attrs["data-input"])

    def handle_data(self, value):
        if self.current:
            target, key = self.current
            target[key] = target.get(key, "") + value

    def handle_endtag(self, tag):
        if tag in {"td", "script"}:
            self.current = None


def read_delivery(directory):
    """Verify bytes, then check numeric meaning using the independent oracle."""
    manifest = json.loads((directory / "manifest.json").read_text(encoding="utf-8"))
    assert set(manifest["files"]) == FILES - {"manifest.json"}
    for name, digest in manifest["files"].items():
        assert hashlib.sha256((directory / name).read_bytes()).hexdigest() == digest, f"{name}: {REBUILD}"
    data = json.loads((directory / "results.json").read_text(encoding="utf-8"))
    page = Page((directory / "index.html").read_text(encoding="utf-8"))
    assert json.loads(page.payloads["calculation"]) == data
    assert_independent_outputs(data)
    return data, page, manifest


def build_delivery(directory):
    """Exercise the command in a fresh external working and output directory."""
    directory.mkdir()
    env = dict(os.environ)
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(name, None)
    env["PYTHONPATH"] = str(ROOT / "src")
    subprocess.run([sys.executable, str(SCRIPT), "--scratch", str(directory / "scratch"),
                    "--output-dir", str(directory / "output")],
                   cwd=directory, env=env, check=True, capture_output=True, text=True, encoding="utf-8")
    return directory / "output"


def test_static_connector_and_tree_outputs_match_annulus_and_flow_balance():
    """Catch geometry, units, stress-sign and flow/pressure handoff defects."""
    from digitalmodel.subsea import connectors, trees

    section = connectors.PipeSection(outer_diameter=0.2, wall_thickness=0.05)
    bend = connectors.bending_from_radius(section, radius=100, youngs_modulus=200e9,
                                          allowable_stress=700e6)
    bore = trees.bore_capacity(0.005, 1000, 25.5 * math.sqrt(1000))
    handoff = trees.flow_handoff(0.0005, 0, bore, None, 5e6, 1e6)
    assert_independent_outputs({"outputs": {
        "geometry": {"inner_diameter_m": section.inner_diameter, "area_m2": section.area,
                     "bore_area_m2": section.bore_area,
                     "second_moment_m4": section.moment_of_inertia,
                     "section_modulus_m3": section.section_modulus},
        "bending": {"curvature_per_m": bend.curvature, "stress_pa": bend.bending_stress,
                    "moment_nm": bend.bending_moment, "utilization": bend.utilisation,
                    "within_supplied_limit": bend.passed},
        "tree": {"velocity_ceiling_m_s": bore.velocity_m_s, "rate_ceiling_m3_s": bore.rate_m3_s},
        "handoff": {"production_rate_m3_s": handoff.production_rate_m3_s,
                    "annulus_rate_m3_s": handoff.annulus_rate_m3_s,
                    "downstream_pressure_pa": handoff.downstream_pressure_pa,
                    "within_supplied_ceiling": handoff.within_capacity},
    }})


def test_rebuild_is_byte_identical(tmp_path):
    """Detect nondeterminism and stale committed deliverables, including README."""
    assert SCRIPT.is_file(), "The subsea module report builder is missing"
    first = build_delivery(tmp_path / "first")
    second = build_delivery(tmp_path / "second")
    assert {p.name for p in first.iterdir()} == FILES
    assert {p.name for p in second.iterdir()} == FILES
    for name in sorted(FILES):
        assert (first / name).read_bytes() == (second / name).read_bytes(), name
        assert (first / name).read_bytes() == (REPORT / name).read_bytes(), f"{name}: {REBUILD}"


def test_report_values_have_independent_oracles(tmp_path):
    """A fresh calculation and delivered calculation each meet the hand basis."""
    build = builder()
    assert_independent_outputs(build.calculate(tmp_path / "calculation"))
    data, page, _ = read_delivery(REPORT)
    assert set(page.results) == {f"{group}.{key}" for group, values in data["outputs"].items()
                                 for key in values}
    for group, values in data["outputs"].items():
        for key, value in values.items():
            assert page.results[f"{group}.{key}"] == build.format_value(value, decision=key == "utilization")
    assert set(page.cells) == {f"{group}.{key}" for group, values in data["inputs"].items()
                               for key in values}
    for group, values in data["inputs"].items():
        for key, value in values.items():
            assert page.cells[f"{group}.{key}"] == build.format_value(value, thickness=key == "wall_thickness_m")


def test_manifest_binds_delivered_bytes():
    """Detect altered outputs and source drift relative to the reviewed revision."""
    _, _, manifest = read_delivery(REPORT)
    for path, digest in manifest["sources"].items():
        assert hashlib.sha256((ROOT / path).read_bytes()).hexdigest() == digest, f"{path}: {REBUILD}"
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    for path, digest in manifest["sources"].items():
        if path.startswith("src/"):
            pinned = subprocess.run(["git", "show", f'{manifest["reviewed_source_revision"]}:{path}'],
                                    cwd=ROOT, env=env, capture_output=True, check=True).stdout
            assert hashlib.sha256(pinned).hexdigest() == digest, f"{path}: {REBUILD}"


@pytest.mark.parametrize("group,key,factor", [
    ("geometry", "area_m2", 2), ("bending", "stress_pa", -1),
    ("tree", "rate_ceiling_m3_s", 2), ("handoff", "downstream_pressure_pa", -1),
])  # Doubling and sign reversal are deliberate corruption probes, not criteria.
def test_tampered_result_is_detected(tmp_path, monkeypatch, group, key, factor):
    """Reject corruption even when the builder regenerates a consistent bundle."""
    build = builder()
    original = build.calculate

    def corrupted(scratch):
        data = copy.deepcopy(original(scratch))
        data["outputs"][group][key] *= factor
        return data

    monkeypatch.setattr(build, "calculate", corrupted)
    build.build(tmp_path / "scratch", tmp_path / "output")
    with pytest.raises(AssertionError):
        read_delivery(tmp_path / "output")


def test_delivered_byte_tampering_is_detected(tmp_path):
    """The manifest rejects a result edit before semantic validation."""
    build = builder()
    build.build(tmp_path / "scratch", tmp_path / "output")
    target = tmp_path / "output/results.json"
    data = json.loads(target.read_text(encoding="utf-8"))
    data["outputs"]["bending"]["stress_pa"] *= -1  # Deliberate sign corruption.
    target.write_text(json.dumps(data), encoding="utf-8", newline="\n")
    with pytest.raises(AssertionError, match="results.json"):
        read_delivery(tmp_path / "output")


def test_report_links_and_required_sections():
    """Catch broken local links, absent scope sections and remote dependencies."""
    data, page, _ = read_delivery(REPORT)
    text = (REPORT / "index.html").read_text(encoding="utf-8")
    assert page.sections == [f"s{index}" for index in range(1, 11)]
    assert text.index("Revision history") < text.index('id="s2"')
    history = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    assert json.loads(page.payloads["changes"]) == history
    for row in history:
        assert all(value in text for value in row.values())
    for href in page.links:
        link = urlsplit(href)
        if link.scheme:
            assert link.scheme == "https"
        elif link.path:
            assert (REPORT / unquote(link.path)).is_file(), href
        elif link.fragment:
            assert link.fragment in page.ids, href
    assert "<svg" in text and "<img" not in text
    assert "Draft for maintainer review" in text
    assert "not erosional qualification" in text
    for gap in ("Fatigue assessment", "Dynamic response", "Network coupling", "Standards compliance"):
        assert gap in text
    assert data["status"] == "SYNTHETIC_STATIC_ONLY"


def test_inventory_covers_public_interfaces():
    """A newly exposed callable cannot disappear silently from the report scope."""
    data, _, _ = read_delivery(REPORT)
    inventory = " ".join(row["interface"] for row in data["inventory"])
    for package in ("trees", "connectors"):
        for path in (ROOT / "src/digitalmodel/subsea" / package).glob("*.py"):
            tree = ast.parse(path.read_text(encoding="utf-8"))
            for node in tree.body:
                if isinstance(node, (ast.FunctionDef, ast.ClassDef)) and not node.name.startswith("_"):
                    assert node.name in inventory, node.name
                if isinstance(node, ast.ClassDef):
                    for child in node.body:
                        if isinstance(child, ast.FunctionDef) and not child.name.startswith("_"):
                            assert child.name in inventory, child.name


def test_unsafe_scratch_is_rejected_before_writes(tmp_path):
    """A caller cannot direct calculation intermediates into the checkout."""
    build = builder()
    with pytest.raises(ValueError, match="outside"):
        build.build(ROOT / "scratch", tmp_path / "output")
    assert not (tmp_path / "output").exists()


def test_embedded_text_and_json_are_escaped(tmp_path, monkeypatch):
    """Untrusted source text cannot become HTML or end an embedded JSON block."""
    build = builder()
    original = build.calculate
    probe = "</script><img src=x onerror=alert(1)>"

    def with_text(scratch):
        data = original(scratch)
        data["inventory"][0]["limits"] = probe
        return data

    monkeypatch.setattr(build, "calculate", with_text)
    build.build(tmp_path / "scratch", tmp_path / "output")
    text = (tmp_path / "output/index.html").read_text(encoding="utf-8")
    assert probe not in text
    assert "&lt;/script&gt;&lt;img" in text
    assert json.loads(Page(text).payloads["calculation"])["inventory"][0]["limits"] == probe


def test_public_input_basis_reaches_delivery_and_independent_results(tmp_path):
    """Reject unreferenced engineering inputs even when delivery is consistent."""
    build = builder()
    build.build(tmp_path / "scratch", tmp_path / "output")
    data = json.loads((tmp_path / "output/results.json").read_text(encoding="utf-8"))
    text = (tmp_path / "output/index.html").read_text(encoding="utf-8")
    page = Page(text)
    expected_sources = {
        "S1": "https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus",
        "S2": "https://www.ssab.com/en-ca/brands-and-products/strenx/product-offer/700/e-f",
        "S3": "https://openstax.org/books/university-physics-volume-1/pages/14-5-fluid-dynamics",
        "S4": "https://openstax.org/books/university-physics-volume-1/pages/14-6-bernoullis-equation",
    }
    assert {key: value["url"] for key, value in data["sources"].items()} == expected_sources
    for key in ("connector.youngs_modulus_pa", "connector.supplied_stress_limit_pa",
                "tree.coefficient_si"):
        basis = data["input_basis"][key]
        assert basis["sources"] and basis["derivation"]
        for source_id in basis["sources"]:
            source = data["sources"][source_id]
            assert source["url"] in page.links
            assert source["locator"] in text
            assert source["edition"] and source["accessed_on"] == "2026-10-28"
    assert data["inputs"]["connector"]["youngs_modulus_pa"] == 200e9
    assert data["inputs"]["connector"]["supplied_stress_limit_pa"] == 700e6
    assert data["inputs"]["tree"]["diameter_m"] == 0.005
    # sigma=(200e9)*(0.1/100)=200e6 Pa; u=200/700=2/7.
    assert data["outputs"]["bending"]["stress_pa"] == pytest.approx(200e6, rel=1e-12, abs=0)
    assert data["outputs"]["bending"]["utilization"] == pytest.approx(0.2857142857142857, rel=1e-12, abs=0)
    # Q=pi*(0.005/2)^2*25.5=0.0005006913291658733 m3/s.
    assert data["outputs"]["tree"]["rate_ceiling_m3_s"] == pytest.approx(0.0005006913291658733, rel=1e-12, abs=0)
    # M=(200e9)*(3*pi/128000)/100=147262.15563702155 N*m.
    assert data["outputs"]["bending"]["moment_nm"] == pytest.approx(147262.15563702155, rel=1e-12, abs=0)
    assert "not a connector design allowable" in text
    assert "not an erosional design coefficient" in text
    assert "No independent design basis is supplied for either" not in text
