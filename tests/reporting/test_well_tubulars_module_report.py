# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent mechanics, byte rebuilds and delivery integrity for tubulars."""

import copy
import hashlib
import importlib.util
import json
import math
import os
import re
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_well_tubulars_module_report.py"
REPORT = ROOT / "docs/reports/modules/well_tubulars"
OUTPUTS = ("index.html", "results.json", "changes.json", "README.md", "manifest.json")
# Rounding budget retained from worked-tubular-stress-invariants.md, not a
# physical uncertainty allowance: decimal wall subtraction and eigenvalue roots.
REL_TOL = 1e-12
ABS_TOL_PSI = 1e-8


def builder():
    spec = importlib.util.spec_from_file_location("tubulars_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def principal_equivalent(axial, hoop, shear):
    """Eigenvalues of the symmetric plane-stress tensor; see worked example."""
    centre = (axial + hoop) / 2.0
    radius = math.hypot((axial - hoop) / 2.0, shear)
    first, second, third = centre + radius, centre - radius, 0.0
    return math.sqrt(((first - second) ** 2 + (second - third) ** 2
                      + (third - first) ** 2) / 2.0)


def assert_independent_values(data):
    """Use fixed example assumptions, never production results, as oracle inputs."""
    # Table 1 and the longitudinal cut in the cited worked example: one inch
    # length, mean diameter 10 in, cut strips 2 * 0.2 * 1 = 0.4 in².
    assert data["stress_unit"] == "psi"
    assert data["geometry"] == {"od_in": 10.2, "id_in": 9.8}
    assert set(data["cases"]) == {"internal", "reversed", "pure_shear"}
    loads = {
        "internal": (500.0, 100.0, 6000.0, 2000.0),
        "reversed": (100.0, 500.0, 6000.0, 2000.0),
        "pure_shear": (0.0, 0.0, 0.0, 1500.0),
    }
    for name, (internal, external, axial, shear) in loads.items():
        case = data["cases"][name]
        assert case["internal_pressure_psi"] == internal
        assert case["external_pressure_psi"] == external
        assert case["differential_pressure_psi"] == internal - external
        assert case["axial_psi"] == axial
        assert case["shear_psi"] == shear
        pressure_resultant_lbf = (internal - external) * 10.0 * 1.0
        hoop = pressure_resultant_lbf / (2.0 * 0.2 * 1.0)
        assert case["hoop_psi"] == pytest.approx(hoop, rel=REL_TOL, abs=ABS_TOL_PSI)
        expected = principal_equivalent(axial, hoop, shear)
        assert case["vme_psi"] == pytest.approx(expected, rel=REL_TOL, abs=ABS_TOL_PSI)


def test_worked_stresses_match_force_balance_and_principal_invariants():
    """Normal package imports are compared directly with independent mechanics."""
    from digitalmodel.well.tubulars import compute_hoop_stress, compute_vme_stress

    # Half-cylinder balance: 400 * 10 * 1 = 4000 lbf over 0.4 in².
    expected_hoop = 4000.0 / 0.4
    hoop = compute_hoop_stress(400.0, 10.2, 9.8)
    assert hoop == pytest.approx(expected_hoop, rel=REL_TOL, abs=ABS_TOL_PSI)
    # Tensor [[6000, 2000], [2000, 10000]] psi has eigenvalues
    # 8000 +/- sqrt(8,000,000) psi and zero; see independent helper above.
    expected = principal_equivalent(6000.0, expected_hoop, 2000.0)
    assert expected == pytest.approx(math.sqrt(88_000_000.0), rel=REL_TOL)
    assert compute_vme_stress(6000.0, hoop, 2000.0) == pytest.approx(
        expected, rel=REL_TOL, abs=ABS_TOL_PSI)
    reversed_hoop = compute_hoop_stress(-400.0, 10.2, 9.8)
    assert reversed_hoop == pytest.approx(-expected_hoop, rel=REL_TOL, abs=ABS_TOL_PSI)
    reversed_expected = principal_equivalent(6000.0, -expected_hoop, 2000.0)
    assert reversed_expected == pytest.approx(math.sqrt(208_000_000.0), rel=REL_TOL)
    assert compute_vme_stress(6000.0, reversed_hoop, 2000.0) == pytest.approx(
        reversed_expected, rel=REL_TOL, abs=ABS_TOL_PSI)
    # Pure shear principals (+tau, -tau, 0) give sqrt(3)*tau.
    assert compute_vme_stress(0.0, 0.0, 1500.0) == pytest.approx(
        1500.0 * math.sqrt(3.0), rel=REL_TOL, abs=ABS_TOL_PSI)


@pytest.fixture(scope="module")
def rebuilds(tmp_path_factory):
    outputs = []
    for _ in range(2):
        directory = tmp_path_factory.mktemp("tubulars")
        output = directory / "delivery"
        environment = dict(os.environ, PYTHONPATH=str(ROOT / "src"),
                           PYTHONDONTWRITEBYTECODE="1")
        subprocess.run([sys.executable, str(SCRIPT), "--scratch",
                        str(directory / "scratch"), "--output-dir", str(output)],
                       cwd=directory, env=environment, check=True,
                       capture_output=True, text=True,
                       timeout=30)
        outputs.append(output)
    return outputs


def embedded_results(page):
    return json.loads(re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>',
        page, re.S)[1])


def assert_manifest(directory):
    """Independent readback of every delivered and source digest."""
    manifest = json.loads((directory / "manifest.json").read_text())
    assert set(manifest["outputs"]) == set(OUTPUTS) - {"manifest.json"}
    for name, digest in manifest["outputs"].items():
        assert hashlib.sha256((directory / name).read_bytes()).hexdigest() == digest
    for name, digest in manifest["sources"].items():
        assert hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == digest
    assert manifest["reviewed_revision"] == "09089a121a16069c4b657d1a320a630a23ab1dd5"


def test_rebuild_is_byte_identical(rebuilds):
    for name in OUTPUTS:
        assert (rebuilds[0] / name).read_bytes() == (rebuilds[1] / name).read_bytes()
        assert (rebuilds[0] / name).read_bytes() == (REPORT / name).read_bytes()


def test_report_values_have_independent_oracles(rebuilds):
    # Independent from both the report generator and its recorded results.
    assert_independent_values(json.loads((REPORT / "results.json").read_text()))
    for output in rebuilds:
        assert_independent_values(json.loads((output / "results.json").read_text()))
        assert_independent_values(embedded_results((output / "index.html").read_text()))


def test_public_api_replay_is_separate_from_oracle(tmp_path):
    """A replay confirms freshness only; it supplies no independent expectation."""
    build = builder()
    assert build.calculate(tmp_path) == json.loads((REPORT / "results.json").read_text())


def test_manifest_binds_delivered_bytes(rebuilds):
    for output in (*rebuilds, REPORT):
        assert_manifest(output)


def test_tampered_result_is_detected(rebuilds, tmp_path):
    for name in OUTPUTS:
        (tmp_path / name).write_bytes((rebuilds[0] / name).read_bytes())
    data = json.loads((tmp_path / "results.json").read_text())
    data["cases"]["internal"]["vme_psi"] += 1.0  # Deliberate corruption, psi.
    (tmp_path / "results.json").write_text(json.dumps(data))
    with pytest.raises(AssertionError):
        assert_manifest(tmp_path)
    with pytest.raises(AssertionError):
        assert_independent_values(data)


@pytest.mark.parametrize("field,alter", [
    ("vme_psi", lambda value: value + 1.0),
    ("hoop_psi", lambda value: -value),
])
def test_consistent_regeneration_cannot_replace_independent_oracle(tmp_path, monkeypatch,
                                                                  field, alter):
    build = builder()
    data = copy.deepcopy(build.calculate(tmp_path / "calculation"))
    data["cases"]["internal"][field] = alter(data["cases"]["internal"][field])
    monkeypatch.setattr(build, "calculate", lambda scratch: copy.deepcopy(data))
    output = tmp_path / "delivery"
    build.build(tmp_path / "regeneration", output)
    delivered = json.loads((output / "results.json").read_text())
    # Builder, HTML, JSON and all manifest hashes agree; mechanics must still fail.
    assert delivered == embedded_results((output / "index.html").read_text())
    assert_manifest(output)
    with pytest.raises(AssertionError):
        assert_independent_values(delivered)


class Links(HTMLParser):
    def __init__(self):
        super().__init__()
        self.ids = set()
        self.links = []
        self.headings = []
        self.assets = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.add(attrs["id"])
        if "href" in attrs:
            self.links.append(attrs["href"])
        if "src" in attrs:
            self.assets.append(attrs["src"])
        if tag == "h2":
            self.headings.append(attrs.get("id"))


def test_report_links_and_required_sections():
    page = (REPORT / "index.html").read_text()
    parsed = Links()
    parsed.feed(page)
    assert parsed.headings == [f"s{number}" for number in range(1, 11)]
    for title in ("Summary", "Context and scope", "Input basis", "Method",
                  "Worked calculation", "Checks", "Results", "Limits / NOT_EVALUATED",
                  "Way forward", "References and evidence", "Revision history"):
        assert title in page
    assert page.index("Summary") < page.index("Revision history") < page.index("Context and scope")
    assert 'class="hs-doc"' in page
    assert "<svg" in page and not parsed.assets
    for target in parsed.links:
        parts = urlsplit(target)
        assert not parts.scheme and not parts.netloc, "Report links must work offline"
        if parts.path:
            assert (REPORT / unquote(parts.path)).resolve().is_file(), target
        else:
            assert parts.fragment in parsed.ids, target
    changes = json.loads((REPORT / "changes.json").read_text())
    for item in changes:
        assert all(str(value) in page for value in item.values())
    assert "NOT_EVALUATED" in page and "rounding budget" in page
    assert "/" + "home/" not in page and "/" + "mnt/" not in page


def test_inventory_covers_package_public_surface():
    import ast
    build = builder()
    records = build.inventory()
    actual = {(item["module"], item["name"]) for item in records}
    package = ROOT / "src/digitalmodel/well/tubulars"
    expected = set()
    for path in sorted(package.glob("*.py")):
        if path.stem == "__init__":
            continue
        for node in ast.parse(path.read_text()).body:
            if isinstance(node, (ast.FunctionDef, ast.ClassDef)) and not node.name.startswith("_"):
                expected.add((path.stem, node.name))
    assert actual == expected
    for row in records:
        assert row["inputs"] and row["outputs"] and row["citation_metadata"]
        assert row["status"] == ("ARITHMETIC_CHECKED" if row["name"] in
                                 {"compute_hoop_stress", "compute_vme_stress"}
                                 else "NOT_EVALUATED")


def test_scratch_must_be_external_and_disjoint(tmp_path):
    build = builder()
    with pytest.raises(ValueError):
        build.build(ROOT / "scratch", tmp_path / "delivery")
    with pytest.raises(ValueError):
        build.build(tmp_path, tmp_path / "delivery")


def test_embedded_text_and_json_are_escaped(tmp_path, monkeypatch):
    build = builder()
    data = build.calculate(tmp_path / "calculation")
    data["basis"] = '</script><img src="bad">&'
    monkeypatch.setattr(build, "calculate", lambda scratch: data)
    page = build.render(data)
    assert data["basis"] not in page
    assert embedded_results(page) == data
