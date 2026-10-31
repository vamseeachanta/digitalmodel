# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent scalar balances and separate delivery-integrity checks."""

import copy
import hashlib
import importlib.util
import json
from html.parser import HTMLParser
from pathlib import Path
import re
import subprocess
import sys

import pytest

from digitalmodel.subsea.capping_stack import (
    CappingStack,
    DeploymentScenario,
    check_containment,
    feasibility,
    shut_in_wellhead_pressure,
    submerged_weight,
)

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_capping_stack_module_report.py"
REPORT = ROOT / "docs/reports/modules/capping_stack"
ARTIFACTS = {"index.html", "results.json", "changes.json", "manifest.json", "README.md"}


def builder():
    spec = importlib.util.spec_from_file_location("capping_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def close(actual, expected):
    """Roundoff-only tolerance; zero and decisions are checked exactly."""
    if expected == 0:
        assert actual == 0
    else:
        assert actual == pytest.approx(expected, rel=1e-12, abs=0)


def assert_hand_balance(data):
    """Independent oracle from static force balance, without model constants.

    Synthetic scalar inputs: rho=1000 kg/m3, h=0 m, R=4000000 Pa,
    p_res=(3000000,4000000,5000000) Pa; integral(rho*g dz)=0 Pa.
    Thus pressure=p_res, U=(3/4,1,5/4), inclusive flags=(True,True,False).
    Buoyancy: (m-rho*V)/m=(2000-1000*1)/2000=1/2, independent of g.
    NIST standard acceleration of gravity (https://physics.nist.gov/cgi-bin/cuu/Value?gn)
    gives exactly 9.80665 m/s2 for the separate dimensional force check.
    The crane limit is the synthetic dry force; DAF=1. Hs=(1,3) m and
    limit=1 m are arbitrary comparison inputs, not operating criteria.
    """
    expected_inputs = {
        "column_density_kg_m3": 1000, "datum_separation_m": 0,
        "reservoir_pressures_pa": [3000000, 4000000, 5000000],
        "supplied_rating_pa": 4000000,
        "pressure_reference": "common atmospheric gauge; synthetic scalar comparison",
        "mass_kg": 2000, "displaced_volume_m3": 1, "dry_volume_m3": 0,
        "water_density_kg_m3": 1000, "daf": 1,
        "crane_swl_n": 2000 * 9.80665, "water_depth_m": 0,
        "hs_m": [1, 3], "hs_limit_m": 1,
    }
    assert data["inputs"] == expected_inputs
    assert len(data["containment"]) == 3
    for row, pressure, ratio, flag in zip(
        data["containment"], [3000000, 4000000, 5000000],
        [3 / 4, 1, 5 / 4], [True, True, False],
    ):
        close(row["hydrostatic_head"], 0)
        close(row["siwhp"], pressure)
        close(row["direct_siwhp"], pressure)
        close(row["rated_working_pressure"], 4000000)
        close(row["column_density"], 1000)
        close(row["utilisation"], ratio)
        assert row["is_contained"] is flag
    lift = data["lift"]
    close(lift["dry_force_n"], 2000 * 9.80665)
    close(lift["wet_force_n"], (2000 - 1000 * 1) * 9.80665)
    close(lift["wet_dry_ratio"], (2000 - 1000 * 1) / 2000)
    assert len(lift["scenarios"]) == 2
    for row, flag, governing in zip(
        lift["scenarios"], [True, False], ["none", "sea_state"],
    ):
        close(row["submerged_weight"], 1000 * 9.80665)
        close(row["hook_load"], 1000 * 9.80665)
        close(row["lift_utilisation"], 1 / 2)
        assert row["feasible"] is flag
        assert row["governing_constraint"] == governing


def test_containment_boundary_and_buoyancy_ratio_match_hand_balance():
    """Exercise normal public imports independently of report generation."""
    for pressure, ratio, flag in zip(
        [3000000, 4000000, 5000000], [3 / 4, 1, 5 / 4], [True, True, False],
    ):
        result = check_containment(4000000, pressure, 0, column_density=1000)
        assert result.hydrostatic_head == 0
        close(result.siwhp, pressure)
        close(shut_in_wellhead_pressure(pressure, 0, 1000), pressure)
        close(result.utilisation, ratio)
        assert result.is_contained is flag
    dry = submerged_weight(2000, 0, seawater_density=1000)
    wet = submerged_weight(2000, 1, seawater_density=1000)
    close(wet / dry, (2000 - 1000 * 1) / 2000)
    stack = CappingStack(2000, displaced_volume=1)
    at_limit, above_limit = [
        feasibility(
            stack, DeploymentScenario(dry, water_depth=0, hs=hs, hs_limit=1, daf=1),
            seawater_density=1000,
        ) for hs in [1, 3]
    ]
    assert at_limit.feasible is True
    assert at_limit.governing_constraint == "none"
    assert above_limit.feasible is False
    assert above_limit.governing_constraint == "sea_state"
    close(at_limit.hook_load, wet)
    assert above_limit.hook_load == at_limit.hook_load


def build_delivery(path):
    output = path / "output"
    subprocess.run(
        [sys.executable, str(SCRIPT), "--scratch", str(path / "scratch"),
         "--output-dir", str(output)], cwd=ROOT, check=True,
        capture_output=True, text=True, timeout=30,
    )
    return output


def test_rebuild_is_byte_identical(tmp_path):
    first = build_delivery(tmp_path / "first")
    second = build_delivery(tmp_path / "second")
    assert {path.name for path in first.iterdir()} == ARTIFACTS
    assert {path.name for path in second.iterdir()} == ARTIFACTS
    for name in ARTIFACTS:
        assert (first / name).read_bytes() == (second / name).read_bytes()
        assert (first / name).read_bytes() == (REPORT / name).read_bytes()


def test_report_values_have_independent_oracles(tmp_path):
    """Neither production replay nor regenerated hashes supply expected numbers."""
    assert_hand_balance(json.loads((REPORT / "results.json").read_text()))
    assert_hand_balance(builder().calculate(tmp_path / "calculation"))


def assert_manifest(directory):
    manifest = json.loads((directory / "manifest.json").read_text())
    assert set(manifest["artifacts"]) == ARTIFACTS - {"manifest.json"}
    assert manifest["reviewed_source_revision"] == "09089a121a16069c4b657d1a320a630a23ab1dd5"
    assert manifest["sources"]
    for name, digest in manifest["artifacts"].items():
        assert hashlib.sha256((directory / name).read_bytes()).hexdigest() == digest
    for name, digest in manifest["sources"].items():
        assert hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == digest
    assert manifest["sources"][SCRIPT.relative_to(ROOT).as_posix()] == hashlib.sha256(SCRIPT.read_bytes()).hexdigest()


def test_manifest_binds_delivered_bytes(tmp_path):
    assert_manifest(REPORT)
    assert_manifest(build_delivery(tmp_path / "manifest"))


@pytest.mark.parametrize("factor", [2, -1])
def test_tampered_result_is_detected(tmp_path, monkeypatch, factor):
    """Even coherent regeneration of wrong values fails the independent oracle."""
    build = builder()
    original = build.calculate

    def changed(scratch):
        data = copy.deepcopy(original(scratch))
        data["containment"][0]["siwhp"] *= factor
        return data

    monkeypatch.setattr(build, "calculate", changed)
    destination = tmp_path / "coherent"
    build.build(tmp_path / "scratch", destination)
    assert_manifest(destination)  # Internally consistent bytes are insufficient.
    damaged = json.loads((destination / "results.json").read_text())
    assert damaged["containment"][0]["siwhp"] == factor * 3000000
    assert f"{factor * 3:.1f}" in (destination / "index.html").read_text()
    with pytest.raises(AssertionError):
        assert_hand_balance(damaged)
    # A changed delivered file without regenerating the manifest fails readback.
    damaged["containment"][0]["siwhp"] = 3000000
    damaged["lift"]["wet_dry_ratio"] = -1 / 2
    (destination / "results.json").write_text(json.dumps(damaged))
    with pytest.raises(AssertionError):
        assert_manifest(destination)
    with pytest.raises(AssertionError):
        assert_hand_balance(damaged)


class Links(HTMLParser):
    def __init__(self):
        super().__init__()
        self.targets = []
        self.ids = set()
        self.scripts = []

    def handle_starttag(self, tag, attributes):
        attrs = dict(attributes)
        if "id" in attrs:
            self.ids.add(attrs["id"])
        for key in ("href", "src"):
            if key in attrs:
                self.targets.append(attrs[key])
        if tag == "script" and "src" in attrs:
            self.scripts.append(attrs["src"])


def test_report_links_and_required_sections():
    page = (REPORT / "index.html").read_text()
    parsed = Links()
    parsed.feed(page)
    assert not parsed.scripts
    assert len(re.findall(r'<h2 id="s\d+">', page)) == 10
    for number in range(1, 11):
        assert f"s{number}" in parsed.ids
    assert page.index("Revision history") < page.index('<h2 id="s2">')
    for target in parsed.targets:
        if target.startswith("https://"):
            continue
        filename, _, anchor = target.partition("#")
        if filename:
            assert (REPORT / filename).is_file(), target
        elif anchor:
            assert anchor in parsed.ids, target
    for term in ("NOT_EVALUATED", "synthetic", "gauge", "absolute", "datum",
                 "default", "density", "09089a121a16069c4b657d1a320a630a23ab1dd5",
                 "changes-panel", 'class="hs-doc"'):
        assert term in page
    embedded = re.search(r'<script id="calculation" type="application/json">(.*?)</script>', page, re.S)
    assert json.loads(embedded[1]) == json.loads((REPORT / "results.json").read_text())
    for change in json.loads((REPORT / "changes.json").read_text()):
        assert all(value in page for value in change.values())


def test_results_replay_public_api_separately(tmp_path):
    """Production replay checks freshness only, independently of the oracle."""
    data = builder().calculate(tmp_path / "replay")
    inputs = data["inputs"]
    for row, pressure in zip(data["containment"], inputs["reservoir_pressures_pa"]):
        fresh = check_containment(inputs["supplied_rating_pa"], pressure,
                                  inputs["datum_separation_m"], inputs["column_density_kg_m3"])
        for field in ("siwhp", "hydrostatic_head", "utilisation", "is_contained"):
            assert row[field] == getattr(fresh, field)


def test_scratch_must_be_external(tmp_path):
    with pytest.raises(ValueError, match="outside"):
        builder().build(ROOT / "scratch", tmp_path / "output")


def test_embedded_values_are_escaped(tmp_path, monkeypatch):
    build = builder()
    original = build.calculate

    def hostile(scratch):
        data = original(scratch)
        data["inputs"]["pressure_reference"] = '</script><script>alert("text")</script>'
        return data

    monkeypatch.setattr(build, "calculate", hostile)
    output = tmp_path / "escaped"
    build.build(tmp_path / "scratch", output)
    page = (output / "index.html").read_text()
    assert '</script><script>alert("text")' not in page
    assert "&lt;/script&gt;" in page
    embedded = re.search(r'<script id="calculation" type="application/json">(.*?)</script>', page, re.S)
    assert json.loads(embedded[1])["inputs"]["pressure_reference"].startswith("</script>")
