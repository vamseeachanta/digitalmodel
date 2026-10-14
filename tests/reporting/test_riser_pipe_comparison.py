# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Reference-case tests for the generated nominal-pipe comparison."""

from importlib import import_module
from pathlib import Path

import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignFactors, DesignLoads, PipeGeometry, PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_2rd import (
    ApiRp2rdStrategy,
)
from digitalmodel.structural.analysis.wall_thickness_codes.api_std_2rd import (
    ApiStd2rdStrategy,
)

ROOT = Path(__file__).resolve().parents[2]


def reference_input():
    """Use the module tests' geometry, material and Method 1 load reference."""
    return {
        "pipe": {"outer_diameter_mm": 273.05, "wall_thickness_mm": 21.44,
                 "grade": "X65", "smys": 448e6, "smts": 531e6,
                 "strength_unit": "Pa", "youngs_modulus_pa": 207e9,
                 "poissons_ratio": 0.3},
        "water_depth_m": 1500,
        "cases": [{"name": "operating", "internal_pressure_pa": 20e6,
                   "external_pressure_pa": 10e6, "bending_moment_nm": 250e3,
                   "effective_tension_n": 1500e3}],
    }


def test_builder_matches_module_reference_cases():
    """Keep the pressure-test references and legacy-validated Method 1 loads.

    Reference tests: test_burst_utilisation_reasonable,
    test_collapse_utilisation_reasonable, test_hoop_check_present in
    test_wall_thickness_api_rp_2rd.py; the first two in
    test_wall_thickness_api_std_2rd.py; and the first parametrisation of
    test_combined_matches_legacy_method1 in test_riser_case_screening.py.
    """
    builder = import_module("scripts.reporting.build_riser_pipe_comparison")
    model = builder.evaluate(reference_input())
    geometry = PipeGeometry(0.27305, 0.02144)
    material = PipeMaterial("X65", 448e6, 531e6)
    loads = DesignLoads(20e6, 10e6, 250e3, 1500e3)
    for code, strategy in [("API RP 2RD", ApiRp2rdStrategy()),
                           ("API STD 2RD", ApiStd2rdStrategy())]:
        expected = strategy.run_checks(geometry, material, loads, DesignFactors())
        actual = model["results"][code]
        assert set(actual) == set(expected)
        for state, (utilisation, _) in expected.items():
            assert actual[state]["utilisation"] == pytest.approx(
                utilisation, rel=1e-12, abs=1e-12)
            assert actual[state]["case"] == "operating"

# Report behaviours are exercised through the builder's file interface.
import hashlib
import json
import re
from html.parser import HTMLParser

import yaml


class Numbers(HTMLParser):
    """Read numeric cells and section order from the actual page."""

    def __init__(self):
        super().__init__()
        self.key = None
        self.values = {}
        self.headings = []
        self.heading = False

    def handle_starttag(self, tag, attrs):
        attributes = dict(attrs)
        if "data-number" in attributes:
            self.key = attributes["data-number"]
        self.heading = tag == "h2"

    def handle_data(self, data):
        if self.key:
            self.values[self.key] = data.strip()
        if self.heading:
            self.headings.append(data)

    def handle_endtag(self, tag):
        self.key = None
        self.heading = False


def builder_and_input():
    builder = import_module("scripts.reporting.build_riser_pipe_comparison")
    return builder, ROOT / "scripts/reporting/riser_pipe_comparison.yml"


def test_repeat_builds_and_every_manifest_digest(tmp_path):
    builder, source = builder_and_input()
    one, two = tmp_path / "one", tmp_path / "two"
    manifest = builder.build(source, one)
    builder.build(source, two)
    for name, evidence in manifest["artifacts"].items():
        payload = (one / name).read_bytes()
        assert payload == (two / name).read_bytes()
        assert hashlib.sha256(payload).hexdigest() == evidence["sha256"]
        assert len(payload) == evidence["size_bytes"]
    assert (one / "riser-pipe-comparison.manifest.json").read_bytes() == (
        two / "riser-pipe-comparison.manifest.json").read_bytes()
    for name, evidence in manifest["inputs"].items():
        assert hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == evidence


def test_numeric_readback_and_revision_history(tmp_path):
    builder, source = builder_and_input()
    manifest = builder.build(source, tmp_path)
    model = builder.evaluate(yaml.safe_load(source.read_text()))
    parser = Numbers()
    parser.feed((tmp_path / "riser-pipe-comparison.html").read_text())
    assert parser.values == builder.numeric_values(model)
    assert parser.headings[:2] == ["Summary", "Revision history"]
    record = json.loads((ROOT / "scripts/reporting/riser_pipe_comparison.revisions.json").read_text())
    assert manifest["revision"] == record["history"][-1]["revision"]
    page = (tmp_path / "riser-pipe-comparison.html").read_text()
    assert f'data-revision="{record["revision"]}"' in page
    assert record["history"][-1]["change"] in page
    criteria = ROOT / "scripts/reporting/riser_pipe_comparison.criteria.json"
    assert record["criteria_sha256"] == hashlib.sha256(criteria.read_bytes()).hexdigest()
    for code, states in model["results"].items():
        for state, result in states.items():
            cell = manifest["results"][code][state]
            assert cell["utilisation_percent"] == round(100 * result["utilisation"], 1)
            assert cell["within_module_limit"] == (result["utilisation"] <= 1)


def test_page_is_self_contained_and_gaps_are_not_zero(tmp_path):
    builder, source = builder_and_input()
    builder.build(source, tmp_path)
    page = (tmp_path / "riser-pipe-comparison.html").read_text()
    assert not re.search(r'<(?:script|link|img|iframe|source|object|embed)\b', page, re.I)
    assert "@import" not in page and "url(" not in page
    assert "<svg" in page
    assert "API RP 1111 is not compared" in page
    assert "Not Evaluated: the legacy module returns no combined-load check (tracked in #377)" in page
    assert "Not Evaluated: legacy edition and clause locators are not established" in page
    assert "NOT DONE" not in page
    assert all(f'id="{state}"' in page for state in ("burst", "collapse", "hoop", "combined"))
    assert "assumed, based on previous experience" in page


def test_citation_inventory_covers_inherited_coefficients(tmp_path):
    builder, source = builder_and_input()
    builder.build(source, tmp_path)
    sidecar = json.loads((tmp_path / "riser-pipe-comparison.citations.json").read_text())
    expected = {
        "API RP 2RD": {"_check_burst.p_b coefficient", "C_A (burst and collapse)",
                       "_check_hoop.sigma_allow coefficient", "_check_collapse.p_el coefficient",
                       "_check_collapse.p_y coefficient"},
        "API STD 2RD": {"_check_burst.p_b coefficient", "_check_burst.f_d",
                        "_check_collapse.f_c", "_check_combined.p_b coefficient",
                        "COMBINED_DESIGN_FACTOR", "_check_collapse.p_el coefficient",
                        "_check_collapse.p_y coefficient"},
    }
    for code, symbols in expected.items():
        entries = [entry for entry in sidecar["citations"] if entry["code"] == code]
        assert {entry["constant"] for entry in entries} == symbols
        for entry in entries:
            assert all(entry[key] for key in ("code_id", "publisher", "revision", "section"))
            assert entry["source_sibling"] == "generic"
            assert "value" not in entry
    combined = next(entry for entry in sidecar["citations"]
                    if entry["constant"] == "COMBINED_DESIGN_FACTOR")
    assert combined["section"] == "Section 5 (dynamic riser design criteria), Method 1"
    assert sidecar["qualification"] == "partial; standard conformity not established"


@pytest.mark.parametrize("field,value", [
    ("internal_pressure_pa", float("nan")),
    ("external_pressure_pa", -1),
    ("bending_moment_nm", float("inf")),
])
def test_invalid_loads_fail_before_report_write(tmp_path, field, value):
    builder, source = builder_and_input()
    config = yaml.safe_load(source.read_text())
    config["cases"][0][field] = value
    changed = tmp_path / "input.yml"
    changed.write_text(yaml.safe_dump(config))
    with pytest.raises(ValueError):
        builder.build(changed, tmp_path / "out")
    assert not (tmp_path / "out").exists()


def test_unbounded_combined_result_is_preserved(tmp_path):
    builder, source = builder_and_input()
    config = yaml.safe_load(source.read_text())
    config["cases"][0]["internal_pressure_pa"] *= 10
    changed = tmp_path / "input.yml"
    changed.write_text(yaml.safe_dump(config))
    result = builder.build(changed, tmp_path / "out")
    combined = result["results"]["API STD 2RD"]["combined"]
    assert combined["utilisation_percent"] == "unbounded"
    assert combined["within_module_limit"] is False


def test_revision_mismatch_rejects_issue(tmp_path):
    builder, source = builder_and_input()
    record = json.loads((ROOT / "scripts/reporting/riser_pipe_comparison.revisions.json").read_text())
    record["revision"] = "invalid"
    changed = tmp_path / "revision.json"
    changed.write_text(json.dumps(record))
    with pytest.raises(ValueError, match="revision"):
        builder.build(source, tmp_path / "out", revisions_path=changed)
    assert not (tmp_path / "out").exists()


def test_default_output_is_not_ignored():
    """The default report destination can be retained without force-adding."""
    import os
    import subprocess

    builder, _ = builder_and_input()
    relative = (builder.DEFAULT_OUTPUT / "riser-pipe-comparison.html").relative_to(ROOT)
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    result = subprocess.run(["git", "check-ignore", "--", relative.as_posix()],
                            cwd=ROOT, env=env, capture_output=True, text=True)
    assert result.returncode == 1


def test_input_pipe_has_assumed_generic_properties(tmp_path):
    """The published pipe basis uses assumptions without a schedule or grade."""
    builder, source = builder_and_input()
    config = yaml.safe_load(source.read_text())
    pipe = config["pipe"]
    assert pipe["grade"] == "generic assumed material"
    assert pipe["outer_diameter_mm"] == 273
    assert pipe["wall_thickness_mm"] == 21.4
    assert (pipe["smys"], pipe["smts"], pipe["strength_unit"]) == (240, 415, "MPa")
    assert config["sources"] == []
    for field in ("outer_diameter_mm", "wall_thickness_mm", "smys", "smts"):
        assert config["input_basis"][field] == "Assumed"
    builder.build(source, tmp_path)
    page = (tmp_path / "riser-pipe-comparison.html").read_text()
    assert "Public source" not in page
    assert "wermac" not in page
    assert "ASTM A106" not in page
    for key in ("od", "wall", "smys", "smts"):
        assert re.search(rf'data-number="{key}">[^<]+</span></td><td>Assumed</td>', page)
