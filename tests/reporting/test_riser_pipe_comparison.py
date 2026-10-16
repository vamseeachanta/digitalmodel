# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Reference-case tests for the generated nominal-pipe comparison."""

from importlib import import_module
from pathlib import Path

import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignFactors, DesignLoads, PipeGeometry, PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_1111 import ApiRp1111Strategy
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


@pytest.mark.parametrize("reference", ["riser", "pipeline"])
def test_builder_matches_module_reference_cases(reference):
    """Keep the pressure-test references and legacy-validated Method 1 loads.

    Reference tests: test_burst_utilisation_reasonable,
    test_collapse_utilisation_reasonable, test_hoop_check_present in
    test_wall_thickness_api_rp_2rd.py; the first two in
    test_wall_thickness_api_std_2rd.py; and the first parametrisation of
    test_combined_matches_legacy_method1 in test_riser_case_screening.py.
    """
    builder = import_module("scripts.reporting.build_riser_pipe_comparison")
    config = reference_input()
    if reference == "pipeline":
        # Geometry and loads from test_api_rp_1111.test_burst_10mpa_thin_wall.
        config["pipe"].update(outer_diameter_mm=273.1, wall_thickness_mm=12.7,
                              smys=358.5e6, smts=455.1e6)
        config["cases"][0].update(internal_pressure_pa=10e6, external_pressure_pa=0,
                                  bending_moment_nm=0, effective_tension_n=0)
    model = builder.evaluate(config)
    pipe, case = config["pipe"], config["cases"][0]
    geometry = PipeGeometry(pipe["outer_diameter_mm"] / 1000, pipe["wall_thickness_mm"] / 1000)
    material = PipeMaterial(pipe["grade"], pipe["smys"], pipe["smts"])
    loads = DesignLoads(case["internal_pressure_pa"], case["external_pressure_pa"],
                        case["bending_moment_nm"], case["effective_tension_n"])
    for code, strategy in [("API RP 1111", ApiRp1111Strategy()),
                           ("API RP 2RD", ApiRp2rdStrategy()),
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
    assert "API RP 1111 module edition metadata is unqualified" in page
    assert "Not compared: edition defect" not in page
    assert "Not Evaluated: the legacy module returns no combined-load check (tracked in #377)" in page
    assert "Not Evaluated: legacy edition and clause locators are not established" in page
    assert "NOT DONE" not in page
    assert all(f'id="{state}"' in page for state in ("burst", "collapse", "propagation", "hoop", "combined"))
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


@pytest.mark.parametrize("depth,pressure,external", [(50, 20e6, 0.5e6), (1500, 35e6, 15e6)])
def test_three_codes_for_shallow_and_deep_reference_configurations(depth, pressure, external):
    """Compare the existing comparison tests' two pipe configurations."""
    builder, _ = builder_and_input()
    config = reference_input()
    deep = depth == 1500
    config["pipe"].update(outer_diameter_mm=219.1 if deep else 273.1,
                          wall_thickness_mm=18.3 if deep else 21.4)
    config["water_depth_m"] = depth
    config["cases"][0].update(internal_pressure_pa=pressure, external_pressure_pa=external,
                               bending_moment_nm=0, effective_tension_n=0)
    model = builder.evaluate(config)
    assert set(model["results"]) == {"API RP 1111", "API RP 2RD", "API STD 2RD"}
    geometry, material = builder._pipe_objects(config)
    loads = DesignLoads(pressure, external)
    for code, enum in builder.CODES.items():
        native = builder.CODE_REGISTRY[enum]().run_checks(geometry, material, loads, DesignFactors())
        assert {state: result["utilisation"] for state, result in model["results"][code].items()} == {
            state: result[0] for state, result in native.items()}


def test_minimum_wall_passes_and_previous_grid_point_fails():
    """The reported wall is the first passing grid point for every supplied case."""
    builder, _ = builder_and_input()
    config = reference_input()
    config["cases"].append({"name": "pressure", "internal_pressure_pa": 45e6,
                             "external_pressure_pa": 2e6, "bending_moment_nm": 0,
                             "effective_tension_n": 0})
    model = builder.evaluate(config)
    for code, minimum in model["minimum_wall"].items():
        assert minimum["status"] == "found"
        assert minimum["state"] in model["results"][code]
        assert minimum["case"] in {case["name"] for case in config["cases"]}
        for offset, expected in [(0, True), (-0.1, False)]:
            from copy import deepcopy
            changed = deepcopy(config)
            changed["pipe"]["wall_thickness_mm"] = minimum["wall_mm"] + offset
            geometry, material = builder._pipe_objects(changed)
            checks = [builder.CODE_REGISTRY[builder.CODES[code]]().run_checks(
                geometry, material, DesignLoads(case["internal_pressure_pa"], case["external_pressure_pa"],
                                                case["bending_moment_nm"], case["effective_tension_n"]),
                DesignFactors()) for case in config["cases"]]
            assert all(utilisation <= 1 for result in checks for utilisation, _ in result.values()) == expected


def test_unreachable_minimum_is_explicit():
    builder, _ = builder_and_input()
    config = reference_input()
    config["cases"][0]["effective_tension_n"] = 1e15
    minimum = builder.evaluate(config)["minimum_wall"]["API STD 2RD"]
    assert minimum == {"status": "not found on search grid"}


def test_three_code_artifacts_and_minimum_wall_readback(tmp_path):
    builder, source = builder_and_input()
    manifest = builder.build(source, tmp_path)
    assert set(manifest["results"]) == {"API RP 1111", "API RP 2RD", "API STD 2RD"}
    parser = Numbers()
    page = (tmp_path / "riser-pipe-comparison.html").read_text()
    parser.feed(page)
    for code, minimum in manifest["minimum_wall"].items():
        assert parser.values[f"minimum-{builder._code_key(code)}"] == f'{minimum["wall_mm"]:.1f}'
    sidecar = json.loads((tmp_path / "riser-pipe-comparison.citations.json").read_text())
    rp = [entry for entry in sidecar["citations"] if entry["code"] == "API RP 1111"]
    assert {entry["constant"] for entry in rp} == {
        "_check_burst.p_b coefficients and branch threshold", "EDITION_FACTORS.f_d",
        "EDITION_FACTORS.f_c", "EDITION_FACTORS.f_p", "_check_collapse.p_el coefficient",
        "_check_collapse.p_y coefficient", "_check_propagation.p_pr coefficient and exponent"}
    assert all("unqualified" in entry["qualification"] for entry in rp)
    assert "Minimum passing nominal wall" in page
    assert "0.1 mm grid" in page
    assert "2015" in page


def test_case_envelopes_keep_independent_governing_cases():
    builder, _ = builder_and_input()
    config = reference_input()
    config["cases"] = [
        {"name": "burst demand", "internal_pressure_pa": 40e6, "external_pressure_pa": 0,
         "bending_moment_nm": 0, "effective_tension_n": 0},
        {"name": "external demand", "internal_pressure_pa": 15e6, "external_pressure_pa": 15e6,
         "bending_moment_nm": 250e3, "effective_tension_n": 1500e3}]
    model = builder.evaluate(config)
    for code, states in model["results"].items():
        assert states["burst"]["case"] == "burst demand"
        assert states["collapse"]["case"] == "external demand"
        assert model["governing"][code]["utilisation"] == max(r["utilisation"] for r in states.values())
    assert model["results"]["API RP 1111"]["propagation"]["case"] == "external demand"


@pytest.mark.parametrize("returned", [{}, {"burst": (float("nan"), {})}, {"burst": (-1, {})}])
def test_invalid_native_results_fail_before_writing(tmp_path, monkeypatch, returned):
    builder, source = builder_and_input()
    strategy = builder.CODE_REGISTRY[builder.CODES["API RP 2RD"]]
    monkeypatch.setattr(strategy, "run_checks", lambda *args: returned)
    with pytest.raises(ValueError, match="module result"):
        builder.build(source, tmp_path / "out")
    assert not (tmp_path / "out").exists()


def test_optional_deductions_are_zero_and_external_input_path_is_portable(tmp_path):
    builder, source = builder_and_input()
    config = yaml.safe_load(source.read_text())
    del config["pipe"]["corrosion_allowance_mm"]
    del config["pipe"]["fabrication_tolerance"]
    geometry, _ = builder._pipe_objects(config)
    assert geometry.corrosion_allowance == geometry.fabrication_tolerance == 0
    changed = tmp_path / "external-input.yml"
    changed.write_text(yaml.safe_dump(config))
    manifest = builder.build(changed, tmp_path / "out")
    assert "external-input.yml" in manifest["inputs"]
    assert not any(Path(name).is_absolute() for name in manifest["inputs"])
