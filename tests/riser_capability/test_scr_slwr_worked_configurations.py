"""Physical and reproducibility checks for the public worked configurations."""

import importlib.util
import json
import math
from pathlib import Path
import subprocess
import sys
import xml.etree.ElementTree as ET

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/riser_capability/build_scr_slwr_worked_configurations.py"
OUTPUT = ROOT / "docs/domains/risers/worked-configurations"
CASES = ["scr-1000m-8in", "scr-1500m-10in", "scr-2000m-12in",
         "slwr-1500m-8in", "slwr-2000m-10in", "slwr-2500m-12in"]


@pytest.fixture(scope="module")
def builder():
    assert SCRIPT.is_file(), "worked-configuration builder is missing"
    spec = importlib.util.spec_from_file_location("worked_builder", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def results(builder):
    return builder.build_results()


@pytest.mark.parametrize("case_id", CASES)
def test_yaml_weight_and_basis(case_id):
    path = ROOT / "examples/workflows/riser-worked-configurations" / f"{case_id}.yml"
    settings = yaml.safe_load(path.read_text())["riser_configuration_screen"]
    pipe, fluid = settings["pipe"], settings["weight_basis"]
    od, t = pipe["outer_diameter_m"], pipe["wall_thickness_m"]
    bore = math.pi * (od - 2 * t)**2 / 4
    displaced = math.pi * od**2 / 4
    expected = fluid["gravity_m_s2"] * (
        fluid["steel_density_kg_m3"] * (displaced - bore)
        + fluid["content_density_kg_m3"] * bore
        - fluid["seawater_density_kg_m3"] * displaced)
    assert settings["submerged_weight_n_m"] == pytest.approx(expected, rel=1e-6)
    assert settings["constraints"]["max_top_tension_n"] == pytest.approx(
        .6 * pipe["yield_strength_pa"] * (displaced - bore), rel=1e-12)
    assert settings["constraints"]["min_bend_radius_m"] == od / .004
    assert pipe["internal_pressure_pa"] == 20e6
    assert pipe["external_pressure_pa"] == 0
    assert settings["criteria_revision"] == "worked-config-1"
    assert settings["provisional"] is True
    assert settings["offsets_m"] == [-.05 * settings["water_depth_m"], 0.,
                                     .05 * settings["water_depth_m"],
                                     {"x_m": 0., "y_m": .05 * settings["water_depth_m"]}]
    assert "assumed" in path.read_text()


def test_scr_closed_form_against_screen(results):
    for case in results["cases"]:
        if case["inputs"]["riser_type"] != "SCR":
            continue
        row = case["offset_results"][case["zero_offset_index"]]
        theta = math.radians(case["configuration"]["hangoff_angle_deg"])
        w, z = case["inputs"]["submerged_weight_n_m"], case["inputs"]["water_depth_m"]
        h = w * z * math.sin(theta) / (1 - math.sin(theta))
        expected = {"horizontal_tension_n": h, "top_tension_n": h + w * z,
                    "suspended_length_m": math.sqrt(z*z + 2*h*z/w)}
        for name, value in expected.items():
            assert row[name] == pytest.approx(value, rel=1e-6)
            comparison = row["comparators"][name]
            assert comparison["comparator_value"] == pytest.approx(value, rel=1e-12)
            assert comparison["screen_value"] == pytest.approx(value, rel=1e-6)
            assert comparison["status"] == "pass"


def test_slwr_global_equilibrium_and_radii(results):
    for case in results["cases"]:
        if case["inputs"]["riser_type"] != "SLWR":
            continue
        w = case["inputs"]["submerged_weight_n_m"]
        factor = case["configuration"]["buoyancy_factor"]
        for row in case["offset_results"]:
            start, length = row["buoyed_start_m"], row["buoyed_length_m"]
            end_bare = row["suspended_length_m"] - start - length
            assert start > 0 and length > 0 and end_bare > 0
            vertical = w * start + w * (1-factor) * length + w * end_bare
            top = math.hypot(row["horizontal_tension_n"], vertical)
            assert row["top_tension_n"] == pytest.approx(top, rel=1e-6)
            assert row["comparators"]["top_tension_n"]["comparator_value"] == pytest.approx(top)
            assert row["comparators"]["sag_radius_m"]["comparator_value"] == pytest.approx(
                row["horizontal_tension_n"] / w)
            assert row["comparators"]["hog_radius_m"]["comparator_value"] == pytest.approx(
                row["horizontal_tension_n"] / abs(w*(1-factor)))


def test_constraint_values_margins_and_selection(results):
    for case in results["cases"]:
        assert case["rank"] == 1
        assert len(case["offset_results"]) == 4
        for row in case["offset_results"]:
            for check in row["criteria"].values():
                criterion = check["criterion"]
                assert math.isfinite(criterion["limit"])
                assert criterion["name"] and criterion["basis"]
                assert "computed" in check and check["status"]
                if check["status"] == "not_applicable":
                    assert check["reason"] == "SCR has no hog bend"
                    continue
                assert math.isfinite(check["computed"])
                sign = -1 if criterion["relation"] == "<=" else 1
                assert check["margin"] == pytest.approx(
                    sign * (check["computed"] - criterion["limit"]), abs=1e-12)
                assert check["status"] == ("pass" if check["margin"] >= 0 else "violation")
        governing = min(range(4), key=lambda i: min(
            c["margin"] / max(abs(c["criterion"]["limit"]), 1e-12)
            for c in case["offset_results"][i]["criteria"].values() if c["margin"] is not None))
        assert case["governing_offset_index"] == governing
        for name, envelope in case["constraint_envelope"].items():
            if envelope["margin"] is not None:
                assert envelope["margin"] == min(r["criteria"][name]["margin"]
                                                  for r in case["offset_results"])


def test_sensitivity_values_and_scr_signs(builder, results):
    stored = json.loads((OUTPUT / "scr-slwr-results.json").read_text())
    for case in stored["cases"]:
        steps = case["sensitivity"]
        base = next(row for row in steps if row["wall_step"] == "base")
        assert all("feasible" in row and "configuration" in row for row in steps)
        for row in steps:
            for name, value in row["metrics"].items():
                expected = 100 * (value / base["metrics"][name] - 1)
                assert row["change_percent"][name] == pytest.approx(expected, abs=5e-4)
        if case["inputs"]["riser_type"] == "SCR":
            thin, _, thick = steps
            assert thin["submerged_weight_n_m"] < base["submerged_weight_n_m"] < thick["submerged_weight_n_m"]
            assert thin["metrics"]["top_tension_kn"] < base["metrics"]["top_tension_kn"] < thick["metrics"]["top_tension_kn"]
    assert [c["case_id"] for c in results["cases"]] == CASES


def test_exact_reproduction_and_check_detects_tampering(builder, tmp_path):
    run = subprocess.run([sys.executable, str(SCRIPT), "--check"], cwd=ROOT,
                         text=True, capture_output=True)
    assert run.returncode == 0, run.stdout + run.stderr
    expected = {Path("sample.json"): '{"value": 1}\n'}
    path = tmp_path / "sample.json"
    path.write_text('{"value": 2}\n')
    assert builder.write_or_check(expected, tmp_path, check=True) == 1
    assert path.read_text() == '{"value": 2}\n'
    path.unlink()
    assert builder.write_or_check(expected, tmp_path, check=True) == 1


def test_report_and_svg_contract():
    text = (OUTPUT / "scr-slwr-worked-configurations.md").read_text()
    assert "## Reproduce appendix" in text
    assert "## Run-it-yourself prompt" in text
    assert "cleared to run" not in text.lower()
    assert "approved to run" not in text.lower()
    assert "pressure pair" in text
    for case_id in CASES:
        source = (OUTPUT / "figures" / f"{case_id}.svg").read_text()
        root = ET.fromstring(source)
        assert root.tag.endswith("svg")
        assert "currentColor" in source and "background" not in source
        assert "Sea surface" in source and "Seabed" in source
        assert "Hang-off" in source and "Touchdown" in source
        assert "Scale" in source
    data = json.loads((OUTPUT / "scr-slwr-results.json").read_text())
    assert "runtime_s" not in json.dumps(data)
    assert data["criteria_revision"] == "worked-config-1"
