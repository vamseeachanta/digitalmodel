"""Physical and reproducibility checks for the public worked configurations."""

import importlib.util
from copy import deepcopy
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
STRESS_RELATIVE_TOLERANCE = 1e-6


def yaml_settings(case_id):
    path = ROOT / "examples/workflows/riser-worked-configurations" / f"{case_id}.yml"
    return yaml.safe_load(path.read_text())["riser_configuration_screen"]


def section_stresses(settings, configuration, row):
    """Independent test calculation from pipe inputs and exposed H, T and w."""
    pipe = settings["pipe"]
    od, t = pipe["outer_diameter_m"], pipe["wall_thickness_m"]
    inner = od - 2*t
    steel = math.pi * (od**2 - inner**2) / 4
    hoop = (pipe["internal_pressure_pa"] - pipe["external_pressure_pa"]) * od / (2*t)
    pressure = math.pi / 4 * (pipe["internal_pressure_pa"]*inner**2 - pipe["external_pressure_pa"]*od**2)
    h, top = row["horizontal_tension_n"], row["top_tension_n"]
    w = settings["submerged_weight_n_m"]
    sections = [("hang-off", top, w*h/top**2), ("touchdown", h, w/h)]
    if settings["riser_type"] == "SLWR":
        sections += [("sag bend", h, w/h),
                     ("hog bend", h, abs(w*(1-configuration["buoyancy_factor"]))/h)]
    stresses = {}
    for name, tension, curvature in sections:
        axial = [(tension + pressure)/steel + sign*pipe["youngs_modulus_pa"]*od*curvature/2
                 for sign in (-1, 1)]
        vm = [math.sqrt(a*a + hoop*hoop - a*hoop) for a in axial]
        stresses[name] = {"axial_pa": axial, "von_mises_pa": vm,
                          "utilisation": max(vm)/pipe["yield_strength_pa"]}
    return stresses


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


@pytest.fixture(scope="module")
def renderer():
    spec = importlib.util.spec_from_file_location("worked_report", SCRIPT.with_name("worked_configuration_report.py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


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


@pytest.mark.parametrize("case_id", CASES)
def test_independent_stress_for_every_offset(case_id, results):
    case = next(c for c in results["cases"] if c["case_id"] == case_id)
    settings = yaml_settings(case_id)
    assert len(case["offset_results"]) == 4
    for row in case["offset_results"]:
        stresses = section_stresses(settings, case["configuration"], row)
        governing = max(stresses, key=lambda name: stresses[name]["utilisation"])
        expected = stresses[governing]["utilisation"]
        assert row["static_von_mises_utilisation"] == pytest.approx(expected, rel=STRESS_RELATIVE_TOLERANCE)
        comp = row["comparators"]["static_von_mises_utilisation"]
        assert comp["comparator_value"] == pytest.approx(expected, rel=1e-12)
        assert comp["screen_value"] == row["static_von_mises_utilisation"]
        assert comp["governing_critical_section"] == governing
        assert comp["relative_tolerance"] == STRESS_RELATIVE_TOLERANCE
        assert comp["difference_percent"] == pytest.approx(
            100*(row["static_von_mises_utilisation"] - expected)/expected, abs=1e-12)
        assert comp["status"] == "pass"


def test_scr_closed_form_against_screen(results):
    for case in results["cases"]:
        if case["inputs"]["riser_type"] != "SCR":
            continue
        w, z = case["inputs"]["submerged_weight_n_m"], case["inputs"]["water_depth_m"]
        for row in case["offset_results"]:
            angle = case["configuration"]["hangoff_angle_deg"] if row["offset_m"] == 0 else row["top_angle_deg"]
            theta = math.radians(angle)
            h = w * z * math.sin(theta) / (1 - math.sin(theta))
            length = math.sqrt(z*z + 2*h*z/w)
            expected = {"horizontal_tension_n": h, "top_tension_n": h + w*z,
                        "suspended_length_m": length, "touchdown_radius_m": h/w,
                        "horizontal_span_m": h/w * math.asinh(length/(h/w))}
            assert 1/row["touchdown_curvature_per_m"] == pytest.approx(h/w, rel=1e-6)
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
            expected = {"top_tension_n": top, "sag_radius_m": row["horizontal_tension_n"]/w,
                        "hog_radius_m": row["horizontal_tension_n"]/abs(w*(1-factor))}
            for name, reference in expected.items():
                assert row[name] == pytest.approx(reference, rel=1e-6)
                comp = row["comparators"][name]
                assert comp["comparator_value"] == pytest.approx(reference, rel=1e-12)
                assert comp["screen_value"] == pytest.approx(reference, rel=1e-6)
                assert comp["status"] == "pass"


def test_scr_zero_offset_hand_worked_stress(results):
    """SI units: base 8 in case, 20 deg angle, w=726.2264466469273 N/m."""
    case = results["cases"][0]
    row = case["offset_results"][case["zero_offset_index"]]
    settings = yaml_settings("scr-1000m-8in")
    pipe = settings["pipe"]
    od, t = pipe["outer_diameter_m"], pipe["wall_thickness_m"]
    bore = math.pi*(od-2*t)**2/4
    displaced = math.pi*od**2/4
    steel = displaced-bore
    assert bore == pytest.approx(0.02617271631154961, rel=1e-12)  # m²
    assert displaced == pytest.approx(0.03769428606223729, rel=1e-12)  # m²
    assert steel == pytest.approx(0.01152156975068768, rel=1e-12)  # m²
    assert 20e6*bore == pytest.approx(523454.3262309922, rel=1e-12)  # pressure end force, N
    assert 20e6*od/(2*t) == pytest.approx(119955648.03153917, rel=1e-12)  # hoop, Pa
    assert row["horizontal_tension_n"] == pytest.approx(377494.9504146719, rel=1e-12)
    assert row["top_tension_n"] == pytest.approx(1103721.3970615994, rel=1e-12)
    stresses = section_stresses(settings, case["configuration"], row)
    assert stresses["hang-off"]["axial_pa"] == pytest.approx([136125978.7412099, 146331318.05126432], rel=1e-12)
    assert stresses["hang-off"]["von_mises_pa"] == pytest.approx([128804346.15761022, 135088652.56985173], rel=1e-12)
    assert stresses["touchdown"]["axial_pa"] == pytest.approx([34575901.189004965, 121817593.12489092], rel=1e-12)
    assert stresses["touchdown"]["von_mises_pa"] == pytest.approx([106945200.00587586, 120897374.5143761], rel=1e-12)
    assert stresses["touchdown"]["utilisation"] == pytest.approx(0.2698602109695895, rel=1e-12)
    assert stresses["hang-off"]["utilisation"] == pytest.approx(0.3015371709148476, rel=1e-12)
    assert row["static_von_mises_utilisation"] == pytest.approx(0.3015371709148476, rel=STRESS_RELATIVE_TOLERANCE)


@pytest.mark.parametrize("kind,factor,governing", [("SCR", None, "touchdown"), ("SLWR", 4., "hog bend")])
def test_stress_comparator_selects_bend_sections(builder, kind, factor, governing):
    settings = yaml_settings("scr-1000m-8in")
    settings["riser_type"] = kind
    row = {"horizontal_tension_n": 10000., "top_tension_n": 100000.}
    configuration = {} if factor is None else {"buoyancy_factor": factor}
    sections = section_stresses(settings, configuration, row)
    comp = builder.stress_comparator(settings["pipe"], settings["submerged_weight_n_m"],
                                     row["horizontal_tension_n"], row["top_tension_n"], factor)
    assert comp["governing_critical_section"] == governing
    assert comp["critical_section_utilisations"] == pytest.approx(
        {name: values["utilisation"] for name, values in sections.items()}, rel=1e-12)
    assert comp["comparator_value"] == pytest.approx(sections[governing]["utilisation"], rel=1e-12)


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


def test_independent_stress_for_all_sensitivity_runs(results):
    count = 0
    for case in results["cases"]:
        for step in case["sensitivity"]:
            settings = yaml_settings(case["case_id"])
            settings["pipe"]["wall_thickness_m"] = step["wall_thickness_m"]
            pipe, fluid = settings["pipe"], settings["weight_basis"]
            bore = math.pi*(pipe["outer_diameter_m"]-2*pipe["wall_thickness_m"])**2/4
            displaced = math.pi*pipe["outer_diameter_m"]**2/4
            weight = fluid["gravity_m_s2"] * (fluid["steel_density_kg_m3"]*(displaced-bore)
                     + fluid["content_density_kg_m3"]*bore - fluid["seawater_density_kg_m3"]*displaced)
            settings["submerged_weight_n_m"] = weight
            assert step["submerged_weight_n_m"] == pytest.approx(weight, rel=1e-12)
            assert len(step["stress_comparisons"]) == 4
            for row in step["stress_comparisons"]:
                sections = section_stresses(settings, step["configuration"], row)
                expected = max(v["utilisation"] for v in sections.values())
                assert row["static_von_mises_utilisation"] == pytest.approx(expected, rel=STRESS_RELATIVE_TOLERANCE)
                comp = row["comparison"]
                assert comp["comparator_value"] == pytest.approx(expected, rel=1e-12)
                assert comp["screen_value"] == row["static_von_mises_utilisation"]
                assert comp["relative_tolerance"] == STRESS_RELATIVE_TOLERANCE
                assert comp["status"] == "pass"
                count += 1
    assert count == 72


def test_report_stress_scope_and_metric_labels(renderer, results):
    text = renderer.render_report(results)
    assert "same thin-wall formulation" in text
    assert "independent implementation and independent section selection" in text
    assert "thick-wall or FE verification" in text
    assert "1e-6 relative" in text
    assert "72 offset rows" in text and "18 sensitivity runs" in text
    assert "an assumed screening ratio of 0.67 (two thirds rounded to two decimals)" in text
    assert "2/3" not in text
    assert "Top tension maximum over offsets (kN)" in text
    assert "Utilisation maximum over offsets (1)" in text
    assert "Radius minimum over offsets (m)" in text
    assert "not established rows: none" in text
    for case in results["cases"]:
        assert renderer.result_tables(case).count("| static_von_mises_utilisation |") == 5  # criterion + four comparator rows


def test_stress_disagreement_is_not_established(builder, renderer, results):
    data = deepcopy(results)
    case = data["cases"][0]
    row = dict(case["offset_results"][0])
    row["static_von_mises_utilisation"] *= .5
    row = builder.enrich_row(case["inputs"], case["configuration"], row)
    comp = row["comparators"]["static_von_mises_utilisation"]
    assert comp["status"] == "not established"
    assert "dense or continuous-section stress-envelope verification" in comp["missing_evidence"]
    case["offset_results"][0] = row
    step = case["sensitivity"][1]
    step["stress_comparisons"][0]["comparison"] = comp
    data["stress_comparator_evidence"] = builder.stress_evidence(data["cases"])
    text = renderer.render_report(data)
    assert "dense or continuous-section stress-envelope verification" in text
    assert "scr-1000m-8in, base, -50 m: not established" in text


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
