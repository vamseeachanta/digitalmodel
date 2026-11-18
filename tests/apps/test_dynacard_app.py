"""Browser arithmetic compared with the unchanged Python workflow."""

import copy
import importlib.util
import json
from pathlib import Path
import re
import shutil
import subprocess
import sys

import numpy as np
import pytest

# Required envelope, fixed before implementation; float64 ports should be far
# below it. Arrays use their reference range; scalar scale is max(abs(ref), 1).
RANGE_REL_TOL = 1e-6
SCALAR_REL_TOL = 1e-6
PROB_ABS_TOL = 1e-6
# Saved results round to nine significant digits; allow their serialization error.
STORED_REL_TOL = 1e-8
ROOT = Path(__file__).resolve().parents[2]
APP = ROOT / "docs/apps/dynacard"
NODE = shutil.which("node")
NODE_REQUIRED = pytest.mark.skipif(NODE is None, reason="node is not on PATH")


def module(name):
    spec = importlib.util.spec_from_file_location(name, APP / f"{name}.py")
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


def run_node(requests, tmp_path):
    request = tmp_path / "request.json"
    request.write_text(json.dumps(requests), encoding="utf-8")
    process = subprocess.run(
        [NODE, str(APP / "test/run_core.cjs"), str(request)],
        capture_output=True, text=True, encoding="utf-8", check=True,
        creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0),
    )
    return json.loads(process.stdout)


@pytest.fixture(scope="module")
def assets():
    return module("build_assets").build_examples()


@NODE_REQUIRED
def test_ten_examples_match_python(assets, tmp_path):
    reference = module("reference")
    requests = [{"card": e["card"], "well": assets["well"]}
                for e in assets["examples"]]
    assert len(requests) == 10
    for request, actual in zip(requests, run_node(requests, tmp_path), strict=True):
        expected = reference.python_analysis(request)
        assert_parity(actual, expected)


@pytest.fixture(scope="module")
def saved_replays(assets, tmp_path_factory):
    requests = [{"card": e["card"], "well": assets["well"]} for e in assets["examples"]]
    saved = json.loads((ROOT / "docs/domains/artificial_lift/dynacard-examples/results.json").read_text(encoding="utf-8"))
    return list(zip(run_node(requests, tmp_path_factory.mktemp("saved-replay")), saved["examples"], strict=True))


@NODE_REQUIRED
@pytest.mark.parametrize("index", range(10), ids=lambda i: f"example-{i + 1}")
def test_saved_example_results(index, saved_replays):
    actual, record = saved_replays[index]
    for axis in ("position", "load"):
        want = np.asarray(record["computed_pump_card"][axis])
        # Preserve the range-based pump-card envelope, plus saved rounding.
        np.testing.assert_allclose(actual["downhole_card"][axis], want,
            rtol=STORED_REL_TOL, atol=RANGE_REL_TOL * np.ptp(want))
    for actual_key, stored_key in (("diagnosis", "shipped"), ("shape_diagnosis", "shape_only")):
        assert actual[actual_key]["classification"] == record["diagnosis"][stored_key]["classification"]
    for group, mappings in {
        "fillage": {"gross_stroke": "pump_gross_stroke_in", "net_stroke": "pump_net_stroke_in", "fillage": "fillage_pct"},
        "fluid_load": {"fluid_load": "fluid_load_lbf", "upstroke_load": "upstroke_mean_load_lbf", "downstroke_load": "downstroke_mean_load_lbf"},
    }.items():
        for key, stored_key in mappings.items():
            want = record["analysis"][stored_key]
            assert actual[group][key] == pytest.approx(want, rel=STORED_REL_TOL, abs=1e-6), stored_key


def assert_parity(actual, expected):
    assert "error" not in actual, actual
    for axis in ("position", "load"):
        want = np.asarray(expected["downhole_card"][axis])
        got = np.asarray(actual["downhole_card"][axis])
        assert got.shape == want.shape
        assert np.isfinite(got).all()
        assert np.max(np.abs(got - want)) <= RANGE_REL_TOL * np.ptp(want)
    for path, value in module("reference").scalar_values(expected).items():
        cursor = actual
        for key in path.split("."):
            cursor = cursor[key]
        assert np.isfinite(cursor), path
        assert abs(cursor - value) <= SCALAR_REL_TOL * max(abs(value), 1), path
    for key in ("corners", "buckling_detected", "solver_method", "solver_flags",
                "max_inclination_deg"):
        assert actual[key] == expected[key], key
    assert actual["fillage"]["corners"] == expected["fillage"]["corners"]
    assert actual["production"]["runtime_source"] == expected["production"]["runtime_source"]
    for key in ("density_source", "pressure_status", "warnings"):
        assert actual["cpip"][key] == expected["cpip"][key], f"cpip.{key}"
    for key in ("diagnosis", "shape_diagnosis"):
        assert actual[key]["classification"] == expected[key]["classification"]
        assert actual[key]["confidence"] == expected[key]["confidence"]
        assert actual[key]["differential"] == expected[key]["differential"]
        assert actual[key]["probabilities"].keys() == expected[key]["probabilities"].keys()
        for label, probability in expected[key]["probabilities"].items():
            assert abs(actual[key]["probabilities"][label] - probability) <= PROB_ABS_TOL


@NODE_REQUIRED
def test_five_field_fixtures(assets, tmp_path):
    reference = module("reference")
    cases = reference.field_requests()
    assert len(cases) == 5
    actual = run_node([case["request"] for case in cases], tmp_path)
    for case, result in zip(cases, actual, strict=True):
        if case["rejection"]:
            assert re.search(case["rejection"], result.get("error", ""), re.I), case["name"]
        else:
            assert_parity(result, reference.python_analysis(case["request"]))


@NODE_REQUIRED
@pytest.mark.parametrize("header", ["position_in,load_lbf",
    "surface_position_in,surface_load_lbf"])
def test_csv_headers_comments_and_units(header, tmp_path):
    text = "# inches and lbf\n" + header + "\n0,10\n1,20\n2,20\n1,10\n"
    result = run_node({"operation": "parse", "text": text}, tmp_path)
    # Four unique phases; the first row is repeated to close the cycle.
    assert result == {"position": [0, 1, 2, 1, 0], "load": [10, 20, 20, 10, 10]}


@NODE_REQUIRED
def test_worked_example_columns_and_template(assets, tmp_path):
    text = "sample,surface_load_lbf,surface_position_in,pump_load_lbf\n0,10,0,1\n1,20,1,2\n2,20,2,2\n3,10,1,1\n"
    assert run_node({"operation": "parse", "text": text}, tmp_path) == {
        "position": [0, 1, 2, 1, 0], "load": [10, 20, 20, 10, 10]}
    template = (APP / "template.csv").read_text(encoding="utf-8")
    assert run_node({"operation": "parse", "text": template}, tmp_path) == assets["examples"][0]["card"]


def card_csv(position, load):
    return "position_in,load_lbf\n" + "".join(
        f"{p!r},{f!r}\n" for p, f in zip(position, load, strict=True))


@NODE_REQUIRED
@pytest.mark.parametrize("index", [0, 1, 3])
def test_open_csv_cycle_matches_its_closed_form(index, assets, tmp_path):
    """An open file and the same cycle with a declared closing row agree."""
    card, well = assets["examples"][index]["card"], assets["well"]
    assert card["position"][0] == card["position"][-1] and card["load"][0] == card["load"][-1]
    closed_text = card_csv(card["position"], card["load"])
    open_text = card_csv(card["position"][:-1], card["load"][:-1])
    closed, opened, undeclared = run_node([
        {"operation": "parse", "text": closed_text, "options": {"closingRow": True}},
        {"operation": "parse", "text": open_text},
        {"operation": "parse", "text": closed_text}], tmp_path)
    assert closed == card
    assert opened == card
    # Undeclared, the repeated last row is a timed sample and the first row
    # is appended again; nothing is dropped.
    assert undeclared == {"position": card["position"] + card["position"][:1],
                          "load": card["load"] + card["load"][:1]}
    results = run_node([{"card": closed, "well": well}, {"card": opened, "well": well}], tmp_path)
    assert results[0]["downhole_card"] == results[1]["downhole_card"]
    assert results[0]["fillage"]["fillage"] == results[1]["fillage"]["fillage"]
    assert results[0]["shape_diagnosis"] == results[1]["shape_diagnosis"]
    saved = json.loads((ROOT / "docs/domains/artificial_lift/dynacard-examples/results.json").read_text(encoding="utf-8"))
    want = saved["examples"][index]["analysis"]["fillage_pct"]
    assert results[1]["fillage"]["fillage"] == pytest.approx(want, rel=STORED_REL_TOL, abs=1e-6)


@NODE_REQUIRED
@pytest.mark.parametrize("body", [
    "0,1\n1,2\n2,no\n1,2", "0,1\n1,2\n2,Infinity\n1,2",
    "0,1\n1,2\n2,NaN\n1,2", "0,1\n1,2", "0,1\n0,2\n0,3\n0,2",
    "0,1\n1,2\n2\n1,2", "0,1\n1,2\n2,\n1,2",
])
def test_csv_rejects_invalid_samples(body, tmp_path):
    result = run_node({"operation": "parse", "text": "position_in,load_lbf\n" + body}, tmp_path)
    assert result.get("error")


@NODE_REQUIRED
@pytest.mark.parametrize("text", [
    "position,load\n0,1\n1,2\n2,2\n1,1",
    "position_in,load_lbf,load_lbf\n0,1,1\n1,2,2\n2,2,2\n1,1,1",
    "position_in,load_lbf\n0,1\n1,2\n2,1e999\n1,1",
    "position_in,load_lbf,aux\n0,1,0\n1,2,text\n2,2,2\n1,1,1",
    "position_in,load_lbf\n0,1\n1,2,3\n2,2\n1,1",
])
def test_csv_rejects_headers_overflow_and_auxiliary_cells(text, tmp_path):
    assert run_node({"operation": "parse", "text": text}, tmp_path).get("error")


@NODE_REQUIRED
@pytest.mark.parametrize("change", ["spm", "depth", "diameter", "density",
    "rod_length", "rod_modulus", "rod_weight", "empty_rods", "mismatch",
    "nonfinite", "zero_stroke", "few_samples", "deviation", "nonsteel", "unanchored", "zero_tubing", "anchor_string",
    "custom_nodes_zero", "malformed_survey", "null_friction"])
def test_validation_errors(change, assets, tmp_path):
    request = copy.deepcopy({"card": assets["examples"][0]["card"], "well": assets["well"]})
    well, card = request["well"], request["card"]
    changes = {
        "spm": lambda: well.update(spm=0),
        "depth": lambda: well["pump"].update(depth=-1),
        "diameter": lambda: well["pump"].update(diameter=0),
        "density": lambda: well.update(fluid_density=-1),
        "rod_length": lambda: well["rod_string"][0].update(length=0),
        "rod_modulus": lambda: well["rod_string"][0].update(modulus_of_elasticity=0),
        "rod_weight": lambda: well["rod_string"][0].update(weight_per_foot=-1),
        "empty_rods": lambda: well.update(rod_string=[]),
        "mismatch": lambda: card["load"].pop(),
        "nonfinite": lambda: card["load"].__setitem__(0, None),
        "zero_stroke": lambda: card.update(position=[0] * len(card["load"])),
        "few_samples": lambda: card.update(position=[0, 1, 0], load=[1, 2, 1]),
        "deviation": lambda: well.update(survey={"measured_depth": [0, 5000], "inclination": [0, 20], "azimuth": [0, 0]}),
        "nonsteel": lambda: well["rod_string"][0].update(material="fiberglass"),
        "unanchored": lambda: well.update(tubing_anchored=False),
        "zero_tubing": lambda: well["input_params"].update(tubing_pressure=0),
        "anchor_string": lambda: well.update(tubing_anchored="false"),
        "custom_nodes_zero": lambda: well.update(n_nodes=0),
        "malformed_survey": lambda: well.update(survey={"measured_depth": [0], "inclination": [0, 0], "azimuth": [0, 0]}),
        "null_friction": lambda: well.update(calc_params={"coulomb_friction_coefficient": None}),
    }
    changes[change]()
    assert run_node(request, tmp_path).get("error"), change


def test_assets_are_reproducible():
    result = subprocess.run([sys.executable, str(APP / "build_assets.py"), "--check"],
        capture_output=True, text=True, encoding="utf-8",
        creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
    assert result.returncode == 0, result.stdout + result.stderr


# Model asset content, size and loading: test_dynacard_app_assets.py.


@NODE_REQUIRED
def test_work_values_use_the_matching_cards(assets, tmp_path):
    from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
        CardData, DynacardAnalysisContext,
    )
    from digitalmodel.marine_ops.artificial_lift.dynacard.power_consumption import (
        PowerConsumptionCalculator,
    )
    requests = [{"card": e["card"], "well": assets["well"]} for e in assets["examples"]]
    for request, actual in zip(requests, run_node(requests, tmp_path), strict=True):
        expected = module("reference").python_analysis(request)
        ctx = DynacardAnalysisContext(**request["well"], surface_card=request["card"])
        area = PowerConsumptionCalculator(ctx)._calculate_card_area
        for key, card in (("polished_rod_work_per_stroke", ctx.surface_card),
                          ("pump_work_per_stroke", CardData(**expected["downhole_card"]))):
            assert expected[key] == pytest.approx(area(card), rel=SCALAR_REL_TOL)
            assert actual[key] == pytest.approx(area(card), rel=SCALAR_REL_TOL)
        assert actual["pump_work_per_stroke"] != actual["polished_rod_work_per_stroke"]


def test_example_crosswalk_uses_source_mapping(assets):
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostic_plate import _LOAD_CLASSES
    for example in assets["examples"]:
        expected = "literal" if _LOAD_CLASSES[example["source_label"]] else "ASSUMED"
        assert example["crosswalk_status"] == expected


def test_examples_use_saved_surface_cards_and_provenance(assets):
    document = json.loads((ROOT / "docs/domains/artificial_lift/dynacard-examples/results.json").read_text(encoding="utf-8"))
    assert assets["well"] == document["basis"]["well"]["value"]
    for example, record in zip(assets["examples"], document["examples"], strict=True):
        assert example["card"] == record["surface_card"]
        for key in ("index", "slug", "english_gloss", "source_label", "crosswalk_mode",
                    "crosswalk_kind", "source_doi", "dataset_url", "sheet_name", "licence", "retrieval_date"):
            assert example[key] == record[key]


def test_readme_crosslinks_resolve(assets):
    path = ROOT / "docs/domains/artificial_lift/dynacard-examples/README.md"
    text = path.read_text(encoding="utf-8")
    for name in ("index.html", "README.md"):
        link = f"../../../apps/dynacard/{name}"
        assert f"]({link})" in text
        assert (path.parent / link).resolve() == APP / name
        assert (path.parent / link).is_file()
    for example in assets["examples"]:
        assert f"## Example {example['index']}: {example['english_gloss']}\n" in text


@NODE_REQUIRED
def test_display_formatting_in_node():
    process = subprocess.run([NODE, str(APP / "test/presentation.cjs")],
        capture_output=True, text=True, encoding="utf-8",
        creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
    assert process.returncode == 0, process.stdout + process.stderr


def test_offline_document_and_scripts():
    html = (APP / "index.html").read_text(encoding="utf-8")
    required = "default-src 'none'; script-src 'self'; style-src 'self'; img-src 'self' data: blob:; connect-src 'none'; form-action 'none'; base-uri 'none'"
    assert 'http-equiv="Content-Security-Policy"' in html
    assert required in html
    for tag in re.findall(r"<[^>]+>", html):
        if not re.match(r"<a\s", tag, re.I):
            assert not re.search(r'(?:src|href)\s*=\s*[\"\']https?://', tag, re.I)
    assert 'type="module"' not in html
    scripts = list(APP.glob("*.js"))
    assert scripts
    for path in scripts:
        assert not re.search(r"fetch\s*\(|XMLHttpRequest|WebSocket|navigator\.sendBeacon|localStorage",
                             path.read_text(encoding="utf-8")), path.name


def test_reproduction_table_meets_envelope():
    document = json.loads((APP / "reproduction.json").read_text(encoding="utf-8"))
    assert len(document["examples"]) == 10
    for row in document["examples"]:
        assert 0 <= row["max_pump_load_difference_lbf"] <= RANGE_REL_TOL * row["pump_load_range_lbf"]
        assert 0 <= row["max_pump_position_difference_in"] <= RANGE_REL_TOL * row["pump_position_range_in"]
        assert 0 <= row["max_scalar_relative_difference"] <= SCALAR_REL_TOL
        assert row["labels_equal"]
        assert 0 <= row["max_probability_difference"] <= PROB_ABS_TOL
        for key in ("polished_rod", "pump"):
            assert 0 <= row[f"{key}_work_difference_ft_lbf"] <= 1e-6
    readme = (APP / "README.md").read_text(encoding="utf-8")
    table = readme.split("<!-- reproduction:start -->")[1].split("<!-- reproduction:end -->")[0]
    rows = [line for line in table.splitlines() if line.startswith("|")][2:]
    assert len(rows) == 10
    for line, evidence in zip(rows, document["examples"], strict=True):
        cells = [cell.strip() for cell in line.strip("|").split("|")]
        assert cells[0] == evidence["example"]
        assert 0 <= float(cells[1]) <= RANGE_REL_TOL * evidence["pump_load_range_lbf"]
        assert 0 <= float(cells[2]) <= RANGE_REL_TOL * evidence["pump_position_range_in"]
        assert 0 <= float(cells[3]) <= SCALAR_REL_TOL
        assert cells[4] == "yes"
        assert 0 <= float(cells[5]) <= PROB_ABS_TOL
        assert float(cells[6]) == pytest.approx(evidence["polished_rod_work_difference_ft_lbf"])
        assert float(cells[7]) == pytest.approx(evidence["pump_work_difference_ft_lbf"])


@NODE_REQUIRED
def test_finite_inputs_cannot_publish_overflowed_results(assets, tmp_path):
    request = copy.deepcopy({"card": assets["examples"][0]["card"], "well": assets["well"]})
    request["well"]["well_test"] = {"oil_rate": 1e308, "water_rate": 1e308}
    result = run_node(request, tmp_path)
    assert re.search("non-finite|overflow", result.get("error", ""), re.I)


def smooth_card(intervals):
    """Smooth 40 in, 8,650-12,350 lbf closed cycle with the given intervals."""
    phase = 2 * np.pi * np.arange(intervals + 1) / intervals
    return {"position": (20 * (1 - np.cos(phase))).tolist(),
            "load": (10500 + 1850 * np.sin(phase)).tolist()}


def courant_limit(well):
    """Largest stable intervals per cycle, dt >= dx / c in every taper.

    Computed here from the well in oilfield units, independently of the
    JavaScript: c = sqrt(E / rho), rho from the rod weight per foot.
    """
    rods = well["rod_string"]
    length_m = sum(rod["length"] for rod in rods) * 0.3048
    dx = length_m / 199
    speeds = []
    for rod in rods:
        area_in2 = np.pi * rod["diameter"] ** 2 / 4
        # A zero or missing weight per foot means plain steel, 490 lb/ft^3.
        weight = rod.get("weight_per_foot") or area_in2 * 490 / 144
        rho = weight * 144 / area_in2 * 16.0185  # kg/m^3
        speeds.append(np.sqrt(rod["modulus_of_elasticity"] * 6894.7572931783 / rho))
    return int(np.floor((60 / well["spm"]) * min(speeds) / dx))


@NODE_REQUIRED
def test_sampling_limit_rejects_unstable_march(assets, tmp_path):
    """Both sides of the Courant limit; 6,400 intervals once gave 2.29e8 lbf."""
    well = assets["well"]
    limit = courant_limit(well)
    assert limit == 6377
    coarse, at_limit, above, review_case = run_node(
        [{"card": smooth_card(n), "well": well} for n in (1000, limit, limit + 1, 6400)], tmp_path)
    assert "error" not in at_limit, at_limit
    assert at_limit["pump_card_max_load"] == pytest.approx(coarse["pump_card_max_load"], rel=1e-4)
    assert at_limit["pump_card_min_load"] == pytest.approx(coarse["pump_card_min_load"], rel=1e-4)
    assert at_limit["cpip"]["pump_intake_pressure"] == pytest.approx(
        coarse["cpip"]["pump_intake_pressure"], abs=2.0)
    for rejected in (above, review_case):
        message = rejected.get("error", "")
        assert "Courant number" in message and "unstable" in message, rejected
        assert f"at most {limit} intervals per cycle" in message


@NODE_REQUIRED
def test_examples_and_fixtures_are_inside_the_sampling_limit(assets):
    reference = module("reference")
    for case in reference.field_requests():
        well = case["request"]["well"]
        intervals = len(case["request"]["card"]["position"]) - 1
        if not case["rejection"]:
            assert intervals <= courant_limit(well), case["name"]
    for example in assets["examples"]:
        assert len(example["card"]["position"]) - 1 <= courant_limit(assets["well"])


def rotated_open_cycle(card, shift):
    position, load = card["position"][:-1], card["load"][:-1]
    shift %= len(position)
    return position[shift:] + position[:shift], load[shift:] + load[:shift]


@NODE_REQUIRED
def test_uploaded_start_phase_is_repeatable(assets, tmp_path):
    """Every start phase of one cycle gives the bottom-of-stroke card."""
    card, well = assets["examples"][8]["card"], assets["well"]
    shifts = (0, 3, 40, 77)
    uploads = run_node([{"operation": "upload", "text": card_csv(*rotated_open_cycle(card, s))}
                        for s in shifts], tmp_path)
    started = uploads[0]["card"]
    assert started["position"][0] == min(card["position"])
    for shift, upload in zip(shifts, uploads, strict=True):
        assert upload["card"] == started, shift
        assert upload["rotation"] == (3 - shift) % (len(card["position"]) - 1)
    raw77 = dict(zip(("position", "load"), rotated_open_cycle(card, 77), strict=True))
    raw77 = {key: value + value[:1] for key, value in raw77.items()}
    stored, bottom, unrotated = run_node(
        [{"card": c, "well": well} for c in (card, started, raw77)], tmp_path)
    # The sensitivity quoted on the page and in the README.
    assert round(stored["cpip"]["pump_intake_pressure"], 1) == 781.9
    assert round(bottom["cpip"]["pump_intake_pressure"], 1) == 797.5
    assert round(unrotated["cpip"]["pump_intake_pressure"], 1) == 45.4


@NODE_REQUIRED
def test_start_phase_ties_choose_one_rotation(tmp_path):
    """A flat bottom dwell starts at the dwell's first sample from any origin."""
    position = [0, 0, 0, 1, 2, 3, 2, 1]
    load = [5, 5.5, 6, 8, 9, 9.5, 7, 6.2]  # unique rows: no row reads as a closing row
    uploads = run_node([{"operation": "upload", "text": card_csv(position[s:] + position[:s],
                                                                 load[s:] + load[:s])}
                        for s in range(len(position))], tmp_path)
    for upload in uploads:
        assert upload["card"] == {"position": position + [0], "load": load + [5]}


@NODE_REQUIRED
def test_export_keeps_assumed_runtime_provenance(assets, tmp_path):
    request = {"card": assets["examples"][0]["card"], "well": assets["well"]}
    analysed, upload, example = run_node([
        request,
        {"operation": "export", **request, "input": {"source": "upload", "rotation": 92}},
        {"operation": "export", **request, "input": {"source": "example"}}], tmp_path)
    # The reference labels the page's 24 h input_params runtime "card".
    assert analysed["production"]["runtime_source"] == "card"
    for record in (upload, example):
        assert record["production"]["runtime_source"] == "assumed_24h"
        assert record["page_inputs"]["runtime"] == {
            "hours_per_day": 24, "source": "assumed_24h",
            "basis": "Not read from the card or a well test; this page supplies 24 h/day.",
            "reference_runtime_source": "card"}
        assert record["downhole_card"] == analysed["downhole_card"]
    assert upload["page_inputs"]["card_start"]["origin"] == "stroke_bottom"
    assert upload["page_inputs"]["card_start"]["rotation_samples"] == 92
    assert example["page_inputs"]["card_start"]["origin"] == "stored"


@NODE_REQUIRED
@pytest.mark.parametrize("well_test, source", [
    (None, "assumed_50pct_mixture"),
    ({"oil_rate": 40.0, "water_rate": 60.0, "water_cut": 60.0}, "well_test_mixture"),
    ({"oil_rate": 40.0, "water_rate": 0.0}, "well_test_mixture"),
], ids=["no-well-test", "well-test-water-cut", "well-test-no-water-cut"])
def test_omitted_density_branches_match_python(well_test, source, assets, tmp_path):
    """CPIP density basis without a supplied fluid density."""
    well = copy.deepcopy(assets["well"])
    del well["fluid_density"]
    if well_test is not None:
        well["well_test"] = well_test
    request = {"card": assets["examples"][0]["card"], "well": well}
    actual = run_node(request, tmp_path)
    expected = module("reference").python_analysis(request)
    assert expected["cpip"]["density_source"] == source
    assert_parity(actual, expected)


def dwell_pair_rows(card):
    """Example cycle with its bottom-of-stroke reading repeated once (a dwell)."""
    position, load = card["position"][:-1], card["load"][:-1]
    bottom = position.index(min(position))
    position = position[:bottom + 1] + position[bottom:]
    load = load[:bottom + 1] + load[bottom:]
    return position, load, bottom


@NODE_REQUIRED
def test_dwell_pair_across_file_boundary_keeps_both_samples(assets, tmp_path):
    """Identical readings straddling the file boundary are not a closing row."""
    card, well = assets["examples"][8]["card"], assets["well"]
    position, load, bottom = dwell_pair_rows(card)
    assert len(position) == 100 and position[bottom] == position[bottom + 1]
    inside = card_csv(position, load)
    # Start at the second reading of the pair: the first reading is now the
    # file's last row and equals its first row.
    split = bottom + 1
    straddle = card_csv(position[split:] + position[:split], load[split:] + load[:split])
    assert straddle.splitlines()[1] == straddle.splitlines()[-1]
    a, b, declared = run_node([
        {"operation": "upload", "text": inside},
        {"operation": "upload", "text": straddle},
        {"operation": "upload", "text": straddle, "options": {"closingRow": True}}], tmp_path)
    assert a["card"] == b["card"]
    assert len(a["card"]["position"]) == 101  # 100 timed samples plus the closing row
    assert len(declared["card"]["position"]) == 100  # the declared reading: 99 samples
    results = run_node([{"card": c["card"], "well": well} for c in (a, b, declared)], tmp_path)
    assert results[0]["downhole_card"] == results[1]["downhole_card"]
    assert round(results[0]["cpip"]["pump_intake_pressure"], 1) == 790.1
    assert round(results[2]["cpip"]["pump_intake_pressure"], 1) == 797.5


@NODE_REQUIRED
def test_declared_closing_row_must_repeat_the_first_row(assets, tmp_path):
    card = assets["examples"][0]["card"]
    result = run_node({"operation": "parse", "options": {"closingRow": True},
                       "text": card_csv(card["position"][:-1], card["load"][:-1])}, tmp_path)
    assert "closing row was declared" in result.get("error", "")
    bad = run_node({"operation": "parse", "options": {"closingRow": "yes"},
                    "text": card_csv(card["position"], card["load"])}, tmp_path)
    assert bad.get("error")


NULL_FIELDS = {
    "fluid_density": lambda w: w.update(fluid_density=None),
    "rod_density": lambda w: w["rod_string"][0].update(density=None),
    "rod_modulus": lambda w: w["rod_string"][0].update(modulus_of_elasticity=None),
    "rod_weight": lambda w: w["rod_string"][0].update(weight_per_foot=None),
    "pump_efficiency": lambda w: w["pump"].update(efficiency=None),
    "water_cut": lambda w: w.update(well_test={"oil_rate": 40.0, "water_cut": None}),
    "oil_rate": lambda w: w.update(well_test={"oil_rate": None}),
    "water_rate": lambda w: w.update(well_test={"water_rate": None}),
    "runtime": lambda w: w["input_params"].update(runtime=None),
}


@NODE_REQUIRED
@pytest.mark.parametrize("field", sorted(NULL_FIELDS))
def test_explicit_null_is_rejected_like_python(field, assets, tmp_path):
    """Presence is checked apart from validity: null is not an omission."""
    from pydantic import ValidationError
    well = copy.deepcopy(assets["well"])
    NULL_FIELDS[field](well)
    request = {"card": assets["examples"][0]["card"], "well": well}
    with pytest.raises(ValidationError):
        module("reference").python_analysis(request)
    assert run_node(request, tmp_path).get("error"), field


@NODE_REQUIRED
def test_null_fluid_density_is_not_an_omission(assets, tmp_path):
    """Beside the omission fixtures: null is rejected, omission uses the mixture."""
    well = copy.deepcopy(assets["well"])
    omitted = copy.deepcopy(well)
    del omitted["fluid_density"]
    well["fluid_density"] = None
    card = assets["examples"][0]["card"]
    rejected, accepted = run_node([{"card": card, "well": well},
                                   {"card": card, "well": omitted}], tmp_path)
    assert "Fluid density must be a finite number when supplied" in rejected.get("error", "")
    assert accepted["cpip"]["density_source"] == "assumed_50pct_mixture"
