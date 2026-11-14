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
@pytest.mark.parametrize("index", [1, 3])
def test_open_csv_cycle_matches_its_closed_form(index, assets, tmp_path):
    """Removing only the exact closing row must not change the pump card."""
    card, well = assets["examples"][index]["card"], assets["well"]
    assert card["position"][0] == card["position"][-1] and card["load"][0] == card["load"][-1]
    closed_text = card_csv(card["position"], card["load"])
    open_text = card_csv(card["position"][:-1], card["load"][:-1])
    closed, opened = run_node([{"operation": "parse", "text": closed_text},
                               {"operation": "parse", "text": open_text}], tmp_path)
    assert closed == card
    assert opened == card
    results = run_node([{"card": closed, "well": well}, {"card": opened, "well": well}], tmp_path)
    assert results[0]["downhole_card"] == results[1]["downhole_card"]
    assert results[0]["fillage"]["fillage"] == results[1]["fillage"]["fillage"]
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


def test_models_embed_exact_source_documents():
    model_script = (APP / "models.js").read_text(encoding="utf-8")
    body = model_script.split("const data = ", 1)[1].split(";\nroot.", 1)[0]
    embedded = json.loads(body)
    assert body == json.dumps(embedded, separators=(",", ":"), ensure_ascii=False)
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
        MODEL_PATH, SHAPE_MODEL_PATH, read_model_bytes,
    )
    for key, path in (("context", MODEL_PATH), ("shape", SHAPE_MODEL_PATH)):
        assert embedded[key] == json.loads(read_model_bytes(path))


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
