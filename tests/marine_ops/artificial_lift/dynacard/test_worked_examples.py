"""Worked plate examples: numerical recovery, provenance and artifact drift."""
import csv
import importlib.util
import io
import json
import os
from pathlib import Path
import re
import subprocess
import sys

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostic_plate import (
    load_diagnostic_plate,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.worked_examples import (
    _round_trip, build_examples, diagnostics_matrix, example_well, fluid_load_scale_lbf,
    results_document,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData

ROOT = Path(__file__).resolve().parents[4]
DATA = Path(__file__).parent / "testdata"
WORKBOOK = DATA / "mendeley_plate" / "Working condition diagnosis plate.xls"
OUTPUT = ROOT / "docs/domains/artificial_lift/dynacard-examples"
BUILDER = ROOT / "examples/domains/artificial_lift/build_dynacard_worked_examples.py"


@pytest.fixture(scope="module")
def records():
    return build_examples(WORKBOOK)


@pytest.fixture(scope="module")
def document(records):
    return results_document(records)


@pytest.fixture(scope="module")
def builder():
    spec = importlib.util.spec_from_file_location("worked_builder", BUILDER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_plate_order_and_provenance(records):
    curves = [c for c in load_diagnostic_plate(WORKBOOK) if c.ordinate == "load"]
    assert len(records) == 10
    assert [r["index"] for r in records] == list(range(1, 11))
    assert [r["source_label"] for r in records] == [c.label for c in curves]
    assert len({r["slug"] for r in records}) == 10
    for r in records:
        assert re.fullmatch(r"[a-z]+(?:-[a-z]+)*", r["slug"])
        assert r["sheet_name"] == r["source_label"]
        assert r["source_doi"] == "10.17632/hnfwm9mjs6.1"
        assert r["dataset_url"] == "https://data.mendeley.com/datasets/hnfwm9mjs6/1"
        assert r["file_url"].endswith("53140ff5-85c3-40d1-b900-364a892e692e/file_downloaded")
        assert r["retrieval_date"] == "2026-10-11"
        assert r["licence"] == "CC0 1.0"


def test_fixed_basis_and_hand_calculation(document):
    well = example_well()
    assert well["api14"] == "PLATE-EXAMPLE"
    assert well["spm"] == well["input_params"]["strokes_per_minute"] == 6.42
    assert well["input_params"]["tubing_pressure"] == 100
    assert well["pump"] == {"diameter": 1.5, "depth": 4844.0}
    assert well["fluid_density"] == 63.237
    assert [(s["diameter"], s["length"], s["weight_per_foot"])
            for s in well["rod_string"]] == [(0.875, 1400, 2.224), (0.75, 3350, 1.634)]
    assert all(s["modulus_of_elasticity"] == 30.5e6 for s in well["rod_string"])
    # Hand calculation: pressure = 2227.222416667 psi; area = 1.767145867644 in2.
    assert fluid_load_scale_lbf() == pytest.approx(3935.82688994, abs=0.000001)
    assert document["schema_version"] == 1
    assert all(v["status"] == "ASSUMED" and v["reason"]
               for v in document["basis"].values())
    assert document["basis"]["load_range_lbf"]["value"] == pytest.approx(fluid_load_scale_lbf())
    assert set(document["model_digests"]) == {"shipped", "shape_only"}
    assert all(re.fullmatch("[0-9a-f]{64}", d) for d in document["model_digests"].values())


def test_surface_transform_and_round_trip(records):
    # Resolution is derived only from source samples, never the recovered card.
    for r in records:
        surface, source = r["surface_card"], r["source_pump_card"]
        assert np.ptp(source["position"]) == pytest.approx(40)
        assert min(source["load"]) == 0
        assert np.ptp(source["load"]) == pytest.approx(fluid_load_scale_lbf())
        assert abs(np.ptp(surface["position"]) - 40) > 0.1
        assert abs(np.ptp(surface["load"]) - np.ptp(source["load"])) > 1
        resolution = 100 * max(abs(b - a) for a, b in zip(
            source["load"], source["load"][1:] + source["load"][:1])) / np.ptp(source["load"])
        assert r["round_trip"]["tolerance_pct"] == pytest.approx(resolution)
        assert 0 <= r["round_trip"]["load_nrmse_pct"] < resolution
        # Retain the stricter historical regression ceiling without treating
        # its prior-run origin as independent qualification evidence.
        assert r["round_trip"]["load_nrmse_pct"] < 3.0
        assert abs(r["round_trip"]["stroke_difference_in"]) < 0.1


def test_shape_calls_match_recorded_score(records):
    score = json.loads((DATA / "plate_classifier_score.json").read_text(encoding="utf-8"))
    expected = score["shape_only"]["rows"]
    assert [(r["source_label"], r["consistency"]["source_shape_only"]["classification"])
            for r in records] == [(r["source_label"], r["predicted_mode"]) for r in expected]
    literal = [r for r in records if r["crosswalk_kind"] == "literal"]
    assert sum(r["consistency"]["source_shape_only"]["classification"] == r["crosswalk_mode"]
               for r in literal) == 5
    assert sum(r["consistency"]["source_shape_only"]["classification"] == r["crosswalk_mode"]
               for r in records) == 6


def test_round_trip_direction_phase_and_zero_reference(records):
    source = CardData(**records[0]["source_pump_card"])
    rotated = CardData(position=np.roll(source.position[::-1], 17).tolist(),
                       load=np.roll(source.load[::-1], 17).tolist())
    assert _round_trip(rotated, source)["load_nrmse_pct"] == 0
    assert _round_trip(rotated, source)["stroke_difference_in"] == 0
    computed = CardData(**records[0]["computed_pump_card"])
    rotated = CardData(position=np.roll(computed.position[::-1], 23).tolist(),
                       load=np.roll(computed.load[::-1], 23).tolist())
    assert _round_trip(rotated, source)["load_nrmse_pct"] == pytest.approx(
        records[0]["round_trip"]["load_nrmse_pct"], abs=1e-12)


def test_matrix_probabilities(document):
    matrix = diagnostics_matrix(document["examples"])
    assert len(matrix["columns"]) == 11
    assert [c["mode"] for c in matrix["columns"][:-1]] == [
        r["crosswalk_mode"] for r in document["examples"]]
    assert [c["mode"] for c in matrix["columns"] if c["crosswalk_kind"] == "assumed"] == [
        "FLUID_POUND", "SAND_ABRASION"]
    for row, record in zip(matrix["rows"], document["examples"], strict=True):
        assert sum(row["probabilities_pct"]) == pytest.approx(100, rel=0, abs=1e-6)
        vector = record["diagnosis"]["shape_only"]["probabilities"]
        assert sum(vector.values()) == pytest.approx(1, rel=0, abs=1e-8)
        assert row["shape_only_argmax"] == max(vector, key=vector.get)
        other = sum(p for m, p in vector.items() if m not in {
            c["mode"] for c in matrix["columns"][:-1]})
        assert row["probabilities_pct"][-1] == pytest.approx(100 * other)


def test_matrix_rows_use_computed_card_probabilities(records):
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
        SHAPE_MODEL_PATH, FeatureExtractor, _classify_shape_with_model, _predict_from_trees,
        read_model_bytes,
    )
    model = json.loads(read_model_bytes(SHAPE_MODEL_PATH))
    matrix = diagnostics_matrix(records)
    modes = [column["mode"] for column in matrix["columns"][:-1]]
    for record, row in zip(records, matrix["rows"], strict=True):
        for group, key, card_key in (
            ("diagnosis", "shape_only", "computed_pump_card"),
            ("consistency", "source_shape_only", "source_pump_card"),
        ):
            card = CardData(**record[card_key])
            features = FeatureExtractor.normalize(
                FeatureExtractor.extract_shape_vector(card), model["scaling"])
            mode, probabilities = _predict_from_trees(features, model)
            vector = dict(zip(model["class_labels"], probabilities, strict=True))
            diagnosis = record[group][key]
            assert diagnosis["input_card"] == card_key
            assert diagnosis["classification"] == mode
            assert diagnosis["probabilities"] == pytest.approx(vector)
            assert diagnosis["differential"] == _classify_shape_with_model(
                card, model).model_dump()["differential"]
            if group == "diagnosis":
                expected = [100 * vector.get(mode, 0) for mode in modes]
                expected.append(100 * sum(p for mode, p in vector.items() if mode not in modes))
                assert row["probabilities_pct"] == pytest.approx(expected)
                assert row["shape_only_argmax"] == mode
        assert "computed_shape_only" not in record["diagnosis"]


def test_card_work_uses_module_area(records):
    for record in records:
        surface = CardData(**record["surface_card"])
        for key, card in (("polished_rod_work_per_stroke_ft_lbf", surface),
                          ("pump_work_per_stroke_ft_lbf", CardData(**record["computed_pump_card"]))):
            # Independent signed polygon cross-products; inches -> feet.
            vertices = list(zip(card.position, card.load))
            twice_area = sum(x * v - u * y for (x, y), (u, v) in zip(
                vertices, vertices[1:] + vertices[:1]))
            assert record["analysis"][key] == pytest.approx(abs(twice_area) / 24)


@pytest.mark.parametrize("reverse,closed", [(False, False), (True, False), (False, True)])
def test_reported_work_has_hand_calculated_oracles(records, reverse, closed):
    from digitalmodel.marine_ops.artificial_lift.dynacard.models import DynacardAnalysisContext
    from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow
    from digitalmodel.marine_ops.artificial_lift.dynacard.worked_examples import _analysis_values

    context = DynacardAnalysisContext(**example_well(), surface_card=CardData(**records[0]["surface_card"]))
    result = DynacardWorkflow(context).run_full_analysis()
    # Offset rectangle: 40 in * 1200 lbf / 12 = 4000 ft·lbf.
    # Offset triangle: (30 in * 800 lbf / 2) / 12 = 1000 ft·lbf.
    cards = [([7, 7, 47, 47], [200, 1400, 1400, 200]),
             ([5, 5, 35], [100, 900, 100])]
    prepared = []
    for position, load in cards:
        if reverse:
            position, load = position[::-1], load[::-1]
        if closed:
            position, load = position + position[:1], load + load[:1]
        prepared.append(CardData(position=position, load=load))
    context.surface_card, result.downhole_card = prepared
    analysis = _analysis_values(context, result)
    assert analysis["polished_rod_work_per_stroke_ft_lbf"] == pytest.approx(4000)
    assert analysis["pump_work_per_stroke_ft_lbf"] == pytest.approx(1000)


@pytest.mark.parametrize("difference,expected", [(1e-12, 0.0), (-1e-12, 0.0),
                                                 (0.0000012, 0.000001), (15.25, 15.25)])
def test_precision_effect_has_absolute_reporting_resolution(monkeypatch, difference, expected):
    from types import SimpleNamespace
    from digitalmodel.marine_ops.artificial_lift.dynacard import worked_examples

    values = iter([difference, 0.0])

    class Workflow:
        def __init__(self, context):
            pass

        def run_full_analysis(self):
            value = next(values)
            return SimpleNamespace(fluid_load=SimpleNamespace(fluid_load=value),
                                   fillage=SimpleNamespace(fillage=value, corners=[0, 1, 2, 3]))

    monkeypatch.setattr(worked_examples, "DynacardWorkflow", Workflow)
    card = CardData(position=[0, 1, 0], load=[0, 1, 0])
    effect = worked_examples._published_precision_effect(card, card)
    assert effect == {"fluid_load_difference_lbf": expected,
                      "fillage_difference_pct": expected, "corners_changed": False}


def test_round_trip_criterion_has_independent_sampling_oracle():
    # Four 100-lbf steps span 400 lbf: resolution = 100 * 100/400 = 25%.
    loads = [0, 100, 200, 300, 400, 300, 200, 100]
    reference = CardData(position=[0, 0, 0, 1, 2, 2, 2, 1], load=loads)
    for offset, expected in [(80, 20), (120, 30)]:
        actual = CardData(position=reference.position, load=[v + offset for v in loads])
        check = _round_trip(actual, reference)
        assert check["tolerance_pct"] == 25
        assert check["load_nrmse_pct"] == pytest.approx(expected)
        assert (check["load_nrmse_pct"] < check["tolerance_pct"]) == (offset == 80)


def test_assumed_numerical_inputs_have_previous_experience_provenance(document):
    for entry in document["basis"].values():
        assert entry["provenance"] == "assumed, based on previous experience"


def test_generated_svg_has_no_trailing_whitespace(tmp_path, builder):
    import matplotlib.pyplot as plt
    plots = builder._helper("plots")
    figure = plt.figure()
    figure.add_subplot().plot([0, 1], [0, 1])
    plots._save(figure, tmp_path, "whitespace")
    raw = (tmp_path / "whitespace.svg").read_bytes()
    assert b"\r" not in raw
    assert raw.endswith(b"\n")
    assert all(line == line.rstrip() for line in raw.splitlines())


def test_analysis_preserves_module_basis(records):
    for r in records:
        a = r["analysis"]
        assert a["runtime_hours"] == 24
        assert a["runtime_source"] == "card"
        assert a["pump_efficiency_pct"] == 100
        assert a["net_displacement_bpd"] == pytest.approx(a["theoretical_production_bpd"])
        assert a["net_displacement_bpd"] == pytest.approx(
            a["gross_displacement_bpd"] * a["fillage_pct"] / 100)
        assert a["fluid_load_lbf"] == pytest.approx(
            a["upstroke_mean_load_lbf"] - a["downstroke_mean_load_lbf"])
        assert a["pump_work_per_stroke_ft_lbf"] > 0
        assert isinstance(a["buckling_detected"], bool)
        assert r["diagnosis"]["status"] == "HYPOTHESIS"


def test_build_ignores_unrelated_cached_model(records, monkeypatch):
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
        MODEL_PATH, PumpDiagnostics,
    )
    stale = json.loads(MODEL_PATH.read_text(encoding="utf-8"))
    stale["class_labels"] = stale["class_labels"][1:] + stale["class_labels"][:1]
    monkeypatch.setattr(PumpDiagnostics, "_model", stale)
    rebuilt = build_examples(WORKBOOK)
    assert [r["diagnosis"] for r in rebuilt] == [r["diagnosis"] for r in records]
    assert PumpDiagnostics._model is stale  # No process-global cache mutation.


def test_document_retains_inference_digests(records, document, monkeypatch, tmp_path):
    from digitalmodel.marine_ops.artificial_lift.dynacard import worked_examples
    # Changed model location after inference cannot re-label historical evidence.
    monkeypatch.setattr(worked_examples, "MODEL_PATH", tmp_path / "absent.json")
    monkeypatch.setattr(worked_examples, "SHAPE_MODEL_PATH", tmp_path / "absent-shape.json")
    assert results_document(records) == document


def test_saved_artifacts_equal_fresh_build(document, builder):
    artifacts = builder.text_artifacts(document)
    saved = json.loads((OUTPUT / "results.json").read_text(encoding="utf-8"))
    assert saved == document
    assert builder.text_artifacts(saved) == artifacts
    for name, content in artifacts.items():
        assert (OUTPUT / name).read_bytes() == content, name


def test_csv_contract_and_readme_links(document, builder):
    artifacts = builder.text_artifacts(document)
    for r in document["examples"]:
        stem = f"example-{r['index']:02d}-{r['slug']}"
        rows = list(csv.reader(io.StringIO(artifacts[f"{stem}.csv"].decode())))
        assert rows[0] == ["sample", "surface_position_in", "surface_load_lbf",
                           "pump_position_in", "pump_load_lbf",
                           "source_pump_position_in", "source_pump_load_lbf"]
        assert len(rows) == 1 + max(len(r[k]["position"]) for k in (
            "surface_card", "computed_pump_card", "source_pump_card"))
        surface = list(csv.reader(io.StringIO(artifacts[f"{stem}-surface-card.csv"].decode())))
        assert surface[0] == ["position_in", "load_lbf"]
        assert all(re.fullmatch(r"-?\d+\.\d{4}", row[0]) and
                   re.fullmatch(r"-?\d+\.\d{2}", row[1]) for row in surface[1:])
    readme = artifacts["README.md"].decode("utf-8")
    assert readme.startswith("<!--") and BUILDER.name in readme.splitlines()[0]
    links = re.findall(r"!\[[^\]]*\]\(([^)]+)\)", readme)
    assert len(links) == 11
    assert all((OUTPUT / link).is_file() for link in links)


def test_each_readme_section_has_figure_analysis_and_source(document, builder):
    readme = builder.text_artifacts(document)["README.md"].decode("utf-8")
    decimals = {"in": 2, "lbf": 0, "%": 1, "psi": 1, "bbl/day": 1,
                "ft·lbf": 0, "lb/ft³": 3, "psi/ft": 4}
    for record in document["examples"]:
        title = f"Example {record['index']}: {record['english_gloss']}"
        section = readme.split(f"## {title}\n", 1)[1].split("\n## ", 1)[0]
        stem = f"example-{record['index']:02d}-{record['slug']}"
        assert f"]({stem}.png)" in section
        assert "| Quantity | Unit | Value |" in section
        assert "| Polished-rod work per stroke | ft·lbf |" in section
        assert "| Pump-card work per stroke | ft·lbf |" in section
        assert f"[DOI {record['source_doi']}]({record['dataset_url']})" in section
        assert f"[original workbook]({record['file_url']})" in section
        assert f'<img src="{stem}.png" width="360"' in readme
        anchor = f"example-{record['index']}-{record['slug']}"
        assert f"[{title}](#{anchor})" in readme
        for label, unit, value in re.findall(r"^\| ([^|]+) \| ([^|]+) \| ([^|]+) \|$", section, re.M):
            if unit in decimals:
                places = decimals[unit]
                pattern = rf"-?\d+\.\d{{{places}}}" if places else r"-?\d+"
                assert re.fullmatch(pattern, value), (title, label, value)


def test_plot_positions_shift_only_for_display(document, builder):
    import matplotlib.pyplot as plt
    plots = builder._helper("plots")
    before = json.dumps(document, sort_keys=True)
    for record in document["examples"]:
        fig = plt.figure()
        try:
            plots._axes(fig, record)
            for axis, card_key in zip(fig.axes, ("surface_card", "computed_pump_card"), strict=True):
                x = np.asarray(axis.lines[0].get_xdata())
                assert min(x) == pytest.approx(0)
                assert np.ptp(x) == pytest.approx(np.ptp(record[card_key]["position"]))
            assert fig.axes[1].lines[1].get_linestyle() == "--"
            assert fig.axes[1].get_legend() is not None
            text = plots._diagnosis_text(record)
            labels = ["Shape-only (computed pump card)", "Shipped model",
                      "Expected condition / crosswalk", "Source-shape check:"]
            assert [text.index(label) for label in labels] == sorted(text.index(label) for label in labels)
        finally:
            plt.close(fig)
    assert json.dumps(document, sort_keys=True) == before


def test_check_cli_detects_drift(tmp_path, document, builder):
    for name, content in builder.text_artifacts(document).items():
        (tmp_path / name).write_bytes(content)
    env = dict(os.environ, PYTHONPATH=str(ROOT / "src"), PYTHONIOENCODING="utf-8")
    command = [sys.executable, str(BUILDER), "--out", str(tmp_path), "--check"]
    options = dict(cwd=ROOT, env=env, capture_output=True, text=True,
                   creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0), timeout=45)
    check = subprocess.run(command, **options)
    assert check.returncode == 0, check.stdout + check.stderr
    (tmp_path / "README.md").write_text("drift\n", encoding="utf-8")
    check = subprocess.run(command, **options)
    assert check.returncode != 0
    assert "README.md" in check.stdout + check.stderr


def test_published_surface_csv_reproduces_stored_analysis(document):
    """The analysed surface card is exactly the published two-column card.

    Corner detection moves in sample steps, so fluid load and fillage can shift
    when an input is rounded after analysis (example 4 moved 15 lbf when its
    card was rounded to nine significant digits). The analysis therefore runs
    on the card at its published CSV precision, and rerunning the workflow on
    each published CSV must reproduce the stored values.
    """
    from digitalmodel.marine_ops.artificial_lift.dynacard.models import DynacardAnalysisContext
    from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow

    for record in document["examples"]:
        path = OUTPUT / f"example-{record['index']:02d}-{record['slug']}-surface-card.csv"
        rows = list(csv.DictReader(io.StringIO(path.read_text(encoding="utf-8"))))
        card = CardData(position=[float(r["position_in"]) for r in rows],
                        load=[float(r["load_lbf"]) for r in rows])
        assert card.position == record["surface_card"]["position"]
        assert card.load == record["surface_card"]["load"]
        context = DynacardAnalysisContext(**example_well(), surface_card=card)
        result = DynacardWorkflow(context).run_full_analysis()
        stored = record["analysis"]
        for value, key in ((result.fluid_load.fluid_load, "fluid_load_lbf"),
                           (result.fillage.fillage, "fillage_pct"),
                           (result.fillage.net_stroke, "pump_net_stroke_in")):
            # results.json keeps nine significant digits.
            assert value == pytest.approx(stored[key], rel=1e-8, abs=1e-9), (record["index"], key)
