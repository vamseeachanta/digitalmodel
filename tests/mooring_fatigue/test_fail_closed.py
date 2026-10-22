# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Reject incomplete mooring fatigue assessments before producing a verdict."""

from copy import deepcopy
import math
from pathlib import Path
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest
import yaml

from digitalmodel.fatigue.damage import miner_damage
from digitalmodel.fatigue.sn_curves import get_sn_curve
from digitalmodel.mooring_fatigue import workflow as w

ROOT = Path(__file__).resolve().parents[2]
BAD = [None, math.nan, math.inf, -math.inf, 0, -1]


@pytest.fixture
def curve():
    return get_sn_curve("D", "seawater_cp")


@pytest.fixture
def line():
    return {
        "id": "test-line",
        "material": "CHAIN",
        "sn_curve_basis": {
            "material": "CHAIN",
            "curve": "D",
            "environment": "seawater_cp",
            "detail": "Nominal stress screening assumption",
            "reference": "Assumed, based on previous experience; test only",
            "protection_basis": "Effective cathodic protection assumed for test",
        },
        "stress_range_bins": [{"stress_range_MPa": 100, "cycles": 1e6}],
    }


@pytest.mark.parametrize("value", BAD)
@pytest.mark.parametrize("quantity", ["stress_range_MPa", "cycles", "tension_range_kN"])
def test_MF01_rejects_invalid_bins(line, curve, quantity, value):
    if quantity == "tension_range_kN":
        del line["stress_range_bins"]
        line.update(area_mm2=100, tension_range_bins=[{quantity: value, "cycles": 1e6}])
    else:
        line["stress_range_bins"][0][quantity] = value
    with pytest.raises(ValueError):
        w._evaluate_line(line, curve, 25, 10)


@pytest.mark.parametrize("column", ["stress_range", "cycles"])
@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf, -1])
def test_MF01_shared_damage_rejects_invalid_inputs(curve, column, value):
    hist = pd.DataFrame({"stress_range": [100.0, 50.0], "cycles": [1e6, 1e3]})
    hist.loc[0, column] = value
    with pytest.raises(ValueError):
        miner_damage(hist, curve)


@pytest.mark.parametrize("allowable", [math.nan, 0, -1, 1e-320])
def test_MF01_rejects_invalid_computed_damage(allowable):
    curve = SimpleNamespace(cycles=lambda stress: np.full(len(stress), allowable))
    with pytest.raises(ValueError):
        miner_damage(pd.DataFrame({"stress_range": [100], "cycles": [1e6]}), curve)


def test_MF01_rejects_aggregate_overflow():
    curve = SimpleNamespace(cycles=lambda stress: np.ones(len(stress)))
    hist = pd.DataFrame({"stress_range": [100, 100], "cycles": [1e308, 1e308]})
    with pytest.raises(ValueError, match="total damage"):
        miner_damage(hist, curve)


@pytest.mark.parametrize("damage", [math.nan, math.inf, -1, 0])
def test_MF01_rejects_invalid_total_before_status(line, curve, monkeypatch, damage):
    result = pd.DataFrame()
    result.attrs["total_damage"] = damage
    monkeypatch.setattr(w, "miner_damage", lambda *args: result)
    with pytest.raises(ValueError):
        w._evaluate_line(line, curve, 25, 10)


def test_MF01_rejects_zero_damage_hidden_in_valid_aggregate(line, curve):
    line["stress_range_bins"].append({"stress_range_MPa": 1e-300, "cycles": 1})
    with pytest.raises(ValueError, match="finite|positive"):
        w._evaluate_line(line, curve, 25, 10)


@pytest.mark.parametrize("area", BAD)
@pytest.mark.parametrize("representation", ["tension_range_bins", "tension_history_kN"])
def test_MF02_requires_finite_positive_area(area, representation):
    values = [{"tension_range_kN": 100, "cycles": 1e6}]
    if representation.endswith("history_kN"):
        values = [0, 100, 0, 100, 0]
    inputs = {"area_mm2": area, representation: values}
    if representation.endswith("history_kN"):
        inputs["n_repeats"] = 10
    with pytest.raises(ValueError, match="area_mm2"):
        w._stress_bins(inputs)


@pytest.mark.parametrize("representation", ["tension_range_bins", "tension_history_kN"])
def test_MF02_rejects_nonfinite_converted_stress(representation):
    values = [{"tension_range_kN": 1e308, "cycles": 1e6}]
    if representation.endswith("history_kN"):
        values = [0, 1e308, 0]
    inputs = {"area_mm2": 1, representation: values}
    if representation.endswith("history_kN"):
        inputs["n_repeats"] = 10
    with pytest.raises(ValueError, match="finite"):
        w._stress_bins(inputs)


@pytest.mark.parametrize("repeats", BAD + ["missing"])
def test_MF03_requires_explicit_positive_repetitions(repeats):
    line = {"stress_history_MPa": [0, 100, 0, 100, 0]}
    if repeats != "missing":
        line["n_repeats"] = repeats
    with pytest.raises(ValueError, match="n_repeats"):
        w._stress_bins(line)


@pytest.mark.parametrize("name", ["design_life_years", "dff"])
@pytest.mark.parametrize("value", BAD)
def test_MF04_validates_controls_at_evaluation_entry(line, curve, name, value):
    controls = {"design_life_years": 25, "dff": 10, name: value}
    with pytest.raises(ValueError, match=name):
        w._evaluate_line(line, curve, **controls)


@pytest.mark.parametrize("denominator", BAD)
def test_MF04_validates_denominator_before_infinity(denominator):
    with pytest.raises(ValueError):
        w._safe_divide(math.inf, denominator)


def test_MF04_rejects_required_life_overflow(line, curve):
    with pytest.raises(ValueError, match="required_life_years"):
        w._evaluate_line(line, curve, 1e308, 10)


@pytest.mark.parametrize("alternative", [
    {"tension_range_bins": [{"tension_range_kN": 1000, "cycles": 1e9}]},
    {"stress_history_MPa": [0, 100, 0]},
    {"tension_history_kN": [0, 100, 0]},
    {"tension_range_bins": None},
])
def test_MF05_rejects_competing_loads(line, alternative):
    line.update(alternative)
    with pytest.raises(ValueError, match="one load representation"):
        w._stress_bins(line)


@pytest.mark.parametrize("source,aliases,label", [
    ({"n_cycles": 1, "cycles": 1e9}, ("n_cycles", "cycles", "count"), "n_cycles"),
    ({"n_cycles": 1, "cycles": 1}, ("n_cycles", "cycles", "count"), "n_cycles"),
    ({"area_mm2": 100, "cross_section_area_mm2": None}, ("area_mm2", "cross_section_area_mm2"), "area_mm2"),
    ({"n_repeats": 1, "n_histories": 100}, ("n_repeats", "n_histories"), "n_repeats"),
    ({"stress_range_MPa": 1, "range": 100}, ("stress_range_MPa", "range"), "stress_range_MPa"),
    ({"tension_range_kN": 1, "range": 100}, ("tension_range_kN", "range"), "tension_range_kN"),
])
def test_MF05_rejects_duplicate_aliases(source, aliases, label):
    with pytest.raises(ValueError, match="one alias"):
        w._first_float(source, aliases, label)


@pytest.mark.parametrize("settings", [{}, {"sn_curve": {}}, {"sn_curve": {"curve": "D"}},
                                         {"sn_curve": {"environment": "seawater_cp"}}])
def test_MF06_requires_explicit_curve_and_environment(settings):
    with pytest.raises(ValueError):
        w._curve_settings(settings)


@pytest.mark.parametrize("field", ["material", "sn_curve_basis"])
def test_MF06_requires_material_and_applicability(line, curve, field):
    del line[field]
    with pytest.raises(ValueError, match=field):
        w._evaluate_line(line, curve, 25, 10)


@pytest.mark.parametrize("field", ["material", "curve", "environment", "detail", "reference", "protection_basis"])
def test_MF06_requires_complete_basis(line, curve, field):
    del line["sn_curve_basis"][field]
    with pytest.raises(ValueError):
        w._evaluate_line(line, curve, 25, 10)


@pytest.mark.parametrize("field,value", [("material", "STEEL-WIRE"), ("curve", "F"),
                                         ("environment", "free_corrosion")])
def test_MF06_rejects_inapplicable_curve(line, curve, field, value):
    line["sn_curve_basis"][field] = value
    with pytest.raises(ValueError, match="match"):
        w._evaluate_line(line, curve, 25, 10)


@pytest.mark.parametrize("control", ["design_life_years", "dff"])
def test_MF07_packaged_defaults_cannot_assess_incomplete_case(line, tmp_path, control):
    path = ROOT / "src/digitalmodel/base_configs/modules/mooring_fatigue/mooring_fatigue.yml"
    cfg = yaml.safe_load(path.read_text(encoding="utf-8"))
    settings = cfg["mooring_fatigue"]
    assert settings[control] is None
    settings.update(lines=[line], output_dir=str(tmp_path),
                    sn_curve={"curve": "D", "environment": "seawater_cp"})
    other = "dff" if control == "design_life_years" else "design_life_years"
    settings[other] = 10
    with pytest.raises(ValueError, match=control):
        w.router(cfg)
    assert not list(tmp_path.iterdir())
    with pytest.raises(ValueError, match=control):
        w.router({"mooring_fatigue": {k: v for k, v in settings.items() if k != control}})


@pytest.mark.parametrize("alias", ["n_repeats", "n_histories"])
def test_explicit_history_repetitions_scale_damage_and_record_basis(line, curve, alias):
    del line["stress_range_bins"]
    line.update(stress_history_MPa=[0, 100, 0, 100, 0])
    line[alias] = 1
    _, once = w._evaluate_line(line, curve, 25, 10)
    line[alias] = 10
    _, repeated = w._evaluate_line(line, curve, 25, 10)
    assert repeated["total_damage"] == pytest.approx(10 * once["total_damage"])
    assert repeated["n_repeats"] == 10
    assert repeated["load_basis"] == "explicit history repetitions over design life"


def test_tension_history_matches_stress_history(line, curve):
    del line["stress_range_bins"]
    line.update(stress_history_MPa=[0, 100, 0, 100, 0], n_repeats=10)
    _, stress = w._evaluate_line(line, curve, 25, 10)
    line["tension_history_kN"] = line.pop("stress_history_MPa")
    line["area_mm2"] = 1000
    _, tension = w._evaluate_line(line, curve, 25, 10)
    assert tension["total_damage"] == stress["total_damage"]


def test_shared_damage_preserves_true_zero_stress(curve):
    result = miner_damage(pd.DataFrame({"stress_range": [0], "cycles": [1]}), curve)
    assert result.attrs["total_damage"] == 0


@pytest.mark.parametrize("values", [[0, math.inf, 0], [0, math.nan, 0], [1, 1, 1], []])
def test_invalid_or_uninformative_histories_are_rejected(values):
    with pytest.raises(ValueError):
        w._stress_bins({"stress_history_MPa": values, "n_repeats": 1})


@pytest.mark.parametrize("version", ["mooring-fatigue", "mooring-fatigue-v2"])
def test_shipped_example_router_preserves_valid_results(version, tmp_path):
    path = ROOT / "examples/workflows" / version / "input.yml"
    cfg = yaml.safe_load(path.read_text(encoding="utf-8"))
    cfg["mooring_fatigue"]["output_dir"] = str(tmp_path)
    expected = []
    curve = get_sn_curve(**{"name": cfg["mooring_fatigue"]["sn_curve"]["curve"],
                            "environment": cfg["mooring_fatigue"]["sn_curve"]["environment"]})
    for item in cfg["mooring_fatigue"]["lines"]:
        bins = item["tension_range_bins"]
        stress = np.array([b["tension_range_kN"] * 1000 / item["area_mm2"] for b in bins])
        counts = np.array([b["n_cycles"] for b in bins])
        expected.append(float(np.sum(counts / curve.cycles(stress))))
    result = w.router(deepcopy(cfg))["mooring_fatigue"]
    assert [s["total_damage"] for s in result["lines"]] == pytest.approx(expected)
    assert result["governing_line"] == "wire-01"
    assert result["design_controls_basis"] == "case-specific inputs"
    for summary in result["lines"]:
        assert summary["dff_margin"] == pytest.approx(1 / (summary["total_damage"] * result["dff"]))
        assert summary["passes_dff"] == (summary["dff_margin"] >= 1)
        assert summary["sn_curve_basis"]["reference"]
    assert len(pd.read_csv(result["results_csv"])) == 6
    assert len(pd.read_csv(result["summary_csv"])) == 2
    assert all(b"\r\n" not in p.read_bytes() for p in tmp_path.glob("*.csv"))
