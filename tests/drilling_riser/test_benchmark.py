# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic and evidence gates for the generic benchmark."""

import copy
import json
import math
from pathlib import Path

import pytest

from digitalmodel.drilling_riser import benchmark

ROOT = Path(__file__).resolve().parents[2]
FIXTURE = Path(__file__).parent / "fixtures/benchmark_synthetic.json"


def test_case_table_is_sourced_unique_and_matches_existing_grid():
    table = benchmark.load_table(ROOT)
    assert len(table["cases"]) == 24
    assert len({case["id"] for case in table["cases"]}) == 24
    for case in table["cases"]:
        for name in ("offset_pct", "current_mps", "current_profile"):
            assert case[name]["source"]
            assert case[name]["required_input"]
            assert "reference to be selected: candidates must be public" in case[name]["qualification"]
    changed = copy.deepcopy(table)
    changed["cases"][1] = changed["cases"][0]
    with pytest.raises(ValueError, match="case table"):
        benchmark.validate_table(ROOT, changed)


def test_screen_offset_hand_calculation():
    """Synthetic arithmetic: L=100 m, X=10 m, w=0, W=80 kN, f=1.5.

    y=X*z/L, slope=X/L=0.1 rad; degrees=0.1*180/pi.
    Required top force=W*f=120 kN. Geometric extension=sqrt(10100)-100
    =0.4987562112089 m. These are mathematical inputs, not design limits.
    Source: riser_response.py module derivation and stackup.py product equation.
    """
    result = benchmark.screen(length_m=100, offset_m=10, weight_kn=80,
                              tension_factor=1.5, ei_nm2=1000, drag_n_per_m=0)
    assert result["lower_angle_deg"] == pytest.approx(5.729577951308233)
    assert result["upper_angle_deg"] == pytest.approx(5.729577951308233)
    assert result["top_tension_kn"] == 120
    assert result["geometric_extension_m"] == pytest.approx(0.4987562112089)
    assert result["stroke_m"] is None


@pytest.mark.parametrize("value", [0, -1, True, float("nan"), float("inf")])
def test_invalid_screen_input(value):
    with pytest.raises(ValueError):
        benchmark.screen(length_m=value, offset_m=0, weight_kn=80,
                         tension_factor=1.5, ei_nm2=1000, drag_n_per_m=0)


def test_reader_hand_checked_synthetic_answer(tmp_path):
    """Fixture arithmetic: screen=100 kN, stored=110 kN.

    Signed difference=100*(110-100)/abs(100)=10.0 percent.
    The fixture's synthetic 10 percent agreement limit passes equality;
    9 percent fails. Neither is an engineering acceptance criterion.
    """
    record = json.loads(FIXTURE.read_text())
    path = tmp_path / "result.json"
    path.write_text(json.dumps(record))
    screens = {"top_tension_kn": 100, "stroke_m": None}
    criteria = {"top_tension_kn": {"value": 10, "source": "synthetic arithmetic only"}}
    result = benchmark.read_result(path, case_id="synthetic", case_digest="synthetic",
                                   screens=screens, criteria=criteria)
    assert result["top_tension_kn"]["difference_pct"] == 10
    assert result["top_tension_kn"]["status"] == "PASS"
    assert result["top_tension_kn"]["origin"] == "synthetic"
    assert result["stroke_m"]["status"] == "NOT_EVALUATED"
    criteria["top_tension_kn"]["value"] = 9
    assert benchmark.read_result(path, case_id="synthetic", case_digest="synthetic",
                                 screens=screens, criteria=criteria)["top_tension_kn"]["status"] == "FAIL"


@pytest.mark.parametrize("payload", [None, "{", "[]", "{}", "null"])
def test_missing_or_malformed_results_are_not_evaluated(tmp_path, payload):
    path = tmp_path / "result.json"
    if payload is not None:
        path.write_text(payload)
    result = benchmark.read_result(path, case_id="synthetic", case_digest="synthetic",
                                   screens={"top_tension_kn": 100}, criteria={})
    assert result["top_tension_kn"]["status"] == "NOT_EVALUATED"
    assert result["top_tension_kn"]["value"] is None


@pytest.mark.parametrize("mutation", ["identity", "unit", "definition", "nonfinite", "bool", "source"])
def test_result_identity_and_quantity_contract(tmp_path, mutation):
    record = json.loads(FIXTURE.read_text())
    if mutation == "identity":
        record["case_digest"] = "stale"
    else:
        key, value = {"unit": ("unit", "N"), "definition": ("definition", "other"),
                      "nonfinite": ("value", float("nan")), "bool": ("value", True),
                      "source": ("source", "")}[mutation]
        record["metrics"]["top_tension_kn"][key] = value
    path = tmp_path / "result.json"
    path.write_text(json.dumps(record))
    result = benchmark.read_result(path, case_id="synthetic", case_digest="synthetic",
                                   screens={"top_tension_kn": 100}, criteria={})
    assert result["top_tension_kn"]["status"] == "NOT_EVALUATED"
    assert result["top_tension_kn"]["value"] is None


def test_absent_criteria_and_zero_reference_are_not_evaluated(tmp_path):
    for reference, criteria in [(100, {}), (0, {"top_tension_kn": {"value": 10, "source": "synthetic"}})]:
        result = benchmark.read_result(FIXTURE, case_id="synthetic", case_digest="synthetic",
                                       screens={"top_tension_kn": reference}, criteria=criteria)
        assert result["top_tension_kn"]["status"] == "NOT_EVALUATED"
        assert result["top_tension_kn"]["reason"]


def test_every_case_generates_valid_template_inputs(tmp_path):
    """Reuse the existing fixed-grid generator and both YAML validators.

    Committed modular inputs are reused, rather than committing another copy.
    Numeric sweep expectations are the existing case_table, not a second grid.
    """
    import yaml
    from digitalmodel.drilling_riser import generic_campaign

    table = benchmark.load_table(ROOT)
    original = generic_campaign.case_table(ROOT)
    assert [case["id"] for case in table["cases"]] == [case["id"] for case in original["cases"]]
    generic_campaign.build_models(ROOT, tmp_path, original)
    for case in table["cases"]:
        folder = tmp_path / case["id"]
        for generated in folder.rglob("*.yml"):
            relative = generated.relative_to(tmp_path)
            committed = ROOT / generic_campaign.DEST / "models" / relative
            assert yaml.safe_load(generated.read_text()) == yaml.safe_load(committed.read_text())
        generic_campaign.validate_model(folder)


def test_report_rebuild_is_reproducible_and_has_only_result_placeholders(tmp_path):
    import importlib.util
    from bs4 import BeautifulSoup

    path = ROOT / "scripts/reporting/build_drilling_riser_benchmark.py"
    spec = importlib.util.spec_from_file_location("benchmark_report", path)
    builder = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(builder)
    builder.build(tmp_path)
    for name in ("index.html", "changes.json"):
        assert (tmp_path / name).read_bytes() == (ROOT / benchmark.REPORT / name).read_bytes()
    document = BeautifulSoup((tmp_path / "index.html").read_text(), "html.parser")
    sections = document.select("main.main > section")
    assert [section["id"] for section in sections][:2] == ["summary", "revision-history"]
    ledger = benchmark.evaluate(ROOT)
    assert len(ledger) == 24
    for row in ledger:
        for result in row["results"].values():
            assert result["status"] == "NOT_EVALUATED"
            assert result["value"] is None
            assert result["difference_pct"] is None
    assert document.select_one("#results").get_text().count("NOT_EVALUATED") >= 96
    for key in ("method", "worked-example", "limits", "pending", "sources"):
        assert document.select_one("#" + key)
    assert len(document.select("svg[data-metric]")) == 4
    for svg in document.select("svg[data-metric]"):
        metric = svg["data-metric"]
        points = svg.select("circle[data-case]")
        assert len(points) == 24
        for point, row in zip(points, ledger):
            assert point["data-case"] == row["case_id"]
            assert float(point["data-value"]) == round(row["screens"][metric], 1)
    assert not document.select("script[src], link[rel=stylesheet], iframe")
    assert "synthetic arithmetic" not in document.select_one("#results").get_text()


def test_uniform_current_hand_calculation():
    """Synthetic pinned beam: L=2 m, T=EI=1 in SI, q=0.02 N/m.

    Thus k=sqrt(T/EI)=1/m. Symmetry gives opposite end slopes with
    magnitude q*L/(2*T)-q*tanh(k*L/2)/(k*T)=0.02*(1-tanh(1)) rad.
    Source: MIT Structural Mechanics (2013), Lecture 6 Eqs (6.3)-(6.6),
    with the pinned-end homogeneous exponential solution in riser_response.py.
    This independent boundary calculation uses no returned reference values.
    """
    result = benchmark.screen(length_m=2, offset_m=0, weight_kn=0.001,
                              tension_factor=1, ei_nm2=1, drag_n_per_m=0.02)
    expected = 0.02 * (1 - math.tanh(1)) * 180 / math.pi
    assert result["lower_angle_deg"] == pytest.approx(expected)
    assert result["upper_angle_deg"] == pytest.approx(expected)


def test_result_identity_changes_with_calculation_fingerprint(monkeypatch):
    original = benchmark.screen_cases(ROOT, benchmark.load_table(ROOT))[0]["case_digest"]
    monkeypatch.setattr(benchmark.generic_campaign, "calculation_fingerprints",
                        lambda: {"changed": "synthetic mutation"})
    changed = benchmark.screen_cases(ROOT, benchmark.load_table(ROOT))[0]["case_digest"]
    assert changed != original


@pytest.mark.parametrize("field", ["offset_pct", "current_mps"])
def test_case_schema_rejects_boolean_quantities(field):
    table = benchmark.load_table(ROOT)
    table["cases"][0][field]["value"] = False
    with pytest.raises(ValueError, match="case table"):
        benchmark.validate_table(ROOT, table)


@pytest.mark.parametrize("criterion", [None, {}, {"value": True, "source": "synthetic"},
    {"value": -1, "source": "synthetic"}, {"value": 10, "source": ""},
    {"value": float("nan"), "source": "synthetic"}])
def test_unsourced_or_invalid_agreement_criteria_are_not_evaluated(criterion):
    result = benchmark.read_result(FIXTURE, case_id="synthetic", case_digest="synthetic",
                                   screens={"top_tension_kn": 100},
                                   criteria={"top_tension_kn": criterion})
    assert result["top_tension_kn"]["status"] == "NOT_EVALUATED"
    assert result["top_tension_kn"]["difference_pct"] == 10


def test_no_licensed_binding_imported_by_report_build(tmp_path):
    """A fresh-process import guard forbids the licensed module before imports.

    This checks the complete report path without checking availability or
    attempting to load the actual binding. A denied attempt fails the test.
    """
    import subprocess
    import sys

    program = r'''
import importlib.abc
import runpy
import sys
class ForbidLicensedBinding(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname == "OrcFxAPI" or fullname.startswith("OrcFxAPI."):
            raise AssertionError("Licensed binding import prohibited")
sys.meta_path.insert(0, ForbidLicensedBinding())
sys.argv = ["scripts/reporting/build_drilling_riser_benchmark.py", "--output", sys.argv[1]]
runpy.run_path(sys.argv[0], run_name="__main__")
assert "OrcFxAPI" not in sys.modules
'''
    subprocess.run([sys.executable, "-c", program, str(tmp_path)], cwd=ROOT, check=True)


def test_out_of_range_json_integer_is_not_evaluated(tmp_path):
    record = json.loads(FIXTURE.read_text())
    record["metrics"]["top_tension_kn"]["value"] = 10 ** 400
    path = tmp_path / "result.json"
    path.write_text(json.dumps(record))
    result = benchmark.read_result(path, case_id="synthetic", case_digest="synthetic",
                                   screens={"top_tension_kn": 100}, criteria={})
    assert result["top_tension_kn"]["status"] == "NOT_EVALUATED"
    assert result["top_tension_kn"]["value"] is None
