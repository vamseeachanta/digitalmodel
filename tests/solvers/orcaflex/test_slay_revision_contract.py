"""Regression contracts for the reviewed S-lay input correction; no solver."""
import json
from copy import deepcopy
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import _NoAliasDumper
from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.lines_builder import LinesBuilder

ROOT = Path(__file__).resolve().parents[3]
CAMPAIGN = ROOT / "docs/domains/pipeline-installation/slay-campaign"
KEY = "ConnectionxBendingStiffness, ConnectionyBendingStiffness"


def case_spec(case_id):
    import importlib.util
    spec = importlib.util.spec_from_file_location(
        "prepare_revision", ROOT / "scripts/orcaflex/prepare_slay_campaign.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    table = yaml.safe_load((CAMPAIGN / "case-table.yml").read_text(encoding="utf-8"))
    case = next(row for row in table["cases"] if row["id"] == case_id)
    return module.case_spec(table["basis"], case)


def assert_end_form(line):
    assert line["IncludeTorsion"] == "No"
    assert line[KEY] == [[], [0, None]]
    assert not any("TwistingStiffness" in key for key in line)


def test_saved_end_form_and_compact_rows():
    line = LinesBuilder(case_spec("d0-t0"), BuilderContext()).build()["Lines"][0]
    assert_end_form(line)
    assert "- []\n" in yaml.dump(line, Dumper=_NoAliasDumper)
    assert "- [0, ~]\n" in yaml.dump(line, Dumper=_NoAliasDumper)
    bad = deepcopy(line)
    bad["IncludeTorsion"] = "Yes"
    with pytest.raises(AssertionError):
        assert_end_form(bad)
    bad = deepcopy(line)
    bad[KEY] = [[], [0, 0, 0, 0]]
    with pytest.raises(AssertionError):
        assert_end_form(bad)


@pytest.mark.parametrize("case_id,arc,end_b", [
    ("d0-t0", 60, -2024.8), ("d0-t1", 60, -2025.0),
    ("d0-t2", 60, -2025.2), ("d1-t0", 90, -2012.2),
    ("d1-t1", 90, -2013.9), ("d1-t2", 90, -2015.3),
])
def test_declared_layout_and_anchor(case_id, arc, end_b):
    from digitalmodel.solvers.orcaflex.modular_generator.builders.vessel_builder import VesselBuilder
    from digitalmodel.solvers.orcaflex.modular_generator.builders.winch_builder import WinchBuilder
    spec = case_spec(case_id)
    stinger = spec.equipment.stinger
    assert sum(s.length for s in stinger.sections) == arc
    assert [r.arc_length for r in stinger.rollers] == list(range(0, arc + 1, 5))
    assert stinger.firing_line_length == 60
    line = LinesBuilder(spec, BuilderContext()).build()["Lines"][0]
    top, bottom = next(v for k, v in line.items() if k.startswith("Connection, "))
    assert top[1:4] == pytest.approx([-31.1, 0, 0])
    assert bottom[1:4] == pytest.approx([end_b, 0, 0.485])
    vessel = VesselBuilder(spec, BuilderContext()).build()["Vessels"][0]
    supports = vessel["Supports"]
    assert len(supports) == len(stinger.rollers) + 12
    firing = supports[:12]
    assert [r["SupportPosition"][0] for r in firing] == pytest.approx(
        [-31.1 - 5*k for k in range(12)])
    assert all(r["SupportPosition"][2] == -1.485 for r in firing)
    assert all(r["SupportType"] == "Support type2" for r in firing)
    winch = WinchBuilder(spec, BuilderContext()).build()["Winches"][0]
    assert winch["Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo"][0][1:4] == pytest.approx([-21.1, 0, 0])


def test_empty_stiffness_table_serializes_without_indexing():
    assert yaml.safe_load(yaml.dump({KEY: []}, Dumper=_NoAliasDumper)) == {KEY: []}


def test_screen_is_labelled_and_preparation_is_reproducible(tmp_path):
    import importlib.util
    spec = importlib.util.spec_from_file_location(
        "prepare_reproducible", ROOT / "scripts/orcaflex/prepare_slay_campaign.py")
    driver = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(driver)
    first = driver.prepare(CAMPAIGN, tmp_path / "first")
    second = driver.prepare(CAMPAIGN, tmp_path / "second")
    assert first == second
    assert all(c["screen"]["overbend_strain_basis"] == "D/(2R) screen" for c in first["cases"])
    assert all(c["preparation_wall_seconds"] is None for c in first["cases"])


def test_conformity_is_separate_from_design_acceptance():
    criteria = json.loads((CAMPAIGN / "criteria.json").read_text())["criteria"]
    s1 = criteria["stinger_conformity"]
    assert s1["status"] == "defined"
    assert s1["numeric_limit"] == pytest.approx(1.10 * 0.762 / (2 * 150))
    assert s1["last_support_force_kn"] == 0
    assert criteria["overbend_strain_limit"]["status"] == "not established"
    assert "DNV-ST-F101" in str(criteria["overbend_strain_limit"])
    assert "concrete" in str(criteria["overbend_strain_limit"]).lower()


def test_anchor_span_probes_are_retired():
    from digitalmodel.solvers.orcaflex.modular_generator import slay_probe
    assert not hasattr(slay_probe, "prepare_anchor_span_probe")
    assert not list((CAMPAIGN / "probes").glob("*-anchor-span.*"))


@pytest.mark.parametrize("field,bad", [
    ("wire_length", 4.9), ("wire_length", 15.1),
    ("end_a_reaction", -0.1), ("last_support_force", 0.1),
    ("lift_off_arc", 61), ("departure_angle", 25),
    ("grounded_tension", [-0.1]), ("overbend_strain", 0.0028),
    ("wire_length", float("nan")),
])
def test_static_qa_table_rejects_each_bad_reading(field, bad):
    from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import assert_static_acceptance
    reference = dict(lift_off_arc=55, departure_angle=21)
    readings = dict(wire_length=10, end_a_reaction=11, last_support_force=0,
                    grounded_tension=[0, 1], overbend_strain=0.00267, **reference)
    assert_static_acceptance(reference=reference, **readings)
    readings[field] = bad
    with pytest.raises(ValueError):
        assert_static_acceptance(reference=reference, **readings)


@pytest.mark.parametrize("wire", [5, 15])
def test_static_qa_accepts_wire_band_edges(wire):
    from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import assert_static_acceptance
    reference = dict(lift_off_arc=55, departure_angle=21)
    assert_static_acceptance(reference=reference, wire_length=wire, end_a_reaction=0,
                             last_support_force=0, grounded_tension=[0],
                             overbend_strain=1.10 * 0.762 / 300, **reference)


def qa_readings():
    return dict(wire_length=10, end_a_reaction=11, last_support_force=0,
                grounded_tension=[0, 1], overbend_strain=0.00267,
                lift_off_arc=55, departure_angle=21,
                touchdown_x=None, sag_bend_strain=None,
                reference=dict(lift_off_arc=55, departure_angle=21))


@pytest.mark.parametrize("row", ["touchdown_x", "sag_bend_strain", "stiff_closure"])
def test_pending_table_b_rows_are_not_evaluated(row):
    from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import evaluate_static_acceptance
    result = evaluate_static_acceptance(**qa_readings())
    assert result["rows"][row] == {
        "status": "NOT_EVALUATED", "reason": "published reference pending, #2608"}


def test_partial_acceptance_does_not_count_pending_rows_as_passes():
    from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import assert_static_acceptance
    result = assert_static_acceptance(**qa_readings())
    assert result["status"] == "accepted with 3 rows not evaluated"
    assert result["fully_accepted"] is False
    assert result["counts"] == {"passed": 6, "failed": 0, "not_evaluated": 3}
    assert sum(result["counts"].values()) == len(result["rows"]) == 9


@pytest.mark.parametrize("value", [None, -999, float("nan")])
def test_pending_rows_cannot_change_evaluated_outcomes(value):
    from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import evaluate_static_acceptance
    readings = qa_readings()
    readings.update(touchdown_x=value, sag_bend_strain=value)
    readings["reference"].update(touchdown_x=value, sag_bend_strain=value, stiff_excess_m=value)
    result = evaluate_static_acceptance(**readings)
    assert result["counts"] == {"passed": 6, "failed": 0, "not_evaluated": 3}


@pytest.mark.parametrize("field,bad", [
    ("wire_length", 4.9), ("wire_length", 15.1),
    ("end_a_reaction", -0.1), ("last_support_force", 0.1),
    ("lift_off_arc", 61), ("departure_angle", 25),
    ("grounded_tension", [-0.1]), ("overbend_strain", 0.0028),
])
def test_pending_rows_do_not_hide_failed_evaluated_rows(field, bad):
    from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import evaluate_static_acceptance
    readings = qa_readings()
    readings[field] = bad
    result = evaluate_static_acceptance(**readings)
    assert result["status"] == "rejected"
    assert result["fully_accepted"] is False
    assert result["counts"] == {"passed": 5, "failed": 1, "not_evaluated": 3}
    assert sum(result["counts"].values()) == len(result["rows"])


@pytest.mark.parametrize("missing", ["lift_off_arc", "departure_angle"])
def test_evaluated_rows_still_require_reference(missing):
    from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import assert_static_acceptance
    readings = qa_readings()
    readings["reference"].pop(missing)
    with pytest.raises(ValueError, match="reference"):
        assert_static_acceptance(**readings)


def test_criteria_and_cases_explicitly_record_pending_rows():
    criteria = json.loads((CAMPAIGN / "criteria.json").read_text())
    pending = {"touchdown_x", "sag_bend_strain", "stiff_closure"}
    for name in pending:
        assert criteria["static_qa"][name]["status"] == "NOT_EVALUATED"
        assert criteria["static_qa"][name]["reason"] == "published reference pending, #2608"
    table = yaml.safe_load((CAMPAIGN / "case-table.yml").read_text())
    for case in table["cases"]:
        assert case["stiff_reference"]["stiff_excess_m"] is None
        assert set(case["stiff_reference"]["not_evaluated"]) == pending
        for row in case["stiff_reference"]["not_evaluated"].values():
            assert row == {"status": "NOT_EVALUATED",
                           "reason": "published reference pending, #2608"}
        assert case["stiff_reference"]["reference_status"] == "published reference pending, #2608"


def test_declared_anchor_does_not_invoke_weight_only_seed(monkeypatch):
    def reject_fallback(*args):
        raise AssertionError("Declared anchor must take precedence over the bound")
    monkeypatch.setattr(LinesBuilder, "_slay_taut_span", reject_fallback)
    LinesBuilder(case_spec("d0-t0"), BuilderContext()).build()


@pytest.mark.parametrize("field", ["firing_line_length", "firing_line_spacing"])
def test_firing_geometry_rejects_infinite_lengths(field):
    from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import Stinger
    with pytest.raises(ValueError):
        Stinger(radius=150, **{field: float("inf")})


def test_queue_requires_the_conformity_criterion(tmp_path):
    import shutil
    from digitalmodel.solvers.orcaflex.modular_generator.slay_execution import read_contract
    read_contract(CAMPAIGN, 1, 2)
    for name in ("case-table.yml", "criteria.json", "first-timed-case.json", "manifest.template.json"):
        shutil.copyfile(CAMPAIGN / name, tmp_path / name)
    path = tmp_path / "criteria.json"
    data = json.loads(path.read_text())
    del data["criteria"]["stinger_conformity"]
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError, match="criteria"):
        read_contract(tmp_path, 1, 2)


@pytest.mark.parametrize("row", ["Touchdown x", "Sag-bend strain", "Stiff closure"])
def test_analysis_lists_pending_rows_and_revised_geometry(row):
    report = (CAMPAIGN / "analysis.html").read_text()
    assert f"<td>{row}</td><td>NOT_EVALUATED</td>" in report
    assert "published reference pending, #2608" in report
    assert "accepted with 3 rows not evaluated" in report
    assert "60.0 m firing line" in report
    assert "Depth is the declared stinger section length times" not in report
    assert '<tr><td>2</td><td>2026-10-16</td>' in report
