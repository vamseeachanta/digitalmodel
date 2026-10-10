"""Regression cases for the PR 2338 review; synthetic inputs only."""

from copy import deepcopy
from pathlib import Path

import pytest
import yaml

from digitalmodel.asset_integrity.dnv_rp_f101 import dnv_f101_single_defect
from digitalmodel.asset_integrity.pipeline_defect_screen import assess, render_report


@pytest.fixture
def reference():
    path = (
        Path(__file__).resolve().parents[2]
        / "examples/workflows/pipeline-corroded-defect-screen/input.yml"
    )
    inputs = yaml.safe_load(path.read_text())["pipeline_defect_screen"]
    inputs["length_confirmed"] = True
    return inputs


@pytest.mark.parametrize("factor", [0.5, 0.648, 0.72, 0.9])
def test_screen_matches_validated_dnv_engine(reference, factor):
    reference["usage_factor"] = factor
    row = next(
        row for row in assess(reference)["methods"] if row["method"] == "DNV-RP-F101"
    )
    engine = dnv_f101_single_defect(
        30, 0.375, 0.15, 8, 66700, operational_usage_factor=factor
    )
    assert row["capacity_pressure_psi"] == pytest.approx(
        engine.capacity_pressure_psi, rel=1e-12
    )
    assert row["allowable_pressure_psi"] == pytest.approx(
        engine.allowable_pressure_psi, rel=1e-12
    )


def test_report_labels_part_b_and_factors(reference):
    report = render_report(reference, assess(reference))
    assert "Part B safe working pressure" in report
    for label, value in [
        ("Modelling factor F1 (1)", "0.900"),
        ("Operational usage factor F2 (1)", "0.720"),
        ("Total usage factor F = F1 x F2 (1)", "0.648"),
    ]:
        assert f"<td>{label}</td><td>{value}</td>" in report
    assert "No separate modelling factor is applied" not in report


@pytest.mark.parametrize("factor", [-0.1, 0, 1.01])
def test_screen_rejects_invalid_operational_factor(reference, factor):
    reference["usage_factor"] = factor
    with pytest.raises(ValueError, match="operational usage factor F2"):
        assess(reference)


def test_dnv_governs_derate_between_old_and_corrected_limits(reference):
    reference.update(usage_factor=0.5, design_pressure_psi=630)
    result = assess(reference)
    assert result["governing_method"] == "DNV-RP-F101"
    assert result["decision"]["verdict"] == "DERATE"
    assert result["decision"]["demand_limits_psi"]["pressure"] == pytest.approx(
        0.9 * 0.5 * 1334.1940092246919, rel=1e-12
    )


def test_report_discloses_above_nominal_rejection(reference):
    assert "Above-nominal readings are rejected" in render_report(
        reference, assess(reference)
    )


def test_above_nominal_readings_rejected_without_modifying_input(reference):
    reference["grid"][1][1] = 0.4
    original = deepcopy(reference)
    with pytest.raises(ValueError, match="thickness in"):
        assess(reference)
    assert reference == original


@pytest.mark.parametrize("confirmation", [None, False])
def test_unconfirmed_boundary_loss_escalates(reference, confirmation):
    reference.pop("length_confirmed", None)
    if confirmation is not None:
        reference["length_confirmed"] = confirmation
    result = assess(reference)
    assert result["decision"]["verdict"] == "ESCALATE"
    assert "DEFECT_LENGTH_UNCONFIRMED" in result["applicability"]["flags"]
    assert all(not row["applicability"]["ok"] for row in result["methods"])
    assert result["decision"]["demand_limits_psi"] is None
    assert "allowable/demand=" not in result["decision"]["governing_criterion"]


@pytest.mark.parametrize("confirmation", ["true", 1, None])
def test_length_confirmation_requires_boolean(reference, confirmation):
    reference["length_confirmed"] = confirmation
    with pytest.raises(ValueError, match="length_confirmed"):
        assess(reference)


def test_confirmed_boundary_preserves_reference_pressures(reference):
    result = assess(reference)
    assert result["decision"]["verdict"] == "ACCEPT"
    assert any(
        "full defect length is caller confirmed." in note
        for note in result["limitations"]
    )
    dnv = next(row for row in result["methods"] if row["method"] == "DNV-RP-F101")
    assert dnv["capacity_pressure_psi"] == pytest.approx(1334.1940092246919, rel=1e-12)
    assert dnv["allowable_pressure_psi"] == pytest.approx(
        0.9 * 0.72 * 1334.1940092246919, rel=1e-12
    )


def test_invalid_method_numbers_withheld_from_report(reference):
    reference["grid"][1][0] = 0.0
    result = assess(reference)
    report = render_report(reference, result)
    assert any(not row["applicability"]["ok"] for row in result["methods"])
    for row in result["methods"]:
        if not row["applicability"]["ok"]:
            label = row["method"] + (
                " — Part B safe working pressure"
                if row["method"] == "DNV-RP-F101"
                else ""
            )
            cells = report.split(f"<td>{label}</td>", 1)[1].split("</tr>", 1)[0]
            assert cells.count("not evaluated") == 4
            assert f"<td>{row['demand_psi']:.3f}</td>" in cells
    assert "allowable/demand=" not in result["decision"]["governing_criterion"]


def test_river_bottom_catalog_row_live_with_max_scope():
    from digitalmodel.asset_integrity.offering_catalog import load, render_markdown

    catalog = load()
    row = next(
        row
        for row in catalog.rows
        if row.mechanism == "river-bottom profile metal loss"
    )
    assert row.status == "live"
    assert "MAX" in render_markdown(catalog)
    assert "No row is `live` today" not in render_markdown(catalog)


def test_above_nominal_boundary_requires_input_correction(reference):
    reference.pop("length_confirmed")
    reference["grid"][0] = [0.4, 0.4]
    with pytest.raises(ValueError, match="thickness in"):
        assess(reference)


def test_report_reads_applied_factors_from_computed_row(reference):
    result = assess(reference)
    row = next(row for row in result["methods"] if row["method"] == "DNV-RP-F101")
    assert row["pressure_basis"] == {
        "modelling_factor": 0.9,
        "operational_usage_factor": 0.72,
        "total_usage_factor": pytest.approx(0.648),
        "result_label": "Part B safe working pressure",
    }
    reference["usage_factor"] = 0.5
    report = render_report(reference, result)
    assert "<td>Operational usage factor F2 (1)</td><td>0.720</td>" in report
    assert "<td>Total usage factor F = F1 x F2 (1)</td><td>0.648</td>" in report


def test_report_reads_geometry_from_saved_assessment(reference):
    result = assess(reference)
    reference["nominal_od_in"] = 99.0
    report = render_report(reference, result)
    assert "<td>OD (in)</td><td>30.000</td>" in report
