"""Offline pipeline comparison against the existing validation records."""

import pytest

from digitalmodel.asset_integrity.pipeline_defect_screen import assess, render_report


@pytest.fixture
def reference():
    return dict(
        component_id="PIPE-REFERENCE",
        nominal_od_in=30.0,
        nominal_wt_in=0.375,
        smys_psi=52000.0,
        smts_psi=66700.0,
        design_pressure_psi=800.0,
        axial_stress_psi=10000.0,
        safety_factor=1.39,
        usage_factor=0.72,
        length_confirmed=True,
        axial_design_factor=0.72,
        axial_positions_in=[0.0, 4.0, 8.0],
        circumferential_width_in=4.0,
        grid=[[0.225, 0.3], [0.225, 0.275], [0.225, 0.3]],
    )


def test_reference_pressures_and_governing_demand(reference):
    result = assess(reference)
    rows = {row["method"]: row for row in result["methods"]}
    for method, expected in [
        ("B31G", 1183),
        ("Modified_B31G", 1219),
        ("DNV-RP-F101", 1334),
    ]:
        assert rows[method]["capacity_pressure_psi"] == pytest.approx(expected, abs=1)
    assert len(rows) == 6
    assert all("applicability" in row for row in rows.values())
    governing = min(result["methods"], key=lambda row: row["margin"])
    assert result["governing_method"] == governing["method"]
    assert result["decision"]["margin"] == pytest.approx(governing["margin"])


def test_depth_extrapolation_escalates_and_report_marks_methods(reference):
    reference["grid"][1][0] = 0.03
    result = assess(reference)
    assert result["decision"]["verdict"] == "ESCALATE"
    assert sum(not row["applicability"]["ok"] for row in result["methods"]) >= 5
    assert "OUTSIDE APPLICABILITY" in render_report(reference, result)


def test_report_links_four_records_and_separates_units(reference):
    report = render_report(reference, assess(reference))
    for record in [
        "b31g-validation-2026-06-27.md",
        "rstreng-2d-validation-2026-06-29.md",
        "circumferential-defect-validation-2026-06-29.md",
        "ffs-validation-record-2026-06-27.md",
    ]:
        assert record in report
    assert "axial stress" in report
    assert "capacity pressure" in report
    assert "Level 1 Assessment" not in report


@pytest.mark.parametrize(
    "key,value",
    [
        ("smys_psi", float("nan")),
        ("smts_psi", -1),
        ("design_pressure_psi", 0),
        ("axial_stress_psi", float("inf")),
        ("safety_factor", 0.5),
        ("usage_factor", 1.2),
        ("axial_design_factor", 1.2),
        ("grid", [[0.3, float("nan")]] * 3),
        ("grid", [[0.3], [0.3, 0.2], [0.3]]),
        ("axial_positions_in", [0, 0, 8]),
    ],
)
def test_invalid_input_rejected(reference, key, value):
    reference[key] = value
    with pytest.raises(ValueError):
        assess(reference)


def test_circumferential_extent_flag_and_legacy_2d_bool(reference):
    from digitalmodel.asset_integrity.circumferential_defect import (
        api579_part5_circumferential_netsection,
    )
    from digitalmodel.asset_integrity.rstreng_2d import rstreng_2d_river_bottom

    circ = api579_part5_circumferential_netsection(29.625, 0.375, 0.15, 20, 62000, s=8)
    assert circ.applicability.ok
    assert circ.level1_screen_ok is False
    through_wall = api579_part5_circumferential_netsection(
        29.625, 0.375, 0.375, 20, 62000, s=8
    )
    assert not through_wall.applicability.ok
    result = rstreng_2d_river_bottom(30, 0.375, [0, 8], [[0.34], [0.34]], 52000)
    assert result.within_applicability == result.applicability.ok == False


def test_html_escapes_component_identifier(reference):
    reference["component_id"] = "<script>alert(1)</script>"
    assert "<script>alert(1)</script>" not in render_report(
        reference, assess(reference)
    )


def test_report_does_not_claim_part5_qualification(reference):
    report = render_report(reference, assess(reference))
    assert "API 579-1/ASME FFS-1 2021 Edition criteria are applied." not in report
    assert "Safety factor" in report
    assert "30.000" in report


def test_axial_demand_can_govern(reference):
    reference["axial_stress_psi"] = 100000
    result = assess(reference)
    assert result["governing_method"] == "Circumferential net-section"
    assert result["decision"]["margin"] < 1


def test_river_bottom_record_golden_and_circumferential_closed_form(reference):
    reference.update(
        nominal_od_in=24.0,
        nominal_wt_in=0.5,
        axial_positions_in=[0, 2, 4, 6, 8],
        grid=[
            [0.5, 0.5, 0.5],
            [0.4, 0.3, 0.45],
            [0.35, 0.2, 0.4],
            [0.45, 0.25, 0.5],
            [0.5, 0.5, 0.5],
        ],
    )
    rows = {row["method"]: row for row in assess(reference)["methods"]}
    for name in ["RSTRENG", "RSTRENG-2D"]:
        assert rows[name]["capacity_pressure_psi"] == pytest.approx(1969.2, abs=0.1)
    import math

    expected = 0.72 * 52000 * (1 - (0.3 / 0.5) * 4 / (math.pi * 23.5))
    assert rows["Circumferential net-section"][
        "allowable_axial_stress_psi"
    ] == pytest.approx(expected)


@pytest.mark.parametrize("thickness", [0.0, 0.375])
def test_through_wall_and_intact_grids_are_explicit(reference, thickness):
    import json

    reference["grid"] = [[thickness, thickness]] * 3
    result = assess(reference)
    json.dumps(result, allow_nan=False)
    assert result["decision"]["remaining_life_yr"] is None
    if thickness == 0:
        assert result["decision"]["verdict"] == "ESCALATE"
    else:
        assert result["decision"]["verdict"] == "ACCEPT"


def test_ratio_screen_has_no_rsf_or_replacement_verdict(reference):
    import json

    result = assess(reference)
    assert result["governing_method"] == "RSTRENG"
    assert result["decision"]["verdict"] == "ACCEPT"
    assert "RSF" not in result["decision"]["governing_criterion"]
    assert "rsf" not in result["decision"]
    reference["axial_stress_psi"] = 100000
    result = assess(reference)
    assert result["decision"]["verdict"] == "DERATE"
    assert result["decision"]["action"] == "REDUCE AXIAL STRESS"
    assert result["decision"]["rerated_mawp_psi"] is None
    json.dumps(result, allow_nan=False)


def test_flagged_result_has_no_qualified_derating(reference):
    reference["grid"][1][0] = 0.03
    decision = assess(reference)["decision"]
    assert decision["verdict"] == "ESCALATE"
    assert decision["derated"] is None
    assert decision["rerated_mawp_psi"] is None
    assert decision["demand_limits_psi"] is None


def test_router_preserves_input_and_distinct_reports(reference, tmp_path):
    from digitalmodel.asset_integrity.pipeline_defect_screen import router

    paths = []
    for stem in ["input", "synthetic"]:
        cfg = dict(
            basename="pipeline_defect_screen",
            pipeline_defect_screen=reference,
            Analysis=dict(result_folder=str(tmp_path), file_name=stem),
        )
        result = router(cfg)["pipeline_defect_screen"]
        assert result["inputs"]["grid"] == reference["grid"]
        assert result["inputs"]["safety_factor"] == reference["safety_factor"]
        paths.append(tmp_path / result["report_file"])
    assert paths[0] != paths[1]
    assert all(path.exists() for path in paths)


@pytest.mark.parametrize(
    "key,value",
    [("component_id", ""), ("smys_psi", "X52"), ("smys_psi", None), ("smys_psi", True)],
)
def test_missing_or_mistyped_fields_are_explicit(reference, key, value):
    reference[key] = value
    with pytest.raises(ValueError):
        assess(reference)


def test_both_exceeded_demands_require_both_limits(reference):
    reference.update(design_pressure_psi=10000, axial_stress_psi=100000)
    result = assess(reference)
    assert result["decision"]["action"] == "REDUCE PRESSURE AND AXIAL STRESS"
    assert result["decision"]["demand_limits_psi"]["pressure"] < 10000
    assert result["decision"]["demand_limits_psi"]["axial_membrane_stress"] < 100000


@pytest.mark.parametrize("key", ["grid", "axial_positions_in"])
def test_missing_grid_fields_raise_value_error(reference, key):
    del reference[key]
    with pytest.raises(ValueError):
        assess(reference)


def test_grid_strings_are_not_measurements(reference):
    reference["grid"] = [["0.3", "0.3"]] * 3
    with pytest.raises(ValueError):
        assess(reference)


def test_boundary_loss_is_disclosed(reference):
    result = assess(reference)
    assert any("boundary" in note for note in result["limitations"])
    assert "boundary" in render_report(reference, result)


def test_area_weighted_property_preserves_projected_profile_contract():
    from digitalmodel.asset_integrity.rstreng_2d import rstreng_2d_river_bottom

    result = rstreng_2d_river_bottom(
        24, 0.5, [0, 8], [[0.475, 0.025]] * 2, 52000, projection="area_weighted"
    )
    # Existing sensitivity mode checks its averaged profile, not the raw pit.
    assert result.within_applicability is result.applicability.ok is True
    assert result.applicability is result.result.applicability


@pytest.mark.parametrize("filename", ["input.yml", "synthetic.yml"])
def test_committed_examples_offline(filename):
    from pathlib import Path
    from digitalmodel.engine import engine

    root = Path(__file__).resolve().parents[2]
    cfg = engine(
        inputfile=str(
            root / "examples/workflows/pipeline-corroded-defect-screen" / filename
        )
    )
    result = cfg["pipeline_defect_screen"]
    report = Path(cfg["Analysis"]["result_folder"]) / result["report_file"]
    assert report.exists()
    assert result["inputs"]["data_origin"].startswith("synthetic")
