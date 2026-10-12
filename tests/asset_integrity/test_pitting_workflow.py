"""Offline pitting workflow: formula anchors, input guards, router and report."""
import math
from pathlib import Path

import pandas as pd
import pytest

from digitalmodel.asset_integrity.assessment import pitting
from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter


def pitting_cfg():
    grid = [[0.5] * 8 for _ in range(8)]
    for row, col in [(1, 1), (1, 3), (3, 1), (3, 3), (3, 5)]:
        grid[row][col] = 0.3
    return {"basename": "api579_pitting_screen", "pitting_assessment": {
        "component_id": "SYNTHETIC-PITTING", "nominal_od_in": 24.0,
        "nominal_wt_in": 0.5, "t_min_in": 0.35, "row_spacing_in": 1.0,
        "grid": grid}}


def test_pitted_grid_routes_and_workflow_returns_formula_anchor():
    cfg = pitting_cfg()
    route = FFSRouter.classify(pd.DataFrame(cfg["pitting_assessment"]["grid"]),
                               24.0, 0.5)
    assert route["assessment_type"] == "PITTING"
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert result["characterization"]["pit_count"] == 5
    assert result["level1"]["verdict"] == "FAIL_LEVEL_1"
    # Independent Part 5 equivalent-LTA closed-form anchor (L = 3 in).
    folias = math.sqrt(1 + 0.48 * (1.285 * 3 / math.sqrt((24 - 2 * 0.35) * 0.35)) ** 2)
    rt = 0.3 / 0.35
    expected = rt / (1 - (1 - rt) / folias)
    assert result["level2"]["rsf"] == pytest.approx(expected)
    assert "equivalent" in result["report_html"]
    assert "not evaluated" in result["report_html"].lower()
    assert "Part 6" in result["report_html"]
    assert "cdn.plot" not in result["report_html"]


def test_future_allowance_cannot_be_bypassed_by_level2():
    cfg = pitting_cfg()
    cfg["pitting_assessment"]["fca_in"] = 0.29
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert result["level1"]["verdict"] == "FAIL_LEVEL_1"
    assert result["decision"]["verdict"] not in {"ACCEPT", "MONITOR"}


@pytest.mark.parametrize("bad_grid", [[], [[float("nan")]], [[-0.1]], [[0.6]]])
def test_invalid_measurements_rejected(bad_grid):
    cfg = pitting_cfg()
    cfg["pitting_assessment"]["grid"] = bad_grid
    with pytest.raises(ValueError):
        pitting.router(cfg)


def test_report_write_and_escaping(tmp_path):
    cfg = pitting_cfg()
    cfg["pitting_assessment"].update(component_id="<script>x</script>",
                                    output_dir=str(tmp_path))
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert Path(result["report_path"]).read_text() == result["report_html"]
    assert "<script>" not in result["report_html"]
    assert "&lt;script&gt;" in result["report_html"]


def test_sound_padding_does_not_dilute_pitted_population():
    cfg = pitting_cfg()
    result = pitting.router(cfg)["api579_pitting_screen"]
    padded = copy_cfg = pitting_cfg()
    copy_cfg["pitting_assessment"]["grid"] += [[0.5] * 8] * 4
    second = pitting.router(padded)["api579_pitting_screen"]
    assert second["level2"]["rsf"] == result["level2"]["rsf"]
    assert second["decision"]["verdict"] == result["decision"]["verdict"]


@pytest.mark.parametrize("key,value", [("rt_min", 0.01), ("rsf_a", 0.1)])
def test_relaxed_screening_defaults_rejected(key, value):
    cfg = pitting_cfg()
    cfg["pitting_assessment"][key] = value
    with pytest.raises(ValueError):
        pitting.router(cfg)


def test_through_wall_after_fca_escalates_without_strength():
    cfg = pitting_cfg()
    cfg["pitting_assessment"]["fca_in"] = 0.31
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert result["decision"]["verdict"] == "ESCALATE"
    assert result["level2"]["rsf"] is None


def test_pipeline_catalog_delivers_workflow_without_claiming_published_validation():
    from digitalmodel.asset_integrity.offering_catalog import load

    cat = load()
    entries = cat.ffs["engines"]
    assert entries["pitting"]["status"] == "workflow"
    assert entries["dents"]["status"] == "workflow"
    assert any(row.status == "workflow" and "pitting" in row.engines for row in cat.rows)


def test_nonuniform_pits_cannot_be_accepted_from_their_mean():
    cfg = pitting_cfg()
    grid = cfg["pitting_assessment"]["grid"]
    for row, col in [(1, 1), (1, 3), (3, 1), (3, 3), (3, 5)]:
        grid[row][col] = 0.44
    grid[1][1] = 0.11
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert result["decision"]["verdict"] == "ESCALATE"
    assert "nonuniform" in result["decision"]["governing_criterion"].lower()


def test_fca_deducted_once_from_finite_strength_estimate():
    cfg = pitting_cfg()
    cfg["pitting_assessment"]["fca_in"] = 0.02
    result = pitting.router(cfg)["api579_pitting_screen"]
    folias = math.sqrt(1 + 0.48 * (1.285 * 3 / math.sqrt(23.3 * 0.35)) ** 2)
    rt = 0.28 / 0.35
    assert result["level2"]["rsf"] == pytest.approx(rt / (1 - (1 - rt) / folias))
    assert result["decision"]["verdict"] == "MONITOR"
    assert "life 0.0" not in result["report_html"]


@pytest.mark.parametrize("wall,minimum,pitch,verdict", [
    (0.4, 0.35, 1.0, "ACCEPT"), (0.3, 0.35, 1.0, "ACCEPT"),
    (0.3, 0.35, 10.0, "ESCALATE"), (0.11, 0.35, 10.0, "ESCALATE"),
])
def test_pitting_shared_bands(wall, minimum, pitch, verdict):
    cfg = pitting_cfg()
    block = cfg["pitting_assessment"]
    block.update(t_min_in=minimum, row_spacing_in=pitch)
    block["grid"] = [[wall if cell == 0.3 else cell for cell in row] for row in block["grid"]]
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert result["decision"]["verdict"] == verdict


@pytest.mark.parametrize("fca,minimum", [(0.0, 0.47), (0.1, 0.38)])
def test_pit_free_grid_cannot_substitute_nominal_wall(fca, minimum):
    cfg = pitting_cfg()
    cfg["pitting_assessment"].update(grid=[[0.46]*4 for _ in range(4)],
                                    fca_in=fca, t_min_in=minimum)
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert result["decision"]["verdict"] == "ESCALATE"
    assert result["level2"]["rsf"] is None


def test_single_axial_row_preserves_physical_pitch_in_lta():
    cfg = pitting_cfg()
    cfg["pitting_assessment"].update(grid=[[0.3, 0.5, 0.3]], row_spacing_in=10.0)
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert result["level2"]["flaw_length_in"] == 10.0
    assert result["level2"]["equivalent_lta"]["axial_extent_in"] == 10.0


def test_report_output_uses_config_directory(tmp_path):
    cfg = pitting_cfg()
    cfg["_config_dir_path"] = str(tmp_path)
    cfg["pitting_assessment"]["output_dir"] = "results"
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert Path(result["report_path"]).parent == tmp_path / "results"


@pytest.mark.parametrize("pitch,fca,l2_verdict,decision", [
    (1.0, 0.0, "ACCEPT", "ACCEPT"),
    (1.0, 0.02, "ACCEPT", "MONITOR"),
    (10.0, 0.0, "FAIL_LEVEL_2", "ESCALATE"),
])
def test_level2_rsf_governs_after_level1_mean_wall_failure(pitch, fca, l2_verdict, decision):
    cfg = pitting_cfg()
    cfg["pitting_assessment"].update(row_spacing_in=pitch, fca_in=fca)
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert result["level1"]["verdict"] == "FAIL_LEVEL_1"
    assert result["level1"]["deep_pit_criterion_pass"] is True
    assert result["level2"]["verdict"] == l2_verdict
    assert (result["level2"]["rsf"] >= 0.9) == (l2_verdict == "ACCEPT")
    assert result["decision"]["verdict"] == decision


def test_report_does_not_claim_a_qualified_pitting_bound():
    result = pitting.router(pitting_cfg())["api579_pitting_screen"]
    assert "conservative bounding practice" not in result["report_html"]
    assert "published-case qualification are not established" in result["level2"]["assessment_basis"]


def test_level2_accepts_at_allowable_and_escalates_above_it():
    cfg = pitting_cfg()
    initial = pitting.router(cfg)["api579_pitting_screen"]
    rsf = initial["level2"]["rsf"]
    cfg["pitting_assessment"]["rsf_a"] = rsf
    assert pitting.router(cfg)["api579_pitting_screen"]["decision"]["verdict"] == "MONITOR"
    cfg["pitting_assessment"]["rsf_a"] = rsf + 1e-6
    assert pitting.router(cfg)["api579_pitting_screen"]["decision"]["verdict"] == "ESCALATE"


def test_pitting_result_marks_published_case_qualification_unestablished():
    result = pitting.router(pitting_cfg())["api579_pitting_screen"]
    assert result["qualification"] == "synthetic_formula_anchor_only"


def test_short_field_cannot_bypass_deepest_ligament_floor():
    cfg = pitting_cfg()
    block = cfg["pitting_assessment"]
    block["grid"] = [[0.09 if cell == 0.3 else cell for cell in row] for row in block["grid"]]
    block["row_spacing_in"] = 0.1
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert result["level2"]["rsf"] >= 0.9
    assert result["decision"]["verdict"] == "ESCALATE"
    assert "pitting.deep_ligament" in result["decision"]["applicability"]["flags"]


@pytest.mark.parametrize("fca", [0.0, 0.02])
def test_passing_disposition_carries_qualification_limit(fca):
    cfg = pitting_cfg()
    cfg["pitting_assessment"]["fca_in"] = fca
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert result["decision"]["verdict"] in {"ACCEPT", "MONITOR"}
    assert "Synthetic formula screening only" in result["decision"]["governing_criterion"]
    assert "measured-component acceptance are not established" in result["report_html"]


@pytest.mark.parametrize("background,minimum,fca", [(0.46, 0.47, 0.0), (0.5, 0.47, 0.04)])
def test_level2_cannot_substitute_required_wall_for_thinned_background(background, minimum, fca):
    cfg = pitting_cfg()
    block = cfg["pitting_assessment"]
    block.update(t_min_in=minimum, fca_in=fca)
    block["grid"] = [[0.44 if cell == 0.3 else background for cell in row] for row in block["grid"]]
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert result["level2"]["rsf"] >= 0.9
    assert result["decision"]["verdict"] == "ESCALATE"
    assert "pitting.background_below_tmin" in result["decision"]["applicability"]["flags"]


def test_background_at_required_wall_does_not_block_level2():
    cfg = pitting_cfg()
    block = cfg["pitting_assessment"]
    block.update(t_min_in=0.47)
    block["grid"] = [[0.44 if cell == 0.3 else 0.47 for cell in row] for row in block["grid"]]
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert result["decision"]["verdict"] == "ACCEPT"
    assert "pitting.background_below_tmin" not in result["decision"]["applicability"]["flags"]


def test_grid_without_background_cannot_establish_lta_reference_wall():
    cfg = pitting_cfg()
    cfg["pitting_assessment"]["grid"] = [[0.3] * 3 for _ in range(3)]
    result = pitting.router(cfg)["api579_pitting_screen"]
    assert result["level2"]["rsf"] >= 0.9
    assert result["decision"]["verdict"] == "ESCALATE"
    assert "pitting.no_background" in result["decision"]["applicability"]["flags"]
