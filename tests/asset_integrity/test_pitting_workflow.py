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
    assert result["decision"]["verdict"] == "ESCALATE"
    assert "life 0.0" not in result["report_html"]


@pytest.mark.parametrize("wall,minimum,pitch,verdict", [
    (0.4, 0.35, 1.0, "ACCEPT"), (0.3, 0.35, 1.0, "ESCALATE"),
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
