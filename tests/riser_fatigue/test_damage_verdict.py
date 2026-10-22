# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent arithmetic for the usage-factor definition, not a code table.

Synthetic caller inputs: F=4, allowable=1/4=0.25. D=0.2 gives U=0.8 (pass);
D=0.25 gives U=1 (boundary pass); D=0.3 gives U=1.2 (fail). This is the
existing touchdown.py step-5 contract, with no standards adequacy claim.
"""
import pytest

from digitalmodel.riser_fatigue.workflow import fatigue_damage_verdict


@pytest.mark.parametrize("damage,usage,passes", [
    (0.0, 0.0, True), (0.2, 0.8, True), (0.25, 1.0, True), (0.3, 1.2, False),
])
def test_independent_damage_verdict(damage, usage, passes):
    result = fatigue_damage_verdict(damage, 4.0)
    assert result["allowable_damage"] == 0.25
    assert result["usage_factor"] == pytest.approx(usage)
    assert result["passes_dff"] is passes


@pytest.mark.parametrize("damage,factor", [
    (-1, 4), (float("nan"), 4), (float("inf"), 4),
    (0.2, 0), (0.2, -1), (0.2, float("nan")), (0.2, float("inf")),
])
def test_invalid_verdict_inputs(damage, factor):
    with pytest.raises(ValueError):
        fatigue_damage_verdict(damage, factor)


def test_router_propagates_class_factor_and_boundary_verdict(tmp_path, monkeypatch):
    """Injected contributions 1/8 + 1/8 = 1/4; F=4 gives U=1, a pass.

    Injection isolates the criteria path from S-N and load-history assumptions.
    The router still resolves metadata, accumulates contributions and writes CSVs.
    """
    from pathlib import Path

    from digitalmodel.riser_fatigue import workflow

    fixture = Path(__file__).resolve().parents[1] / "citations" / "fixtures"
    original = workflow.get_riser_dff
    monkeypatch.setattr(workflow, "get_riser_dff", lambda **kw: original(
        **{**kw, "repo_root": fixture}
    ))
    monkeypatch.setattr(workflow, "_wave_damage", lambda *args: ([{"damage": 0.125}], 0.125))
    monkeypatch.setattr(workflow, "_viv_damage", lambda *args: ([{"damage": 0.125}], 0.125))
    cfg = {"riser_fatigue": {
        "safety_class": "normal", "class_dff": 4.0,
        "design_life_years": 1.0, "output_dir": str(tmp_path),
        "segments": [{"id": "synthetic"}],
    }}
    result = workflow.router(cfg)["riser_fatigue"]
    assert result["dff"] == 4.0
    assert result["screening_status"] == "pass"
    assert result["segments"][0]["total_damage"] == 0.25
    assert result["segments"][0]["usage_factor"] == 1.0
    assert result["segments"][0]["allowable_damage"] == 0.25
    assert Path(result["results_csv"]).is_file()
    assert Path(result["summary_csv"]).is_file()
