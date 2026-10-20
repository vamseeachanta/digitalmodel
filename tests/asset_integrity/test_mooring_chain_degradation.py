"""Workflow arithmetic and explicit evidence gaps; no invented chain capacity."""

from copy import deepcopy
import json
from pathlib import Path

import pytest

from digitalmodel.asset_integrity.mooring_chain_degradation import router


@pytest.fixture
def cfg():
    """Synthetic inspection and operator limits; not standard acceptance values."""
    return {
        "mooring_chain_degradation": {
            "component_id": "chain-segment",
            "inspection": {
                "nominal_diameter_mm": 100.0,
                "application": "mobile_offshore_drilling_unit",
                "grade": "R3",
                "stud_attachment": "mechanical_and_other_end_fillet_weld",
                "min_area_ratio": 0.8,
                "max_diameter_loss_fraction": 0.08,
                "operator_limit_reference": "Synthetic arithmetic example only",
                "readings": [
                    {
                        "location_id": "bend",
                        "region": "bend",
                        "diameter_a_mm": 96.0,
                        "diameter_b_mm": 98.0,
                        "uncertainty_mm": 1.0,
                    }
                ],
                "uniform_loss_confirmed": True,
                "perpendicular_readings_confirmed": True,
                "coverage_confirmed": True,
                "visual": dict.fromkeys(
                    (
                        "crack",
                        "loose_stud",
                        "missing_stud",
                        "geometry_anomaly",
                        "pitting",
                    ),
                    False,
                ),
            },
        }
    }


def test_dimensions_and_gap_verdict(cfg):
    """Hand: mean=(96+98)/2=97; interval=[96,98]; area=[.9216,.9604].

    IACS Rec 38 rev 2 (2020), sections 5.2/5.4: perpendicular mean.
    Circular geometry gives (d/100)^2; it does not establish remaining MBL.
    """
    result = router(cfg)["mooring_chain_degradation"]["result"]
    sample = result["dimensions"]["samples"][0]
    assert sample["mean_diameter_range_mm"] == pytest.approx((96, 98))
    assert sample["area_ratio_range"] == pytest.approx((0.9216, 0.9604))
    assert sample["status"] == "WITHIN_LIMITS"
    assert result["decision"]["verdict"] == "ESCALATE"
    assert result["decision"]["margin"] is None
    assert result["remaining_mbl_kn"] is None
    assert result["remaining_fatigue_life_years"] is None
    for name in ("remaining_mbl", "pitting", "interlink_wear", "fatigue"):
        assert result["mechanisms"][name]["status"] == "NOT_EVALUATED"
        assert result["mechanisms"][name]["required_evidence"]
    json.dumps(result, allow_nan=False)


def test_fairlead_and_limit_exceedance(cfg):
    """Hand: mean=80; loss=20/100=.20 > .08; area=.64 < .8."""
    reading = cfg["mooring_chain_degradation"]["inspection"]["readings"][0]
    reading.update(
        region="fairlead_shoulder", diameter_a_mm=80, diameter_b_mm=80, uncertainty_mm=0
    )
    result = router(cfg)["mooring_chain_degradation"]["result"]
    assert result["dimensions"]["samples"][0]["status"] == "EXCEEDS_LIMITS"
    assert "DIMENSIONAL_LIMIT_EXCEEDED" in result["decision"]["applicability"]["flags"]
    assert "WITHDRAW" in result["next_action"]


@pytest.mark.parametrize(
    "defect", ["crack", "loose_stud", "missing_stud", "geometry_anomaly", "pitting"]
)
@pytest.mark.parametrize("observed", [True, None])
def test_defects_and_unknowns_require_review(cfg, defect, observed):
    """IACS Rec 38 (2020), sections 2.2.2/5.3/5.4: no clearance inferred."""
    cfg["mooring_chain_degradation"]["inspection"]["visual"][defect] = observed
    result = router(cfg)["mooring_chain_degradation"]["result"]
    assert result["decision"]["verdict"] == "ESCALATE"
    assert f"VISUAL_{defect.upper()}" in result["decision"]["applicability"]["flags"]


def test_report_escapes_input_and_preserves_config(cfg):
    cfg["mooring_chain_degradation"]["component_id"] = "<script>bad</script>"
    original = deepcopy(cfg)
    output = router(cfg)
    assert cfg == original
    report = output["mooring_chain_degradation"]["report_html"]
    assert "<script>bad</script>" not in report
    assert "&lt;script&gt;bad&lt;/script&gt;" in report
    assert "Revision history" in report
    assert "NOT_EVALUATED" in report
    assert "Remaining MBL" in report
    assert "RF=nan" not in report
    assert "92.2 to 96.0" in report


@pytest.mark.parametrize(
    "field,value",
    [
        ("nominal_diameter_mm", float("nan")),
        ("min_area_ratio", True),
        ("operator_limit_reference", ""),
    ],
)
def test_invalid_inputs(cfg, field, value):
    cfg["mooring_chain_degradation"]["inspection"][field] = value
    with pytest.raises(ValueError):
        router(cfg)


def test_unknown_input_not_silently_ignored(cfg):
    cfg["mooring_chain_degradation"]["inspection"]["fatigue_curve"] = "unqualified"
    with pytest.raises(ValueError):
        router(cfg)


def test_worked_example():
    """Run the documented input, keeping the example and its answer executable."""
    import yaml

    text = Path(
        "docs/domains/asset-integrity/mooring-chain-degradation-screen.md"
    ).read_text()
    example = yaml.safe_load(text.split("```yaml\n")[1].split("```")[0])
    result = router(example)["mooring_chain_degradation"]["result"]
    assert result["dimensions"]["samples"][0]["status"] == "WITHIN_LIMITS"
    assert result["decision"]["verdict"] == "ESCALATE"


def test_engine_dispatch_without_solver_imports(cfg):
    """Execute the actual added branch without importing unrelated solver routers.

    Full engine and full-suite imports are outside the no-licensed-solver rule;
    this test compiles only the mooring degradation dispatch arm from its AST.
    """
    import ast

    tree = ast.parse(Path("src/digitalmodel/engine.py").read_text())
    branches = [
        node
        for node in ast.walk(tree)
        if isinstance(node, ast.If)
        and ast.unparse(node.test) == "basename == 'mooring_chain_degradation'"
    ]
    assert len(branches) == 1, "engine must expose the mooring-chain workflow"
    namespace = {"cfg_base": cfg}
    isolated = ast.Module(body=branches[0].body, type_ignores=[])
    exec(compile(isolated, "engine_dispatch", "exec"), namespace)
    assert namespace["cfg_base"]["mooring_chain_degradation"]["result"][
        "workflow_id"
    ] == ("mooring-chain-degradation-screen")
