"""Workflow arithmetic and explicit evidence gaps; no invented chain capacity."""

from copy import deepcopy
import json
from pathlib import Path
import re

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


@pytest.mark.parametrize("region", ["bend", "fairlead_shoulder"])
@pytest.mark.parametrize(
    "changes,visual,flag",
    [
        ({}, {"pitting": True}, "VISUAL_PITTING"),
        ({}, {"crack": None}, "VISUAL_CRACK"),
        ({"coverage_confirmed": False}, {}, "COVERAGE_NOT_CONFIRMED"),
        ({"uniform_loss_confirmed": False}, {}, "UNIFORM_LOSS_NOT_CONFIRMED"),
        ({"perpendicular_readings_confirmed": False}, {},
         "PERPENDICULAR_READINGS_NOT_CONFIRMED"),
        ({"grade": "R4S"}, {}, "UNSUPPORTED_CHAIN_SCOPE"),
    ],
)
def test_flags_cannot_mask_dimensional_exceedance(cfg, region, changes, visual, flag):
    """Hand: 80/100 gives 20% loss and 64% area despite evidence gaps."""
    inspection = cfg["mooring_chain_degradation"]["inspection"]
    inspection.update(changes)
    inspection["visual"].update(visual)
    inspection["readings"][0].update(
        region=region, diameter_a_mm=80, diameter_b_mm=80, uncertainty_mm=0
    )
    output = router(cfg)["mooring_chain_degradation"]
    result = output["result"]
    sample = result["dimensions"]["samples"][0]
    assert sample["status"] == "EXCEEDS_LIMITS"
    assert sample["diameter_loss_range_fraction"] == pytest.approx((0.2, 0.2))
    assert sample["area_ratio_range"] == pytest.approx((0.64, 0.64))
    assert flag in sample["applicability"]["flags"]
    assert "DIMENSIONAL_LIMIT_EXCEEDED" in result["decision"]["applicability"]["flags"]
    assert result["decision"]["verdict"] == "ESCALATE"
    assert result["next_action"].startswith("WITHDRAW")
    assert "20.0 to 20.0" in output["report_html"]


def test_multiple_readings_and_indeterminate_flag(cfg):
    """Hand: 92 +/- 1 mm straddles the 8% loss limit; do not average sites."""
    inspection = cfg["mooring_chain_degradation"]["inspection"]
    inspection["readings"].append({
        "location_id": "uncertain-shoulder", "region": "fairlead_shoulder",
        "diameter_a_mm": 92, "diameter_b_mm": 92, "uncertainty_mm": 1,
    })
    output = router(cfg)["mooring_chain_degradation"]
    result = output["result"]
    assert [s["status"] for s in result["dimensions"]["samples"]] == [
        "WITHIN_LIMITS", "INDETERMINATE",
    ]
    assert "DIMENSIONAL_LIMIT_INDETERMINATE" in result["decision"]["applicability"]["flags"]
    assert result["decision"]["verdict"] == "ESCALATE"
    assert "Location: bend" in output["report_html"]
    assert "Location: uncertain-shoulder" in output["report_html"]
    assert "7.0 to 9.0" in output["report_html"]


@pytest.mark.parametrize("unsupported_region", [False, True])
def test_report_out_of_scope_samples(cfg, unsupported_region):
    inspection = cfg["mooring_chain_degradation"]["inspection"]
    if unsupported_region:
        inspection["readings"][0]["region"] = "interlink"
    else:
        inspection["visual"]["pitting"] = True
    output = router(cfg)["mooring_chain_degradation"]
    sample = output["result"]["dimensions"]["samples"][0]
    assert sample["status"] == "OUT_OF_SCOPE"
    report = output["report_html"]
    assert "OUT_OF_SCOPE" in report
    assert "Diameter loss interval (%)" in report
    if unsupported_region:
        assert sample["diameter_loss_range_fraction"] is None
        assert "not evaluated" in report
    else:
        assert sample["diameter_loss_range_fraction"] == pytest.approx((0.02, 0.04))
        assert "2.0 to 4.0" in report


def test_report_inputs_criteria_and_method_revision(cfg):
    inspection = cfg["mooring_chain_degradation"]["inspection"]
    inspection["operator_limit_reference"] = "<operator criteria>"
    report = router(cfg)["mooring_chain_degradation"]["report_html"]
    for text in (
        "Inputs and criteria", "Nominal diameter (mm)</td><td>100.0",
        "Maximum diameter loss (%)</td><td>8.0",
        "Minimum area ratio (%)</td><td>80.0", "Operator limit reference",
        "&lt;operator criteria&gt;", "Diameter a (mm)</td><td>96.0",
        "Diameter b (mm)</td><td>98.0", "Uncertainty (mm)</td><td>1.0",
        "Diameter loss interval (%)</td><td>2.0 to 4.0",
        "Method revision", "2026-10-20",
    ):
        assert text in report
    assert "<operator criteria>" not in report
    assert "Initial issue" not in report


def test_configure_lookup_and_router(cfg, tmp_path, monkeypatch):
    """Use the real default/custom YAML lookup, without solver imports or writes."""
    import yaml
    from assetutilities.common.ApplicationManager import ConfigureApplicationInputs

    inputfile = tmp_path / "inspection.yml"
    inputfile.write_text(
        yaml.safe_dump({"basename": "mooring_chain_degradation", **cfg}),
        encoding="utf-8",
    )
    monkeypatch.chdir(tmp_path)
    manager = ConfigureApplicationInputs()
    configured = manager.unify_application_and_default_and_custom_yamls(
        None, "digitalmodel", "mooring_chain_degradation", {}, str(inputfile)
    )
    assert configured["basename"] == "mooring_chain_degradation"
    assert configured["default"]["config"]["overwrite"]["output"] is True
    assert configured["mooring_chain_degradation"]["inspection"] == cfg[
        "mooring_chain_degradation"
    ]["inspection"]
    assert router(configured)["mooring_chain_degradation"]["result"][
        "workflow_id"
    ] == "mooring-chain-degradation-screen"


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


@pytest.mark.parametrize("line_ending", ["\n", "\r\n"])
def test_worked_example(tmp_path, monkeypatch, line_ending):
    """Run the documented input, keeping the example and its answer executable."""
    import yaml

    document = Path(__file__).resolve().parents[2] / (
        "docs/domains/asset-integrity/mooring-chain-degradation-screen.md"
    )
    read_text = Path.read_text

    def read_checkout(path, *, encoding):
        assert encoding == "utf-8"
        return read_text(path, encoding=encoding).replace("\n", line_ending)

    monkeypatch.setattr(Path, "read_text", read_checkout)
    monkeypatch.chdir(tmp_path)
    text = document.read_text(encoding="utf-8")
    match = re.search(r"^```yaml\s*\r?\n(.*?)^```", text, re.MULTILINE | re.DOTALL)
    assert match is not None, "document must contain a YAML example"
    example = yaml.safe_load(match.group(1))
    result = router(example)["mooring_chain_degradation"]["result"]
    assert result["dimensions"]["samples"][0]["status"] == "WITHIN_LIMITS"
    assert result["decision"]["verdict"] == "ESCALATE"


def test_engine_dispatch_without_solver_imports(cfg):
    """Execute the actual added branch without importing unrelated solver routers.

    Full engine and full-suite imports are outside the no-licensed-solver rule;
    this test compiles only the mooring degradation dispatch arm from its AST.
    """
    import ast

    engine = Path(__file__).resolve().parents[2] / "src/digitalmodel/engine.py"
    tree = ast.parse(engine.read_text(encoding="utf-8"))
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
