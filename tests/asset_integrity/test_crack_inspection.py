# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Crack inspection intervals from independent synthetic constant-rate lives."""

import json
import math
import runpy
from dataclasses import replace
from pathlib import Path

import pytest

from digitalmodel.asset_integrity.inspection_planning import plan_crack_inspection
from digitalmodel.asset_integrity.inspection_planning import router
from digitalmodel.asset_integrity.crack_life import (
    assess_crack_life,
    plan_crack_from_fad,
    screen_leak_before_break,
)
from digitalmodel.asset_integrity.rbi_screening import (
    RBIParameters,
    screen_crack_plan,
)
from digitalmodel.fatigue.threshold_growth import GrowthLaw, integrate_growth


def growth(threshold=0, initial_m=0.001):
    """C=1e-11, K=10, m=3 => 1e-8 m/cycle; .004 m => 400000 cycles."""
    return integrate_growth(
        GrowthLaw(1e-11, 3, threshold, 0, 1, "Synthetic arithmetic fixture"),
        10,
        initial_m=initial_m,
        limit_m=0.005,
        steps=4,
    )


def test_growth_cycles_feed_half_life_rule():
    """400000/100000=4 years; half=2 years, below supplied cap of 3."""
    plan = plan_crack_inspection(
        growth(),
        cycles_per_year=100_000,
        code_max_interval_years=3,
        life_divisor=2,
        policy_basis="Synthetic inspection policy",
    )
    assert plan.remaining_life_years == pytest.approx(4)
    assert plan.half_life_interval_years == pytest.approx(2)
    assert plan.next_inspection_interval_years == pytest.approx(2)
    assert plan.screening_status == "pass"


def options():
    return dict(
        law=GrowthLaw(1e-11, 3, 0, 0, 1, "Synthetic arithmetic fixture"),
        delta_k=10,
        initial_depth_mm=1,
        steps=4,
        cycles_per_year=100_000,
        code_max_interval_years=3,
        life_divisor=2,
        policy_basis="Synthetic inspection policy",
    )


def test_assessment_limit_and_wall_use_first_event():
    """K=10 gives 1e-8 m/cycle; 1 to 3 mm => 200000 cycles=2 yr."""
    result = assess_crack_life(
        **options(),
        critical_depth_mm=5,
        wall_thickness_mm=3,
        critical_depth_basis="Synthetic depth limit",
    )
    assert result.limiting_depth_mm == 3
    assert result.governing_limit == "WALL_PENETRATION"
    assert result.growth.life_cycles == pytest.approx(200_000)
    assert result.inspection.next_inspection_interval_years == pytest.approx(1)


def test_fad_zero_limit_requires_immediate_action():
    result = assess_crack_life(
        **options(),
        critical_depth_mm=0,
        wall_thickness_mm=10,
        critical_depth_basis="No acceptable depth established",
    )
    assert result.inspection.remaining_life_years == 0
    assert result.inspection.next_inspection_interval_years == 0
    assert result.inspection.screening_status == "fail"


def test_real_fad_limit_feeds_growth_without_extrapolation():
    """Integration identity: constant rate => life=(limit-initial)/1e-8.

    The FAD endpoint is tested as an interface, not a new FAD validation.
    Existing test_crack_fad.py supplies that module's independent tests.
    """
    result = plan_crack_from_fad(
        fad_inputs=dict(
            length_mm=60,
            t_mm=20,
            sigma_m_mpa=250,
            sigma_y_mpa=350,
            sigma_u_mpa=500,
            k_mat=100,
        ),
        **options(),
    )
    from digitalmodel.asset_integrity.assessment.crack_fad import assess_crack_like_flaw

    # The FAD consumes c = length_mm / 2: the total surface length is 2c=60 mm.
    inputs = dict(c_mm=30, t_mm=20, sigma_m_mpa=250,
                  sigma_y_mpa=350, sigma_u_mpa=500, k_mat=100)
    at_limit = assess_crack_like_flaw(a_mm=result.limiting_depth_mm, **inputs)
    beyond = assess_crack_like_flaw(a_mm=result.limiting_depth_mm + 0.01, **inputs)
    assert at_limit.acceptable
    assert at_limit.kr <= at_limit.f_lr
    assert not beyond.acceptable
    assert result.limiting_depth_mm == pytest.approx(9.777, abs=0.01)
    assert result.governing_limit == "ASSESSMENT_LIMIT"
    expected = (result.limiting_depth_mm - 1) / 1000 / 1e-8
    assert result.growth.life_cycles == pytest.approx(expected)
    assert "validity" in result.critical_depth_basis


def test_rbi_uses_crack_life_and_never_extends_interval():
    plan = plan_crack_inspection(
        growth(),
        cycles_per_year=100_000,
        code_max_interval_years=3,
        life_divisor=2,
        policy_basis="Synthetic inspection policy",
    )
    result = screen_crack_plan(
        plan,
        "C",
        component_id="synthetic-component",
        params=RBIParameters(),
    )
    assert result.pof_subscores["life_ratio"] == 5  # 4/5 < final edge 1
    assert result.pof_subscores["corrosion_trend_bump"] is None
    assert result.recommended_interval_yr <= plan.next_inspection_interval_years
    assert "corrosion_rate_mm_yr" not in result.inputs


def test_lbb_missing_evidence_is_not_evaluated():
    result = screen_leak_before_break(through_wall=True)
    assert result.status == "NOT_EVALUATED"
    assert "leak-rate and detection qualification" in result.missing_evidence


def test_lbb_ordering_alone_does_not_establish_acceptance():
    result = screen_leak_before_break(
        through_wall=True,
        detectable_length_mm=10,
        critical_length_mm=30,
        length_margin=2,
        basis="Synthetic length margin, not a code criterion",
    )
    assert result.geometry_screen == "pass"
    assert result.status == "NOT_EVALUATED"


def test_lbb_length_margin_failure_is_reported():
    """Synthetic 2*20=40 mm exceeds supplied stable length 30 mm."""
    result = screen_leak_before_break(
        through_wall=True,
        detectable_length_mm=20,
        critical_length_mm=30,
        length_margin=2,
        basis="Synthetic length margin",
    )
    assert result.status == "fail"
    assert result.geometry_screen == "fail"


@pytest.mark.parametrize(
    "overrides",
    [
        {"cycles_per_year": 0},
        {"cycles_per_year": float("nan")},
        {"code_max_interval_years": -1},
        {"life_divisor": 1},
        {"policy_basis": " "},
    ],
)
def test_invalid_inspection_policy_rejected(overrides):
    inputs = dict(
        cycles_per_year=100_000,
        code_max_interval_years=3,
        life_divisor=2,
        policy_basis="Synthetic policy",
    )
    with pytest.raises(ValueError):
        plan_crack_inspection(growth(), **(inputs | overrides))


def test_arrest_matches_zero_corrosion_interval_semantics():
    """Null life and half-life, capped interval, under unchanged loading only."""
    plan = plan_crack_inspection(
        growth(threshold=10),
        cycles_per_year=100_000,
        code_max_interval_years=3,
        life_divisor=2,
        policy_basis="Synthetic policy",
    )
    assert plan.remaining_life_years is None
    assert plan.half_life_interval_years is None
    assert plan.next_inspection_interval_years == 3
    assert any("Arrest" in note for note in plan.notes)


def test_policy_cap_governs():
    plan = plan_crack_inspection(
        growth(),
        cycles_per_year=100_000,
        code_max_interval_years=1,
        life_divisor=2,
        policy_basis="Synthetic policy",
    )
    assert plan.next_inspection_interval_years == 1


def test_rbi_exhausted_life_remains_zero():
    plan = plan_crack_inspection(
        growth(initial_m=0.005),
        cycles_per_year=100_000,
        code_max_interval_years=3,
        life_divisor=2,
        policy_basis="Synthetic policy",
    )
    result = screen_crack_plan(
        plan,
        "E",
        component_id="synthetic-component",
        params=RBIParameters(),
    )
    assert result.recommended_interval_yr == 0
    assert result.pof_category == 5
    assert "crack" in result.governing
    assert "half-life" not in result.governing


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -1])
def test_invalid_critical_depth_rejected(value):
    with pytest.raises(ValueError):
        assess_crack_life(
            **options(),
            critical_depth_mm=value,
            wall_thickness_mm=10,
            critical_depth_basis="Synthetic depth limit",
        )


def test_fad_adapter_rejects_nonfinite_material_inputs():
    with pytest.raises(ValueError, match="finite"):
        plan_crack_from_fad(
            fad_inputs=dict(
                length_mm=60,
                t_mm=20,
                sigma_m_mpa=100,
                sigma_y_mpa=350,
                sigma_u_mpa=500,
                k_mat=float("nan"),
            ),
            **options(),
        )


def test_surface_flaw_cannot_establish_lbb():
    result = screen_leak_before_break(
        through_wall=False,
        detectable_length_mm=10,
        critical_length_mm=30,
        length_margin=2,
        basis="Synthetic margin",
    )
    assert result.status == result.geometry_screen == "NOT_EVALUATED"


@pytest.mark.parametrize("value", [0, -1, float("nan"), float("inf")])
def test_invalid_lbb_lengths_rejected(value):
    with pytest.raises(ValueError):
        screen_leak_before_break(through_wall=True, detectable_length_mm=value)


def test_existing_inspection_router_accepts_crack_growth(tmp_path):
    """K=100 sqrt(pi*a), m=2 => N=ln(5)/(1e-7*pi) by hand integration."""
    inputs = options()
    inputs.pop("delta_k")
    inputs["law"] = dict(
        coefficient=1e-11,
        exponent=2,
        threshold=0,
        stress_ratio=0,
        walker_exponent=1,
        basis="Synthetic integration fixture",
    )
    inputs.update(
        stress_range_mpa=100,
        geometry_factor=1,
        steps=100,
        critical_depth_mm=5,
        wall_thickness_mm=10,
        critical_depth_basis="Synthetic endpoint",
    )
    cfg = {
        "inspection_planning": {
            "crack_growth": inputs,
            "outputs": {"directory": str(tmp_path)},
        }
    }
    result = router(cfg)["inspection_planning"]
    expected_years = math.log(5) / (1e-7 * math.pi) / 100_000
    assert result["result"]["remaining_life_years"] == pytest.approx(expected_years)
    summary = tmp_path / "inspection_planning_summary.json"
    saved = json.loads(summary.read_text(encoding="utf-8"))
    assert b"\r" not in summary.read_bytes()
    assert result["summary_json"] == summary.as_posix()
    assert saved["result"] == result["result"]
    assert saved["crack_assessment"]["growth"]["status"] == "LIMIT_REACHED"


@pytest.mark.parametrize("steps", [1, 4, 100])
def test_coarse_router_growth_preserves_life_interval_and_error(steps, tmp_path):
    """Route the MIT hand integral through inspection policy and saved JSON."""
    inputs = dict(
        law=dict(coefficient=1e-11, exponent=3, threshold=0, stress_ratio=0,
                 walker_exponent=1, basis="Independent arithmetic fixture"),
        stress_range_mpa=950,
        geometry_factor=1,
        initial_depth_mm=0.1,
        critical_depth_mm=2.5,
        wall_thickness_mm=10,
        critical_depth_basis="Supplied arithmetic endpoint",
        steps=steps,
        cycles_per_year=1000,
        code_max_interval_years=2,
        life_divisor=2,
        policy_basis="Synthetic inspection policy",
    )
    payload = router({"inspection_planning": {
        "crack_growth": inputs,
        "outputs": {"directory": tmp_path.as_posix()},
    }})["inspection_planning"]
    saved = json.loads(Path(payload["summary_json"]).read_text(encoding="utf-8"))
    # Integrate a**(-3/2) directly, independently of the numerical kernel.
    expected_cycles = 160 / (1e-11 * 950**3 * math.pi**1.5)
    expected_years = expected_cycles / 1000
    for result in (payload, saved):
        plan = result["result"]
        growth_result = result["crack_assessment"]["growth"]
        assert plan["remaining_life_years"] == pytest.approx(expected_years, rel=2e-6)
        assert plan["next_inspection_interval_years"] == pytest.approx(
            expected_years / 2, rel=2e-6,
        )
        assert plan["next_inspection_interval_years"] < plan["code_max_interval_years"]
        assert result["screening_status"] == "pass"
        assert growth_result["status"] == "LIMIT_REACHED"
        assert growth_result["life_cycles"] == pytest.approx(expected_cycles, rel=2e-6)
        assert 0 <= growth_result["estimated_error_cycles"] <= (
            1e-6 + 1e-6 * growth_result["life_cycles"]
        )
        assert growth_result["history"][-1]["cycles"] == growth_result["life_cycles"]
        assert growth_result["history"][-1]["depth_m"] == 0.0025


def test_worked_example_matches_independent_published_integral():
    """MIT Table 1 / Problem 6: direct integral, not the implementation output."""
    root = Path(__file__).resolve().parents[2]
    script = root / "examples/workflows/crack-growth-inspection/run_example.py"
    example = runpy.run_path(str(script))["build_example"]()
    expected = 160 / (1e-11 * 950**3 * math.pi**1.5)
    assert example["published_life_cycles"] == pytest.approx(expected, rel=2e-6)
    assert example["inspection"]["next_inspection_interval_years"] == pytest.approx(
        expected / 1000 / 2,
        rel=2e-6,
    )
    assert example["lbb"]["status"] == "NOT_EVALUATED"
    # Null life is JSON null, never NaN or Infinity in a saved payload.
    json.dumps(example, allow_nan=False)


def test_ordinary_thickness_router_remains_compatible(tmp_path):
    """Hand calculation: (8-6)/.5=4 years, half-life=2 below cap 3."""
    result = router(
        {
            "inspection_planning": {
                "thickness": {"current_mm": 8, "required_mm": 6},
                "corrosion": {"rate_mm_per_year": 0.5},
                "design": {"code_max_interval_years": 3},
                "outputs": {"directory": str(tmp_path)},
            }
        }
    )["inspection_planning"]["result"]
    assert result["remaining_life_years"] == 4
    assert result["next_inspection_interval_years"] == 2


def test_router_rejects_ambiguous_damage_inputs():
    with pytest.raises(ValueError, match="both"):
        router({"inspection_planning": {"crack_growth": {}, "thickness": {}}})


def test_crack_rbi_basis_uses_only_applied_policy():
    inputs = dict(cycles_per_year=100_000, code_max_interval_years=3,
                  life_divisor=4, policy_basis="Synthetic inspection policy")
    plan = plan_crack_inspection(growth(), **inputs)
    result = screen_crack_plan(plan, "C", component_id="synthetic-component",
                               params=RBIParameters())
    basis = "\n".join(result.basis)
    assert "plan maximum interval: 3" in basis
    assert "life divisor: 4" in basis
    assert "100000" in basis
    assert "remaining-life ratio band edges" in basis
    assert "LOW=10" in basis and "HIGH=2" in basis
    assert "PoF x CoF" in basis
    assert "Synthetic inspection policy" in basis
    assert "rsf" not in basis and "corrosion" not in basis
    assert "API 510/570/653" not in basis
    assert "code maximum interval parameter: 10" not in basis
    # Constant rate gives 4 yr; divisor 4 gives 1 yr, below both caps.
    assert result.recommended_interval_yr == pytest.approx(1)
    assert result.inputs["life_divisor"] == 4
    assert result.inputs["cycles_per_year"] == 100_000


@pytest.mark.parametrize("name", ["cycles_per_year", "code_max_interval_years",
                                 "life_divisor"])
@pytest.mark.parametrize("value", [True, False])
def test_boolean_inspection_inputs_rejected(name, value):
    inputs = dict(cycles_per_year=100_000, code_max_interval_years=3,
                  life_divisor=2, policy_basis="Synthetic policy")
    with pytest.raises(ValueError):
        plan_crack_inspection(growth(), **(inputs | {name: value}))


@pytest.mark.parametrize("name", ["initial_depth_mm", "critical_depth_mm",
                                 "wall_thickness_mm"])
@pytest.mark.parametrize("value", [True, False])
def test_boolean_assessment_depths_rejected(name, value):
    inputs = options() | dict(critical_depth_mm=5, wall_thickness_mm=10,
                              critical_depth_basis="Synthetic endpoint")
    with pytest.raises(ValueError):
        assess_crack_life(**(inputs | {name: value}))


@pytest.mark.parametrize("name", ["detectable_length_mm", "critical_length_mm",
                                 "length_margin"])
@pytest.mark.parametrize("value", [True, False])
def test_boolean_lbb_inputs_rejected(name, value):
    inputs = dict(through_wall=True, detectable_length_mm=10,
                  critical_length_mm=30, length_margin=2, basis="Synthetic margin")
    with pytest.raises(ValueError):
        screen_leak_before_break(**(inputs | {name: value}))


@pytest.mark.parametrize("name", ["length_mm", "sigma_m_mpa", "e_mpa"])
def test_boolean_fad_inputs_rejected(name):
    inputs = dict(length_mm=60, t_mm=20, sigma_m_mpa=100, sigma_y_mpa=350,
                  sigma_u_mpa=500, k_mat=100)
    with pytest.raises(ValueError):
        plan_crack_from_fad(fad_inputs=inputs | {name: True}, **options())


@pytest.mark.parametrize("name", ["stress_range_mpa", "geometry_factor"])
def test_boolean_router_inputs_rejected(name, tmp_path):
    inputs = options()
    inputs.pop("delta_k")
    inputs.update(law=dict(coefficient=1e-11, exponent=2, threshold=0,
                           stress_ratio=0, walker_exponent=1, basis="Synthetic law"),
                  stress_range_mpa=100, geometry_factor=1, critical_depth_mm=5,
                  wall_thickness_mm=10, critical_depth_basis="Synthetic endpoint")
    with pytest.raises(ValueError):
        router({"inspection_planning": {"crack_growth": inputs | {name: True},
                                        "outputs": {"directory": tmp_path.as_posix()}}})


@pytest.mark.parametrize("life", [True, float("nan"), float("inf"), -1])
def test_invalid_growth_life_never_passes_inspection(life):
    with pytest.raises(ValueError):
        plan_crack_inspection(replace(growth(), life_cycles=life),
                              cycles_per_year=100_000, code_max_interval_years=3,
                              life_divisor=2, policy_basis="Synthetic policy")


def test_crack_rbi_rejects_unexplained_null_life():
    plan = plan_crack_inspection(growth(), cycles_per_year=100_000,
                                 code_max_interval_years=3, life_divisor=2,
                                 policy_basis="Synthetic policy")
    with pytest.raises(ValueError):
        screen_crack_plan(replace(plan, remaining_life_years=None), "C",
                           component_id="synthetic-component", params=RBIParameters())


@pytest.mark.parametrize("changes", [
    {"reference_interval_yr": float("nan")},
    {"life_ratio_edges": (8, 4, 2, float("nan"))},
    {"band_interval_cap_yr": {"LOW": 10, "MEDIUM": 6,
                               "MEDIUM-HIGH": 4, "HIGH": float("nan")}},
])
def test_crack_rbi_rejects_invalid_policy(changes):
    plan = plan_crack_inspection(growth(), cycles_per_year=100_000,
                                 code_max_interval_years=3, life_divisor=2,
                                 policy_basis="Synthetic policy")
    with pytest.raises(ValueError):
        screen_crack_plan(plan, "C", component_id="synthetic-component",
                           params=replace(RBIParameters(), **changes))


@pytest.mark.parametrize("delta_k", [True, "invalid", -1, float("nan")])
def test_zero_depth_limit_still_validates_delta_k(delta_k):
    with pytest.raises(ValueError):
        assess_crack_life(**(options() | {"delta_k": delta_k}), critical_depth_mm=0,
                          wall_thickness_mm=10, critical_depth_basis="Synthetic limit")


def test_zero_depth_limit_requires_growth_law():
    with pytest.raises(ValueError):
        assess_crack_life(**(options() | {"law": None}), critical_depth_mm=0,
                          wall_thickness_mm=10, critical_depth_basis="Synthetic limit")


@pytest.mark.parametrize("basis", [True, None, 1, []])
def test_basis_fields_reject_nontext(basis):
    with pytest.raises(ValueError):
        assess_crack_life(**options(), critical_depth_mm=5, wall_thickness_mm=10,
                          critical_depth_basis=basis)
    with pytest.raises(ValueError):
        plan_crack_inspection(growth(), cycles_per_year=100_000,
                              code_max_interval_years=3, life_divisor=2,
                              policy_basis=basis)
    if basis is not None:
        with pytest.raises(ValueError):
            screen_leak_before_break(through_wall=True, detectable_length_mm=10,
                                     critical_length_mm=30, length_margin=2, basis=basis)


@pytest.mark.parametrize("name", ["length_mm", "t_mm", "sigma_m_mpa",
                                 "sigma_y_mpa", "sigma_u_mpa"])
@pytest.mark.parametrize("missing", [False, True])
def test_required_fad_inputs_raise_value_error(name, missing):
    inputs = dict(length_mm=60, t_mm=20, sigma_m_mpa=100, sigma_y_mpa=350,
                  sigma_u_mpa=500, k_mat=100)
    if missing:
        inputs.pop(name)
    else:
        inputs[name] = None
    with pytest.raises(ValueError):
        plan_crack_from_fad(fad_inputs=inputs, **options())


@pytest.mark.parametrize("name", ["sigma_b_mpa", "e_mpa", "tol_mm",
                                 "max_depth_fraction"])
def test_supplied_fad_controls_cannot_be_null(name):
    inputs = dict(length_mm=60, t_mm=20, sigma_m_mpa=100, sigma_y_mpa=350,
                  sigma_u_mpa=500, k_mat=100)
    with pytest.raises(ValueError):
        plan_crack_from_fad(fad_inputs=inputs | {name: None}, **options())


def test_missing_crack_rbi_band_cap_raises_value_error():
    plan = plan_crack_inspection(growth(), cycles_per_year=100_000,
                                 code_max_interval_years=3, life_divisor=2,
                                 policy_basis="Synthetic policy")
    params = replace(RBIParameters(), band_interval_cap_yr={"LOW": 10})
    with pytest.raises(ValueError):
        screen_crack_plan(plan, "C", component_id="synthetic-component", params=params)


@pytest.mark.parametrize("value", [True, 1.5, float("nan")])
def test_crack_consequence_requires_integer_category(value):
    plan = plan_crack_inspection(growth(), cycles_per_year=100_000,
                                 code_max_interval_years=3, life_divisor=2,
                                 policy_basis="Synthetic policy")
    with pytest.raises(ValueError):
        screen_crack_plan(plan, value, component_id="synthetic-component",
                           params=RBIParameters())


def test_initial_depth_beyond_wall_is_rejected():
    with pytest.raises(ValueError):
        assess_crack_life(**(options() | {"initial_depth_mm": 11}),
                          critical_depth_mm=5, wall_thickness_mm=10,
                          critical_depth_basis="Synthetic endpoint")


@pytest.mark.parametrize("stresses", [
    {"sigma_m_mpa": -100}, {"sigma_b_mpa": -1},
    {"sigma_m_mpa": 0, "sigma_b_mpa": 0},
])
def test_fad_adapter_rejects_unsupported_stress_signs(stresses):
    inputs = dict(length_mm=60, t_mm=20, sigma_m_mpa=100,
                  sigma_y_mpa=350, sigma_u_mpa=500, k_mat=100)
    with pytest.raises(ValueError):
        plan_crack_from_fad(fad_inputs=inputs | stresses, **options())


@pytest.mark.parametrize("stresses", [
    {"sigma_m_mpa": 100, "sigma_b_mpa": 0},
    {"sigma_m_mpa": 0, "sigma_b_mpa": 100},
])
def test_fad_adapter_allows_zero_one_component_with_tensile_loading(stresses):
    result = plan_crack_from_fad(
        fad_inputs=dict(length_mm=60, t_mm=20, sigma_y_mpa=350,
                        sigma_u_mpa=500, k_mat=100) | stresses, **options())
    assert result.limiting_depth_mm > 0


@pytest.mark.parametrize("changes", [
    {"cycles_per_year": 0}, {"life_divisor": 1},
    {"code_max_interval_years": -1}, {"policy_basis": None},
])
def test_invalid_policy_is_rejected_before_integration(monkeypatch, changes):
    def unexpected_integration(*args, **kwargs):
        pytest.fail("Invalid policy reached integration")

    monkeypatch.setattr("digitalmodel.asset_integrity.crack_life.integrate_growth",
                        unexpected_integration)
    with pytest.raises(ValueError):
        assess_crack_life(**(options() | changes), critical_depth_mm=5,
                          wall_thickness_mm=10, critical_depth_basis="Synthetic endpoint")


@pytest.mark.parametrize("changes", [
    {"life_divisor": 1}, {"policy_basis": None},
    {"remaining_life_years": 0, "remaining_life_cycles": 0,
     "screening_status": "pass", "next_inspection_interval_years": 1},
    {"next_inspection_interval_years": 3}, {"half_life_interval_years": 3},
    {"remaining_life_years": 5},
])
def test_crack_rbi_rejects_inconsistent_mutable_plan(changes):
    plan = plan_crack_inspection(growth(), cycles_per_year=100_000,
                                 code_max_interval_years=3, life_divisor=2,
                                 policy_basis="Synthetic policy")
    with pytest.raises(ValueError):
        screen_crack_plan(replace(plan, **changes), "C", component_id="synthetic-component",
                           params=RBIParameters())


@pytest.mark.parametrize("inputs", [None, {}, {"law": None},
                                   {"law": {"unknown": 1}}])
def test_malformed_router_inputs_raise_value_error(inputs, tmp_path):
    with pytest.raises(ValueError):
        router({"inspection_planning": {"crack_growth": inputs,
                                        "outputs": {"directory": tmp_path.as_posix()}}})


def test_fad_adapter_rejects_mixed_endpoint_inputs():
    with pytest.raises(ValueError):
        plan_crack_from_fad(
            fad_inputs=dict(length_mm=60, t_mm=20, sigma_m_mpa=100,
                            sigma_y_mpa=350, sigma_u_mpa=500, k_mat=100),
            **options(), critical_depth_mm=5)


@pytest.mark.parametrize("case", ["law_not_mapping", "unknown_law_key",
                                 "missing_depth", "mixed_endpoints", "unknown_input"])
def test_router_checks_complete_contract(case, tmp_path):
    inputs = options()
    inputs.pop("delta_k")
    inputs.update(law=dict(coefficient=1e-11, exponent=2, threshold=0, stress_ratio=0,
                           walker_exponent=1, basis="Synthetic law"),
                  stress_range_mpa=100, geometry_factor=1, critical_depth_mm=5,
                  wall_thickness_mm=10, critical_depth_basis="Synthetic endpoint")
    if case == "law_not_mapping":
        inputs["law"] = None
    elif case == "unknown_law_key":
        inputs["law"]["unknown"] = 1
    elif case == "missing_depth":
        inputs.pop("initial_depth_mm")
    elif case == "mixed_endpoints":
        inputs["fad_inputs"] = {}
    else:
        inputs["unknown"] = 1
    with pytest.raises(ValueError):
        router({"inspection_planning": {"crack_growth": inputs,
                                        "outputs": {"directory": tmp_path.as_posix()}}})
