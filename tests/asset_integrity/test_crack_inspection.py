# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Crack inspection intervals from independent synthetic constant-rate lives."""

import json
import math
import runpy
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
            sigma_m_mpa=100,
            sigma_y_mpa=350,
            sigma_u_mpa=500,
            k_mat=100,
        ),
        **options(),
    )
    assert 1 < result.limiting_depth_mm < 20
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
    saved = json.loads((tmp_path / "inspection_planning_summary.json").read_text())
    assert saved["result"] == result["result"]
    assert saved["crack_assessment"]["growth"]["status"] == "LIMIT_REACHED"


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
